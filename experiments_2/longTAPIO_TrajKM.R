# longTAPIO_TrajKM: Ensemble of k-means on trajectory PCA + co-association consensus
# - Designed for perfectly aligned longitudinal trajectories
# - Avoids unstable per-tree PCA; uses global PCA for stability
# - Uses k-means as base learner (robust to noise via centroid updates)
# - Final Ward cut -> Lloyd refinement (k-means initialized from Ward)
#
# Arguments:
#  DATA        : data.frame / matrix where rows = stacked timepoints (subject1 time1..timeT, subject2 time1..timeT, ...)
#  user_id     : vector of subject ids aligned with rows of DATA
#  k           : integer number of clusters; if NA will choose by silhouette on co-assoc graph
#  n_trees     : number of ensemble k-means runs (random starts / bootstrap views)
#  n_init_kmeans: number of random starts inside each kmeans run (kmeans nstart)
#  n_pcs       : number of global PCA components to keep before k-means
#  p_feature_fraction : fraction of outcome-features to sample per tree (without replacement); 1 = all
#  do_smooth   : logical; apply light smoothing over time before flattening
#  smooth_k    : integer window for moving average smoothing
#  do_scale    : logical; z-scale flattened vectors per-subject
#  verbose     : logical
#  method      : linkage used for final hierarchical (Ward.D2 recommended)
#  max.k       : maximum k to consider when k is NA (silhouette search)
#
# Returns: list with cl, PART (list of co-association matrices for each tree), AFF, feature_importance, final_kmeans, hc
longTAPIO_TrajKM <- function(DATA,
                             user_id,
                             k = NA,
                             n_trees = 300,
                             n_init_kmeans = 5,
                             n_pcs = 10,
                             p_feature_fraction = 1.0,
                             do_smooth = FALSE,
                             smooth_k = 3,
                             do_scale = TRUE,
                             verbose = FALSE,
                             method = "ward.D2",
                             max.k = 10,
                             seed = 123){
  if (!is.matrix(DATA) && !is.data.frame(DATA)) stop("DATA must be matrix or data.frame")
  if (is.null(user_id)) stop("user_id required (aligned grid assumed)")
  set.seed(seed)

  # reorder by user_id (stable)
  ord <- order(user_id)
  DATAo <- as.data.frame(DATA[ord, , drop = FALSE])
  user_id_o <- user_id[ord]
  subj_ids <- unique(user_id_o)
  n_subj <- length(subj_ids)
  total_rows <- nrow(DATAo)
  n_time <- total_rows / n_subj
  if (abs(round(n_time) - n_time) > 1e-8) stop("Rows must be evenly divisible by number of subjects (aligned grid).")
  n_time <- as.integer(round(n_time))
  p <- ncol(DATAo) # number of outcome-dimensions per timepoint

  # Helper: flatten subject block (time x p) into vector (time rolled fastest)
  flatten_subject <- function(block_mat){
    as.numeric(t(block_mat)) # yields vector length = time * p
  }

  # Optional smoothing per feature (moving average)
  smooth_block <- function(block){
    if (!do_smooth) return(block)
    # block: n_time x p
    for (j in seq_len(ncol(block))){
      colj <- block[, j]
      # simple symmetric moving average; handle edges by using available values
      filt <- stats::filter(colj, rep(1/smooth_k, smooth_k), sides = 2)
      # replace NA edges by original values (or nearest non-NA)
      nas <- is.na(filt)
      filt[nas] <- colj[nas]
      block[, j] <- as.numeric(filt)
    }
    return(block)
  }

  # Build subject x (time*p) flattened matrix
  subj_mat <- matrix(NA, nrow = n_subj, ncol = n_time * p)
  for (s in seq_len(n_subj)){
    rows <- ((s-1)*n_time + 1):(s*n_time)
    block <- as.matrix(DATAo[rows, , drop = FALSE]) # n_time x p
    block <- smooth_block(block)
    subj_mat[s, ] <- flatten_subject(block)
  }

  # Optional scaling per subject (shape focus)
  if (do_scale){
    subj_mat <- t(apply(subj_mat, 1, function(v){
      v <- v - mean(v)
      sdv <- sd(v)
      if (sdv == 0) v else v / sdv
    }))
  }

  # Global PCA for dimensionality reduction (more stable than per-tree PCA)
  if (n_pcs > 0){
    pca_res <- stats::prcomp(subj_mat, center = TRUE, scale. = FALSE)
    n_pcs_use <- min(n_pcs, ncol(pca_res$x))
    scores_global <- pca_res$x[, 1:n_pcs_use, drop = FALSE] # n_subj x n_pcs_use
  } else {
    scores_global <- subj_mat
    n_pcs_use <- ncol(scores_global)
  }

  # Prepare ensemble: each tree runs kmeans on a view of the reduced data
  # Views: sample a subset of PCA columns (or features) without replacement OR use all
  # Build list of co-association matrices (PART)
  PART <- vector("list", n_trees)
  IMP  <- matrix(NA, nrow = n_trees, ncol = p) # approximate feature importance per tree (aggregate later)

  for (t in seq_len(n_trees)){
    # sample features to use in this tree (operate in PCA score space)
    if (p_feature_fraction >= 1) {
      cols_use <- seq_len(n_pcs_use)
    } else {
      m <- max(1, floor(p_feature_fraction * n_pcs_use))
      cols_use <- sample(seq_len(n_pcs_use), m, replace = FALSE)
    }

    view_data <- scores_global[, cols_use, drop = FALSE] # n_subj x m

    # Run kmeans as base learner (we don't know k yet; we run kmeans for a few candidate ks and keep inertia info later)
    # We'll run kmeans with many starts for robustness. We'll store the partition for each candidate k.
    # To keep it consistent with final k unknown, we instead run kmeans with a relatively large K_init and later
    # use co-association across partitions for a range of cluster numbers. But simpler and effective: ask user k or use max.k.
    # Here we run kmeans for k = 2:max.k and sum their co-association weighted by inverse inertia (lower inertia -> better)
    k_candidates <- 2:max(2, max.k)
    tree_assocs <- matrix(0, nrow = n_subj, ncol = n_subj)
    tree_inertia_sum <- 0
    # loop candidate ks and accumulate
    for (kc in k_candidates){
      km <- try(stats::kmeans(view_data, centers = kc, nstart = n_init_kmeans, iter.max = 50), silent = TRUE)
      if (inherits(km, "try-error")) next
      cl <- km$cluster
      # association matrix
      asoc <- outer(cl, cl, FUN = function(a,b) as.integer(a==b))
      # weight by inverse within-cluster sum-of-squares (total within inertia)
      inertia <- km$tot.withinss
      if (inertia <= 0) weight <- 1 else weight <- 1 / inertia
      tree_assocs <- tree_assocs + weight * asoc
      tree_inertia_sum <- tree_inertia_sum + weight
      # estimate feature importance by squared difference of centroids on original flattened space:
      # map centroids in PCA back to original space approx: centroid_orig = rotation[,1:m] %*% centroid_pca
    }

    if (tree_inertia_sum == 0){
      PART[[t]] <- matrix(0, nrow = n_subj, ncol = n_subj)
    } else {
      PART[[t]] <- tree_assocs / tree_inertia_sum
    }

    # approximate feature importance:
    # project mean squared loading back to original outcome-feature groups if p not too large
    # We'll compute importance on original flattened space via correlation to cluster labels (coarse)
    # Quick approximate: compute difference between group means for k = 2 (if possible)
    km2 <- try(stats::kmeans(view_data, centers = 2, nstart = n_init_kmeans, iter.max = 50), silent = TRUE)
    if (!inherits(km2, "try-error")){
      lab <- km2$cluster
      # compute per-original-feature (outcome) importance as absolute Cohen's d across groups averaged over timepoints
      feat_imp <- numeric(p); feat_imp[] <- 0
      # original subj-level flattened matrix subj_mat is n_subj x (n_time*p)
      # for each outcome feature j (1..p), positions are: pos = seq(j, n_time*p, by = p)
      for (j in seq_len(p)){
        pos <- seq(j, n_time * p, by = p)
        vals <- subj_mat[, pos, drop = FALSE]   # n_subj x n_time
        # compute mean difference across groups averaged over time
        m1 <- colMeans(vals[lab==1,, drop=FALSE]); m2 <- colMeans(vals[lab==2,, drop=FALSE])
        pooled_sd <- sqrt((var(as.vector(vals[lab==1,,drop=FALSE])) + var(as.vector(vals[lab==2,,drop=FALSE]))) / 2)
        if (pooled_sd == 0) feat_imp[j] <- 0 else feat_imp[j] <- mean(abs(m1 - m2)) / pooled_sd
      }
      IMP[t, ] <- feat_imp
    } else {
      IMP[t, ] <- NA
    }

    if (verbose && (t %% 50 == 0)) cat("Tree", t, "of", n_trees, "done\n")
  }

  # Combine PART into affinity matrix AFF
  AFF <- Reduce("+", PART)
  # normalize to [0,1]
  AFF <- AFF / max(AFF, na.rm = TRUE)
  DIST <- 1 - AFF

  # Final hierarchical clustering on DIST
  hc_final <- fastcluster::hclust(as.dist(DIST), method = method)

  # choose k if NA via silhouette on DIST
  if (is.na(k)){
    ks <- 2:max.k
    sils <- numeric(length(ks))
    for (i in seq_along(ks)){
      cl_try <- cutree(hc_final, ks[i])
      si <- cluster::silhouette(cl_try, as.dist(DIST))
      sils[i] <- mean(si[, 3])
    }
    best_idx <- which.max(sils)
    k <- ks[best_idx]
    if (verbose) cat("Selected k =", k, "by silhouette\n")
  }

  # get Ward clusters
  ward_cl <- cutree(hc_final, k)

  # Now do Lloyd refinement: run kmeans on the global PCA scores, initialized by ward centroids (mapped to PCA space)
  # compute initial centers in PCA score space: centers are mean of scores_global per ward cluster
  init_centers <- matrix(NA, nrow = k, ncol = ncol(scores_global))
  for (ci in 1:k){
    members <- which(ward_cl == ci)
    if (length(members) == 1) init_centers[ci, ] <- scores_global[members, ] else init_centers[ci, ] <- colMeans(scores_global[members, , drop = FALSE])
  }

  # run kmeans initialized (use stats::kmeans with algorithm default; to set initial centers use kmeans++ like approach:
  km_final <- stats::kmeans(scores_global, centers = init_centers, iter.max = 100, nstart = 1)

  final_cl <- km_final$cluster

  # Aggregate feature importance across trees (mean)
  feat_imp_mat <- IMP
  feat_imp_mean <- apply(feat_imp_mat, 2, function(x) mean(x, na.rm = TRUE))

  return(list(
    cl = final_cl,
    AFF = AFF,
    PART = PART,
    feature_importance = feat_imp_mean,
    DIST = DIST,
    hc = hc_final,
    kmeans = km_final
  ))
}
