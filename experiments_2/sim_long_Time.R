# ==============================================================================
# BENCHMARK
# longTAPIO_inductive vs clusterMLD vs kml3d
#
# FIXES:
#   * clusterMLD: fixed-K solution extracted from Cluster.Lists[[K]]
#   * easier but still non-trivial synthetic trajectories
#   * kml3d receives N x V x P array
#
# Scenarios:
#   1. standard
#   2. irregular
#   3. nuisance
#   4. imbalanced
#
# Evaluation:
#   ARI + NMI
# ==============================================================================

library(aricode)
library(clusterMLD)
library(kml3d)
library(longitudinalData)

# ==============================================================================
# SETTINGS
# ==============================================================================

N_RUNS <- 10
K <- 4

N_VISITS <- 10
TIME_MAX <- 10

N_TREES <- 500
LEVELS <- 4
PCA_SELECTION <- "random_weighted"

if (!exists("longTAPIO_inductive")) {
  stop("Please load/source longTAPIO_inductive() first.")
}


# ==============================================================================
# 1. SIMULATION
# ==============================================================================

simulate_benchmark <- function(
    scenario = c(
      "standard",
      "irregular",
      "nuisance",
      "imbalanced"
    ),
    seed = 1) {

  scenario <- match.arg(scenario)

  set.seed(seed)


  # ============================================================================
  # SCENARIO SETTINGS
  # ============================================================================

  if (scenario == "standard") {

    N <- 160
    P <- 5
    P_signal <- 5

    proportions <- rep(
      0.25,
      4
    )

    irregular <- FALSE


  } else if (scenario == "irregular") {

    N <- 160
    P <- 5
    P_signal <- 5

    proportions <- rep(
      0.25,
      4
    )

    irregular <- TRUE


  } else if (scenario == "nuisance") {

    N <- 160
    P <- 30
    P_signal <- 5

    proportions <- rep(
      0.25,
      4
    )

    irregular <- FALSE


  } else {

    N <- 160
    P <- 10
    P_signal <- 5

    proportions <- c(
      0.55,
      0.25,
      0.125,
      0.075
    )

    irregular <- FALSE
  }


  # ============================================================================
  # TRUE CLUSTERS
  # ============================================================================

  sizes <- floor(
    N * proportions
  )

  sizes[1] <- sizes[1] +
    N - sum(sizes)


  truth <- rep(
    1:K,
    times = sizes
  )


  truth <- sample(
    truth
  )


  # ============================================================================
  # SUBJECT HETEROGENEITY
  #
  # Easier than previous benchmark:
  #
  # old intercept SD = 0.15
  # new intercept SD = 0.10
  #
  # old scale SD = 0.08
  # new scale SD = 0.05
  #
  # old phase SD = 0.15
  # new phase SD = 0.10
  # ============================================================================

  subject_intercept <- rnorm(
    N,
    mean = 0,
    sd = 0.10
  )


  subject_scale <- rlnorm(
    N,
    meanlog = 0,
    sdlog = 0.05
  )


  phase <- rnorm(
    N,
    mean = 0,
    sd = 0.10
  )


  # ============================================================================
  # LATENT TRAJECTORIES
  #
  # More separation than before.
  # ============================================================================

  latent_curve <- function(
      t,
      cluster,
      phase = 0) {

    tt <- t + phase


    if (cluster == 1) {

      # ------------------------------------------------------------
      # Stable / weak increase
      # ------------------------------------------------------------

      value <-
        0.15 +
        0.015 * tt


    } else if (cluster == 2) {

      # ------------------------------------------------------------
      # Strong progressive trajectory
      # ------------------------------------------------------------

      value <-
        0.05 +
        0.095 * tt


    } else if (cluster == 3) {

      # ------------------------------------------------------------
      # Oscillating phenotype
      # ------------------------------------------------------------

      value <-
        0.48 +
        0.38 *
        sin(
          0.85 * tt - 0.60
        )


    } else {

      # ------------------------------------------------------------
      # Pronounced flare
      # ------------------------------------------------------------

      value <-
        0.12 +
        0.85 *
        exp(
          -0.5 *
          ((tt - 5.0) / 1.10)^2
        )
    }


    value
  }


  # ============================================================================
  # FEATURE PARAMETERS
  # ============================================================================

  signal_loadings <- seq(
    0.90,
    1.30,
    length.out = P_signal
  )


  feature_offsets <- seq(
    -0.10,
    0.10,
    length.out = P_signal
  )


  # ============================================================================
  # GENERATE LONGITUDINAL DATA
  # ============================================================================

  rows <- vector(
    "list",
    N
  )


  for (i in seq_len(N)) {


    # ==========================================================================
    # OBSERVATION TIMES
    # ==========================================================================

    if (!irregular) {

      ti <- seq(
        0,
        TIME_MAX,
        length.out = N_VISITS
      )


    } else {

      # Slightly less sparse than before:
      # previously 6-12 observations
      # now 7-12 observations

      n_i <- sample(
        7:12,
        size = 1
      )


      ti <- sort(
        runif(
          n_i,
          min = 0,
          max = TIME_MAX
        )
      )
    }


    # ==========================================================================
    # LATENT TRAJECTORY
    # ==========================================================================

    latent <- vapply(
      ti,
      function(tt) {

        latent_curve(
          t = tt,
          cluster = truth[i],
          phase = phase[i]
        )

      },
      numeric(1)
    )


    Xi <- matrix(
      NA_real_,
      nrow = length(ti),
      ncol = P
    )


    # ==========================================================================
    # SIGNAL VARIABLES
    #
    # Previous measurement noise: 0.10
    # New measurement noise:      0.07
    # ==========================================================================

    for (j in seq_len(P_signal)) {

      Xi[, j] <-
        feature_offsets[j] +
        subject_intercept[i] +
        subject_scale[i] *
        signal_loadings[j] *
        latent +
        rnorm(
          length(ti),
          mean = 0,
          sd = 0.07
        )
    }


    # ==========================================================================
    # NUISANCE VARIABLES
    # ==========================================================================

    if (P > P_signal) {

      for (j in (P_signal + 1):P) {

        frequency <-
          0.25 +
          0.03 * j


        random_phase <- runif(
          1,
          0,
          2 * pi
        )


        Xi[, j] <-
          0.08 *
          sin(
            frequency * ti +
            random_phase
          ) +
          rnorm(
            length(ti),
            mean = 0,
            sd = 0.20
          )
      }
    }


    # ==========================================================================
    # LONG FORMAT
    # ==========================================================================

    tmp <- data.frame(
      id = i,
      time = ti
    )


    for (j in seq_len(P)) {

      tmp[
        [paste0("y", j)]
      ] <- Xi[, j]
    }


    rows[[i]] <- tmp
  }


  dat <- do.call(
    rbind,
    rows
  )


  rownames(dat) <- NULL


  list(
    data = dat,
    truth = truth,
    N = N,
    P = P,
    P_signal = P_signal,
    irregular = irregular
  )
}


# ==============================================================================
# 2. WINDOW DISCRETIZATION
# ==============================================================================

window_discretize <- function(
    dat,
    n_windows = N_VISITS) {


  features <- grep(
    "^y[0-9]+$",
    names(dat),
    value = TRUE
  )


  ids <- sort(
    unique(dat$id)
  )


  breaks <- seq(
    0,
    TIME_MAX,
    length.out = n_windows + 1
  )


  centers <- (
    breaks[-1] +
    breaks[-length(breaks)]
  ) / 2


  N <- length(ids)
  P <- length(features)


  X <- array(
    NA_real_,
    dim = c(
      N,
      n_windows,
      P
    )
  )


  for (ii in seq_along(ids)) {


    d <- dat[
      dat$id == ids[ii],
      ,
      drop = FALSE
    ]


    win <- cut(
      d$time,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE,
      labels = FALSE
    )


    win[
      d$time >= TIME_MAX
    ] <- n_windows


    # --------------------------------------------------------------------------
    # WINDOW MEANS
    # --------------------------------------------------------------------------

    for (w in seq_len(n_windows)) {

      ind <- which(
        win == w
      )


      if (length(ind) > 0) {

        X[
          ii,
          w,
        ] <- colMeans(
          d[
            ind,
            features,
            drop = FALSE
          ],
          na.rm = TRUE
        )
      }
    }


    # --------------------------------------------------------------------------
    # INTERPOLATE EMPTY WINDOWS
    # --------------------------------------------------------------------------

    for (j in seq_len(P)) {


      y <- X[
        ii,
        ,
        j
      ]


      ok <- is.finite(
        y
      )


      if (sum(ok) >= 2) {


        X[
          ii,
          ,
          j
        ] <- approx(
          x = centers[ok],
          y = y[ok],
          xout = centers,
          rule = 2
        )$y


      } else if (sum(ok) == 1) {


        X[
          ii,
          ,
          j
        ] <- rep(
          y[ok][1],
          n_windows
        )


      } else {


        X[
          ii,
          ,
          j
        ] <- 0
      }
    }
  }


  list(
    X = X,
    ids = ids,
    time = centers,
    breaks = breaks
  )
}


# ==============================================================================
# 3. LONGTAPIO
# ==============================================================================

fit_longTAPIO <- function(
    disc,
    k = K) {


  X <- disc$X


  N <- dim(X)[1]
  V <- dim(X)[2]
  P <- dim(X)[3]


  # ============================================================================
  # ARRAY -> LONG FORMAT
  # ============================================================================

  DATA <- matrix(
    NA_real_,
    nrow = N * V,
    ncol = P
  )


  id <- rep(
    seq_len(N),
    each = V
  )


  for (i in seq_len(N)) {


    rows <- (
      (i - 1) * V + 1
    ):(
      i * V
    )


    DATA[
      rows,
    ] <- X[
      i,
      ,
    ]
  }


  DATA <- as.data.frame(
    DATA
  )


  names(DATA) <- paste0(
    "y",
    seq_len(P)
  )


  # ============================================================================
  # FEATURES PER TREE
  # ============================================================================

  n_features_tree <- max(
    2,
    ceiling(
      sqrt(P)
    )
  )


  # ============================================================================
  # FIT
  # ============================================================================

  fit <- longTAPIO_inductive(
    DATA = DATA,
    user_id = id,
    k = k,
    n_features = n_features_tree,
    n_trees = N_TREES,
    levels = LEVELS,
    method = "ward.D2",
    scale = TRUE,
    replace = TRUE,
    pca_selection = PCA_SELECTION
  )


  # ============================================================================
  # EXTRACT TRAINING PARTITION
  # ============================================================================

  candidates <- c(
    "train_cluster",
    "train_clusters",
    "cluster",
    "clusters",
    "clustering",
    "labels",
    "membership"
  )


  for (nm in candidates) {


    z <- fit[[nm]]


    if (
      !is.null(z) &&
      length(z) == N
    ) {

      return(
        as.integer(z)
      )
    }
  }


  if (
    !is.null(fit$result) &&
    is.list(fit$result)
  ) {


    for (nm in candidates) {


      z <- fit$result[[nm]]


      if (
        !is.null(z) &&
        length(z) == N
      ) {

        return(
          as.integer(z)
        )
      }
    }
  }


  stop(
    paste0(
      "Could not extract longTAPIO partition.\n",
      "Available components: ",
      paste(
        names(fit),
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# 4. CLUSTERMLD
# ==============================================================================

# ------------------------------------------------------------------------------
# IMPORTANT:
#
# Cluster.res is NOT necessarily the fixed-K solution.
#
# According to clusterMLD documentation:
#
#   Cluster.res = solution selected by Gap_b
#
#   Cluster.Lists = complete hierarchy
#
# Therefore:
#
#   Cluster.Lists[[4]]
#
# is used for a fixed K=4 benchmark.
# ------------------------------------------------------------------------------


cluster_list_to_labels <- function(
    cluster_list,
    ids) {


  labels <- rep(
    NA_integer_,
    length(ids)
  )


  names(labels) <- as.character(
    ids
  )


  for (g in seq_along(
    cluster_list
  )) {


    members <-
      cluster_list[[g]]


    # members should be subject IDs

    labels[
      as.character(members)
    ] <- g
  }


  if (anyNA(labels)) {

    stop(
      paste0(
        "clusterMLD fixed-K partition did not assign ",
        sum(is.na(labels)),
        " subjects."
      )
    )
  }


  as.integer(
    labels
  )
}


fit_clusterMLD <- function(
    dat,
    k = K,
    verbose = FALSE) {


  features <- grep(
    "^y[0-9]+$",
    names(dat),
    value = TRUE
  )


  ids <- sort(
    unique(dat$id)
  )


  # ============================================================================
  # FIT
  #
  # Original irregular observation times are retained.
  # ============================================================================

  fit <- clusterMLD::LongDataCluster(
    x = dat$time,
    Y = dat[
      ,
      features,
      drop = FALSE
    ],
    id = dat$id,
    No.Class = k,
    parallel = FALSE
  )


  # ============================================================================
  # CHECK HIERARCHY
  # ============================================================================

  if (is.null(
    fit$Cluster.Lists
  )) {

    stop(
      "clusterMLD did not return Cluster.Lists."
    )
  }


  if (
    length(
      fit$Cluster.Lists
    ) < k
  ) {

    stop(
      paste0(
        "clusterMLD hierarchy only contains ",
        length(
          fit$Cluster.Lists
        ),
        " levels; cannot extract K=",
        k,
        "."
      )
    )
  }


  # ============================================================================
  # FIXED-K PARTITION
  # ============================================================================

  fixed_k <-
    fit$Cluster.Lists[[k]]


  # ============================================================================
  # SANITY CHECK
  #
  # Cluster.Lists[[k]] should itself contain k clusters.
  # ============================================================================

  if (
    !is.list(
      fixed_k
    )
  ) {

    stop(
      "clusterMLD Cluster.Lists[[k]] is not a list."
    )
  }


  if (
    length(
      fixed_k
    ) != k
  ) {

    stop(
      paste0(
        "Expected ",
        k,
        " clusters from Cluster.Lists[[",
        k,
        "]], but obtained ",
        length(
          fixed_k
        ),
        "."
      )
    )
  }


  labels <- cluster_list_to_labels(
    cluster_list = fixed_k,
    ids = ids
  )


  # ============================================================================
  # DIAGNOSTIC INFORMATION
  # ============================================================================

  if (verbose) {


    cat(
      "\nclusterMLD diagnostics:\n"
    )


    cat(
      "  requested K : ",
      k,
      "\n",
      sep = ""
    )


    cat(
      "  Gap_b K     : ",
      fit$No.Gapb,
      "\n",
      sep = ""
    )


    cat(
      "  CH K        : ",
      fit$No.CH,
      "\n",
      sep = ""
    )


    cat(
      "  fixed K used: ",
      length(
        fixed_k
      ),
      "\n",
      sep = ""
    )


    cat(
      "  cluster sizes: ",
      paste(
        table(labels),
        collapse = ", "
      ),
      "\n",
      sep = ""
    )


    cat(
      "  weights: ",
      paste(
        round(
          fit$weight,
          3
        ),
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }


  labels
}


# ==============================================================================
# 5. KML3D
# ==============================================================================

make_cld3d <- function(
    X,
    time) {


  # ============================================================================
  # cld3d
  # ============================================================================

  if (
    exists(
      "cld3d",
      envir = asNamespace(
        "kml3d"
      ),
      inherits = FALSE
    )
  ) {


    fun <- get(
      "cld3d",
      envir = asNamespace(
        "kml3d"
      )
    )


    return(
      fun(
        traj = X,
        time = time
      )
    )
  }


  # ============================================================================
  # longitudinalData internal constructor
  # ============================================================================

  if (
    exists(
      "clusterLongData3d",
      envir = asNamespace(
        "longitudinalData"
      ),
      inherits = FALSE
    )
  ) {


    fun <- get(
      "clusterLongData3d",
      envir = asNamespace(
        "longitudinalData"
      )
    )


    return(
      fun(
        traj = X,
        time = time
      )
    )
  }


  # ============================================================================
  # kml3d internal constructor
  # ============================================================================

  if (
    exists(
      "clusterLongData3d",
      envir = asNamespace(
        "kml3d"
      ),
      inherits = FALSE
    )
  ) {


    fun <- get(
      "clusterLongData3d",
      envir = asNamespace(
        "kml3d"
      )
    )


    return(
      fun(
        traj = X,
        time = time
      )
    )
  }


  stop(
    "Could not find kml3d constructor."
  )
}


fit_kml3d <- function(
    disc,
    k = K) {


  X <- disc$X

  N <- dim(X)[1]


  # ============================================================================
  # X = subjects x visits x outcomes
  # ============================================================================

  cld <- make_cld3d(
    X = X,
    time = disc$time
  )


  suppressMessages(

    kml3d::kml3d(
      cld,
      nbClusters = k,
      nbRedrawing = 20,
      toPlot = "none"
    )

  )


  # ============================================================================
  # EXTRACT PARTITION
  # ============================================================================

  part <- cld[
    paste0(
      "c",
      k
    )
  ][[1]]


  cluster <- as.integer(
    part[
      "clustersAsInteger"
    ]
  )


  if (
    length(
      cluster
    ) != N
  ) {

    stop(
      paste0(
        "kml3d returned ",
        length(cluster),
        " labels instead of ",
        N,
        "."
      )
    )
  }


  cluster
}


# ==============================================================================
# 6. METRICS
# ==============================================================================

get_scores <- function(
    truth,
    pred) {


  c(

    ARI = aricode::ARI(
      truth,
      pred
    ),

    NMI = aricode::NMI(
      truth,
      pred
    )

  )
}


# ==============================================================================
# 7. RUN ONE REPLICATE
# ==============================================================================

run_one <- function(
    scenario,
    run) {


  scenario_number <- match(
    scenario,
    c(
      "standard",
      "irregular",
      "nuisance",
      "imbalanced"
    )
  )


  seed <-
    10000 +
    scenario_number * 1000 +
    run


  cat(
    sprintf(
      "\n%-12s | run %2d/%d\n",
      scenario,
      run,
      N_RUNS
    )
  )


  # ============================================================================
  # SIMULATE
  # ============================================================================

  sim <- simulate_benchmark(
    scenario = scenario,
    seed = seed
  )


  dat <- sim$data
  truth <- sim$truth


  # ============================================================================
  # WINDOW REPRESENTATION
  # ============================================================================

  disc <- window_discretize(
    dat = dat,
    n_windows = N_VISITS
  )


  output <- list()


  # ============================================================================
  # LONGTAPIO
  # ============================================================================

  cat(
    "  longTAPIO  ... "
  )


  t0 <- proc.time()[3]


  pred <- try(
    fit_longTAPIO(
      disc = disc,
      k = K
    ),
    silent = TRUE
  )


  elapsed <-
    proc.time()[3] -
    t0


  if (
    !inherits(
      pred,
      "try-error"
    )
  ) {


    sc <- get_scores(
      truth,
      pred
    )


    cat(
      sprintf(
        "ARI %.3f | NMI %.3f\n",
        sc["ARI"],
        sc["NMI"]
      )
    )


    output[
      [length(output) + 1]
    ] <- data.frame(

      scenario = scenario,
      run = run,
      method = "longTAPIO",

      ARI = unname(
        sc["ARI"]
      ),

      NMI = unname(
        sc["NMI"]
      ),

      seconds = elapsed
    )


  } else {


    cat(
      "FAILED\n"
    )

    print(
      pred
    )
  }


  # ============================================================================
  # CLUSTERMLD
  # ============================================================================

  cat(
    "  clusterMLD ... "
  )


  t0 <- proc.time()[3]


  pred <- try(
    fit_clusterMLD(
      dat = dat,
      k = K,
      verbose = FALSE
    ),
    silent = TRUE
  )


  elapsed <-
    proc.time()[3] -
    t0


  if (
    !inherits(
      pred,
      "try-error"
    )
  ) {


    sc <- get_scores(
      truth,
      pred
    )


    cat(
      sprintf(
        "ARI %.3f | NMI %.3f\n",
        sc["ARI"],
        sc["NMI"]
      )
    )


    output[
      [length(output) + 1]
    ] <- data.frame(

      scenario = scenario,
      run = run,
      method = "clusterMLD",

      ARI = unname(
        sc["ARI"]
      ),

      NMI = unname(
        sc["NMI"]
      ),

      seconds = elapsed
    )


  } else {


    cat(
      "FAILED\n"
    )

    print(
      pred
    )
  }


  # ============================================================================
  # KML3D
  # ============================================================================

  cat(
    "  kml3d      ... "
  )


  t0 <- proc.time()[3]


  pred <- try(
    fit_kml3d(
      disc = disc,
      k = K
    ),
    silent = TRUE
  )


  elapsed <-
    proc.time()[3] -
    t0


  if (
    !inherits(
      pred,
      "try-error"
    )
  ) {


    sc <- get_scores(
      truth,
      pred
    )


    cat(
      sprintf(
        "ARI %.3f | NMI %.3f\n",
        sc["ARI"],
        sc["NMI"]
      )
    )


    output[
      [length(output) + 1]
    ] <- data.frame(

      scenario = scenario,
      run = run,
      method = "kml3d",

      ARI = unname(
        sc["ARI"]
      ),

      NMI = unname(
        sc["NMI"]
      ),

      seconds = elapsed
    )


  } else {


    cat(
      "FAILED\n"
    )

    print(
      pred
    )
  }


  if (
    length(output) == 0
  ) {

    return(
      NULL
    )
  }


  do.call(
    rbind,
    output
  )
}


# ==============================================================================
# 8. RUN BENCHMARK
# ==============================================================================

SCENARIOS <- c(
  "standard",
  "irregular",
  "nuisance",
  "imbalanced"
)


cat(
  "\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  "LONGITUDINAL CLUSTERING BENCHMARK\n",
  "longTAPIO_inductive vs clusterMLD vs kml3d\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


all_results <- list()

counter <- 1


for (scenario in SCENARIOS) {


  cat(
    "\n",
    paste(
      rep(
        "-",
        80
      ),
      collapse = ""
    ),
    "\n",
    toupper(
      scenario
    ),
    "\n",
    paste(
      rep(
        "-",
        80
      ),
      collapse = ""
    ),
    "\n",
    sep = ""
  )


  for (run in seq_len(
    N_RUNS
  )) {


    z <- run_one(
      scenario = scenario,
      run = run
    )


    if (!is.null(z)) {


      all_results[
        [counter]
      ] <- z


      counter <-
        counter + 1
    }
  }
}


results <- do.call(
  rbind,
  all_results
)


rownames(
  results
) <- NULL


# ==============================================================================
# 9. SUMMARY
# ==============================================================================

mean_sd <- function(x) {


  sprintf(
    "%.3f +/- %.3f",

    mean(
      x,
      na.rm = TRUE
    ),

    sd(
      x,
      na.rm = TRUE
    )
  )
}


summary_list <- list()

counter <- 1


for (scenario in SCENARIOS) {


  for (
    method in c(
      "longTAPIO",
      "clusterMLD",
      "kml3d"
    )
  ) {


    d <- results[
      results$scenario == scenario &
      results$method == method,
      ,
      drop = FALSE
    ]


    if (
      nrow(d) == 0
    ) {

      next
    }


    summary_list[
      [counter]
    ] <- data.frame(

      scenario = scenario,

      method = method,

      ARI = mean_sd(
        d$ARI
      ),

      NMI = mean_sd(
        d$NMI
      ),

      seconds = sprintf(
        "%.1f",
        mean(
          d$seconds,
          na.rm = TRUE
        )
      )
    )


    counter <-
      counter + 1
  }
}


summary_table <- do.call(
  rbind,
  summary_list
)


rownames(
  summary_table
) <- NULL


cat(
  "\n\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  "FINAL RESULTS\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


print(
  summary_table,
  row.names = FALSE
)


# ==============================================================================
# 10. PAIRED ARI COMPARISONS
# ==============================================================================

cat(
  "\n\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  "PAIRED ARI DIFFERENCES\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


comparisons <- list(

  c(
    "longTAPIO",
    "clusterMLD"
  ),

  c(
    "longTAPIO",
    "kml3d"
  ),

  c(
    "clusterMLD",
    "kml3d"
  )

)


for (scenario in SCENARIOS) {


  d <- results[
    results$scenario == scenario,
    ,
    drop = FALSE
  ]


  wide <- reshape(
    d[
      ,
      c(
        "run",
        "method",
        "ARI"
      )
    ],
    idvar = "run",
    timevar = "method",
    direction = "wide"
  )


  cat(
    "\n",
    toupper(
      scenario
    ),
    "\n",
    sep = ""
  )


  for (cmp in comparisons) {


    name1 <- paste0(
      "ARI.",
      cmp[1]
    )


    name2 <- paste0(
      "ARI.",
      cmp[2]
    )


    if (
      name1 %in% names(wide) &&
      name2 %in% names(wide)
    ) {


      x <- wide[[name1]]
      y <- wide[[name2]]


      ok <-
        is.finite(x) &
        is.finite(y)


      if (
        sum(ok) >= 2
      ) {


        delta <-
          x[ok] -
          y[ok]


        cat(
          sprintf(
            "  %-10s - %-10s : %+0.3f +/- %.3f",
            cmp[1],
            cmp[2],
            mean(delta),
            sd(delta)
          )
        )


        if (
          sum(ok) >= 3
        ) {


          p <- wilcox.test(
            x[ok],
            y[ok],
            paired = TRUE,
            exact = FALSE
          )$p.value


          cat(
            sprintf(
              " | p = %.4g",
              p
            )
          )
        }


        cat(
          "\n"
        )
      }
    }
  }
}


# ==============================================================================
# 11. SAVE
# ==============================================================================

write.csv(
  results,
  "benchmark_longTAPIO_clusterMLD_kml3d_raw.csv",
  row.names = FALSE
)


write.csv(
  summary_table,
  "benchmark_longTAPIO_clusterMLD_kml3d_summary.csv",
  row.names = FALSE
)


cat(
  "\nBenchmark complete.\n"
)


# ==============================================================================
# OPTIONAL: CHECK clusterMLD ON ONE DATASET
# ==============================================================================

cat(
  "\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  "clusterMLD SANITY CHECK\n",
  paste(
    rep(
      "=",
      80
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


check_sim <- simulate_benchmark(
  scenario = "standard",
  seed = 999
)


check_pred <- fit_clusterMLD(
  dat = check_sim$data,
  k = K,
  verbose = TRUE
)


cat(
  sprintf(
    "\nFixed-K clusterMLD ARI = %.3f\n",
    aricode::ARI(
      check_sim$truth,
      check_pred
    )
  )
)


cat(
  sprintf(
    "Fixed-K clusterMLD NMI = %.3f\n",
    aricode::NMI(
      check_sim$truth,
      check_pred
    )
  )
)