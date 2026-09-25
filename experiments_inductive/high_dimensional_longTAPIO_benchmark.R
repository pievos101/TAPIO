# =============================================================================
# HIGH-DIMENSIONAL MULTIVARIATE LONGITUDINAL CLUSTERING BENCHMARK
#
# METHODS
#
#   1. longTAPIO-sqrtP
#   2. clusterMLD
#   3. kml3d
#
# AIM
#
#   Stress-test clustering as the number of longitudinal variables P increases,
#   while keeping the number of truly cluster-informative variables fixed.
#
#   P = 10, 25, 50, 100, 250, 500
#   P_signal = 5 throughout
#
# Thus the informative fraction decreases from 50% to 1%.
#
# IMPORTANT
#
#   * longTAPIO uses ONLY ceil(sqrt(P)) variables per tree.
#   * longTAPIO-fullP has intentionally been removed.
#   * All methods receive the same original regular 10-visit data.
#   * No sliding windows/interpolation are needed in this experiment.
#   * The true labels are used ONLY for external evaluation (ARI/NMI), never
#     during fitting.
#
# REQUIREMENTS
#
#   library(aricode)
#   library(clusterMLD)
#   library(kml3d)
#   library(longitudinalData)
#
#   longTAPIO_inductive() must already be loaded.
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(aricode)
library(clusterMLD)
library(kml3d)
library(longitudinalData)


# =============================================================================
# 1. GLOBAL SETTINGS
# =============================================================================

N_RUNS <- 30

K <- 4

N <- 160

N_VISITS <- 10

TIME_MIN <- 0

TIME_MAX <- 10

P_SIGNAL <- 5

P_VALUES <- c(
  10,
  25,
  50,
  100,
  250,
  500
)

N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"

METHODS <- c(
  "longTAPIO-sqrtP",
  "clusterMLD",
  "kml3d"
)


if (!exists("longTAPIO_inductive")) {

  stop(
    "Please source/load longTAPIO_inductive() before running benchmark."
  )
}


# =============================================================================
# 2. HIGH-DIMENSIONAL SYNTHETIC DATA GENERATOR
#
# Five variables carry the longitudinal cluster structure.
# All remaining variables are temporally structured nuisance trajectories
# independent of the true cluster.
# =============================================================================

simulate_highdim_benchmark <- function(
    P,
    seed = 1) {


  if (P < P_SIGNAL) {

    stop(
      "P must be >= P_SIGNAL."
    )
  }


  set.seed(
    seed
  )


  # ===========================================================================
  # TRUE CLUSTERS
  # ===========================================================================

  proportions <- c(
    0.25,
    0.25,
    0.25,
    0.25
  )


  sizes <- floor(
    N * proportions
  )


  sizes[1] <- sizes[1] +
    N -
    sum(
      sizes
    )


  truth <- rep(
    seq_len(K),
    times = sizes
  )


  truth <- sample(
    truth
  )


  # ===========================================================================
  # SUBJECT-SPECIFIC HETEROGENEITY
  # ===========================================================================

  subject_intercept <- rnorm(
    N,
    mean = 0,
    sd = 0.13
  )


  subject_scale <- rlnorm(
    N,
    meanlog = 0,
    sdlog = 0.07
  )


  subject_phase <- rnorm(
    N,
    mean = 0,
    sd = 0.14
  )


  # ===========================================================================
  # LATENT CLUSTER TRAJECTORIES
  # ===========================================================================

  latent_curve <- function(
      t,
      cluster,
      phase = 0) {


    tt <- t +
      phase


    if (cluster == 1) {

      # stable / weak progression

      value <- 0.17 +
        0.020 * tt


    } else if (cluster == 2) {

      # progressive

      value <- 0.08 +
        0.080 * tt


    } else if (cluster == 3) {

      # oscillatory

      value <- 0.45 +
        0.30 *
        sin(
          0.82 * tt -
            0.60
        )


    } else {

      # transient flare

      value <- 0.16 +
        0.68 *
        exp(
          -0.5 *
            (
              (tt - 5.0) /
                1.30
            )^2
        )
    }


    value
  }


  # ===========================================================================
  # SIGNAL PARAMETERS
  # ===========================================================================

  signal_loadings <- seq(
    0.90,
    1.25,
    length.out = P_SIGNAL
  )


  feature_offsets <- seq(
    -0.10,
    0.10,
    length.out = P_SIGNAL
  )


  # ===========================================================================
  # GENERATE SUBJECTS
  # ===========================================================================

  rows <- vector(
    "list",
    N
  )


  common_time <- seq(
    TIME_MIN,
    TIME_MAX,
    length.out = N_VISITS
  )


  for (i in seq_len(N)) {


    ti <- common_time


    latent <- vapply(
      ti,
      function(tt) {

        latent_curve(
          t = tt,
          cluster = truth[i],
          phase = subject_phase[i]
        )

      },
      numeric(1)
    )


    Xi <- matrix(
      NA_real_,
      nrow = length(ti),
      ncol = P
    )


    # =========================================================================
    # INFORMATIVE VARIABLES
    # =========================================================================

    for (j in seq_len(P_SIGNAL)) {


      Xi[, j] <-
        feature_offsets[j] +
        subject_intercept[i] +
        subject_scale[i] *
        signal_loadings[j] *
        latent +
        rnorm(
          length(ti),
          mean = 0,
          sd = 0.095
        )
    }


    # =========================================================================
    # HIGH-DIMENSIONAL NUISANCE VARIABLES
    #
    # Temporally structured, patient-specific, but independent of true cluster.
    # =========================================================================

    if (P > P_SIGNAL) {


      for (j in (P_SIGNAL + 1):P) {


        nuisance_frequency <- runif(
          1,
          min = 0.20,
          max = 0.90
        )


        nuisance_phase <- runif(
          1,
          min = 0,
          max = 2 * pi
        )


        nuisance_amplitude <- runif(
          1,
          min = 0.03,
          max = 0.12
        )


        nuisance_offset <- rnorm(
          1,
          mean = 0,
          sd = 0.10
        )


        Xi[, j] <-
          nuisance_offset +
          nuisance_amplitude *
          sin(
            nuisance_frequency * ti +
              nuisance_phase
          ) +
          rnorm(
            length(ti),
            mean = 0,
            sd = 0.25
          )
      }
    }


    # =========================================================================
    # LONG FORMAT
    # =========================================================================

    tmp <- data.frame(
      id = i,
      time = ti
    )


    for (j in seq_len(P)) {

      tmp[[paste0("y", j)]] <- Xi[, j]
    }


    rows[[i]] <- tmp
  }


  dat <- do.call(
    rbind,
    rows
  )


  rownames(
    dat
  ) <- NULL


  list(

    data = dat,

    truth = truth,

    N = N,

    P = P,

    P_signal = P_SIGNAL
  )
}


# =============================================================================
# 3. REGULAR DATA -> N x V x P ARRAY
# =============================================================================

regular_to_array <- function(
    dat) {


  features <- grep(
    "^y[0-9]+$",
    names(
      dat
    ),
    value = TRUE
  )


  ids <- sort(
    unique(
      dat$id
    )
  )


  times <- sort(
    unique(
      dat$time
    )
  )


  N_local <- length(
    ids
  )


  V <- length(
    times
  )


  P <- length(
    features
  )


  visits_per_subject <- table(
    dat$id
  )


  if (
    length(
      unique(
        visits_per_subject
      )
    ) != 1
  ) {

    stop(
      "regular_to_array(): unequal number of visits."
    )
  }


  if (
    unique(
      visits_per_subject
    ) != V
  ) {

    stop(
      "regular_to_array(): subjects do not share common time grid."
    )
  }


  X <- array(
    NA_real_,
    dim = c(
      N_local,
      V,
      P
    ),
    dimnames = list(
      as.character(
        ids
      ),
      as.character(
        times
      ),
      features
    )
  )


  for (ii in seq_along(
    ids
  )) {


    d_i <- dat[
      dat$id == ids[ii],
      ,
      drop = FALSE
    ]


    d_i <- d_i[
      order(
        d_i$time
      ),
      ,
      drop = FALSE
    ]


    X[
      ii,
      ,
      ] <- as.matrix(
        d_i[
          ,
          features,
          drop = FALSE
        ]
      )
  }


  if (
    any(
      !is.finite(
        X
      )
    )
  ) {

    stop(
      "Non-finite values found in trajectory array."
    )
  }


  list(

    X = X,

    ids = ids,

    time = times,

    features = features,

    representation = "original_regular"
  )
}


# =============================================================================
# 4. ARRAY -> LONGTAPIO DATA
# =============================================================================

array_to_longTAPIO <- function(
    X) {


  N_local <- dim(
    X
  )[1]


  V <- dim(
    X
  )[2]


  P <- dim(
    X
  )[3]


  DATA <- matrix(
    NA_real_,
    nrow = N_local * V,
    ncol = P
  )


  user_id <- rep(
    seq_len(N_local),
    each = V
  )


  for (i in seq_len(
    N_local
  )) {


    rows_i <- (
      (i - 1) * V + 1
    ):(
      i * V
    )


    DATA[
      rows_i,
      ] <- X[
        i,
        ,
      ]
  }


  DATA <- as.data.frame(
    DATA
  )


  names(
    DATA
  ) <- paste0(
    "y",
    seq_len(
      P
    )
  )


  list(

    DATA = DATA,

    user_id = user_id
  )
}


# =============================================================================
# 5. EXTRACT LONGTAPIO CLUSTERS
# =============================================================================

extract_longTAPIO_cluster <- function(
    fit,
    N_local) {


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
      !is.null(
        z
      ) &&
        length(
          z
        ) == N_local
    ) {

      return(
        as.integer(
          z
        )
      )
    }
  }


  if (
    !is.null(
      fit$result
    ) &&
      is.list(
        fit$result
      )
  ) {


    for (nm in candidates) {


      z <- fit$result[[nm]]


      if (
        !is.null(
          z
        ) &&
          length(
            z
          ) == N_local
      ) {

        return(
          as.integer(
            z
          )
        )
      }
    }
  }


  stop(
    paste0(
      "Could not extract longTAPIO partition. Components: ",
      paste(
        names(
          fit
        ),
        collapse = ", "
      )
    )
  )
}


# =============================================================================
# 6. LONGTAPIO-sqrtP ONLY
# =============================================================================

fit_longTAPIO <- function(
    representation,
    k = K) {


  X <- representation$X


  N_local <- dim(
    X
  )[1]


  P <- dim(
    X
  )[3]


  prepared <- array_to_longTAPIO(
    X
  )


  n_features_tree <- max(
    2,
    ceiling(
      sqrt(
        P
      )
    )
  )


  fit <- longTAPIO_inductive(

    DATA = prepared$DATA,

    user_id = prepared$user_id,

    k = k,

    n_features = n_features_tree,

    n_trees = N_TREES,

    levels = LEVELS,

    method = "ward.D2",

    scale = TRUE,

    replace = FALSE,

    pca_selection = PCA_SELECTION
  )


  extract_longTAPIO_cluster(

    fit = fit,

    N_local = N_local
  )
}


# =============================================================================
# 7. CLUSTERMLD LABEL HELPER
# =============================================================================

cluster_list_to_labels <- function(
    cluster_list,
    ids) {


  labels <- rep(
    NA_integer_,
    length(
      ids
    )
  )


  names(
    labels
  ) <- as.character(
    ids
  )


  for (g in seq_along(
    cluster_list
  )) {


    members <- cluster_list[[g]]


    labels[
      as.character(
        members
      )
    ] <- g
  }


  if (
    anyNA(
      labels
    )
  ) {

    stop(
      paste0(
        "clusterMLD failed to assign ",
        sum(
          is.na(
            labels
          )
        ),
        " subjects."
      )
    )
  }


  as.integer(
    labels
  )
}


# =============================================================================
# 8. CLUSTERMLD
# =============================================================================

fit_clusterMLD <- function(
    dat,
    k = K) {


  features <- grep(
    "^y[0-9]+$",
    names(
      dat
    ),
    value = TRUE
  )


  ids <- sort(
    unique(
      dat$id
    )
  )


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


  if (
    is.null(
      fit$Cluster.Lists
    )
  ) {

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
      "clusterMLD hierarchy does not contain requested K."
    )
  }


  fixed_k <- fit$Cluster.Lists[[k]]


  cluster_list_to_labels(

    cluster_list = fixed_k,

    ids = ids
  )
}


# =============================================================================
# 9. KML3D CONSTRUCTOR
# =============================================================================

make_cld3d <- function(
    X,
    time) {


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
    "Could not locate ClusterLongData3d constructor."
  )
}


# =============================================================================
# 10. KML3D
# =============================================================================

fit_kml3d <- function(
    representation,
    k = K) {


  X <- representation$X


  N_local <- dim(
    X
  )[1]


  cld <- make_cld3d(

    X = X,

    time = representation$time
  )


  invisible(
    capture.output(
      suppressMessages(
        suppressWarnings(
          kml3d::kml3d(

            cld,

            nbClusters = k,

            nbRedrawing = 20,

            toPlot = "none"
          )
        )
      )
    )
  )


  cluster_name <- paste0(
    "c",
    k
  )


  part <- cld[
    cluster_name
  ][[1]]


  cluster <- as.integer(
    part[
      "clustersAsInteger"
    ]
  )


  if (
    length(
      cluster
    ) != N_local
  ) {

    stop(
      "Unexpected number of kml3d labels."
    )
  }


  cluster
}


# =============================================================================
# 11. METRICS
# =============================================================================

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


# =============================================================================
# 12. RESULT HELPER
# =============================================================================

make_result <- function(
    P,
    run,
    method,
    truth,
    pred,
    seconds) {


  scores <- get_scores(
    truth,
    pred
  )


  data.frame(

    P = P,

    P_signal = P_SIGNAL,

    signal_fraction = P_SIGNAL / P,

    n_features_tree = if (
      method == "longTAPIO-sqrtP"
    ) {
      max(
        2,
        ceiling(
          sqrt(
            P
          )
        )
      )
    } else {
      NA_integer_
    },

    run = run,

    method = method,

    ARI = unname(
      scores["ARI"]
    ),

    NMI = unname(
      scores["NMI"]
    ),

    seconds = seconds,

    stringsAsFactors = FALSE
  )
}


# =============================================================================
# 13. RUN ONE REPLICATE
# =============================================================================

run_one <- function(
    P,
    run) {


  p_index <- match(
    P,
    P_VALUES
  )


  seed <- 70000 +
    p_index * 1000 +
    run


  cat(
    sprintf(
      "\nP = %-3d | run %2d/%d | signal = %d/%d (%.1f%%)\n",
      P,
      run,
      N_RUNS,
      P_SIGNAL,
      P,
      100 * P_SIGNAL / P
    )
  )


  sim <- simulate_highdim_benchmark(

    P = P,

    seed = seed
  )


  dat <- sim$data

  truth <- sim$truth


  representation <- regular_to_array(
    dat
  )


  cat(
    sprintf(
      "  data             : %d subjects x %d visits x %d variables\n",
      dim(
        representation$X
      )[1],
      dim(
        representation$X
      )[2],
      dim(
        representation$X
      )[3]
    )
  )


  cat(
    sprintf(
      "  longTAPIO m      : ceil(sqrt(P)) = %d variables/tree\n",
      max(
        2,
        ceiling(
          sqrt(
            P
          )
        )
      )
    )
  )


  output <- list()


  # ===========================================================================
  # LONGTAPIO sqrt(P)
  # ===========================================================================

  cat(
    "  longTAPIO-sqrtP ... "
  )


  start_time <- proc.time()[3]


  pred <- try(
    fit_longTAPIO(

      representation = representation,

      k = K
    ),
    silent = TRUE
  )


  elapsed <- proc.time()[3] -
    start_time


  if (
    !inherits(
      pred,
      "try-error"
    )
  ) {


    scores <- get_scores(
      truth,
      pred
    )


    cat(
      sprintf(
        "ARI %.3f | NMI %.3f | %.2fs\n",
        scores["ARI"],
        scores["NMI"],
        elapsed
      )
    )


    output[[length(output) + 1L]] <- make_result(

      P = P,

      run = run,

      method = "longTAPIO-sqrtP",

      truth = truth,

      pred = pred,

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


  # ===========================================================================
  # CLUSTERMLD
  # ===========================================================================

  cat(
    "  clusterMLD       ... "
  )


  start_time <- proc.time()[3]


  pred <- try(
    fit_clusterMLD(

      dat = dat,

      k = K
    ),
    silent = TRUE
  )


  elapsed <- proc.time()[3] -
    start_time


  if (
    !inherits(
      pred,
      "try-error"
    )
  ) {


    scores <- get_scores(
      truth,
      pred
    )


    cat(
      sprintf(
        "ARI %.3f | NMI %.3f | %.2fs\n",
        scores["ARI"],
        scores["NMI"],
        elapsed
      )
    )


    output[[length(output) + 1L]] <- make_result(

      P = P,

      run = run,

      method = "clusterMLD",

      truth = truth,

      pred = pred,

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


  # ===========================================================================
  # KML3D
  # ===========================================================================

  cat(
    "  kml3d            ... "
  )


  start_time <- proc.time()[3]


  pred <- try(
    fit_kml3d(

      representation = representation,

      k = K
    ),
    silent = TRUE
  )


  elapsed <- proc.time()[3] -
    start_time


  if (
    !inherits(
      pred,
      "try-error"
    )
  ) {


    scores <- get_scores(
      truth,
      pred
    )


    cat(
      sprintf(
        "ARI %.3f | NMI %.3f | %.2fs\n",
        scores["ARI"],
        scores["NMI"],
        elapsed
      )
    )


    output[[length(output) + 1L]] <- make_result(

      P = P,

      run = run,

      method = "kml3d",

      truth = truth,

      pred = pred,

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
    length(
      output
    ) == 0
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


# =============================================================================
# 14. RUN COMPLETE HIGH-DIMENSIONAL BENCHMARK
# =============================================================================

cat(
  "\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  "HIGH-DIMENSIONAL MULTIVARIATE LONGITUDINAL CLUSTERING BENCHMARK\n",
  "\n",
  "N             : ",
  N,
  "\n",
  "Visits        : ",
  N_VISITS,
  "\n",
  "Clusters      : ",
  K,
  "\n",
  "Signal vars   : ",
  P_SIGNAL,
  " (fixed)\n",
  "P values      : ",
  paste(
    P_VALUES,
    collapse = ", "
  ),
  "\n",
  "longTAPIO     : ceil(sqrt(P)) variables/tree\n",
  "\n",
  "Methods:\n",
  "  longTAPIO-sqrtP\n",
  "  clusterMLD\n",
  "  kml3d\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


all_results <- list()

counter <- 1L


for (P in P_VALUES) {


  cat(
    "\n",
    paste(
      rep(
        "-",
        100
      ),
      collapse = ""
    ),
    "\n",
    sprintf(
      "P = %d | %d signal + %d nuisance | %.1f%% informative\n",
      P,
      P_SIGNAL,
      P - P_SIGNAL,
      100 * P_SIGNAL / P
    ),
    paste(
      rep(
        "-",
        100
      ),
      collapse = ""
    ),
    "\n",
    sep = ""
  )


  for (run in seq_len(
    N_RUNS
  )) {


    result_run <- run_one(

      P = P,

      run = run
    )


    if (
      !is.null(
        result_run
      )
    ) {


      all_results[[counter]] <- result_run


      counter <- counter +
        1L
    }
  }
}


if (
  length(
    all_results
  ) == 0
) {

  stop(
    "All benchmark runs failed."
  )
}


results <- do.call(
  rbind,
  all_results
)


rownames(
  results
) <- NULL


# =============================================================================
# 15. SUMMARY
# =============================================================================

mean_sd <- function(
    x) {


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

counter <- 1L


for (P in P_VALUES) {


  for (method in METHODS) {


    d <- results[
      results$P == P &
        results$method == method,
      ,
      drop = FALSE
    ]


    if (
      nrow(
        d
      ) == 0
    ) {

      next
    }


    summary_list[[counter]] <- data.frame(

      P = P,

      P_signal = P_SIGNAL,

      signal_fraction = P_SIGNAL / P,

      method = method,

      n_runs = nrow(
        d
      ),

      ARI = mean_sd(
        d$ARI
      ),

      NMI = mean_sd(
        d$NMI
      ),

      mean_ARI = mean(
        d$ARI,
        na.rm = TRUE
      ),

      sd_ARI = sd(
        d$ARI,
        na.rm = TRUE
      ),

      mean_NMI = mean(
        d$NMI,
        na.rm = TRUE
      ),

      sd_NMI = sd(
        d$NMI,
        na.rm = TRUE
      ),

      seconds = mean(
        d$seconds,
        na.rm = TRUE
      ),

      stringsAsFactors = FALSE
    )


    counter <- counter +
      1L
  }
}


summary_table <- do.call(
  rbind,
  summary_list
)


rownames(
  summary_table
) <- NULL


display_summary <- summary_table[
  ,
  c(
    "P",
    "P_signal",
    "signal_fraction",
    "method",
    "n_runs",
    "ARI",
    "NMI",
    "seconds"
  )
]


display_summary$signal_fraction <- sprintf(
  "%.3f",
  display_summary$signal_fraction
)


display_summary$seconds <- sprintf(
  "%.2f",
  display_summary$seconds
)


cat(
  "\n\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  "FINAL RESULTS\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


print(
  display_summary,
  row.names = FALSE
)


# =============================================================================
# 16. PAIRED COMPARISONS
# =============================================================================

comparisons <- list(

  c(
    "longTAPIO-sqrtP",
    "clusterMLD"
  ),

  c(
    "longTAPIO-sqrtP",
    "kml3d"
  ),

  c(
    "clusterMLD",
    "kml3d"
  )
)


paired_comparison <- function(
    metric = c(
      "ARI",
      "NMI"
    )) {


  metric <- match.arg(
    metric
  )


  output <- list()

  output_counter <- 1L


  cat(
    "\n\n",
    paste(
      rep(
        "=",
        100
      ),
      collapse = ""
    ),
    "\n",
    "PAIRED ",
    metric,
    " DIFFERENCES\n",
    paste(
      rep(
        "=",
        100
      ),
      collapse = ""
    ),
    "\n",
    sep = ""
  )


  for (P in P_VALUES) {


    d <- results[
      results$P == P,
      ,
      drop = FALSE
    ]


    d_metric <- d[
      ,
      c(
        "run",
        "method",
        metric
      ),
      drop = FALSE
    ]


    names(
      d_metric
    )[3] <- "score"


    wide <- reshape(
      d_metric,
      idvar = "run",
      timevar = "method",
      direction = "wide"
    )


    cat(
      "\nP = ",
      P,
      "\n",
      sep = ""
    )


    for (cmp in comparisons) {


      name1 <- paste0(
        "score.",
        cmp[1]
      )


      name2 <- paste0(
        "score.",
        cmp[2]
      )


      if (
        name1 %in% names(
          wide
        ) &&
          name2 %in% names(
            wide
          )
      ) {


        x <- wide[[name1]]

        y <- wide[[name2]]


        ok <- is.finite(
          x
        ) &
          is.finite(
            y
          )


        if (
          sum(
            ok
          ) >= 2
        ) {


          delta <- x[ok] -
            y[ok]


          mean_delta <- mean(
            delta
          )


          sd_delta <- sd(
            delta
          )


          if (
            sum(
              ok
            ) >= 3 &&
              any(
                abs(
                  delta
                ) > 1e-12
              )
          ) {


            p_value <- suppressWarnings(
              wilcox.test(
                x[ok],
                y[ok],
                paired = TRUE,
                exact = FALSE
              )$p.value
            )


          } else {


            p_value <- 1
          }


          cat(
            sprintf(
              "  %-16s - %-16s : %+0.3f +/- %.3f | p = %.4g\n",
              cmp[1],
              cmp[2],
              mean_delta,
              sd_delta,
              p_value
            )
          )


          output[[output_counter]] <- data.frame(

            P = P,

            metric = metric,

            method_1 = cmp[1],

            method_2 = cmp[2],

            n_pairs = sum(
              ok
            ),

            mean_difference = mean_delta,

            sd_difference = sd_delta,

            p_value = p_value,

            stringsAsFactors = FALSE
          )


          output_counter <- output_counter +
            1L
        }
      }
    }
  }


  if (
    length(
      output
    ) == 0
  ) {

    return(
      data.frame()
    )
  }


  do.call(
    rbind,
    output
  )
}


paired_ARI <- paired_comparison(
  metric = "ARI"
)


paired_NMI <- paired_comparison(
  metric = "NMI"
)


# =============================================================================
# 17. LONGTAPIO RANDOM-SUBSPACE DIAGNOSTICS
#
# Probability that a uniformly sampled sqrt(P) subspace contains:
#   - no informative variable
#   - at least one informative variable
#
# This is a theoretical diagnostic only and does not use the true labels
# during model fitting.
# =============================================================================

subspace_diagnostics <- do.call(
  rbind,
  lapply(
    P_VALUES,
    function(P) {


      m <- max(
        2,
        ceiling(
          sqrt(
            P
          )
        )
      )


      p_no_signal <- if (
        P - P_SIGNAL >= m
      ) {

        exp(
          lchoose(
            P - P_SIGNAL,
            m
          ) -
            lchoose(
              P,
              m
            )
        )

      } else {

        0
      }


      data.frame(

        P = P,

        P_signal = P_SIGNAL,

        n_features_tree = m,

        expected_signal_per_tree = m *
          P_SIGNAL /
          P,

        prob_no_signal = p_no_signal,

        prob_at_least_one_signal = 1 -
          p_no_signal,

        stringsAsFactors = FALSE
      )
    }
  )
)


cat(
  "\n\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  "LONGTAPIO RANDOM-SUBSPACE DIAGNOSTICS\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  sep = ""
)


print(
  subspace_diagnostics,
  row.names = FALSE
)


# =============================================================================
# 18. SIMPLE PERFORMANCE PLOTS
# =============================================================================

plot_metric <- function(
    metric = c(
      "mean_ARI",
      "mean_NMI"
    ),
    ylab = NULL) {


  metric <- match.arg(
    metric
  )


  if (
    is.null(
      ylab
    )
  ) {

    ylab <- metric
  }


  method_order <- METHODS


  plot(
    NA,
    xlim = range(
      P_VALUES
    ),
    ylim = c(
      0,
      1
    ),
    log = "x",
    xlab = "Number of longitudinal variables P (log scale)",
    ylab = ylab,
    xaxt = "n"
  )


  axis(
    1,
    at = P_VALUES,
    labels = P_VALUES
  )


  for (mm in seq_along(
    method_order
  )) {


    method <- method_order[mm]


    d <- summary_table[
      summary_table$method == method,
      ,
      drop = FALSE
    ]


    d <- d[
      order(
        d$P
      ),
      ,
      drop = FALSE
    ]


    lines(
      d$P,
      d[[metric]],
      type = "b",
      pch = 14 + mm,
      lty = mm
    )
  }


  legend(
    "bottomleft",
    legend = method_order,
    lty = seq_along(
      method_order
    ),
    pch = 14 + seq_along(
      method_order
    ),
    bty = "n"
  )
}


pdf(
  "HIGH_DIMENSIONAL_benchmark_ARI.pdf",
  width = 7,
  height = 5
)


plot_metric(
  metric = "mean_ARI",
  ylab = "Mean ARI"
)


dev.off()


pdf(
  "HIGH_DIMENSIONAL_benchmark_NMI.pdf",
  width = 7,
  height = 5
)


plot_metric(
  metric = "mean_NMI",
  ylab = "Mean NMI"
)


dev.off()


# =============================================================================
# 19. SAVE RESULTS
# =============================================================================

write.csv(
  results,
  file = "HIGH_DIMENSIONAL_benchmark_raw.csv",
  row.names = FALSE
)


write.csv(
  summary_table,
  file = "HIGH_DIMENSIONAL_benchmark_summary.csv",
  row.names = FALSE
)


write.csv(
  paired_ARI,
  file = "HIGH_DIMENSIONAL_benchmark_paired_ARI.csv",
  row.names = FALSE
)


write.csv(
  paired_NMI,
  file = "HIGH_DIMENSIONAL_benchmark_paired_NMI.csv",
  row.names = FALSE
)


write.csv(
  subspace_diagnostics,
  file = "HIGH_DIMENSIONAL_subspace_diagnostics.csv",
  row.names = FALSE
)


capture.output(
  sessionInfo(),
  file = "HIGH_DIMENSIONAL_benchmark_sessionInfo.txt"
)


# =============================================================================
# 20. COMPLETE
# =============================================================================

cat(
  "\n\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n",
  "HIGH-DIMENSIONAL BENCHMARK COMPLETE\n",
  paste(
    rep(
      "=",
      100
    ),
    collapse = ""
  ),
  "\n\n",
  sep = ""
)


cat(
  "Design:\n",
  "  N             = ",
  N,
  "\n",
  "  visits        = ",
  N_VISITS,
  "\n",
  "  signal vars   = ",
  P_SIGNAL,
  " fixed\n",
  "  P             = ",
  paste(
    P_VALUES,
    collapse = ", "
  ),
  "\n",
  "  longTAPIO     = sqrt(P) feature sampling only\n\n",
  sep = ""
)


cat(
  "Files written:\n",
  "  HIGH_DIMENSIONAL_benchmark_raw.csv\n",
  "  HIGH_DIMENSIONAL_benchmark_summary.csv\n",
  "  HIGH_DIMENSIONAL_benchmark_paired_ARI.csv\n",
  "  HIGH_DIMENSIONAL_benchmark_paired_NMI.csv\n",
  "  HIGH_DIMENSIONAL_subspace_diagnostics.csv\n",
  "  HIGH_DIMENSIONAL_benchmark_ARI.pdf\n",
  "  HIGH_DIMENSIONAL_benchmark_NMI.pdf\n",
  "  HIGH_DIMENSIONAL_benchmark_sessionInfo.txt\n",
  sep = ""
)
