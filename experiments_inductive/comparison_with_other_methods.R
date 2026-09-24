# =============================================================================
# LONGITUDINAL CLUSTERING BENCHMARK
#
# METHODS
#
#   1. longTAPIO-sqrtP
#   2. longTAPIO-fullP
#   3. clusterMLD
#   4. kml3d
#
#
# PREPROCESSING
#
# REGULAR scenarios:
#
#   standard
#   nuisance
#   imbalanced
#
#       -> use ORIGINAL 10 aligned visits
#
#
# IRREGULAR scenario:
#
#       original irregular observations
#           ->
#       overlapping sliding windows
#           ->
#       aggregate REAL observations
#           ->
#       complete subject x window grid
#           ->
#       interpolate empty windows
#
#
# clusterMLD:
#
#       always receives ORIGINAL observations + ORIGINAL times
#
#
# longTAPIO and kml3d:
#
#       always receive exactly the SAME trajectory representation
#
#
# Sliding-window definition:
#
#   window_size = total_range / n_bins
#   step        = window_size * (1 - overlap)
#
#   window = [start, start + window_size)
#
#
# With:
#
#   n_bins = 10
#   overlap = 0.50
#
# approximately 21 overlapping window positions are produced.
#
#
# REQUIREMENTS
#
#   library(aricode)
#   library(clusterMLD)
#   library(kml3d)
#   library(longitudinalData)
#
# longTAPIO_inductive() must already be loaded.
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

N_VISITS <- 10

TIME_MIN <- 0

TIME_MAX <- 10


# -----------------------------------------------------------------------------
# longTAPIO
# -----------------------------------------------------------------------------

N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"


# -----------------------------------------------------------------------------
# Sliding-window parameters
#
# ONLY used for irregular scenario
# -----------------------------------------------------------------------------

N_BINS <- 10

WINDOW_OVERLAP <- 0.50


# -----------------------------------------------------------------------------
# Scenarios
# -----------------------------------------------------------------------------

SCENARIOS <- c(
  "standard",
  "irregular",
  "nuisance",
  "imbalanced"
)


# -----------------------------------------------------------------------------
# Methods
# -----------------------------------------------------------------------------

METHODS <- c(
  "longTAPIO-sqrtP",
  "longTAPIO-fullP",
  "clusterMLD",
  "kml3d"
)


if (!exists("longTAPIO_inductive")) {

  stop(
    "Please source/load longTAPIO_inductive() before running benchmark."
  )
}


# =============================================================================
# 2. SYNTHETIC DATA GENERATOR
# =============================================================================

simulate_benchmark <- function(
    scenario = c(
      "standard",
      "irregular",
      "nuisance",
      "imbalanced"
    ),
    seed = 1) {


  scenario <- match.arg(
    scenario
  )


  set.seed(
    seed
  )


  # ===========================================================================
  # SCENARIO SETTINGS
  # ===========================================================================

  if (scenario == "standard") {


    N <- 160

    P <- 5

    P_signal <- 5


    proportions <- c(
      0.25,
      0.25,
      0.25,
      0.25
    )


    irregular <- FALSE


  } else if (scenario == "irregular") {


    N <- 160

    P <- 5

    P_signal <- 5


    proportions <- c(
      0.25,
      0.25,
      0.25,
      0.25
    )


    irregular <- TRUE


  } else if (scenario == "nuisance") {


    N <- 160

    P <- 50

    P_signal <- 5


    proportions <- c(
      0.25,
      0.25,
      0.25,
      0.25
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


  # ===========================================================================
  # TRUE CLUSTERS
  # ===========================================================================

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
    length.out = P_signal
  )


  feature_offsets <- seq(
    -0.10,
    0.10,
    length.out = P_signal
  )


  # ===========================================================================
  # GENERATE SUBJECTS
  # ===========================================================================

  rows <- vector(
    "list",
    N
  )


  for (i in seq_len(N)) {


    # =========================================================================
    # OBSERVATION TIMES
    # =========================================================================

    if (!irregular) {


      ti <- seq(
        TIME_MIN,
        TIME_MAX,
        length.out = N_VISITS
      )


    } else {


      n_i <- sample(
        7:12,
        size = 1
      )


      ti <- sort(
        runif(
          n_i,
          min = TIME_MIN,
          max = TIME_MAX
        )
      )
    }


    # =========================================================================
    # LATENT TRAJECTORY
    # =========================================================================

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
    # SIGNAL VARIABLES
    # =========================================================================

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
          sd = 0.095
        )
    }


    # =========================================================================
    # NUISANCE VARIABLES
    #
    # Temporally structured but independent of true cluster.
    # =========================================================================

    if (P > P_signal) {


      for (j in (P_signal + 1):P) {


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

    P_signal = P_signal,

    irregular = irregular
  )
}


# =============================================================================
# 3. REGULAR DATA -> ORIGINAL ARRAY
#
# No discretization.
# No windowing.
# No interpolation.
#
# Converts original aligned visits directly to:
#
#     N x V x P
#
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


  N <- length(
    ids
  )


  V <- length(
    times
  )


  P <- length(
    features
  )


  # ===========================================================================
  # CHECK EQUAL VISITS
  # ===========================================================================

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


  # ===========================================================================
  # ARRAY
  # ===========================================================================

  X <- array(
    NA_real_,
    dim = c(
      N,
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
      "Non-finite values found in regular trajectory array."
    )
  }


  list(

    X = X,

    ids = ids,

    time = times,

    features = features,

    representation = "original_regular",

    n_created_windows = NA_integer_,

    empty_fraction = 0,

    duplication_factor = 1,

    window_size = NA_real_,

    step = NA_real_
  )
}


# =============================================================================
# 4. IRREGULAR DATA -> OVERLAPPING SLIDING WINDOWS
#
# ONLY used for irregular scenario.
#
# 1. overlapping windows
# 2. aggregate actual observations
# 3. complete N x W x P array
# 4. interpolate empty windows
# =============================================================================

sliding_window_discretize <- function(
    dat,
    n_bins = N_BINS,
    overlap = WINDOW_OVERLAP) {


  # ===========================================================================
  # CHECKS
  # ===========================================================================

  if (
    n_bins < 1
  ) {


    stop(
      "'n_bins' must be >= 1."
    )
  }


  if (
    overlap < 0 ||
      overlap >= 1
  ) {


    stop(
      "'overlap' must satisfy 0 <= overlap < 1."
    )
  }


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


  N <- length(
    ids
  )


  P <- length(
    features
  )


  # ===========================================================================
  # GLOBAL TIME RANGE
  # ===========================================================================

  t_min <- min(
    dat$time,
    na.rm = TRUE
  )


  t_max <- max(
    dat$time,
    na.rm = TRUE
  )


  total_range <- t_max -
    t_min


  if (
    !is.finite(
      total_range
    ) ||
      total_range <= 0
  ) {


    stop(
      "Invalid time range."
    )
  }


  # ===========================================================================
  # WINDOW WIDTH AND STEP
  # ===========================================================================

  window_size <- total_range /
    n_bins


  step <- window_size *
    (
      1 -
        overlap
    )


  if (
    step <= 0
  ) {


    stop(
      "Window step must be positive."
    )
  }


  # ===========================================================================
  # DEFINE WINDOWS
  # ===========================================================================

  window_list <- list()


  visit <- 0L

  start <- t_min


  while (
    start <= t_max
  ) {


    end <- start +
      window_size


    window_list[[length(window_list) + 1L]] <- data.frame(

      visit = visit,

      window_start = start,

      window_end = end,

      window_center = (
        start +
          end
      ) /
        2
    )


    visit <- visit +
      1L


    start <- start +
      step
  }


  window_info <- do.call(
    rbind,
    window_list
  )


  rownames(
    window_info
  ) <- NULL


  W <- nrow(
    window_info
  )


  visits <- window_info$visit


  # ===========================================================================
  # COMPLETE ARRAY
  # ===========================================================================

  X <- array(
    NA_real_,
    dim = c(
      N,
      W,
      P
    ),
    dimnames = list(
      as.character(
        ids
      ),
      as.character(
        visits
      ),
      features
    )
  )


  # ===========================================================================
  # NUMBER OF REAL OBSERVATIONS PER SUBJECT/WINDOW
  # ===========================================================================

  n_real_obs <- matrix(
    0L,
    nrow = N,
    ncol = W,
    dimnames = list(
      as.character(
        ids
      ),
      as.character(
        visits
      )
    )
  )


  # ===========================================================================
  # ASSIGN REAL OBSERVATIONS TO OVERLAPPING WINDOWS
  # ===========================================================================

  expanded_count <- 0L


  for (ww in seq_len(
    W
  )) {


    start_w <- window_info$window_start[ww]

    end_w <- window_info$window_end[ww]


    # -------------------------------------------------------------------------
    # Same interval definition as Python:
    #
    #     time >= start
    #     time < end
    # -------------------------------------------------------------------------

    d_w <- dat[
      dat$time >= start_w &
        dat$time < end_w,
      ,
      drop = FALSE
    ]


    expanded_count <- expanded_count +
      nrow(
        d_w
      )


    if (
      nrow(
        d_w
      ) == 0
    ) {


      next
    }


    # =========================================================================
    # SUBJECTS WITH DATA IN THIS WINDOW
    # =========================================================================

    subjects_w <- unique(
      d_w$id
    )


    for (subject_id in subjects_w) {


      ii <- match(
        subject_id,
        ids
      )


      d_iw <- d_w[
        d_w$id == subject_id,
        ,
        drop = FALSE
      ]


      n_real_obs[
        ii,
        ww
      ] <- nrow(
        d_iw
      )


      # =======================================================================
      # AGGREGATE REAL OBSERVATIONS
      # =======================================================================

      for (j in seq_len(
        P
      )) {


        values <- d_iw[[
            features[j]
          ]
        ]


        values <- values[
          is.finite(
            values
          )
        ]


        if (
          length(
            values
          ) > 0
        ) {


          X[
            ii,
            ww,
            j
          ] <- mean(
            values
          )
        }
      }
    }
  }


  # ===========================================================================
  # EMPTY WINDOWS BEFORE FILLING
  # ===========================================================================

  empty_subject_window <- (
    n_real_obs == 0
  )


  empty_fraction <- mean(
    empty_subject_window
  )


  # ===========================================================================
  # INTERPOLATE EMPTY WINDOWS
  #
  # Uses actual window centers as x-axis.
  # ===========================================================================

  centers <- window_info$window_center


  for (ii in seq_len(
    N
  )) {


    for (j in seq_len(
      P
    )) {


      y <- X[
        ii,
        ,
        j
      ]


      ok <- is.finite(
        y
      )


      n_ok <- sum(
        ok
      )


      # -----------------------------------------------------------------------
      # >= 2 observed windows:
      # linear interpolation + boundary propagation
      # -----------------------------------------------------------------------

      if (
        n_ok >= 2
      ) {


        X[
          ii,
          ,
          j
        ] <- approx(

          x = centers[ok],

          y = y[ok],

          xout = centers,

          rule = 2,

          ties = "ordered"

        )$y


      # -----------------------------------------------------------------------
      # exactly one observed window:
      # constant trajectory
      # -----------------------------------------------------------------------

      } else if (
        n_ok == 1
      ) {


        X[
          ii,
          ,
          j
        ] <- rep(
          y[ok][1],
          W
        )


      # -----------------------------------------------------------------------
      # no observation:
      # global feature mean fallback
      # -----------------------------------------------------------------------

      } else {


        global_mean <- mean(
          X[
            ,
            ,
            j
          ],
          na.rm = TRUE
        )


        if (
          !is.finite(
            global_mean
          )
        ) {


          global_mean <- 0
        }


        X[
          ii,
          ,
          j
        ] <- rep(
          global_mean,
          W
        )
      }
    }
  }


  # ===========================================================================
  # CHECK
  # ===========================================================================

  if (
    any(
      !is.finite(
        X
      )
    )
  ) {


    stop(
      "Non-finite values remain after interpolation."
    )
  }


  duplication_factor <- expanded_count /
    nrow(
      dat
    )


  # ===========================================================================
  # RETURN
  # ===========================================================================

  list(

    X = X,

    ids = ids,

    time = centers,

    features = features,

    representation = "overlapping_sliding_windows",

    window_info = window_info,

    n_real_obs = n_real_obs,

    empty_subject_window = empty_subject_window,

    empty_fraction = empty_fraction,

    duplication_factor = duplication_factor,

    window_size = window_size,

    step = step,

    overlap = overlap,

    n_bins = n_bins,

    n_created_windows = W
  )
}


# =============================================================================
# 5. REPRESENTATION ROUTER
#
# THIS IS THE IMPORTANT PART.
#
# regular:
#     original aligned observations
#
# irregular:
#     overlapping sliding windows
# =============================================================================

prepare_trajectory_representation <- function(
    dat,
    irregular) {


  if (irregular) {


    sliding_window_discretize(

      dat = dat,

      n_bins = N_BINS,

      overlap = WINDOW_OVERLAP
    )


  } else {


    regular_to_array(
      dat
    )
  }
}


# =============================================================================
# 6. ARRAY -> LONGTAPIO DATA
#
# Converts:
#
#     N x V x P
#
# to:
#
#     DATA with N*V rows
#
# Every patient has exactly V visits.
# =============================================================================

array_to_longTAPIO <- function(
    X) {


  N <- dim(
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
    nrow = N * V,
    ncol = P
  )


  user_id <- rep(
    seq_len(N),
    each = V
  )


  for (i in seq_len(
    N
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
# 7. EXTRACT LONGTAPIO CLUSTERS
# =============================================================================

extract_longTAPIO_cluster <- function(
    fit,
    N) {


  candidates <- c(
    "train_cluster",
    "train_clusters",
    "cluster",
    "clusters",
    "clustering",
    "labels",
    "membership"
  )


  # ===========================================================================
  # TOP LEVEL
  # ===========================================================================

  for (nm in candidates) {


    z <- fit[[nm]]


    if (
      !is.null(
        z
      ) &&
        length(
          z
        ) == N
    ) {


      return(
        as.integer(
          z
        )
      )
    }
  }


  # ===========================================================================
  # NESTED RESULT
  # ===========================================================================

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
          ) == N
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
# 8. LONGTAPIO
# =============================================================================

fit_longTAPIO <- function(
    representation,
    k = K,
    feature_mode = c(
      "sqrtP",
      "fullP"
    )) {


  feature_mode <- match.arg(
    feature_mode
  )


  X <- representation$X


  N <- dim(
    X
  )[1]


  P <- dim(
    X
  )[3]


  prepared <- array_to_longTAPIO(
    X
  )


  # ===========================================================================
  # NUMBER OF VARIABLES PER TREE
  # ===========================================================================

  if (
    feature_mode == "sqrtP"
  ) {


    n_features_tree <- max(
      2,
      ceiling(
        sqrt(
          P
        )
      )
    )


  } else {


    n_features_tree <- P
  }


  # ===========================================================================
  # FIT
  # ===========================================================================

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

    N = N
  )
}


# =============================================================================
# 9. CLUSTERMLD LABEL HELPER
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
# 10. CLUSTERMLD
#
# IMPORTANT:
#
# clusterMLD always receives ORIGINAL data.
#
# No sliding windows.
# =============================================================================

fit_clusterMLD <- function(
    dat,
    k = K,
    verbose = FALSE) {


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


  labels <- cluster_list_to_labels(

    cluster_list = fixed_k,

    ids = ids
  )


  if (verbose) {


    cat(
      "\nclusterMLD diagnostics\n"
    )


    cat(
      "Requested K       : ",
      k,
      "\n",
      sep = ""
    )


    cat(
      "Gap-selected K    : ",
      fit$No.Gapb,
      "\n",
      sep = ""
    )


    cat(
      "CH-selected K     : ",
      fit$No.CH,
      "\n",
      sep = ""
    )


    cat(
      "Fixed-K sizes     : ",
      paste(
        as.integer(
          table(
            labels
          )
        ),
        collapse = ", "
      ),
      "\n",
      sep = ""
    )


    if (
      !is.null(
        fit$weight
      )
    ) {


      cat(
        "Outcome weights  : ",
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
  }


  labels
}


# =============================================================================
# 11. KML3D CONSTRUCTOR
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
# 12. KML3D
#
# Receives same trajectory representation as longTAPIO:
#
# regular   -> original aligned visits
# irregular -> overlapping sliding-window representation
# =============================================================================

fit_kml3d <- function(
    representation,
    k = K) {


  X <- representation$X


  N <- dim(
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
    ) != N
  ) {


    stop(
      "Unexpected number of kml3d labels."
    )
  }


  cluster
}


# =============================================================================
# 13. METRICS
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
# 14. RESULT HELPER
# =============================================================================

make_result <- function(
    scenario,
    run,
    method,
    truth,
    pred,
    seconds,
    representation = NULL) {


  scores <- get_scores(
    truth,
    pred
  )


  data.frame(

    scenario = scenario,

    run = run,

    method = method,

    ARI = unname(
      scores["ARI"]
    ),

    NMI = unname(
      scores["NMI"]
    ),

    seconds = seconds,

    representation = if (
      is.null(
        representation
      )
    ) {

      "original_raw"

    } else {

      representation$representation
    },

    n_visits_representation = if (
      is.null(
        representation
      )
    ) {

      NA_integer_

    } else {

      dim(
        representation$X
      )[2]
    },

    empty_fraction = if (
      is.null(
        representation
      )
    ) {

      NA_real_

    } else {

      representation$empty_fraction
    },

    duplication_factor = if (
      is.null(
        representation
      )
    ) {

      NA_real_

    } else {

      representation$duplication_factor
    },

    stringsAsFactors = FALSE
  )
}


# =============================================================================
# 15. RUN ONE BENCHMARK REPLICATE
# =============================================================================

run_one <- function(
    scenario,
    run) {


  scenario_number <- match(
    scenario,
    SCENARIOS
  )


  seed <- 50000 +
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


  # ===========================================================================
  # SIMULATE
  # ===========================================================================

  sim <- simulate_benchmark(

    scenario = scenario,

    seed = seed
  )


  dat <- sim$data

  truth <- sim$truth


  # ===========================================================================
  # PREPARE TRAJECTORIES
  #
  # irregular:
  #     sliding windows
  #
  # everything else:
  #     original aligned visits
  # ===========================================================================

  representation <- prepare_trajectory_representation(

    dat = dat,

    irregular = sim$irregular
  )


  # ===========================================================================
  # PRINT PREPROCESSING
  # ===========================================================================

  if (sim$irregular) {


    cat(
      sprintf(
        paste0(
          "  representation : OVERLAPPING SLIDING WINDOWS\n",
          "  windows        : %d\n",
          "  width          : %.3f\n",
          "  step           : %.3f\n",
          "  duplication    : %.2fx\n",
          "  empty pre-fill : %.3f\n"
        ),
        dim(
          representation$X
        )[2],
        representation$window_size,
        representation$step,
        representation$duplication_factor,
        representation$empty_fraction
      )
    )


  } else {


    cat(
      sprintf(
        paste0(
          "  representation : ORIGINAL REGULAR VISITS\n",
          "  visits         : %d\n"
        ),
        dim(
          representation$X
        )[2]
      )
    )
  }


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

      k = K,

      feature_mode = "sqrtP"
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
        "ARI %.3f | NMI %.3f\n",
        scores["ARI"],
        scores["NMI"]
      )
    )


    output[[length(output) + 1L]] <- make_result(

      scenario = scenario,

      run = run,

      method = "longTAPIO-sqrtP",

      truth = truth,

      pred = pred,

      seconds = elapsed,

      representation = representation
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
  # LONGTAPIO full P
  # ===========================================================================

  cat(
    "  longTAPIO-fullP ... "
  )


  start_time <- proc.time()[3]


  pred <- try(
    fit_longTAPIO(

      representation = representation,

      k = K,

      feature_mode = "fullP"
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
        "ARI %.3f | NMI %.3f\n",
        scores["ARI"],
        scores["NMI"]
      )
    )


    output[[length(output) + 1L]] <- make_result(

      scenario = scenario,

      run = run,

      method = "longTAPIO-fullP",

      truth = truth,

      pred = pred,

      seconds = elapsed,

      representation = representation
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

      k = K,

      verbose = FALSE
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
        "ARI %.3f | NMI %.3f\n",
        scores["ARI"],
        scores["NMI"]
      )
    )


    output[[length(output) + 1L]] <- make_result(

      scenario = scenario,

      run = run,

      method = "clusterMLD",

      truth = truth,

      pred = pred,

      seconds = elapsed,

      representation = NULL
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
        "ARI %.3f | NMI %.3f\n",
        scores["ARI"],
        scores["NMI"]
      )
    )


    output[[length(output) + 1L]] <- make_result(

      scenario = scenario,

      run = run,

      method = "kml3d",

      truth = truth,

      pred = pred,

      seconds = elapsed,

      representation = representation
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
# 16. RUN COMPLETE BENCHMARK
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
  "LONGITUDINAL CLUSTERING BENCHMARK\n",
  "\n",
  "Regular data   : ORIGINAL VISITS\n",
  "Irregular data : OVERLAPPING SLIDING WINDOWS\n",
  "\n",
  "Methods:\n",
  "  longTAPIO-sqrtP\n",
  "  longTAPIO-fullP\n",
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


for (scenario in SCENARIOS) {


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
    toupper(
      scenario
    ),
    "\n",
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

      scenario = scenario,

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
# 17. SUMMARY
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


for (scenario in SCENARIOS) {


  for (method in METHODS) {


    d <- results[
      results$scenario == scenario &
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

      scenario = scenario,

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


# =============================================================================
# 18. PRINT FINAL RESULTS
# =============================================================================

display_summary <- summary_table[
  ,
  c(
    "scenario",
    "method",
    "n_runs",
    "ARI",
    "NMI",
    "seconds"
  )
]


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
# 19. PAIRED COMPARISONS
# =============================================================================

comparisons <- list(

  c(
    "longTAPIO-sqrtP",
    "longTAPIO-fullP"
  ),

  c(
    "longTAPIO-sqrtP",
    "clusterMLD"
  ),

  c(
    "longTAPIO-sqrtP",
    "kml3d"
  ),

  c(
    "longTAPIO-fullP",
    "clusterMLD"
  ),

  c(
    "longTAPIO-fullP",
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


  for (scenario in SCENARIOS) {


    d <- results[
      results$scenario == scenario,
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
      "\n",
      toupper(
        scenario
      ),
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

            scenario = scenario,

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


# =============================================================================
# 20. PAIRED ARI AND NMI
# =============================================================================

paired_ARI <- paired_comparison(
  metric = "ARI"
)


paired_NMI <- paired_comparison(
  metric = "NMI"
)


# =============================================================================
# 21. LONGTAPIO FEATURE-SAMPLING ABLATION
#
# Difference:
#
#       fullP - sqrtP
#
# Positive = fullP better
# Negative = sqrtP better
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
  "LONGTAPIO FEATURE-SAMPLING ABLATION\n",
  "Difference = fullP - sqrtP\n",
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


feature_ablation <- list()


feature_counter <- 1L


for (scenario in SCENARIOS) {


  d <- results[
    results$scenario == scenario &
      results$method %in% c(
        "longTAPIO-sqrtP",
        "longTAPIO-fullP"
      ),
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
      ),
      drop = FALSE
    ],
    idvar = "run",
    timevar = "method",
    direction = "wide"
  )


  sqrt_name <- "ARI.longTAPIO-sqrtP"

  full_name <- "ARI.longTAPIO-fullP"


  if (
    sqrt_name %in% names(
      wide
    ) &&
      full_name %in% names(
        wide
      )
  ) {


    x <- wide[[full_name]]

    y <- wide[[sqrt_name]]


    ok <- is.finite(
      x
    ) &
      is.finite(
        y
      )


    delta <- x[ok] -
      y[ok]


    if (
      length(
        delta
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
        "%-12s : %+0.3f +/- %.3f | p = %.4g\n",
        scenario,
        mean(
          delta
        ),
        sd(
          delta
        ),
        p_value
      )
    )


    feature_ablation[[feature_counter]] <- data.frame(

      scenario = scenario,

      n_pairs = length(
        delta
      ),

      mean_fullP_minus_sqrtP = mean(
        delta
      ),

      sd_fullP_minus_sqrtP = sd(
        delta
      ),

      p_value = p_value,

      stringsAsFactors = FALSE
    )


    feature_counter <- feature_counter +
      1L
  }
}


if (
  length(
    feature_ablation
  ) > 0
) {


  feature_ablation_table <- do.call(
    rbind,
    feature_ablation
  )


} else {


  feature_ablation_table <- data.frame()
}


# =============================================================================
# 22. PREPROCESSING SANITY CHECK
#
# Explicitly demonstrate:
#
# standard  -> 10 original visits
# irregular -> sliding windows
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
  "PREPROCESSING SANITY CHECK\n",
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


# -----------------------------------------------------------------------------
# Regular
# -----------------------------------------------------------------------------

check_regular <- simulate_benchmark(

  scenario = "standard",

  seed = 12345
)


rep_regular <- prepare_trajectory_representation(

  dat = check_regular$data,

  irregular = check_regular$irregular
)


cat(
  "\nSTANDARD:\n"
)


cat(
  "  representation = ",
  rep_regular$representation,
  "\n",
  sep = ""
)


cat(
  "  visits         = ",
  dim(
    rep_regular$X
  )[2],
  "\n",
  sep = ""
)


cat(
  "  array          = ",
  paste(
    dim(
      rep_regular$X
    ),
    collapse = " x "
  ),
  "\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# Irregular
# -----------------------------------------------------------------------------

check_irregular <- simulate_benchmark(

  scenario = "irregular",

  seed = 12345
)


rep_irregular <- prepare_trajectory_representation(

  dat = check_irregular$data,

  irregular = check_irregular$irregular
)


cat(
  "\nIRREGULAR:\n"
)


cat(
  "  representation = ",
  rep_irregular$representation,
  "\n",
  sep = ""
)


cat(
  "  windows        = ",
  dim(
    rep_irregular$X
  )[2],
  "\n",
  sep = ""
)


cat(
  "  window width   = ",
  round(
    rep_irregular$window_size,
    3
  ),
  "\n",
  sep = ""
)


cat(
  "  window step    = ",
  round(
    rep_irregular$step,
    3
  ),
  "\n",
  sep = ""
)


cat(
  "  overlap        = ",
  WINDOW_OVERLAP,
  "\n",
  sep = ""
)


cat(
  "  duplication    = ",
  round(
    rep_irregular$duplication_factor,
    3
  ),
  "x\n",
  sep = ""
)


cat(
  "  empty pre-fill = ",
  round(
    rep_irregular$empty_fraction,
    3
  ),
  "\n",
  sep = ""
)


cat(
  "  array          = ",
  paste(
    dim(
      rep_irregular$X
    ),
    collapse = " x "
  ),
  "\n",
  sep = ""
)


# =============================================================================
# 23. CLUSTERMLD FIXED-K SANITY CHECK
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
  "CLUSTERMLD FIXED-K SANITY CHECK\n",
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
  "\nContingency table:\n"
)


print(
  table(
    Truth = check_sim$truth,
    clusterMLD = check_pred
  )
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


# =============================================================================
# 24. SAVE RESULTS
# =============================================================================

write.csv(
  results,
  file = "FINAL_benchmark_raw.csv",
  row.names = FALSE
)


write.csv(
  summary_table,
  file = "FINAL_benchmark_summary.csv",
  row.names = FALSE
)


write.csv(
  paired_ARI,
  file = "FINAL_benchmark_paired_ARI.csv",
  row.names = FALSE
)


write.csv(
  paired_NMI,
  file = "FINAL_benchmark_paired_NMI.csv",
  row.names = FALSE
)


write.csv(
  feature_ablation_table,
  file = "FINAL_benchmark_feature_ablation.csv",
  row.names = FALSE
)


capture.output(
  sessionInfo(),
  file = "FINAL_benchmark_sessionInfo.txt"
)


# =============================================================================
# 25. COMPLETE
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
  "BENCHMARK COMPLETE\n",
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
  "Preprocessing:\n",
  "  standard   -> original regular visits\n",
  "  nuisance   -> original regular visits\n",
  "  imbalanced -> original regular visits\n",
  "  irregular  -> overlapping sliding windows\n\n",
  sep = ""
)


cat(
  "Files written:\n",
  "  FINAL_benchmark_raw.csv\n",
  "  FINAL_benchmark_summary.csv\n",
  "  FINAL_benchmark_paired_ARI.csv\n",
  "  FINAL_benchmark_paired_NMI.csv\n",
  "  FINAL_benchmark_feature_ablation.csv\n",
  "  FINAL_benchmark_sessionInfo.txt\n"
)