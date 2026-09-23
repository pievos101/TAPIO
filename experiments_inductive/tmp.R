# ==============================================================================
# longTAPIO: IRREGULAR-TIME BENCHMARK
# ==============================================================================
#
# QUESTION:
#
# Is simple time-window discretization sufficient for longTAPIO when
# longitudinal observations occur at irregular times?
#
# We compare:
#
#   1. ORACLE
#      The trajectory is observed on a common regular grid.
#
#   2. WINDOW
#      Irregular observations are aggregated into common time windows.
#
#   3. SPLINE
#      A smoothing spline is fitted to each irregular patient-feature
#      trajectory and evaluated on the same common grid.
#
# IMPORTANT:
#
#   * Same patients in all three representations
#   * Same phenotype labels
#   * Same underlying continuous trajectories
#   * Same subject-level heterogeneity
#   * Same simulation seed within each paired comparison
#   * longTAPIO settings identical across representations
#
# The downstream clustering method is therefore unchanged.
#
# ==============================================================================


# ==============================================================================
# 0. PACKAGES
# ==============================================================================

required_packages <- c(
  "aricode"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(aricode)


# ==============================================================================
# 1. GLOBAL SETTINGS
# ==============================================================================

N_RUNS <- 20

N_PATIENTS <- 160
N_CLUSTERS <- 4

N_FEATURES <- 12
N_SIGNAL <- 8

TIME_MIN <- 0
TIME_MAX <- 10

# Number of common time bins / grid points used by longTAPIO
N_WINDOWS <- 10

# Mean number of irregular observations per patient
N_VISITS <- 10

# Irregularity levels.
#
# sigma = 0 means observations are at nominal visit times.
# Increasing sigma introduces visit-time jitter.
#
JITTER_LEVELS <- c(
  0.00,
  0.20,
  0.50,
  1.00
)

# Measurement noise
MEASUREMENT_NOISE <- 0.10

# Probability that a scheduled visit is absent
VISIT_DROPOUT <- 0.10

# longTAPIO settings
TAPIO_K <- 4
TAPIO_TREES <- 500
TAPIO_LEVELS <- 4
TAPIO_PCA <- "random_weighted"

# Number of features sampled per tree
TAPIO_N_FEATURES <- max(
  2,
  ceiling(sqrt(N_FEATURES))
)

BASE_SEED <- 42000


# ==============================================================================
# 2. PHENOTYPE TRAJECTORIES
# ==============================================================================

# Continuous-time phenotype functions.
#
# We deliberately include both smooth and relatively localized dynamics:
#
#   phenotype 1: stable / weak progression
#   phenotype 2: progressive
#   phenotype 3: oscillatory
#   phenotype 4: flare
#
# The flare is particularly important because excessive discretization
# can smooth away short-lived temporal structure.

phenotype_curve <- function(t, phenotype) {

  if (phenotype == 1) {

    # Stable / slowly progressive
    y <- 0.20 + 0.025 * t

  } else if (phenotype == 2) {

    # Progressive
    y <- 0.10 + 0.075 * t

  } else if (phenotype == 3) {

    # Oscillatory
    y <- 0.45 +
      0.28 * sin(
        0.85 * t - 0.60
      )

  } else if (phenotype == 4) {

    # Localized flare
    y <- 0.20 +
      0.65 *
      exp(
        -0.5 *
        ((t - 5.0) / 1.20)^2
      )

  } else {

    stop("Unknown phenotype.")
  }

  y
}


# ==============================================================================
# 3. SUBJECT-SPECIFIC CONTINUOUS TRAJECTORY
# ==============================================================================

# Each patient receives:
#
#   * phenotype
#   * random intercept
#   * random scale
#   * small temporal phase shift
#   * feature-specific loading
#
# These are generated once and shared by ORACLE/WINDOW/SPLINE.

generate_subject_parameters <- function(
  n_patients,
  n_features,
  n_signal,
  n_clusters,
  seed
) {

  set.seed(seed)

  # --------------------------------------------------------------------------
  # Balanced cluster membership
  # --------------------------------------------------------------------------

  base_size <- floor(
    n_patients / n_clusters
  )

  labels <- rep(
    seq_len(n_clusters),
    each = base_size
  )

  if (length(labels) < n_patients) {

    labels <- c(
      labels,
      seq_len(
        n_patients - length(labels)
      )
    )
  }

  labels <- sample(labels)

  # --------------------------------------------------------------------------
  # Patient heterogeneity
  # --------------------------------------------------------------------------

  patient_intercept <- rnorm(
    n_patients,
    mean = 0,
    sd = 0.08
  )

  patient_scale <- rlnorm(
    n_patients,
    meanlog = 0,
    sdlog = 0.08
  )

  patient_phase <- rnorm(
    n_patients,
    mean = 0,
    sd = 0.20
  )

  # --------------------------------------------------------------------------
  # Feature loadings
  # --------------------------------------------------------------------------

  signal_loading <- seq(
    0.80,
    1.20,
    length.out = n_signal
  )

  if (n_features > n_signal) {

    nuisance_loading <- rep(
      0,
      n_features - n_signal
    )

    feature_loading <- c(
      signal_loading,
      nuisance_loading
    )

  } else {

    feature_loading <- signal_loading
  }

  # --------------------------------------------------------------------------
  # Feature-specific baseline offsets
  # --------------------------------------------------------------------------

  feature_offset <- seq(
    -0.20,
    0.20,
    length.out = n_features
  )

  list(
    labels = labels,
    intercept = patient_intercept,
    scale = patient_scale,
    phase = patient_phase,
    feature_loading = feature_loading,
    feature_offset = feature_offset
  )
}


# ==============================================================================
# 4. CONTINUOUS PATIENT-FEATURE VALUE
# ==============================================================================

continuous_value <- function(
  patient,
  feature,
  time,
  pars
) {

  phenotype <- pars$labels[patient]

  # --------------------------------------------------------------------------
  # Informative features
  # --------------------------------------------------------------------------

  if (
    pars$feature_loading[feature] > 0
  ) {

    latent <- phenotype_curve(
      time + pars$phase[patient],
      phenotype
    )

    value <-
      pars$feature_offset[feature] +
      pars$intercept[patient] +
      pars$scale[patient] *
      pars$feature_loading[feature] *
      latent

  } else {

    # ------------------------------------------------------------------------
    # Nuisance features
    #
    # Longitudinally structured, but independent of phenotype.
    # ------------------------------------------------------------------------

    freq <-
      0.30 +
      0.04 * feature

    value <-
      pars$feature_offset[feature] +
      pars$intercept[patient] +
      0.10 *
      sin(
        freq * time +
        0.5 * pars$phase[patient]
      )

  }

  value
}


# ==============================================================================
# 5. COMMON GRID / WINDOWS
# ==============================================================================

# Window boundaries
WINDOW_BREAKS <- seq(
  TIME_MIN,
  TIME_MAX,
  length.out = N_WINDOWS + 1
)

# Window centers
GRID_TIMES <- (
  WINDOW_BREAKS[-1] +
  WINDOW_BREAKS[-length(WINDOW_BREAKS)]
) / 2


# ==============================================================================
# 6. GENERATE IRREGULAR VISIT TIMES
# ==============================================================================

generate_visit_times <- function(
  n_patients,
  n_visits,
  jitter_sd,
  dropout,
  seed
) {

  set.seed(seed)

  # Nominal visit locations
  nominal_times <- seq(
    TIME_MIN,
    TIME_MAX,
    length.out = n_visits
  )

  times <- vector(
    "list",
    n_patients
  )

  for (i in seq_len(n_patients)) {

    # ------------------------------------------------------------------------
    # Jitter
    # ------------------------------------------------------------------------

    tt <- nominal_times +
      rnorm(
        n_visits,
        mean = 0,
        sd = jitter_sd
      )

    # Keep inside observation interval
    tt <- pmax(
      TIME_MIN,
      pmin(
        TIME_MAX,
        tt
      )
    )

    # ------------------------------------------------------------------------
    # Random visit dropout
    # ------------------------------------------------------------------------

    keep <- runif(n_visits) > dropout

    # Ensure sufficiently many observations
    if (sum(keep) < 4) {

      keep[
        sample(
          seq_len(n_visits),
          size = 4
        )
      ] <- TRUE
    }

    tt <- tt[keep]

    # Remove possible duplicated boundary times
    tt <- sort(
      unique(tt)
    )

    times[[i]] <- tt
  }

  times
}


# ==============================================================================
# 7. GENERATE IRREGULAR OBSERVATIONS
# ==============================================================================

generate_irregular_observations <- function(
  pars,
  visit_times,
  measurement_noise,
  seed
) {

  set.seed(seed)

  n_patients <- length(
    pars$labels
  )

  n_features <- length(
    pars$feature_loading
  )

  observations <- vector(
    "list",
    n_patients
  )

  for (i in seq_len(n_patients)) {

    tt <- visit_times[[i]]

    Xi <- matrix(
      NA_real_,
      nrow = length(tt),
      ncol = n_features
    )

    for (v in seq_along(tt)) {

      for (j in seq_len(n_features)) {

        Xi[v, j] <-
          continuous_value(
            patient = i,
            feature = j,
            time = tt[v],
            pars = pars
          ) +
          rnorm(
            1,
            mean = 0,
            sd = measurement_noise
          )
      }
    }

    colnames(Xi) <- paste0(
      "y.",
      seq_len(n_features)
    )

    observations[[i]] <- list(
      time = tt,
      X = Xi
    )
  }

  observations
}


# ==============================================================================
# 8. ORACLE REGULAR-GRID REPRESENTATION
# ==============================================================================

# The oracle observes the same underlying patient trajectories directly at
# the common grid points.
#
# It is not intended as a practical method. It gives us the performance
# ceiling if irregularity were absent.

make_oracle_representation <- function(
  pars,
  grid_times,
  measurement_noise,
  seed
) {

  set.seed(seed)

  n_patients <- length(
    pars$labels
  )

  n_features <- length(
    pars$feature_loading
  )

  arr <- array(
    NA_real_,
    dim = c(
      n_patients,
      length(grid_times),
      n_features
    )
  )

  for (i in seq_len(n_patients)) {

    for (v in seq_along(grid_times)) {

      for (j in seq_len(n_features)) {

        arr[i, v, j] <-
          continuous_value(
            patient = i,
            feature = j,
            time = grid_times[v],
            pars = pars
          ) +
          rnorm(
            1,
            mean = 0,
            sd = measurement_noise
          )
      }
    }
  }

  arr
}


# ==============================================================================
# 9. WINDOW DISCRETIZATION
# ==============================================================================

window_discretize <- function(
  observations,
  breaks
) {

  n_patients <- length(
    observations
  )

  n_windows <- length(
    breaks
  ) - 1

  n_features <- ncol(
    observations[[1]]$X
  )

  arr <- array(
    NA_real_,
    dim = c(
      n_patients,
      n_windows,
      n_features
    )
  )

  for (i in seq_len(n_patients)) {

    tt <- observations[[i]]$time
    Xi <- observations[[i]]$X

    # ------------------------------------------------------------------------
    # Assign observations to bins
    # ------------------------------------------------------------------------

    bin <- cut(
      tt,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE,
      labels = FALSE
    )

    # t == TIME_MAX may otherwise fall outside
    bin[
      tt == max(breaks)
    ] <- n_windows

    for (w in seq_len(n_windows)) {

      ids <- which(
        bin == w
      )

      if (length(ids) > 0) {

        arr[i, w, ] <-
          colMeans(
            Xi[ids, , drop = FALSE],
            na.rm = TRUE
          )
      }
    }
  }

  arr
}


# ==============================================================================
# 10. SPLINE RECONSTRUCTION
# ==============================================================================

spline_reconstruct <- function(
  observations,
  grid_times,
  spar = NULL
) {

  n_patients <- length(
    observations
  )

  n_features <- ncol(
    observations[[1]]$X
  )

  arr <- array(
    NA_real_,
    dim = c(
      n_patients,
      length(grid_times),
      n_features
    )
  )

  for (i in seq_len(n_patients)) {

    tt <- observations[[i]]$time
    Xi <- observations[[i]]$X

    for (j in seq_len(n_features)) {

      yy <- Xi[, j]

      ok <- is.finite(tt) &
        is.finite(yy)

      x <- tt[ok]
      y <- yy[ok]

      # ----------------------------------------------------------------------
      # Deal with duplicated visit times
      # ----------------------------------------------------------------------

      if (length(x) > 0) {

        ux <- sort(
          unique(x)
        )

        y2 <- sapply(
          ux,
          function(z) {
            mean(
              y[x == z],
              na.rm = TRUE
            )
          }
        )

        x <- ux
        y <- y2
      }

      # ----------------------------------------------------------------------
      # Smoothing spline if enough observations exist
      # ----------------------------------------------------------------------

      if (length(x) >= 4) {

        fit <- try(
          smooth.spline(
            x = x,
            y = y,
            spar = spar
          ),
          silent = TRUE
        )

        if (
          !inherits(
            fit,
            "try-error"
          )
        ) {

          pred <- predict(
            fit,
            x = grid_times
          )$y

          arr[i, , j] <- pred

        } else {

          # Linear interpolation fallback
          arr[i, , j] <-
            approx(
              x,
              y,
              xout = grid_times,
              rule = 2
            )$y
        }

      } else if (length(x) >= 2) {

        arr[i, , j] <-
          approx(
            x,
            y,
            xout = grid_times,
            rule = 2
          )$y

      } else if (length(x) == 1) {

        arr[i, , j] <- y[1]
      }
    }
  }

  arr
}


# ==============================================================================
# 11. EMPTY-WINDOW IMPUTATION
# ==============================================================================

# Simple interpolation is used ONLY after binning.
#
# This is intentionally conservative:
#
#   WINDOW = bin first, then interpolate only genuinely empty windows.
#
# It does not fit a continuous smooth trajectory like the spline method.

fill_empty_windows <- function(
  arr,
  grid_times
) {

  out <- arr

  n_patients <- dim(arr)[1]
  n_features <- dim(arr)[3]

  for (i in seq_len(n_patients)) {

    for (j in seq_len(n_features)) {

      y <- arr[i, , j]

      ok <- is.finite(y)

      if (sum(ok) >= 2) {

        out[i, , j] <-
          approx(
            x = grid_times[ok],
            y = y[ok],
            xout = grid_times,
            rule = 2
          )$y

      } else if (sum(ok) == 1) {

        out[i, , j] <-
          rep(
            y[ok][1],
            length(grid_times)
          )
      }
    }
  }

  out
}


# ==============================================================================
# 12. CONVERT ARRAY TO longTAPIO DATA FORMAT
# ==============================================================================

# Assumed format:
#
# user_id | time | y.1 | ... | y.p
#
# If your current longTAPIO implementation uses another name for the time
# column, only this function needs changing.

array_to_long_df <- function(
  arr,
  grid_times
) {

  n_patients <- dim(arr)[1]
  n_times <- dim(arr)[2]
  n_features <- dim(arr)[3]

  rows <- vector(
    "list",
    n_patients
  )

  for (i in seq_len(n_patients)) {

    tmp <- data.frame(
      user_id = rep(
        i,
        n_times
      ),
      time = grid_times
    )

    for (j in seq_len(n_features)) {

      tmp[[paste0(
        "y.",
        j
      )]] <- arr[i, , j]
    }

    rows[[i]] <- tmp
  }

  do.call(
    rbind,
    rows
  )
}


# ==============================================================================
# 13. EXTRACT longTAPIO TRAINING CLUSTERS
# ==============================================================================

# Because implementations may use slightly different object names,
# this function checks several plausible locations.
#
# If your model has a known exact component, replace this with something like:
#
#   return(as.integer(fit$cluster))
#

extract_longtapio_clusters <- function(
  fit,
  n_patients
) {

  candidates <- c(
    "cluster",
    "clusters",
    "clustering",
    "labels",
    "membership",
    "train_cluster",
    "train_clusters",
    "training_cluster",
    "training_clusters"
  )

  for (nm in candidates) {

    if (
      !is.null(
        fit[[nm]]
      )
    ) {

      z <- fit[[nm]]

      if (
        length(z) == n_patients
      ) {

        return(
          as.integer(z)
        )
      }
    }
  }

  # --------------------------------------------------------------------------
  # Sometimes training information is nested
  # --------------------------------------------------------------------------

  nested_candidates <- c(
    "train",
    "training",
    "model"
  )

  for (outer in nested_candidates) {

    obj <- fit[[outer]]

    if (
      is.list(obj)
    ) {

      for (nm in candidates) {

        if (
          !is.null(
            obj[[nm]]
          )
        ) {

          z <- obj[[nm]]

          if (
            length(z) == n_patients
          ) {

            return(
              as.integer(z)
            )
          }
        }
      }
    }
  }

  stop(
    paste0(
      "\nCould not automatically find the longTAPIO cluster vector.\n",
      "Run names(fit) once and modify extract_longtapio_clusters().\n"
    )
  )
}


# ==============================================================================
# 14. RUN longTAPIO ON ONE REPRESENTATION
# ==============================================================================

run_longtapio <- function(
  arr,
  labels,
  grid_times,
  seed
) {

  set.seed(seed)

  DATA <- array_to_long_df(
    arr,
    grid_times
  )

  # ------------------------------------------------------------
  # IMPORTANT:
  # longTAPIO_inductive() expects the actual ID vector,
  # not the name of the ID column.
  # ------------------------------------------------------------

  ids <- DATA$user_id

  # Remove ID column from the feature data supplied to longTAPIO
  DATA_model <- DATA[
    ,
    setdiff(
      names(DATA),
      "user_id"
    ),
    drop = FALSE
  ]

  fit <- longTAPIO_inductive(
    DATA = DATA_model,
    user_id = ids,
    k = TAPIO_K,
    n_features = TAPIO_N_FEATURES,
    n_trees = TAPIO_TREES,
    levels = TAPIO_LEVELS,
    method = "ward.D2",
    scale = TRUE,
    replace = TRUE,
    pca_selection = TAPIO_PCA
  )

  cluster <- extract_longtapio_clusters(
    fit,
    n_patients = length(labels)
  )

  ARI <- aricode::ARI(
    cluster,
    labels
  )

  NMI <- aricode::NMI(
    cluster,
    labels
  )

  list(
    fit = fit,
    cluster = cluster,
    ARI = ARI,
    NMI = NMI
  )
}

# ==============================================================================
# 15. SINGLE BENCHMARK RUN
# ==============================================================================

run_one_benchmark <- function(
  run_id,
  jitter_sd
) {

  seed <-
    BASE_SEED +
    run_id * 1000 +
    round(
      jitter_sd * 100
    )

  cat(
    "\n",
    paste(
      rep(
        "=",
        75
      ),
      collapse = ""
    ),
    "\n",
    sep = ""
  )

  cat(
    "RUN:",
    run_id,
    " | jitter:",
    jitter_sd,
    "\n"
  )

  # --------------------------------------------------------------------------
  # Shared patient parameters
  # --------------------------------------------------------------------------

  pars <- generate_subject_parameters(
    n_patients = N_PATIENTS,
    n_features = N_FEATURES,
    n_signal = N_SIGNAL,
    n_clusters = N_CLUSTERS,
    seed = seed
  )

  labels <- pars$labels

  # --------------------------------------------------------------------------
  # Irregular visit schedule
  # --------------------------------------------------------------------------

  visit_times <- generate_visit_times(
    n_patients = N_PATIENTS,
    n_visits = N_VISITS,
    jitter_sd = jitter_sd,
    dropout = VISIT_DROPOUT,
    seed = seed + 1
  )

  # --------------------------------------------------------------------------
  # Irregular measurements
  # --------------------------------------------------------------------------

  obs <- generate_irregular_observations(
    pars = pars,
    visit_times = visit_times,
    measurement_noise = MEASUREMENT_NOISE,
    seed = seed + 2
  )

  # --------------------------------------------------------------------------
  # ORACLE
  # --------------------------------------------------------------------------

  oracle_arr <- make_oracle_representation(
    pars = pars,
    grid_times = GRID_TIMES,
    measurement_noise = MEASUREMENT_NOISE,
    seed = seed + 3
  )

  # --------------------------------------------------------------------------
  # WINDOW
  # --------------------------------------------------------------------------

  window_raw <- window_discretize(
    observations = obs,
    breaks = WINDOW_BREAKS
  )

  window_arr <- fill_empty_windows(
    window_raw,
    GRID_TIMES
  )

  # --------------------------------------------------------------------------
  # SPLINE
  # --------------------------------------------------------------------------

  spline_arr <- spline_reconstruct(
    observations = obs,
    grid_times = GRID_TIMES
  )

  # --------------------------------------------------------------------------
  # Diagnostic: fraction of empty patient-windows BEFORE interpolation
  # --------------------------------------------------------------------------

  empty_matrix <- apply(
    window_raw,
    c(1, 2),
    function(x) {
      all(
        !is.finite(x)
      )
    }
  )

  empty_fraction <- mean(
    empty_matrix
  )

  cat(
    "Empty patient-window fraction:",
    sprintf(
      "%.3f",
      empty_fraction
    ),
    "\n"
  )

  # --------------------------------------------------------------------------
  # Same longTAPIO settings for every representation
  # --------------------------------------------------------------------------

  cat(
    "  Fitting ORACLE...\n"
  )

  res_oracle <- run_longtapio(
    arr = oracle_arr,
    labels = labels,
    grid_times = GRID_TIMES,
    seed = seed + 10
  )

  cat(
    sprintf(
      "    ARI = %.3f | NMI = %.3f\n",
      res_oracle$ARI,
      res_oracle$NMI
    )
  )

  cat(
    "  Fitting WINDOW...\n"
  )

  res_window <- run_longtapio(
    arr = window_arr,
    labels = labels,
    grid_times = GRID_TIMES,
    seed = seed + 10
  )

  cat(
    sprintf(
      "    ARI = %.3f | NMI = %.3f\n",
      res_window$ARI,
      res_window$NMI
    )
  )

  cat(
    "  Fitting SPLINE...\n"
  )

  res_spline <- run_longtapio(
    arr = spline_arr,
    labels = labels,
    grid_times = GRID_TIMES,
    seed = seed + 10
  )

  cat(
    sprintf(
      "    ARI = %.3f | NMI = %.3f\n",
      res_spline$ARI,
      res_spline$NMI
    )
  )

  # --------------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------------

  data.frame(
    run = run_id,
    jitter = jitter_sd,
    empty_fraction = empty_fraction,

    oracle_ARI = res_oracle$ARI,
    window_ARI = res_window$ARI,
    spline_ARI = res_spline$ARI,

    oracle_NMI = res_oracle$NMI,
    window_NMI = res_window$NMI,
    spline_NMI = res_spline$NMI,

    window_loss_ARI =
      res_oracle$ARI -
      res_window$ARI,

    spline_loss_ARI =
      res_oracle$ARI -
      res_spline$ARI,

    window_minus_spline_ARI =
      res_window$ARI -
      res_spline$ARI,

    window_minus_spline_NMI =
      res_window$NMI -
      res_spline$NMI
  )
}


# ==============================================================================
# 16. RUN COMPLETE EXPERIMENT
# ==============================================================================

all_results <- list()

counter <- 1

for (
  jitter in JITTER_LEVELS
) {

  cat(
    "\n\n",
    "######################################################################\n",
    "# JITTER SD = ",
    jitter,
    "\n",
    "######################################################################\n",
    sep = ""
  )

  for (
    run_id in seq_len(
      N_RUNS
    )
  ) {

    result <- try(
      run_one_benchmark(
        run_id = run_id,
        jitter_sd = jitter
      ),
      silent = FALSE
    )

    if (
      !inherits(
        result,
        "try-error"
      )
    ) {

      all_results[[counter]] <- result

      counter <- counter + 1
    }
  }
}

results <- do.call(
  rbind,
  all_results
)


# ==============================================================================
# 17. SUMMARY FUNCTION
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


# ==============================================================================
# 18. MAIN SUMMARY TABLE
# ==============================================================================

summary_rows <- list()

counter <- 1

for (
  jitter in JITTER_LEVELS
) {

  d <- results[
    results$jitter == jitter,
    ,
    drop = FALSE
  ]

  summary_rows[[counter]] <- data.frame(

    jitter = jitter,

    empty_windows =
      mean_sd(
        d$empty_fraction
      ),

    oracle_ARI =
      mean_sd(
        d$oracle_ARI
      ),

    window_ARI =
      mean_sd(
        d$window_ARI
      ),

    spline_ARI =
      mean_sd(
        d$spline_ARI
      ),

    oracle_NMI =
      mean_sd(
        d$oracle_NMI
      ),

    window_NMI =
      mean_sd(
        d$window_NMI
      ),

    spline_NMI =
      mean_sd(
        d$spline_NMI
      )
  )

  counter <- counter + 1
}

summary_table <- do.call(
  rbind,
  summary_rows
)

cat(
  "\n\n",
  "======================================================================\n",
  "MAIN RESULTS\n",
  "======================================================================\n"
)

print(
  summary_table,
  row.names = FALSE
)


# ==============================================================================
# 19. PERFORMANCE LOSS RELATIVE TO ORACLE
# ==============================================================================

loss_rows <- list()

counter <- 1

for (
  jitter in JITTER_LEVELS
) {

  d <- results[
    results$jitter == jitter,
    ,
    drop = FALSE
  ]

  loss_rows[[counter]] <- data.frame(

    jitter = jitter,

    window_loss_ARI =
      mean_sd(
        d$window_loss_ARI
      ),

    spline_loss_ARI =
      mean_sd(
        d$spline_loss_ARI
      ),

    window_minus_spline_ARI =
      mean_sd(
        d$window_minus_spline_ARI
      ),

    window_minus_spline_NMI =
      mean_sd(
        d$window_minus_spline_NMI
      )
  )

  counter <- counter + 1
}

loss_table <- do.call(
  rbind,
  loss_rows
)

cat(
  "\n\n",
  "======================================================================\n",
  "LOSS RELATIVE TO REGULAR-GRID ORACLE\n",
  "======================================================================\n"
)

print(
  loss_table,
  row.names = FALSE
)


# ==============================================================================
# 20. PAIRED TESTS: WINDOW VS SPLINE
# ==============================================================================

cat(
  "\n\n",
  "======================================================================\n",
  "PAIRED WINDOW VS SPLINE TESTS\n",
  "======================================================================\n"
)

for (
  jitter in JITTER_LEVELS
) {

  d <- results[
    results$jitter == jitter,
    ,
    drop = FALSE
  ]

  cat(
    "\nJitter =",
    jitter,
    "\n"
  )

  # --------------------------------------------------------------------------
  # ARI
  # --------------------------------------------------------------------------

  diff_ari <-
    d$window_ARI -
    d$spline_ARI

  cat(
    sprintf(
      paste0(
        "ARI: WINDOW - SPLINE = ",
        "%.4f +/- %.4f\n"
      ),
      mean(
        diff_ari,
        na.rm = TRUE
      ),
      sd(
        diff_ari,
        na.rm = TRUE
      )
    )
  )

  if (
    sum(
      is.finite(
        diff_ari
      )
    ) >= 3
  ) {

    wt <- wilcox.test(
      d$window_ARI,
      d$spline_ARI,
      paired = TRUE,
      exact = FALSE
    )

    cat(
      sprintf(
        "  paired Wilcoxon p = %.5f\n",
        wt$p.value
      )
    )
  }

  # --------------------------------------------------------------------------
  # NMI
  # --------------------------------------------------------------------------

  diff_nmi <-
    d$window_NMI -
    d$spline_NMI

  cat(
    sprintf(
      paste0(
        "NMI: WINDOW - SPLINE = ",
        "%.4f +/- %.4f\n"
      ),
      mean(
        diff_nmi,
        na.rm = TRUE
      ),
      sd(
        diff_nmi,
        na.rm = TRUE
      )
    )
  )

  if (
    sum(
      is.finite(
        diff_nmi
      )
    ) >= 3
  ) {

    wt <- wilcox.test(
      d$window_NMI,
      d$spline_NMI,
      paired = TRUE,
      exact = FALSE
    )

    cat(
      sprintf(
        "  paired Wilcoxon p = %.5f\n",
        wt$p.value
      )
    )
  }
}


# ==============================================================================
# 21. EQUIVALENCE-STYLE DIAGNOSTIC
# ==============================================================================

# Instead of asking only whether WINDOW and SPLINE differ significantly,
# it is useful to ask whether their practical difference is small.
#
# Here we use an illustrative ARI tolerance of 0.05.
#
# This is NOT a formal equivalence test. It is a descriptive diagnostic.

ARI_TOLERANCE <- 0.05

cat(
  "\n\n",
  "======================================================================\n",
  "PRACTICAL SIMILARITY: WINDOW VS SPLINE\n",
  "======================================================================\n"
)

for (
  jitter in JITTER_LEVELS
) {

  d <- results[
    results$jitter == jitter,
    ,
    drop = FALSE
  ]

  diff <- abs(
    d$window_ARI -
    d$spline_ARI
  )

  proportion_close <- mean(
    diff <= ARI_TOLERANCE,
    na.rm = TRUE
  )

  cat(
    sprintf(
      paste0(
        "Jitter %.2f: ",
        "P(|ARI_window - ARI_spline| <= %.2f) = %.3f\n"
      ),
      jitter,
      ARI_TOLERANCE,
      proportion_close
    )
  )
}


# ==============================================================================
# 22. PLOTS
# ==============================================================================

# --------------------------------------------------------------------------
# ARI vs irregularity
# --------------------------------------------------------------------------

mean_by_jitter <- aggregate(
  cbind(
    oracle_ARI,
    window_ARI,
    spline_ARI
  ) ~ jitter,
  data = results,
  FUN = mean
)

ylim_ari <- range(
  c(
    mean_by_jitter$oracle_ARI,
    mean_by_jitter$window_ARI,
    mean_by_jitter$spline_ARI
  ),
  na.rm = TRUE
)

plot(
  mean_by_jitter$jitter,
  mean_by_jitter$oracle_ARI,
  type = "b",
  pch = 16,
  lty = 1,
  ylim = ylim_ari,
  xlab = "Visit-time jitter SD",
  ylab = "Mean ARI",
  main = "longTAPIO under irregular sampling"
)

lines(
  mean_by_jitter$jitter,
  mean_by_jitter$window_ARI,
  type = "b",
  pch = 17,
  lty = 2
)

lines(
  mean_by_jitter$jitter,
  mean_by_jitter$spline_ARI,
  type = "b",
  pch = 15,
  lty = 3
)

legend(
  "bottomleft",
  legend = c(
    "Oracle regular grid",
    "Window discretization",
    "Spline reconstruction"
  ),
  pch = c(
    16,
    17,
    15
  ),
  lty = c(
    1,
    2,
    3
  ),
  bty = "n"
)


# --------------------------------------------------------------------------
# Loss relative to oracle
# --------------------------------------------------------------------------

mean_loss <- aggregate(
  cbind(
    window_loss_ARI,
    spline_loss_ARI
  ) ~ jitter,
  data = results,
  FUN = mean
)

ylim_loss <- range(
  c(
    0,
    mean_loss$window_loss_ARI,
    mean_loss$spline_loss_ARI
  ),
  na.rm = TRUE
)

plot(
  mean_loss$jitter,
  mean_loss$window_loss_ARI,
  type = "b",
  pch = 17,
  lty = 2,
  ylim = ylim_loss,
  xlab = "Visit-time jitter SD",
  ylab = "ARI loss relative to oracle",
  main = "Cost of irregular-time preprocessing"
)

lines(
  mean_loss$jitter,
  mean_loss$spline_loss_ARI,
  type = "b",
  pch = 15,
  lty = 3
)

abline(
  h = 0,
  lty = 3
)

legend(
  "topleft",
  legend = c(
    "Window",
    "Spline"
  ),
  pch = c(
    17,
    15
  ),
  lty = c(
    2,
    3
  ),
  bty = "n"
)


# ==============================================================================
# 23. SAVE RESULTS
# ==============================================================================

write.csv(
  results,
  file = "longTAPIO_irregular_time_raw_results.csv",
  row.names = FALSE
)

write.csv(
  summary_table,
  file = "longTAPIO_irregular_time_summary.csv",
  row.names = FALSE
)

write.csv(
  loss_table,
  file = "longTAPIO_irregular_time_losses.csv",
  row.names = FALSE
)


# ==============================================================================
# 24. FINAL OUTPUT
# ==============================================================================

cat(
  "\n\n",
  "======================================================================\n",
  "BENCHMARK COMPLETE\n",
  "======================================================================\n"
)

cat(
  "\nInterpretation:\n\n"
)

cat(
  paste0(
    "1. Compare WINDOW against SPLINE directly.\n",
    "2. Compare both against the regular-grid ORACLE.\n",
    "3. Check whether WINDOW degradation increases with visit-time jitter.\n",
    "4. Check whether SPLINE materially reduces that degradation.\n",
    "5. Inspect the empty-window fraction as irregularity increases.\n",
    "6. If WINDOW remains close to SPLINE, simple temporal discretization\n",
    "   is likely sufficient for longTAPIO.\n"
  )
)