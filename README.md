# TAPIO — inductive clustering

This branch packages the inductive TAPIO and longTAPIO implementations. Fit an
ensemble with `TAPIO()` or `longTAPIO()`, then use `predict()` without refitting.
The core strength is visit-wise assignment for early risk stratification, with
patient- and visit-wise feature importance.

## Install

From this checkout:

```sh
R CMD INSTALL .
```

Install the `inductive` branch from GitHub:

```r
install.packages("remotes")
remotes::install_github("pievos101/TAPIO", ref = "inductive")
```

For the evaluation examples, install `aricode` once with
`install.packages("aricode")`.

## TAPIO: new samples

```r
library(TAPIO)
set.seed(42)
x <- as.matrix(iris[, 1:4])
train <- unlist(lapply(split(seq_len(nrow(x)), iris$Species),
                       function(i) sample(i, 35)))

fit <- TAPIO(x[train, ], k = 3, n_trees = 100, levels = 3)
fit$cluster                    # training assignments
pred <- predict(fit, x[-train, , drop = FALSE])
pred$cluster                   # new sample assignments
c(ARI = aricode::ARI(iris$Species[-train], pred$cluster),
  NMI = aricode::NMI(iris$Species[-train], pred$cluster))
importance_TAPIO_inductive(fit)
```

`random_weighted` is the default PCA selection. Use `pca_selection = "first"`
for PC1. Keep feature names and order identical at prediction time. Estimate
external preprocessing on the training data and reuse it for new data.

## longTAPIO: irregular times require discretization

**For irregular observation times, preprocess with `discretize_longitudinal()`
before fitting or predicting.** Estimate the time windows on training patients
only and reuse `reference$breaks` for new patients. This produces a common,
chronologically ordered representation with equal windows per patient.

```r
set.seed(42)
long <- simLongData(n_total = 40, ranTimes = TRUE)
wide <- reshape(long, idvar = c("subject", "time", "cluster"),
                timevar = "outcome", direction = "wide")
xlong <- as.matrix(wide[, grep("^y\\.", names(wide))])
patients <- unique(wide[, c("subject", "cluster")])
train_ids <- unlist(lapply(split(patients$subject, patients$cluster),
                          function(ids) ids[1:8]))
training <- wide$subject %in% train_ids

reference <- discretize_longitudinal(
    xlong[training, ], user_id = wide$subject[training],
    time = wide$time[training], n_windows = 5
)
new_patients <- discretize_longitudinal(
    xlong[!training, ], user_id = wide$subject[!training],
    time = wide$time[!training], breaks = reference$breaks
)
reference$empty_fraction        # empty patient-windows before interpolation
stopifnot(all(is.finite(as.matrix(reference$DATA))),
          all(is.finite(as.matrix(new_patients$DATA))))

long_fit <- longTAPIO(reference$DATA, user_id = reference$user_id,
                               k = 4, levels = 3, n_trees = 100)
long_pred <- predict(long_fit, new_patients$DATA, user_id = new_patients$user_id)
truth <- patients$cluster[match(long_pred$patients, patients$subject)]
c(ARI = aricode::ARI(truth, long_pred$cluster),
  NMI = aricode::NMI(truth, long_pred$cluster))

long_imp <- importance_longTAPIO_inductive(
    long_fit, newdata = new_patients$DATA, user_id = new_patients$user_id
)
long_imp$global
```

Discretization averages observations within windows, interpolates missing values,
and by default fills edges with the nearest observed value. `fill_empty = FALSE`
keeps empty values; `extrapolate = FALSE` leaves unobserved edges missing. A
patient-feature with no finite observations remains missing. Inductive models
require complete numeric inputs, so inspect the result before fitting. Values
outside the training window range are ignored with a warning.

Regular data already aligned to the same visit positions can be supplied directly.
Training needs equal visit counts per patient and chronological within-patient
row order. The fitter itself does not use observation times.

## Predict from early histories

Use only measurements available at the cutoff before discretizing. Filling a
complete trajectory and then taking its prefix could use future observations.

```r
available <- !training & wide$time <= reference$breaks[3]
early <- discretize_longitudinal(
    xlong[available, ], user_id = wide$subject[available],
    time = wide$time[available], breaks = reference$breaks
)
prefix <- early$window <= 2
pred_early <- predict(long_fit, early$DATA[prefix, ],
                      user_id = early$user_id[prefix], visits = 2)
truth_early <- patients$cluster[match(pred_early$patients, patients$subject)]
c(ARI = aricode::ARI(truth_early, pred_early$cluster),
  NMI = aricode::NMI(truth_early, pred_early$cluster))
```

`visits` is the number of leading aligned visits/windows to use, up to the training
count. Prediction returns sorted `patients`, `cluster`, `cluster_scores`, and
`margin`. Scores and margins are affinities, not calibrated probabilities.
Patients without available measurements need more data before prediction.

## PBC2: visit-wise risk assignment and feature importance

The [PBC2 application](experiments_inductive/PBC2_application/pbc2_application.R)
uses two trajectory clusters, characterizes risk using training survival, and
assigns held-out patients after visits 1–5. Biomarker importance is available for
each patient and visit. Using the importance result from the example above:

```r
long_imp$predicted_cluster[, 2]       # assignments after the first two visits
long_imp$patient[, , 2]              # patient-by-biomarker importance at visit 2
long_imp$cluster_progressive[, , 2]   # importance grouped by final cluster
```

The full application needs `DynForest`, `survival`, `survminer`, `ggplot2`, and
`aricode`. Its early-versus-final ARI/NMI measure assignment stability; survival
analysis characterizes risk. For irregular elapsed times, discretize first using
fixed training boundaries, as above.

## Interpretation and model storage

ARI and NMI compare predicted clusters with reference labels independently of
cluster numbering; both reach 1 for perfect agreement. ARI is adjusted for chance
and can be negative. NMI ranges from 0 to 1. Labels are used only for evaluation.

Both fitters default to `pca_selection = "random_weighted"`. Longitudinal fitting
also supports `"random_weighted_95"`. Choose `levels` smaller than the training
sample/patient count, and specify the desired cluster count with `k`.

Save models with `saveRDS()` and restore them with `readRDS()`. For longitudinal
use, also retain the training discretization breaks and preprocessing settings.
The original transductive API remains on `main`; this branch exposes the existing
inductive function names directly.

## Validation and comparison with main

```sh
Rscript --vanilla tests/inductive.R
Rscript --vanilla tests/discretize.R
R CMD build .
R CMD check --no-manual TAPIO_1.3.tar.gz
```

See [benchmarks/README.md](benchmarks/README.md) for the reproducible accuracy and
runtime comparison with the untouched `main` package, including ARI/NMI on shared
held-out samples and patients.
