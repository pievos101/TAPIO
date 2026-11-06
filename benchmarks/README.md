# Accuracy and runtime against untouched main

Run on 6 November 2026 with R 4.5.3 on the same Linux cloud machine.
All 60 fits completed: 3 scenarios × 10 paired seeds × 2 methods. Each method
runs in a fresh R process with a separate package library, one BLAS/OpenMP thread,
and the exact same generated dataset and held-out IDs as its counterpart.

## Sources and settings

- Baseline: untouched `main` commit `d281950a7ecfe2ac329d60853141b1ff31de9a7c`,
  package version 1.2. All 219 source files were verified against their Git blobs.
- Inductive: the version 1.3 package based on `inductive` commit
  `acf2d56bf44781343596fe391d367ab0b0397791`, with the package changes in this review.
- Seeds 1–10; 100 trees; 3 hierarchy levels; 2 features per tree;
  `pca_selection = "random_weighted"`; Ward.D2 linkage; feature sampling with replacement.
- Iris: 150 samples, 3 clusters; 105 training and 45 held-out samples, stratified
  by species. Min-max bounds come from training data only, and are shared by
  both methods. PCA scaling is off for both.
- Longitudinal: 80 simulated patients, 4 clusters; 56 training and 24 held-out
  patients, stratified by true cluster. Five regular visits or five discretized
  windows and five features. PCA scaling is on for both.
- Irregular observations are **discretized before either method is run**.
  Training patients determine the five window boundaries; held-out patients reuse
  those exact boundaries. Both packages receive the same aligned feature values.
  Across five runs, 1, 4, 2, 1 and 1 held-out observation rows respectively fell
  outside the training range and were ignored by the preprocessor with warnings.
- Reference labels are used for splitting/scoring, not passed to either fitter.
  Longitudinal labels are matched by patient ID. ARI/NMI use `aricode`; NMI uses
  its default `variant = "max"`.

## What is being compared

The inductive package fits training subjects only and calls `predict()` on held-out
subjects. The original `main` package cannot predict unseen subjects: it clusters
**all training and held-out subjects together**, using `TAPIO()` or
`longTAPIO_trajectories()`. We score its clustering on the same held-out subjects.
Thus main has access to their features during fitting, while the inductive model
does not. This is a transductive reference comparison, not two out-of-sample
predictors. The sample and MLD longitudinal algorithms are outside this comparison.

## Results

Scores are means ± sample SD over 10 paired runs. Times are mean elapsed seconds
for each batch, excluding package loading, data simulation, common preprocessing,
and scoring. Main's prediction column is zero because it has no prediction step;
its fit time includes clustering the entire cohort.

| Scenario | Method | ARI | NMI | Fit s | Predict s | Total s |
| --- | --- | --- | --- | --- | --- | --- |
| Iris | main | 0.778 ± 0.112 | 0.803 ± 0.073 | 0.143 | 0.000 | 0.143 |
| Iris | inductive | 0.820 ± 0.095 | 0.832 ± 0.073 | 0.120 | 0.039 | 0.158 |
| Regular longitudinal | main | 0.842 ± 0.156 | 0.892 ± 0.106 | 0.106 | 0.000 | 0.106 |
| Regular longitudinal | inductive | 0.879 ± 0.114 | 0.914 ± 0.079 | 0.105 | 0.159 | 0.264 |
| Irregular longitudinal | main | 0.964 ± 0.081 | 0.974 ± 0.059 | 0.094 | 0.000 | 0.094 |
| Irregular longitudinal | inductive | 0.941 ± 0.116 | 0.958 ± 0.080 | 0.104 | 0.156 | 0.260 |

Paired mean score differences (`inductive − main`):

| Scenario | Δ ARI | Δ NMI |
| --- | --- | --- |
| Iris | +0.042 | +0.029 |
| Regular longitudinal | +0.037 | +0.022 |
| Irregular longitudinal | -0.023 | -0.015 |

Inductive accuracy was similar overall in these small examples: higher mean
scores for Iris and regular longitudinal data, and slightly lower mean scores for
irregular longitudinal data. Inductive fit-plus-predict took longer for these
batches. Its added capability is assigning new subjects with a frozen model;
these measurements do not establish general superiority or production latency.

Full per-run results are in [results.csv](results.csv), means/SDs in
[summary.csv](summary.csv), package versions in [versions.csv](versions.csv), and
R/library details in [sessionInfo.txt](sessionInfo.txt).
The README also demonstrates prefix prediction; these benchmark tables evaluate
complete held-out trajectories.

## Reproduce

Run from the repository root. First install both packages' dependencies and
`aricode` into the R installation used for comparison. Export main without editing
or switching its source, and install each package into its own library:

```sh
mkdir -p /tmp/tapio-main-src /tmp/tapio-main-lib /tmp/tapio-inductive-lib
git archive d281950a7ecfe2ac329d60853141b1ff31de9a7c -o /tmp/tapio-main.tar
tar -xf /tmp/tapio-main.tar -C /tmp/tapio-main-src
R CMD INSTALL --library=/tmp/tapio-main-lib /tmp/tapio-main-src
R CMD INSTALL --library=/tmp/tapio-inductive-lib .
Rscript --vanilla benchmarks/run.R /tmp/tapio-main-lib /tmp/tapio-inductive-lib /tmp/tapio-comparison 10
```

`run.R` prepares shared data once per scenario/seed and launches `worker.R` once
per method in isolated R processes. A worker failure stops the run; no failed fits
are silently dropped. The output directory contains the generated datasets, logs,
raw metrics, summaries, and session information.

The recorded results predate the package rename to `iTAPIO`; fitting algorithms
are unchanged. The runner now loads `iTAPIO` for the inductive worker and `TAPIO`
for the untouched main baseline.

## Package validation

- Both installed-package test scripts passed, including prediction dispatch,
  serialization, frozen models, progressive visits, and discretization.
- All executable README examples passed against the new library.
- `R CMD build` succeeded; `R CMD check --no-manual` finished with **0 errors,
  0 warnings, 1 note**. The remaining note is the existing LICENSE metadata:
  DESCRIPTION declares GPL (>= 3), while the retained LICENSE text is MIT.
  No license choice or license text was changed as part of packaging.
