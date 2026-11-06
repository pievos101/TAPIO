# Noise comparison: inductive versus untouched transductive longTAPIO

Completed 6 November 2026: **160 successful fits**, covering regular/irregular
times × four noise levels × ten paired datasets × two methods. There are no
fixed RNG initializations. Fresh runs give different data, splits and fits.

## Design

- `simLongData(n_total=120, eta=..., ranTimes=...)` with eta 1, 3, 6, 9;
  five biomarkers and four true clusters. Regular data have eight visits.
  Irregular data explicitly use `ranTimes=TRUE`, then eight common windows.
- Random patient split, stratified by simulated cluster: 84 training and 36 test
  patients. Each pair of methods receives the exact same saved dataset and split.
- For irregular data, training patients determine window boundaries; held-out
  patients reuse them. Both methods receive identical discretized values.
  Out-of-range held-out observations are ignored with the preprocessor's warning.
- Both use 100 trees, three hierarchy levels, two sampled features per tree,
  `random_weighted` PCA, scaled features and Ward.D2 linkage.
- Inductive `iTAPIO::longTAPIO()` fits training patients and predicts the test set.
  Transductive `TAPIO::longTAPIO_trajectories()` clusters the full 120-patient
  cohort, including test features. Both are scored on the same 36 held-out labels.
  Main is a transductive reference, not an out-of-sample predictor.
- Baseline source: untouched main commit
  `d281950a7ecfe2ac329d60853141b1ff31de9a7c` (TAPIO 1.2).
  Separate libraries and fresh R processes prevent namespace collisions.
- ARI/NMI use aricode, with its default NMI variant `max`. Labels are used for
  stratification and evaluation, not supplied to either clustering algorithm.

## Observed results

Means over ten pairs. Times are elapsed batch fit-plus-assignment seconds,
excluding simulation, shared preprocessing, package loading and scoring.
Main has no separate prediction step. Runtime measurements are descriptive.

| Timing | eta | Main ARI / NMI | Inductive ARI / NMI | Main s | Inductive s |
| --- | --- | --- | --- | --- | --- |
| regular | 1 | 0.992 / 0.993 | 0.977 / 0.980 | 0.155 | 0.373 |
| regular | 3 | 0.984 / 0.987 | 0.956 / 0.966 | 0.196 | 0.416 |
| regular | 6 | 0.932 / 0.941 | 0.940 / 0.950 | 0.145 | 0.404 |
| regular | 9 | 0.853 / 0.882 | 0.899 / 0.918 | 0.147 | 0.418 |
| irregular | 1 | 0.978 / 0.981 | 0.978 / 0.981 | 0.144 | 0.376 |
| irregular | 3 | 0.977 / 0.980 | 0.970 / 0.974 | 0.143 | 0.379 |
| irregular | 6 | 0.850 / 0.887 | 0.890 / 0.914 | 0.144 | 0.385 |
| irregular | 9 | 0.759 / 0.805 | 0.743 / 0.794 | 0.140 | 0.381 |

The maximum absolute difference in mean ARI was **0.0461** across the eight
settings: comparable mean accuracy in these simulations. This is descriptive
evidence, not a formal equivalence claim or a guarantee on other datasets.
The README plot shows mean ± standard error. Paired mean ARI differences and
95% t intervals are in [paired-differences.csv](paired-differences.csv).

- [Raw results](results.csv): all 160 fits, without dropped failures.
- [Summary](summary.csv): means, SDs, counts and fit/prediction times.
- [Versions](versions.csv) and [session information](sessionInfo.txt).
- [Plot script](plot.R).

## Run again

Install both packages' dependencies and aricode, then from this repository root:

```sh
mkdir -p /tmp/tapio-main-src /tmp/tapio-main-lib /tmp/itapio-lib
git archive d281950a7ecfe2ac329d60853141b1ff31de9a7c -o /tmp/tapio-main.tar
tar -xf /tmp/tapio-main.tar -C /tmp/tapio-main-src
R CMD INSTALL --library=/tmp/tapio-main-lib /tmp/tapio-main-src
R CMD INSTALL --library=/tmp/itapio-lib .
Rscript --vanilla benchmarks/run.R /tmp/tapio-main-lib /tmp/itapio-lib /tmp/noise-comparison 10
```

The output directory retains each generated dataset, worker logs and results.
A failed fit stops the run; the script does not retry until a favorable outcome.
To redraw the README plot, copy the new summary.csv to benchmarks/ and run
`Rscript --vanilla benchmarks/plot.R`. Exact scores vary because randomness is
not fixed. Model code is unchanged; this update changes data packaging, examples
and validation rather than tuning algorithms to match a desired conclusion.
