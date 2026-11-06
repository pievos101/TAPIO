# PBC2 validation without fixed seeds

The full application was rerun on 6 November 2026 with installed iTAPIO 1.3,
using the bundled `data(pbc2, package="iTAPIO")`. DynForest was not loaded.
The patient split and model fitting used fresh randomness.

- 72 training and 33 held-out patients; two clusters, 500 trees, five visits.
- All nine CSV tables and nine requested PDF figures were generated.
- At every visit, direct `predict()` assignments matched `importance()`.
- Patient-level importance was finite; both risk groups were present.
- README figure inputs are saved in `readme_results/` for inspection.

| Visits | ARI | NMI | Agreement with final assignment |
| --- | --- | --- | --- |
| 1 | 0.243856 | 0.203666 | 0.757576 |
| 2 | 0.657795 | 0.535249 | 0.909091 |
| 3 | 0.657795 | 0.535249 | 0.909091 |
| 4 | 0.764855 | 0.697095 | 0.939394 |
| 5 | 1.000000 | 1.000000 | 1.000000 |

ARI/NMI quantify early versus final assignment to the frozen training clusters.
Risk labels use training survival only; Kaplan–Meier curves start at the fifth
visit landmark. The importance panels group patients by final risk cluster.

Environment: R 4.5.3, survminer 0.5.2, ggplot2 4.0.3, ggpubr 1.0.0.
One upstream ggpubr line-size deprecation warning did not prevent output.
New runs produce different splits and results because no RNG state is fixed.
