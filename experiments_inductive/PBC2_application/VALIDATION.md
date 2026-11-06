# Full PBC2 rerun

Validated on 6 November 2026 with the installed **iTAPIO 1.3** package,
using the complete `pbc2_application.R` script without changing its analysis.
Outputs were written to a separate directory.

- 72 training patients, 33 held-out patients; two clusters, 500 trees, five visits.
- All seven CSV tables and nine requested PDF figures were generated successfully.
- At every visit, `predict()` assignments matched `importance()$predicted_cluster`.
- Patient-level importance was finite. Saved ARI/NMI and both importance tables
  reproduced the repository's previous results (importance rows matched by keys).

| Visits | ARI | NMI | Agreement with final assignment |
| --- | --- | --- | --- |
| 1 | 0.181912 | 0.143275 | 0.727273 |
| 2 | 0.468660 | 0.372782 | 0.848485 |
| 3 | 0.764855 | 0.697095 | 0.939394 |
| 4 | 0.878704 | 0.818297 | 0.969697 |
| 5 | 1.000000 | 1.000000 | 1.000000 |

Environment: R 4.5.3, DynForest 1.2.0, survminer 0.5.2, ggplot2 4.0.3,
ggpubr 1.0.0. One upstream ggpubr warning concerned the deprecated `size`
aesthetic for lines; it did not prevent any output. ARI/NMI measure early versus
final assignment to the frozen training clusters, not survival-outcome accuracy.
