# Dataset provenance

`data/pbc2.rda` preserves the full, unmodified PBC2 object from DynForest 1.2.0,
CRAN mirror commit `7c7d489e560bb0810b18b68ff05c7e073b8ce1e2`.
`data/iris.rda` is an unmodified copy of `datasets::iris` from R 4.5.3.
Attribution and applicable license texts ship in `inst/DATA-SOURCES` and
`inst/data-licenses/`. Neither dataset requires DynForest at runtime.

Rebuild from the source data file with:

```sh
Rscript --vanilla data-raw/build.R /path/to/DynForest/data/pbc2.RData
```

The build script performs no random sampling or RNG initialization.
