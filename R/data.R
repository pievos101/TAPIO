#' PBC2 longitudinal primary biliary cirrhosis cohort
#'
#' Mayo Clinic follow-up data for the PBC2 application, bundled so the example
#' does not require DynForest. Values and missingness are unchanged upstream.
#' @name pbc2
#' @docType data
#' @usage data(pbc2, package = "iTAPIO")
#' @format A data frame with 1,945 observations, 19 variables and 312 patients:
#' \describe{
#'   \item{id, time}{Patient identifier and measurement time in years.}
#'   \item{ascites, hepatomegaly, spiders, edema}{Clinical signs.}
#'   \item{serBilir, serChol, albumin, alkaline, SGOT, platelets, prothrombin}{Biomarkers.}
#'   \item{histologic}{Histologic stage.}
#'   \item{drug, age, sex}{Treatment and baseline characteristics.}
#'   \item{years}{Time to event in years.}
#'   \item{event}{0: alive; 1: transplanted; 2: died.}
#' }
#' @source DynForest 1.2.0 (LGPL >= 3), CRAN mirror commit
#'   7c7d489e560bb0810b18b68ff05c7e073b8ce1e2. Upstream documentation credits joineRML.
#'   See the bundled DATA-SOURCES notice for attribution.
"pbc2"

#' Anderson's Iris measurements
#'
#' Unmodified copy of the standard Iris data, included for self-contained examples.
#' @name iris
#' @docType data
#' @usage data(iris, package = "iTAPIO")
#' @format A data frame with 150 flowers, four numeric measurement columns
#'   (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width; centimeters) and
#'   a Species factor with three levels, 50 flowers per species.
#' @source R datasets 4.5.3; Fisher (1936), The use of multiple measurements in
#'   taxonomic problems; measurements collected by Edgar Anderson (1935).
#'   See the bundled DATA-SOURCES notice for attribution.
"iris"
