#' @rdname inductive
#' @export
importance <- function(res, ...) {
    UseMethod("importance")
}

#' @rdname inductive
#' @export
importance.default <- function(res, ...) {
    stop("res must be a fitted TAPIO() or longTAPIO() model.")
}
