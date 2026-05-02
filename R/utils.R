#' Internal Utilities
#'
#' Small, pure helpers shared across htna. Not exported.
#'
#' @keywords internal
#' @name htna-utils
NULL

#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
#' @noRd
.weights_of <- function(net) {
  net$weights %||% as.matrix(net)
}