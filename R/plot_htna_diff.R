#' Plot the Difference Between Two htna Networks
#'
#' Renders the elementwise edge-weight difference `x - y` as a heterogeneous
#' transition network using [plot_htna()]. Both networks must share the same
#' actor partition (same group labels in `$node_groups`).
#'
#' This is the htna-side workaround for `cograph::plot_compare()`, which does
#' not (yet) accept a `node_list` or pre-computed multi-group layout. As a
#' result, the signed (positive / negative) edge coloring that `plot_compare`
#' provides upstream is lost here - edges are colored as in any other
#' [plot_htna()] call. When cograph adds `node_list` (or a layout-matrix
#' argument) to `plot_compare`, swap this implementation for a real
#' `plot_compare` wrapper that preserves diff semantics.
#'
#' @param x,y htna networks produced by [build_htna()] with matching
#'   actor partitions.
#' @param ... Forwarded to [cograph::plot_htna()]. User args win.
#'
#' @return The value returned by [cograph::plot_htna()] (invisibly).
#' @seealso [plot_htna()], [build_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net1 <- build_htna(list(Human = human_long,        AI = ai_long))
#' net2 <- build_htna(list(Human = human_long[1:50,], AI = ai_long))
#' plot_htna_diff(net1, net2)
#' }
#'
#' @export
plot_htna_diff <- function(x, y, ...) {
  if (!inherits(x, "htna") || !inherits(y, "htna")) {
    stop("Both `x` and `y` must be htna networks.", call. = FALSE)
  }
  if (!identical(sort(unique(x$node_groups$group)),
                 sort(unique(y$node_groups$group)))) {
    stop("x and y must share the same actor partition.", call. = FALSE)
  }

  diff_mat <- cograph::to_matrix(x) - cograph::to_matrix(y)
  rownames(diff_mat) <- colnames(diff_mat) <- x$nodes$label

  node_list <- split(x$node_groups$node, x$node_groups$group)
  defaults  <- .htna_style_defaults(x)
  args      <- modifyList(defaults, list(...))

  do.call(cograph::plot_htna,
          c(list(x = diff_mat, node_list = node_list), args))
}
