#' Plot a Heterogeneous Transition Network
#'
#' htna's wrapper around [cograph::plot_htna()]. Reads the actor partition
#' from the canonical `$node_groups` schema written by [build_htna()],
#' applies the htna visual defaults, and forwards everything to
#' [cograph::plot_htna()] with `node_list` passed explicitly (no reliance on
#' cograph's column-name auto-detection).
#'
#' User arguments in `...` always win over the defaults.
#'
#' @param x An htna network produced by [build_htna()].
#' @param ... Additional arguments forwarded to [cograph::plot_htna()]. These
#'   override the htna visual defaults.
#'
#' @return The value returned by [cograph::plot_htna()] (invisibly).
#' @seealso [build_htna()], [plot_htna_bootstrap()], [plot_htna_diff()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' plot_htna(net)
#' plot_htna(net, threshold = 0.1, legend = FALSE)
#' }
#'
#' @export
plot_htna <- function(x, ...) {
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network produced by build_htna().",
         call. = FALSE)
  }

  node_list <- split(x$node_groups$node, x$node_groups$group)
  defaults  <- .htna_style_defaults(x)
  args      <- modifyList(defaults, list(...))

  do.call(cograph::plot_htna,
          c(list(x = x, node_list = node_list), args))
}
