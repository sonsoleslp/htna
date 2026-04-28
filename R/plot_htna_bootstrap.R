#' Plot an htna Bootstrap Result
#'
#' Composes htna's visual defaults and the htna multi-group circular layout
#' with cograph's bootstrap renderer ([cograph::splot.net_bootstrap()]). The
#' CI / significance overlay is whatever cograph produces; htna only owns the
#' geometry and the visual identity (so the bootstrap plot looks like the
#' same network as [plot_htna()]).
#'
#' The layout is computed on the htna side and injected as a matrix together
#' with `rescale = FALSE` and `layout_scale = 1`. Those three arguments
#' together are the contract that lets the htna geometry survive into
#' cograph's renderer unchanged - drop any one and cograph re-normalizes.
#'
#' @param boot An object returned by [bootstrap_htna()].
#' @param ... Forwarded to [cograph::splot.net_bootstrap()]. User args win
#'   over the htna defaults.
#'
#' @return The value returned by [cograph::splot.net_bootstrap()] (invisibly).
#' @seealso [bootstrap_htna()], [plot_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net  <- build_htna(list(Human = human_long, AI = ai_long))
#' boot <- bootstrap_htna(net, iter = 200)
#' plot_htna_bootstrap(boot)
#' plot_htna_bootstrap(boot, display = "significant")
#' }
#'
#' @export
plot_htna_bootstrap <- function(boot, ...) {
  if (!inherits(boot, "htna_bootstrap")) {
    stop("`boot` must come from bootstrap_htna().", call. = FALSE)
  }
  net <- boot$model
  if (is.null(net) || is.null(net$node_groups)) {
    stop("boot$model has no $node_groups; was it built by build_htna()?",
         call. = FALSE)
  }

  node_list  <- split(net$node_groups$node, net$node_groups$group)
  labels     <- net$nodes$label %||% rownames(.weights_of(net))
  xy         <- .htna_circular_layout(node_list, labels, angle_spacing = 0.35)
  layout_mat <- cbind(x = xy$x, y = xy$y)

  defaults <- .htna_style_defaults(net)
  args     <- modifyList(
    defaults,
    list(layout = layout_mat, rescale = FALSE, layout_scale = 1, ...)
  )

  do.call(cograph::splot.net_bootstrap, c(list(x = boot), args))
}
