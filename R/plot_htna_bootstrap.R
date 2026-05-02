#' Plot an htna Bootstrap Result
#'
#' Renders the bootstrap result via [cograph::splot.net_bootstrap()] so the
#' CI / significance / dashed-edge overlay is preserved, but overrides the
#' layout and node styling to match [plot_htna()] (multi-group circular
#' layout, warm [htna_palette], darkened donut).
#'
#' For an `htna_bootstrap_group` (i.e. the result of running
#' [bootstrap_htna()] on a `build_htna(..., group = ...)` output), each
#' element is plotted individually with its name as the title - this
#' function does not manage `par(mfrow=...)`.
#'
#' @param boot An object from [bootstrap_htna()] (or a `htna_bootstrap_group`
#'   list of them).
#' @param x Same as `boot`; used when calling via the `plot()` generic.
#' @param group_colors Character vector of colours, one per actor group.
#'   Defaults to the built-in [htna_palette].
#' @param ... Forwarded to [cograph::splot.net_bootstrap()] (e.g. `display`,
#'   `show_ci`, `show_stars`). User args win.
#'
#' @return The value returned by [cograph::splot.net_bootstrap()]
#'   (invisibly), or the input group invisibly.
#' @seealso [bootstrap_htna()], [plot_htna()],
#'   [cograph::splot.net_bootstrap()].
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
#' @rdname plot_htna_bootstrap
plot_htna_bootstrap <- function(boot, group_colors = htna_palette, ...) {
  if (inherits(boot, "htna_bootstrap_group") ||
      (is.list(boot) && !inherits(boot, "htna_bootstrap"))) {
    if (length(boot) == 0L) stop("Empty htna_bootstrap_group.", call. = FALSE)
    nms <- names(boot) %||% as.character(seq_along(boot))
    for (i in seq_along(boot)) {
      plot_htna_bootstrap(boot[[i]], group_colors = group_colors,
                          title = nms[i], ...)
    }
    return(invisible(boot))
  }
  if (!inherits(boot, "htna_bootstrap")) {
    stop("`boot` must come from bootstrap_htna().", call. = FALSE)
  }
  net <- boot$model
  if (is.null(net) || is.null(net$node_groups)) {
    stop("boot$model has no $node_groups; was it built by build_htna()?",
         call. = FALSE)
  }

  ng_styling <- .htna_node_styling(net, group_colors)

  result <- do.call(cograph::splot.net_bootstrap, c(
    list(x      = boot,
         layout = ng_styling$layout,
         node_fill     = ng_styling$node_colors,
         node_shape    = ng_styling$node_shapes,
         node_size     = 12,
         donut_color   = ng_styling$dark,
         curvature     = 0.4,
         layout_margin = 0.15,
         rescale       = FALSE,
         layout_scale  = 1,
         tna_styling   = TRUE,
         edge_label_size = 0.55,
         label_size      = 0.8,
         legend        = FALSE),
    list(...)
  ))
  if (ng_styling$n_groups >= 2L) {
    .htna_legend(ng_styling$groups, ng_styling$colors)
  }
  invisible(result)
}

#' @export
#' @rdname plot_htna_bootstrap
plot.htna_bootstrap <- function(x, ...) plot_htna_bootstrap(x, ...)

htna_shape_palette <- c("circle", "square", "diamond", "triangle",
                        "pentagon", "hexagon", "star", "cross")

#' @keywords internal
.htna_node_styling <- function(net, group_colors,
                               group_shapes = htna_shape_palette) {
  groups   <- net$actor_levels %||% unique(as.character(net$node_groups$group))
  n_groups <- length(groups)
  colors   <- group_colors[seq_len(n_groups)]
  shapes   <- rep_len(group_shapes, n_groups)

  ng        <- net$node_groups
  node_list <- lapply(groups,
                      function(g) ng$node[as.character(ng$group) == g])
  names(node_list) <- groups

  labels <- net$nodes$label %||% rownames(.weights_of(net))
  layout <- .htna_circular(node_list, labels)

  color_map     <- setNames(colors, groups)
  shape_map     <- setNames(shapes, groups)
  node_to_actor <- setNames(as.character(ng$group), as.character(ng$node))
  actor_per     <- unname(node_to_actor[labels])
  node_colors   <- unname(color_map[actor_per])
  node_shapes   <- unname(shape_map[actor_per])
  dark          <- .darken(node_colors)

  list(groups   = groups,    n_groups = n_groups,
       colors   = colors,    shapes   = shapes,
       node_list = node_list,
       layout   = layout,    node_colors = node_colors,
       node_shapes = node_shapes, dark = dark)
}
