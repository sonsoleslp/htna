#' Plot the bootstrap-assessed edges of an htna network
#'
#' @description
#' `plot_htna_bootstrap()` plots the bootstrap-assessed edges of an htna
#' network: it shows which transitions of a heterogeneous network are
#' stable enough to interpret. Edges that are significant across resamples
#' are drawn solid, the others dashed (or hidden), so the reader sees which
#' within-actor transitions and which hand-overs between actors the data
#' support. A bootstrap of a transition network resamples the sessions many
#' times and records how often each edge is present and how much its weight
#' varies.
#'
#' `plot_htna_bootstrap()` draws the result of [bootstrap_htna()] in the
#' layout and actor styling of [plot_htna()]. With `display = "significant"`
#' only the significant edges are drawn; `show_ci = TRUE` adds the
#' confidence interval of each edge weight. A grouped bootstrap is drawn as
#' one plot per cohort, titled with its cohort name. `plot()` on a
#' bootstrap result calls this function. It is called for the plot and
#' returns, invisibly, the value of the drawing call, or `boot` for a
#' grouped bootstrap.
#'
#' The table behind the plot (one row per edge, with its weight, interval
#' and p-value) is returned by `summary()` of the bootstrap result.
#'
#' @param boot,x A result of [bootstrap_htna()]: an `htna_bootstrap` or an
#'   `htna_bootstrap_group`.
#' @param group_colors Colours of the actor types, in actor order. Default
#'   [htna_palette].
#' @param ... Further drawing arguments: `display` (`"styled"` (default),
#'   `"significant"` or `"full"`), `show_ci` (default `FALSE`), `show_stars`
#'   (default `TRUE`), `title`, and the arguments of [cograph::splot()].
#'
#' @return Called for the plot. Invisibly, the value of the drawing call for
#'   one bootstrap, or `boot` for a grouped bootstrap.
#'
#' @section Conditions:
#' `htna_empty_input` for a grouped bootstrap without cohorts;
#' `htna_bad_input` when `boot` is not a result of [bootstrap_htna()] on an
#' htna network.
#'
#' @seealso [bootstrap_htna()], [plot_htna()], [certainty_htna()]
#'
#' @examples
#' data(human_ai)
#' net  <- build_htna(human_ai, actor_type = "actor_type")
#' boot <- bootstrap_htna(net, iter = 50, seed = 1)
#' plot_htna_bootstrap(boot)
#' plot_htna_bootstrap(boot, display = "significant")
#'
#' @export
plot_htna_bootstrap <- function(boot, group_colors = htna_palette, ...) {
  if (inherits(boot, "htna_bootstrap_group") ||
      (is.list(boot) && !inherits(boot, "htna_bootstrap"))) {
    if (length(boot) == 0L) .htna_stop("Empty htna_bootstrap_group.",
                                       class = "htna_empty_input")
    nms <- names(boot) %||% as.character(seq_along(boot))
    lapply(seq_along(boot), function(i) {
      plot_htna_bootstrap(boot[[i]], group_colors = group_colors,
                          title = nms[i], ...)
    })
    return(invisible(boot))
  }
  if (!inherits(boot, "htna_bootstrap")) {
    .htna_stop("`boot` must come from bootstrap_htna().")
  }
  net <- boot$model
  if (is.null(net) || is.null(net$node_groups)) {
    .htna_stop("boot$model has no $node_groups; was it built by build_htna()?")
  }

  ng_styling <- .htna_node_styling(net, group_colors)

  defaults <- list(
    x      = boot,
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
    legend        = FALSE
  )
  user_args <- list(...)
  defaults[names(user_args)] <- user_args
  result <- do.call(cograph::splot.net_bootstrap, defaults)
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
#' @noRd
.htna_node_styling <- function(net, group_colors,
                               group_shapes = htna_shape_palette) {
  groups   <- net$actor_levels %||% unique(as.character(net$node_groups$group))
  n_groups <- length(groups)
  colors   <- unname(.htna_actor_colors(n_groups, palette = group_colors))
  shapes   <- rep_len(group_shapes, n_groups)

  ng        <- net$node_groups
  node_list <- lapply(groups,
                      function(g) ng$node[as.character(ng$group) == g])
  names(node_list) <- groups

  # Apply the same reversal as plot_htna() for consistent layout and styling
  # across plotting functions. This ensures the first group appears on the
  # left for 2+ group networks.
  layout_node_list <- node_list
  layout_colors    <- colors
  layout_shapes    <- shapes
  if (n_groups >= 2L) {
    layout_node_list <- rev(node_list)
    layout_colors    <- rev(colors)
    layout_shapes    <- rev(shapes)
  }

  labels <- net$nodes$label %||% rownames(.weights_of(net))
  layout <- .htna_circular(layout_node_list, labels)

  # Use the layout-reversed colors and shapes for consistent styling
  if (n_groups >= 2L) {
    color_map     <- setNames(layout_colors, names(layout_node_list))
    shape_map     <- setNames(layout_shapes, names(layout_node_list))
  } else {
    color_map     <- setNames(colors, groups)
    shape_map     <- setNames(shapes, groups)
  }
  node_to_actor <- setNames(as.character(ng$group), as.character(ng$node))
  actor_per     <- unname(node_to_actor[labels])
  node_colors   <- unname(color_map[actor_per])
  node_shapes   <- unname(shape_map[actor_per])
  dark          <- .darken(node_colors)

  list(groups   = groups,    n_groups = n_groups,
       colors   = layout_colors,    shapes   = layout_shapes,
       node_list = layout_node_list,
       layout   = layout,    node_colors = node_colors,
       node_shapes = node_shapes, dark = dark)
}
