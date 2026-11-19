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
#' `plot_htna_bootstrap()` draws the result of [bootstrap_htna()], or the
#' closed-form result of [certainty_htna()], in the layout and actor styling
#' of [plot_htna()]. With `display = "significant"`
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
#' @param boot,x A result of [bootstrap_htna()] (an `htna_bootstrap` or an
#'   `htna_bootstrap_group`) or of [certainty_htna()].
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
#' `htna_bad_input` when `boot` is not a result of [bootstrap_htna()] or
#' [certainty_htna()] on an htna network.
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
      (is.list(boot) && !inherits(boot, "net_bootstrap"))) {
    if (length(boot) == 0L) .htna_stop("Empty htna_bootstrap_group.",
                                       class = "htna_empty_input")
    nms <- names(boot) %||% as.character(seq_along(boot))
    lapply(seq_along(boot), function(i) {
      plot_htna_bootstrap(boot[[i]], group_colors = group_colors,
                          title = nms[i], ...)
    })
    return(invisible(boot))
  }
  if (!inherits(boot, "net_bootstrap")) {
    .htna_stop("`boot` must come from bootstrap_htna() or certainty_htna().")
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

#' Colours, shapes and coordinates of the nodes of an htna network
#'
#' The actor types are laid out on arcs of a circle, the first on the left,
#' as `plot_htna()` draws them. The start and end states are not an arc:
#' they are anchors above and below the actor types, whose coordinates are
#' therefore the same with and without them.
#' @return A list: `groups`, `colors` and `shapes` (legend entries, in actor
#'   order), `layout` (one row of x, y per node, radius 2), `node_colors`,
#'   `node_shapes`, `dark` (per node) and `n_groups`.
#' @keywords internal
#' @noRd
.htna_node_styling <- function(net, group_colors,
                               group_shapes = htna_shape_palette) {
  groups   <- net$actor_levels %||% unique(as.character(net$node_groups$group))
  n_groups <- length(groups)
  colors   <- unname(.htna_actor_colors(n_groups, palette = group_colors))
  shapes   <- rep_len(group_shapes, n_groups)

  ng       <- net$node_groups
  labels   <- net$nodes$label %||% rownames(.weights_of(net))
  boundary <- .htna_boundary_nodes(net)

  # The actor types that get an arc. cograph's circular layout places the
  # first list entry clockwise from the top, i.e. on the right for two
  # types; the reversed list puts the first actor type on the left.
  arc_groups <- if (length(boundary)) setdiff(groups, .boundary_type) else groups
  node_list  <- lapply(arc_groups, function(g) {
    as.character(ng$node[as.character(ng$group) == g])
  })
  names(node_list) <- arc_groups
  if (length(arc_groups) >= 2L) node_list <- rev(node_list)
  layout <- .htna_place_boundary(.htna_circular(node_list, labels), labels,
                                 boundary, n_arcs = length(arc_groups))

  node_to_actor <- setNames(as.character(ng$group), as.character(ng$node))
  actor_per     <- unname(node_to_actor[labels])
  node_colors   <- unname(setNames(colors, groups)[actor_per])
  node_shapes   <- unname(setNames(shapes, groups)[actor_per])

  # The legend names the start and end states only when they are drawn.
  shown <- groups != .boundary_type | length(boundary) > 0L |
    .boundary_type %in% actor_per
  list(groups = groups[shown], n_groups = n_groups,
       colors = colors[shown], shapes = shapes[shown],
       node_list = node_list, layout = layout, boundary = boundary,
       node_colors = node_colors, node_shapes = node_shapes,
       dark = .darken(node_colors))
}

#' The start and end states among the nodes of a network
#' @return A character vector named `start` and/or `end`; empty when the
#'   network has none.
#' @keywords internal
#' @noRd
.htna_boundary_nodes <- function(net) {
  boundary <- net$boundary
  if (is.null(boundary)) return(character(0L))
  labels <- net$nodes$label %||% rownames(.weights_of(net))
  boundary[boundary %in% labels]
}

#' Place the start state above and the end state below the actor types
#'
#' With two actor types the circle has a gap at the top and at the bottom,
#' and the two states sit in them, on the circle. With more, the arcs cover
#' the top and bottom, so the states go above and below the bounding box of
#' the actor-type nodes, centred on it.
#' @keywords internal
#' @noRd
.htna_place_boundary <- function(layout, labels, boundary, n_arcs,
                                 radius = 2) {
  if (!length(boundary)) return(layout)
  rows <- match(boundary, labels)
  if (n_arcs == 2L) {
    centre <- 0
    height <- c(start = radius, end = -radius)
  } else {
    arcs <- layout[-rows, , drop = FALSE]
    centre <- mean(range(arcs[, 1L]))
    height <- c(start = max(arcs[, 2L]) + 0.3 * radius,
                end = min(arcs[, 2L]) - 0.3 * radius)
  }
  layout[rows, ] <- cbind(centre, unname(height[names(boundary)]))
  layout
}
