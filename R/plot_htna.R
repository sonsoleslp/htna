#' Colours of the actor types in htna plots
#'
#' @description
#' `htna_palette` gives the default fill colours of the actor types in the
#' network plots of htna: the first actor type takes the first colour, the
#' second the second, and so on. With more than six actor types the colours
#' are recycled with a warning, so pass a longer vector to `group_colors` in
#' that case. Each plot also gives the actor types different node shapes, so
#' the actor types are not distinguished by colour alone.
#'
#' @format A character vector of six hexadecimal colours.
#'
#' @return A character vector of six hexadecimal colours.
#'
#' @seealso [plot_htna()], [plot_htna_diff()], [plot_htna_bootstrap()]
#'
#' @examples
#' htna_palette
#' @export
htna_palette <- c(
  "#FFD966",
  "#B4A7D6",
  "#93C47D",
  "#E06666",
  "#6FA8DC",
  "#F6B26B"
)

#' Plot a heterogeneous transition network
#'
#' @description
#' `plot_htna()` plots a heterogeneous transition network: the codes of
#' every actor type as nodes and the transitions between them as directed
#' edges whose width and label give the transition weight. Each actor type
#' has its own colour and node shape and occupies its own arc of the
#' circular layout, so the transitions within an actor type run along its
#' arc and the hand-overs between actor types cross the center. The plot
#' shows which transitions dominate the interaction and how the actors pass
#' control to each other.
#'
#' `plot_htna()` draws one network, or one plot per cohort for a grouped
#' network, each titled with its cohort name; arrange several plots with
#' `par(mfrow = ...)`. Edges with a weight below `minimum` are hidden. The
#' first actor type is placed on the left of the circular layout. `plot()`
#' on an htna network or `htna_group` calls `plot_htna()`. It is called for
#' the plot and returns, invisibly, the value of [cograph::plot_htna()] for
#' one network, or `x` for a grouped network.
#'
#' To show the difference between two networks use [plot_htna_diff()]; to
#' show only the edges that survive bootstrapping use [plot_htna_bootstrap()].
#'
#' @param x An htna network from [build_htna()], or an `htna_group`.
#' @param layout Node layout. Default `"circular"`; `"polygon"` places three
#'   or more actor types on the sides of a polygon.
#' @param group_colors Colours of the actor types, in actor order. Default
#'   [htna_palette].
#' @param group_shapes Node shapes of the actor types, in actor order.
#'   Default `c("circle", "square", "diamond", "triangle", "pentagon",
#'   "hexagon", "star", "cross")`.
#' @param minimum Smallest edge weight drawn. Default `0.05`.
#' @param ... Further drawing arguments of [cograph::plot_htna()] and
#'   [cograph::splot()], such as `title`, `threshold` or `edge_labels`.
#'
#' @return Called for the plot. Invisibly, the value of
#'   [cograph::plot_htna()] for one network, or `x` for a grouped network.
#'
#' @section Conditions:
#' `htna_empty_input` when a grouped network has no cohorts.
#'
#' @seealso [build_htna()], [plot_htna_diff()], [plot_htna_bootstrap()],
#'   [htna_palette]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' plot_htna(net)
#' plot_htna(net, minimum = 0.1)
#'
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' plot(grp)
#'
#' @export
plot_htna <- function(x, layout = "circular",
                      group_colors = htna_palette,
                      group_shapes = htna_shape_palette,
                      minimum = 0.05, ...) {
  if (inherits(x, "htna_group") || (is.list(x) && !is.data.frame(x) &&
                                    !inherits(x, "htna"))) {
    if (length(x) == 0L) .htna_stop("Empty htna_group.",
                                    class = "htna_empty_input")
    nms <- names(x) %||% as.character(seq_along(x))
    lapply(seq_along(x), function(i) {
      plot_htna(x[[i]], layout = layout, group_colors = group_colors,
                group_shapes = group_shapes, title = nms[i], ...)
    })
    return(invisible(x))
  }

  groups <- x$actor_levels %||% unique(as.character(x$node_groups$group))
  n_groups <- length(groups)
  colors <- unname(.htna_actor_colors(n_groups, palette = group_colors))

  ng <- x$node_groups
  node_list <- lapply(groups, function(g) ng$node[as.character(ng$group) == g])
  names(node_list) <- groups

  color_map <- setNames(colors, groups)
  node_colors <- unname(color_map[as.character(ng$group)])
  dark <- .darken(node_colors)

  # cograph's circular layout places `node_list[[1]]` clockwise from
  # the top, which puts the first group on the right for two-group
  # networks. Reversing `node_list` and the matching colour / shape
  # vectors makes the first group land on the left while keeping
  # per-group colour and shape assignments consistent with the
  # bootstrap plot. The legend (emitted below in original order)
  # remains correct.
  shapes <- rep_len(group_shapes, n_groups)
  cograph_node_list <- node_list
  cograph_colors    <- colors
  cograph_shapes    <- shapes
  if (n_groups >= 2L && (identical(layout, "circular") ||
                         identical(layout, "auto"))) {
    cograph_node_list <- rev(node_list)
    cograph_colors    <- rev(colors)
    cograph_shapes    <- rev(shapes)
  }

  # Create clean vertex labels for disambiguated nodes
  vertex_labels <- as.character(ng$node)
  disambiguated <- grepl(":", vertex_labels, fixed = TRUE)
  if (any(disambiguated)) {
    vertex_labels[disambiguated] <- sapply(vertex_labels[disambiguated], function(node) {
      strsplit(node, ":", fixed = TRUE)[[1]][2]
    })
  }

  result <- cograph::plot_htna(x, node_list = cograph_node_list,
                               node_fill    = node_colors,
                               layout       = layout,
                               group_colors = cograph_colors,
                               group_shapes = cograph_shapes,
                               donut_color  = dark,
                               minimum      = minimum,
                               legend       = FALSE,
                               vertex.label = vertex_labels,
                               ...)
  if (n_groups >= 2L) .htna_legend(groups, colors)
  invisible(result)
}

#' @export
#' @rdname plot_htna
plot.htna <- function(x, ...) plot_htna(x, ...)

#' @export
#' @rdname plot_htna
plot.htna_group <- function(x, ...) plot_htna(x, ...)

#' Pick actor colours, recycling the palette for >6 groups
#'
#' Returns one colour per actor group. When the number of groups exceeds
#' the palette length, recycles with a one-time warning so plots remain
#' readable instead of silently producing `NA` fills.
#'
#' @param n_or_levels Either an integer count of groups or a character
#'   vector of group labels (used for `names()` on the result).
#' @param palette Colour palette to draw from. Defaults to [htna_palette].
#' @return Character vector of colours, length `n` (named when levels given).
#' @noRd
.htna_actor_colors <- function(n_or_levels, palette = htna_palette) {
  if (is.character(n_or_levels)) {
    nms <- n_or_levels
    n   <- length(nms)
  } else {
    nms <- NULL
    n   <- as.integer(n_or_levels)
  }
  if (n <= length(palette)) {
    cols <- palette[seq_len(n)]
  } else {
    warning(sprintf(
      "htna: %d actor groups exceeds palette size %d; recycling colours.",
      n, length(palette)), call. = FALSE)
    cols <- rep_len(palette, n)
  }
  if (!is.null(nms)) names(cols) <- nms
  cols
}

#' @keywords internal
#' @noRd
.htna_circular <- function(node_list, labels, radius = 2,
                           angle_spacing = 0.35) {
  n <- length(labels)
  n_groups <- length(node_list)
  group_indices <- lapply(node_list, function(nodes) match(nodes, labels))

  angle_per_group <- 2 * pi / n_groups
  gap_angle <- angle_per_group * angle_spacing
  arc_angle <- angle_per_group - gap_angle

  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Loop kept: each actor group writes its own arc of positions into the
  # shared x_pos / y_pos vectors (at most a handful of groups).
  for (i in seq_len(n_groups)) {
    g_idx <- group_indices[[i]]
    n_nodes <- length(g_idx)
    # Match cograph::plot_htna's "circular" layout: first group placed
    # clockwise from the top. First group center is at angle
    # pi/2 - angle_per_group/2 (top, then half a group rotation clockwise).
    # Subsequent groups continue clockwise (decreasing angle).
    center_angle <- pi/2 - angle_per_group/2 - (i - 1L) * angle_per_group
    start_angle  <- center_angle + arc_angle / 2
    end_angle    <- center_angle - arc_angle / 2
    if (n_nodes > 1L) {
      angles <- seq(start_angle, end_angle, length.out = n_nodes)
    } else {
      angles <- center_angle
    }
    x_pos[g_idx] <- radius * cos(angles)
    y_pos[g_idx] <- radius * sin(angles)
  }
  cbind(x_pos, y_pos)
}

#' @keywords internal
#' @noRd
.htna_legend <- function(groups, colors, position = "bottomright") {
  graphics::legend(position, legend = groups, fill = colors,
                   border = NA, bty = "n", title = "Groups")
}

.darken <- function(cols, factor = 0.65) {
  vapply(cols, function(col) {
    rgb_val <- grDevices::col2rgb(col)
    grDevices::rgb(
      rgb_val[1L] * factor,
      rgb_val[2L] * factor,
      rgb_val[3L] * factor,
      maxColorValue = 255
    )
  }, character(1L), USE.NAMES = FALSE)
}
