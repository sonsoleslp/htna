#' Default HTNA colour palette
#'
#' A colour palette for up to 6 actor groups, used by [plot_htna()] when
#' no explicit colours are supplied.
#'
#' @keywords internal
htna_palette <- c(
  "#FFD966",
  "#B4A7D6",
  "#93C47D",
  "#E06666",
  "#6FA8DC",
  "#F6B26B"
)

#' Plot a Heterogeneous Transition Network
#'
#' Wrapper around [cograph::plot_htna()] with defaults suited for HTNA
#' networks built by [build_htna()]. When `layout = "circular"`, a custom
#' layout is computed so that the first actor group appears on the left.
#'
#' @param x A network object produced by [build_htna()].
#' @param layout Character. Layout algorithm. Default `"circular"`.
#' @param group_colors Character vector of colours, one per actor group.
#'   Defaults to the built-in [htna_palette].
#' @param ... Additional arguments passed to [cograph::plot_htna()].
#'
#' @return Called for its side effect (a plot). Returns `x` invisibly.
#'
#' @seealso [cograph::plot_htna()], [build_htna()]
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' plot_htna(net)
#' plot_htna(net, layout = "auto", threshold = 0.1)
#' }
#'
#' @export
plot_htna <- function(x, layout = "circular",
                      group_colors = htna_palette, ...) {
  groups <- x$actor_levels %||% unique(as.character(x$node_groups$group))
  n_groups <- length(groups)
  colors <- group_colors[seq_len(n_groups)]

  ng <- x$node_groups
  node_list <- lapply(groups, function(g) ng$node[as.character(ng$group) == g])
  names(node_list) <- groups

  color_map <- setNames(colors, groups)
  node_colors <- unname(color_map[as.character(ng$group)])
  dark <- .darken(node_colors)

  if (identical(layout, "circular")) {
    lay <- .htna_circular(node_list, x$nodes$label)
    cograph::splot(x, layout = lay, node_fill = node_colors,
                   donut_color = dark, legend = FALSE, ...)
    .htna_legend(groups, colors)
  } else {
    cograph::plot_htna(x, node_list = node_list, layout = layout,
                       group_colors = colors, donut_color = dark, ...)
  }
}

#' @keywords internal
.htna_circular <- function(node_list, labels, radius = 2,
                           angle_spacing = 0.15) {
  n <- length(labels)
  n_groups <- length(node_list)
  group_indices <- lapply(node_list, function(nodes) match(nodes, labels))

  angle_per_group <- 2 * pi / n_groups
  gap_angle <- angle_per_group * angle_spacing
  arc_angle <- angle_per_group - gap_angle

  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  for (i in seq_len(n_groups)) {
    g_idx <- group_indices[[i]]
    n_nodes <- length(g_idx)
    start_angle <- pi - (i - 1L) * angle_per_group - gap_angle / 2
    end_angle <- start_angle - arc_angle
    if (n_nodes > 1L) {
      angles <- seq(start_angle, end_angle, length.out = n_nodes)
    } else {
      angles <- (start_angle + end_angle) / 2
    }
    x_pos[g_idx] <- radius * cos(angles)
    y_pos[g_idx] <- radius * sin(angles)
  }
  cbind(x_pos, y_pos)
}

#' @keywords internal
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
