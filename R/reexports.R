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
#' networks built by [build_htna()].
#'
#' @param x A network object produced by [build_htna()].
#' @param layout Character. Layout algorithm. Default `"circular"`.
#' @param threshold Numeric. Hide edges with weight below this value.
#'   Default `0.05`.
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

  cograph::plot_htna(x, node_list = node_list, layout = layout,
                     group_colors = colors, donut_color = dark, ...)
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
