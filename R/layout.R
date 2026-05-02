#' Compute a Circular Multi-Group Layout (htna port)
#'
#' Mirrors the geometry inside `cograph::plot_htna()` (private). htna ports it
#' so wrappers that drive cograph plotters which do *not* auto-detect groups
#' (`splot.net_bootstrap`, `splot.net_permutation`, `plot_compare`) can inject
#' a layout matrix and keep multi-plot visual identity consistent.
#'
#' Drop this and switch all callers to the public helper as soon as cograph
#' exports `compute_htna_layout()`.
#'
#' @param node_list Named list of character vectors, one per group.
#' @param all_labels Character vector of every node label, in network order.
#' @param angle_spacing Inter-group gap as fraction of the per-group arc.
#' @param scale Radius multiplier.
#'
#' @return Named list with `x`, `y` numeric vectors aligned to `all_labels`.
#' @keywords internal
#' @noRd
.htna_circular_layout <- function(node_list, all_labels,
                                  angle_spacing = 0.35, scale = 1) {
  n_groups <- length(node_list)
  group_indices <- lapply(node_list, function(g) match(g, all_labels))

  n <- length(all_labels)
  x_pos <- rep(0, n); y_pos <- rep(0, n)
  radius <- 2 * scale

  angle_per_group <- 2 * pi / n_groups
  gap_angle       <- angle_per_group * angle_spacing
  arc_angle       <- angle_per_group - gap_angle

  for (i in seq_len(n_groups)) {
    g_idx <- group_indices[[i]]
    n_in  <- length(g_idx)
    start <- pi/2 - (i - 1) * angle_per_group - gap_angle/2
    end   <- start - arc_angle
    if (n_in > 1) {
      ang <- seq(start, end, length.out = n_in)
    } else if (n_in == 1) {
      ang <- (start + end) / 2
    } else next
    x_pos[g_idx] <- radius * cos(ang)
    y_pos[g_idx] <- radius * sin(ang)
  }
  list(x = x_pos, y = y_pos)
}
