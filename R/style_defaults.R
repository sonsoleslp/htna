#' htna Visual Defaults
#'
#' Returns the standard htna visual defaults as a named list of
#' [cograph::plot_htna()] parameters. Used by every htna plot wrapper as the
#' base layer that user `...` arguments are merged into.
#'
#' Centralizing visual defaults here (rather than in cograph) keeps cograph
#' generic and lets htna evolve its visual identity without coupling cograph's
#' release cadence.
#'
#' @param net An htna network (built by [build_htna()]).
#' @param directed Logical or `NULL`. If `NULL` (default), inferred from the
#'   network: an explicit `attr(net, "directed")` or `net$directed` slot wins;
#'   otherwise the weight matrix is checked for symmetry.
#'
#' @return Named list of [cograph::plot_htna()] parameters.
#' @keywords internal
#' @noRd
.htna_style_defaults <- function(net, directed = NULL) {
  n_groups <- if (!is.null(net$node_groups)) {
    length(unique(net$node_groups$group))
  } else NA_integer_

  if (is.null(directed)) {
    directed <- isTRUE(attr(net, "directed")) ||
                isTRUE(net$directed) ||
                !.is_symmetric_matrix(.weights_of(net))
  }

  defaults <- list(
    layout                  = "circular",
    angle_spacing           = 0.35,
    legend                  = TRUE,
    legend_position         = "bottom",
    legend_horiz            = TRUE,
    edge.color              = "#003355",
    edge_label_style        = "estimate",
    edge_label_leading_zero = FALSE,
    edge.label.cex          = 0.4,
    edge.label.position     = 0.7,
    minimum                 = 0.05,
    vsize                   = 7,
    layout_margin           = 0.15,
    curvature               = 0.4
  )

  if (isTRUE(directed)) {
    # defaults$arrow_size        <- 0.61
    defaults$edge_start_length <- 0.2
    defaults$edge_start_style  <- "dotted"
  }

  if (!is.na(n_groups) && n_groups >= 2) {
    palette <- c("#4FC3F7", "#fbb550", "#7eb5d6", "#98d4a2",
                 "#f4a582", "#92c5de", "#d6c1de", "#b8e186")
    shapes  <- c("circle", "square", "diamond", "triangle",
                 "pentagon", "hexagon", "star", "cross")
    defaults$group_colors <- rep_len(palette, n_groups)
    defaults$group_shapes <- rep_len(shapes,  n_groups)
  }

  defaults
}
