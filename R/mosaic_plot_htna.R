#' Chi-square Mosaic Plot of a Transition Network
#'
#' htna-named alias of [Nestimate::mosaic_plot()]. Renders a
#' chi-square mosaic where row x column area equals the joint share
#' of `(from, to)` transitions and fill encodes the standardized
#' residual (blue = over-represented, red = under-represented,
#' white = at-expected).
#'
#' Designed with htna in mind: Nestimate ships an explicit
#' `mosaic_plot.htna` S3 method that recognises the actor partition
#' on `htna` networks. Pass an htna network from [build_htna()]
#' directly. The underlying transition matrix must be integer-weighted
#' (build with `method = "frequency"`), since the chi-square test
#' needs counts.
#'
#' Axis tick labels are coloured by actor group using the htna
#' palette ([htna_palette]), matching the colours used elsewhere in
#' htna (e.g. [plot_htna()]).
#'
#' Suffixed `_htna` to avoid clashing with `Nestimate::mosaic_plot()`
#' when both packages are loaded.
#'
#' @param x An htna network built with `method = "frequency"` (or other
#'   object accepted by [Nestimate::mosaic_plot()]); S3 dispatch on `x`
#'   is preserved, so `htna` objects use `mosaic_plot.htna`.
#' @param ... Additional arguments passed to [Nestimate::mosaic_plot()],
#'   such as `n_perm` and `seed`. See that function for details.
#'
#' @return A ggplot or gtable, depending on input shape. See
#'   [Nestimate::mosaic_plot()] for full details and the per-class S3
#'   methods.
#'
#' @seealso [plot_frequencies_htna()] for the marginal-frequency
#'   companion view.
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type",
#'                   method = "frequency")
#' mosaic_plot_htna(net, n_perm = 50, seed = 1)
#' }
#' @export
mosaic_plot_htna <- function(x, ...) {
  p <- Nestimate::mosaic_plot(x, ...)
  if (inherits(x, "htna") && inherits(p, "ggplot") &&
      !is.null(x$node_groups)) {
    p <- .htna_color_mosaic_axes(p, x)
  }
  p
}

# Re-tint x/y tick labels by actor group, preserving every other
# axis-text attribute (angle/hjust/vjust/size) set upstream.
.htna_color_mosaic_axes <- function(p, net) {
  ng <- net$node_groups
  actor_levels  <- net$actor_levels %||% unique(ng$group)
  palette       <- .htna_actor_colors(length(actor_levels))
  names(palette) <- actor_levels
  node_to_color <- stats::setNames(unname(palette[ng$group]), ng$node)

  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
  x_labels <- as.character(pp$x$get_labels(pp$x$breaks))
  y_labels <- as.character(pp$y$get_labels(pp$y$breaks))

  pick <- function(labels) {
    out <- unname(node_to_color[labels])
    out[is.na(out)] <- "black"
    out
  }
  x_cols <- pick(x_labels)
  y_cols <- pick(y_labels)

  cur_x <- ggplot2::calc_element("axis.text.x", p$theme)
  cur_y <- ggplot2::calc_element("axis.text.y", p$theme)

  # Per-tick label colouring requires a vector `colour` argument to
  # `element_text()`, which ggplot2 flags as "not officially supported".
  # The pattern is the standard idiom for this effect; muffle just that
  # warning, leave any other warnings alone.
  withCallingHandlers(
    p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        family = cur_x$family, face = cur_x$face, size = cur_x$size,
        hjust  = cur_x$hjust,  vjust = cur_x$vjust, angle = cur_x$angle,
        colour = x_cols),
      axis.text.y = ggplot2::element_text(
        family = cur_y$family, face = cur_y$face, size = cur_y$size,
        hjust  = cur_y$hjust,  vjust = cur_y$vjust, angle = cur_y$angle,
        colour = y_cols)
    ),
    warning = function(w) {
      if (grepl("Vectorized input to .element_text",
                conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
