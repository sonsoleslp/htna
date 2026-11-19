#' Plot a chi-square mosaic of the transitions of an htna network
#'
#' @description
#' `mosaic_plot_htna()` plots a chi-square mosaic of the transitions of an
#' htna network. A mosaic plot of a transition table shows, for every pair
#' of codes, the share of all transitions that go from the first code to the
#' second (the area of the tile) and how far that count lies from what
#' independence of the source and target codes would predict (the colour of
#' the tile, from the standardized residual of a chi-square test).
#' Over-represented transitions are shaded blue and under-represented ones
#' red. In a heterogeneous network the plot shows which hand-overs between
#' actors are more or less frequent than the overall frequencies of the
#' codes imply.
#'
#' `mosaic_plot_htna()` draws the mosaic of the transition counts of an htna
#' network with Nestimate's `mosaic_plot()`, with the axis labels coloured
#' by actor type. The chi-square test needs counts, so build the network
#' with `method = "frequency"`. With `n_perm`, p-values are obtained by
#' permutation (set `seed` for a reproducible result). It returns a `ggplot`
#' object.
#'
#' For the marginal frequencies of the codes use [plot_frequencies_htna()].
#'
#' @param x An htna network built with `method = "frequency"`.
#' @param ... Further arguments of `Nestimate::mosaic_plot()`, such as
#'   `n_perm` and `seed`. With Nestimate 1.0.13 or later, `nested = TRUE`
#'   draws residuals that allow for transitions nested in sessions, or in
#'   the actors named by `actor`; `nested_method` chooses the test and
#'   `report = TRUE` adds the overall test as the subtitle.
#'
#' @return A `ggplot` object (or a `gtable` for some inputs), which is drawn
#'   when printed.
#'
#' @seealso [plot_frequencies_htna()], [frequencies_htna()], [build_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type",
#'                   method = "frequency")
#' mosaic_plot_htna(net, n_perm = 50, seed = 1)
#' @export
mosaic_plot_htna <- function(x, ...) {
  # Nestimate's group strips colour each tick label through a vectorized
  # `element_text()` colour, which ggplot2 flags as "not officially
  # supported". It is the standard idiom for per-tick colours (htna uses it
  # below too); muffle only that warning, leave any other alone.
  p <- withCallingHandlers(
    Nestimate::mosaic_plot(x, ...),
    warning = function(w) {
      if (grepl("Vectorized input to .element_text", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
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
