#' Plot State Frequencies (htna-named)
#'
#' Renders one of three views of an htna network's state frequencies:
#' the upstream treemap (default), a combined actor-coloured bar
#' chart, or a per-actor faceted bar chart.
#'
#' Suffixed `_htna` to avoid clashing with
#' `Nestimate::plot_state_frequencies()` when both packages are loaded.
#'
#' @param x An htna network from [build_htna()].
#' @param view One of `"treemap"` (default), `"bars"`, or `"facet"`.
#'   * `"treemap"` forwards to [Nestimate::plot_state_frequencies()]
#'     and renders the chart automatically; returns the underlying
#'     `state_freq` object invisibly.
#'   * `"bars"` builds a combined bar chart with all states on one
#'     y-axis, sorted by count, fill coloured by actor (using
#'     `htna_palette` keyed off `x$actor_levels`). Returns the ggplot.
#'   * `"facet"` builds a per-actor faceted bar chart with
#'     `scales = "free_y"` so each panel only shows its own actor's
#'     states. Returns the ggplot.
#' @param ... Forwarded to [Nestimate::plot_state_frequencies()] when
#'   `view = "treemap"`. Ignored for `"bars"` and `"facet"` — those
#'   return ggplot objects, so customisation works through standard
#'   ggplot composition (`+ theme(...)`, `+ labs(...)`,
#'   `+ scale_fill_*()`, etc.).
#'
#' @return For `view = "treemap"`: the `state_freq` object invisibly.
#'   For `"bars"` / `"facet"`: a ggplot, returned visibly so the
#'   chart auto-prints and standard `+` composition works.
#'
#' @seealso [frequencies_htna()] for the underlying tidy table,
#'   [state_frequencies_htna()], [state_distribution_htna()],
#'   [mosaic_plot_htna()].
#' @examples
#' \donttest{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' plot_frequencies_htna(net, view = "treemap")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_frequencies_htna(net, view = "bars")
#' }
#' }
#' @export
plot_frequencies_htna <- function(x,
                                  view = c("treemap", "bars", "facet"),
                                  ...) {
  view <- match.arg(view)

  if (view == "treemap") {
    res <- Nestimate::plot_state_frequencies(x, ...)
    plot(res)
    return(invisible(res))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {  # nocov start
    stop("`plot_frequencies_htna(view = '", view, "')` needs the ",
         "'ggplot2' package. Install it with ",
         "`install.packages(\"ggplot2\")`.", call. = FALSE)
  }                                                    # nocov end

  if (!inherits(x, "htna")) {
    stop("`view = '", view, "'` requires an htna network from ",
         "build_htna().", call. = FALSE)
  }

  sd <- frequencies_htna(x)
  actor_colors <- .htna_actor_colors(x$actor_levels)

  p <- ggplot2::ggplot(sd, ggplot2::aes(x = stats::reorder(.data$state,
                                                           .data$count),
                                        y = .data$count,
                                        fill = .data$group)) +
       ggplot2::geom_col(show.legend = view == "bars") +
       ggplot2::coord_flip() +
       ggplot2::scale_fill_manual(values = actor_colors) +
       ggplot2::labs(x = NULL, y = "Count", fill = "Actor") +
       ggplot2::theme_minimal()

  if (view == "facet") {
    p <- p + ggplot2::facet_wrap(~ .data$group, scales = "free_y")
  }
  p
}
