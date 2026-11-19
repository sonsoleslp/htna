#' Plot the frequencies of the codes of an htna network
#'
#' @description
#' `plot_frequencies_htna()` plots the frequencies of the codes of an htna
#' network: the number of occurrences of every code, coloured by its actor
#' type. Knowing how often each code occurs helps before transitions are
#' interpreted: a rare code can have extreme transition probabilities that
#' rest on a few events, and the balance between the actor types (how much
#' the human and the AI each contribute) frames every later comparison.
#'
#' `plot_frequencies_htna()` draws one of three views: a treemap of the codes
#' (`"treemap"`, the default, drawn with Nestimate's
#' `plot_state_frequencies()`), a single bar chart of all codes sorted by
#' count (`"bars"`), or one bar chart per actor type (`"facet"`). The bar
#' views are 'ggplot2' charts and need that package. It returns the
#' frequency object for the treemap and a `ggplot` object for the bar views.
#'
#' The counts and proportions behind the plot are returned as a table by
#' [frequencies_htna()]. For how the transitions from each code deviate from
#' independence use [mosaic_plot_htna()].
#'
#' @param x An htna network from [build_htna()].
#' @param view `"treemap"` (default), `"bars"` or `"facet"`.
#' @param ... Further arguments of `Nestimate::plot_state_frequencies()` for
#'   the treemap; ignored by the bar views. An argument the treemap does
#'   not take raises a warning (Nestimate 1.0.12 or later).
#'
#' @return For `"treemap"`, invisibly, the frequency object of class
#'   `state_freq` (its table via `as.data.frame()`). For `"bars"` and
#'   `"facet"`, a `ggplot` object, which can be modified with `+`.
#'
#' @section Conditions:
#' `htna_missing_package` when 'ggplot2' is not installed for a bar view;
#' `htna_bad_input` when a bar view is asked for an object that is not an
#' htna network.
#'
#' @seealso [frequencies_htna()], [state_frequencies_htna()],
#'   [state_distribution_htna()], [mosaic_plot_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' plot_frequencies_htna(net)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_frequencies_htna(net, view = "facet")
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
    .htna_stop("`plot_frequencies_htna(view = '", view, "')` needs the ",
               "'ggplot2' package. Install it with ",
               "`install.packages(\"ggplot2\")`.",
               class = "htna_missing_package")
  }                                                    # nocov end

  if (!inherits(x, "htna")) {
    .htna_stop("`view = '", view, "'` requires an htna network from ",
               "build_htna().")
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
