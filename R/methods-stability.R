#' Plot centrality stability against the proportion of sessions dropped
#'
#' @description
#' `plot()` plots the case-dropping stability of an `htna_stability` result
#' from [centrality_stability_htna()]: for each centrality measure, how
#' closely the centralities of networks estimated from a subset of sessions
#' correlate with those of the full network as more sessions are dropped.
#' A measure whose mean correlation stays above the threshold until a large
#' share of the sessions is dropped has a node ranking that does not depend
#' on a few sessions; a measure whose line falls early cannot be interpreted
#' with confidence.
#'
#' The plot draws one line per measure (distinguished by colour, line type
#' and point shape), a ribbon of plus or minus one standard deviation of the
#' correlations, and a dashed line at the stability threshold. The grouped
#' method draws one such chart per cohort, titled with the cohort name. The
#' charts are drawn with 'ggplot2', which must be installed, and are
#' returned invisibly as `ggplot` objects. The CS coefficients themselves
#' are reported by [print.htna_stability()] and returned as a table by
#' [as.data.frame.htna_stability()].
#'
#' @param x A result of [centrality_stability_htna()]: an `htna_stability`
#'   or, for a grouped network, an `htna_stability_group`.
#' @param ... Unused.
#'
#' @return Invisibly, the `ggplot` object (for `htna_stability`) or a named
#'   list of `ggplot` objects, one per cohort (for `htna_stability_group`).
#'   The charts are printed.
#'
#' @section Conditions:
#' An error of class `htna_missing_package` is raised when 'ggplot2' is not
#' installed.
#'
#' @seealso [centrality_stability_htna()], [as.data.frame.htna_stability()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(cs)
#' }
#' @export
plot.htna_stability <- function(x, ...) {
  p <- .htna_stability_ggplot(x)
  print(p)
  invisible(p)
}

#' @rdname plot.htna_stability
#' @export
plot.htna_stability_group <- function(x, ...) {
  plots <- lapply(names(x), function(group_name) {
    .htna_stability_ggplot(x[[group_name]], title = group_name)
  })
  names(plots) <- names(x)
  lapply(plots, print)
  invisible(plots)
}

#' Is 'ggplot2' available? (a function so tests can mock it)
#' @keywords internal
#' @noRd
.has_ggplot2 <- function() requireNamespace("ggplot2", quietly = TRUE)

#' Build the stability chart of one `htna_stability` object
#' @keywords internal
#' @noRd
.htna_stability_ggplot <- function(x, title = "Centrality stability") {
  if (!.has_ggplot2()) {
    .htna_stop("Plotting centrality stability needs the 'ggplot2' package. ",
               "Install it with `install.packages(\"ggplot2\")`.",
               class = "htna_missing_package")
  }
  summ <- as.data.frame(x, what = "correlations")
  summ$measure <- factor(summ$measure, levels = names(x$cs))
  summ$lower <- summ$mean_cor - summ$sd_cor
  summ$upper <- pmin(summ$mean_cor + summ$sd_cor, 1)
  n_measures <- nlevels(summ$measure)
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00",
                 "#CC79A7", "#F0E442", "#999999", "#000000")
  colours <- rep_len(okabe_ito, n_measures)
  shapes  <- rep_len(c(16L, 17L, 15L, 18L, 1L, 2L, 0L, 5L, 4L), n_measures)

  ggplot2::ggplot(summ, ggplot2::aes(x = .data$drop_prop,
                                     y = .data$mean_cor,
                                     colour = .data$measure,
                                     fill = .data$measure,
                                     linetype = .data$measure,
                                     shape = .data$measure)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower,
                                      ymax = .data$upper),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = x$threshold, linetype = "dashed",
                        colour = "#999999", linewidth = 0.4) +
    ggplot2::annotate("text", x = max(x$drop_prop), y = x$threshold,
                      label = sprintf("threshold = %.2f", x$threshold),
                      hjust = 1, vjust = -0.5, size = 3.5,
                      colour = "#555555") +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::scale_x_continuous(breaks = x$drop_prop) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "Proportion of sessions dropped",
                  y = sprintf("Mean correlation (%s)", x$method),
                  title = title, colour = "Measure", fill = "Measure",
                  linetype = "Measure", shape = "Measure") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

#' Print centrality stability coefficients
#'
#' @description
#' `print()` prints the case-dropping stability (CS) coefficient of each
#' centrality measure of an `htna_stability` result. The CS coefficient is
#' the largest proportion of sessions that can be dropped while the
#' correlation with the full-network centralities stays above the threshold
#' with the required certainty. The header gives the number of resamples,
#' the threshold and the drop proportions. The grouped method prints one
#' block per cohort. It returns `x` invisibly.
#'
#' @param x A result of [centrality_stability_htna()]: an `htna_stability`
#'   or an `htna_stability_group`.
#' @param ... Unused.
#'
#' @return Invisibly, `x`, unchanged.
#'
#' @seealso [as.data.frame.htna_stability()] for the coefficients and the
#'   correlation table as data frames, [plot.htna_stability()].
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
#' print(cs)
#' @export
print.htna_stability <- function(x, ...) {
  cat("HTNA centrality stability\n")
  cat(sprintf("  %d iterations, threshold = %.2f, drop proportions %s\n",
              x$iter, x$threshold, paste(x$drop_prop, collapse = ", ")))
  cat("\n  CS-coefficients:\n")
  cat(sprintf("    %-15s  %.2f\n", names(x$cs), x$cs), sep = "")
  invisible(x)
}

#' @rdname print.htna_stability
#' @export
print.htna_stability_group <- function(x, ...) {
  cat("HTNA centrality stability, ", length(x), " cohorts\n", sep = "")
  lapply(names(x), function(group_name) {
    cat("\n--- ", group_name, " ---\n", sep = "")
    print.htna_stability(x[[group_name]], ...)
  })
  invisible(x)
}

#' Tabulate centrality stability results
#'
#' @description
#' `as.data.frame()` returns the results of [centrality_stability_htna()] as
#' a tidy data frame; `summary()` of a grouped result returns the stacked
#' correlation table. The CS coefficients (one row per measure) summarize
#' how many sessions can be dropped before the centrality ranking changes;
#' the correlation table (one row per measure and drop proportion) shows the
#' trace behind each coefficient. For a grouped result the rows of every
#' cohort are stacked with a leading `group` column.
#'
#' @param x An `htna_stability` or `htna_stability_group`.
#' @param row.names,optional Ignored; present for the generic.
#' @param what `"cs"` (default) for the CS coefficients, or
#'   `"correlations"` for the correlation table.
#' @param ... Unused.
#'
#' @return A data frame. For `what = "cs"`: columns `measure` and `cs`, one
#'   row per measure. For `what = "correlations"`: columns `measure`,
#'   `drop_prop`, `mean_cor`, `sd_cor` and `prop_above` (the share of
#'   resamples with a correlation above the threshold), one row per measure
#'   and drop proportion. Grouped results add a first column `group`.
#'   `summary()` of a grouped result returns the stacked correlation table.
#'
#' @seealso [centrality_stability_htna()], [plot.htna_stability()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
#' as.data.frame(cs)
#' as.data.frame(cs, what = "correlations")
#' @export
as.data.frame.htna_stability <- function(x, row.names = NULL,
                                         optional = FALSE, ...,
                                         what = c("cs", "correlations")) {
  what <- match.arg(what)
  if (identical(what, "cs")) {
    return(data.frame(measure = names(x$cs), cs = unname(x$cs),
                      stringsAsFactors = FALSE))
  }
  out <- as.data.frame(summary(x), stringsAsFactors = FALSE)
  class(out) <- "data.frame"
  rownames(out) <- NULL
  out
}

#' @rdname as.data.frame.htna_stability
#' @export
as.data.frame.htna_stability_group <- function(x, row.names = NULL,
                                               optional = FALSE, ...,
                                               what = c("cs",
                                                        "correlations")) {
  what <- match.arg(what)
  .stack_by(x, "group", function(item) {
    as.data.frame.htna_stability(item, what = what)
  })
}

#' @rdname as.data.frame.htna_stability
#' @param object An `htna_stability_group`.
#' @export
summary.htna_stability_group <- function(object, ...) {
  as.data.frame.htna_stability_group(object, what = "correlations")
}
