#' Plot Method for htna Centrality Stability Objects
#'
#' S3 method for plotting htna centrality stability results.
#' Dispatches to cograph's plotting method with htna-specific styling.
#'
#' @param x An object of class `htna_stability` from [centrality_stability_htna()].
#' @param ... Additional arguments passed to the plotting method.
#'
#' @return A ggplot object or plot output, depending on the underlying method.
#'
#' @export
plot.htna_stability <- function(x, ...) {
  # Use cograph's implementation for htna stability plots
    summ <- summary(x)
    p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$drop_prop, 
                                            y = .data$mean_cor, color = .data$measure)) + ggplot2::geom_line(linewidth = 0.8) + 
      ggplot2::geom_point(size = 2) + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$mean_cor - 
                                                                          .data$sd_cor, ymax = pmin(.data$mean_cor + .data$sd_cor, 
                                                                                                    1), fill = .data$measure), alpha = 0.15, color = NA) + 
      ggplot2::geom_hline(yintercept = x$threshold, linetype = "dashed", 
                          color = "grey40") + ggplot2::annotate("text", x = max(x$drop_prop), 
                                                                y = x$threshold, label = sprintf("threshold = %.1f", 
                                                                                                 x$threshold), hjust = 1, vjust = -0.5, size = 3, 
                                                                color = "grey40") + ggplot2::scale_x_continuous(breaks = x$drop_prop, 
                                                                                                                labels = x$drop_prop) + ggplot2::coord_cartesian(ylim = c(0, 
                                                                                                                                                                          1)) + ggplot2::labs(x = "Proportion dropped", y = sprintf("Mean correlation (%s)", 
                                                                                                                                                                                                                                    x$method), title = "Centrality Stability", color = "Measure", 
                                                                                                                                                                                              fill = "Measure") + ggplot2::theme_minimal(base_size = 12) + 
      ggplot2::theme(legend.position = "bottom")
    print(p)
    invisible(p)
}

#' Print Method for htna Centrality Stability Objects
#'
#' S3 method for printing htna centrality stability results.
#'
#' @param x An object of class `htna_stability` from [centrality_stability_htna()].
#' @param ... Additional arguments passed to the print method.
#'
#' @return Invisibly returns the object, prints stability information.
#'
#' @export
print.htna_stability <- function(x, ...) {
  cat("HTNA Centrality Stability Analysis\n")
  cat("===================================\n")

  # Call the underlying method but add htna context
  cat(sprintf("Centrality Stability (%d iterations, threshold = %.1f)\n", 
              x$iter, x$threshold))
  cat(sprintf("  Drop proportions: %s\n", paste(x$drop_prop, 
                                                collapse = ", ")))
  cat("\n  CS-coefficients:\n")
  for (m in names(x$cs)) {
    cat(sprintf("    %-15s  %.2f\n", m, x$cs[m]))
  }
  invisible(x)
}

#' Plot Method for htna Stability Groups
#'
#' S3 method for plotting grouped htna stability results.
#'
#' @param x An object of class `htna_stability_group` from [centrality_stability_htna()].
#' @param ... Additional arguments passed to the plotting method.
#'
#' @return A combined plot or list of plots for each group.
#'
#' @export
plot.htna_stability_group <- function(x, ...) {
  # Plot each group's stability
  plots <- lapply(names(x), function(group_name) {
    p <- plot.htna_stability(x[[group_name]], ...)
    if (inherits(p, "ggplot")) {
      p <- p + ggplot2::ggtitle(paste("Group:", group_name))
    }
    p
  })
  names(plots) <- names(x)

  if (length(plots) == 1) {
    return(plots[[1]])
  } else {
    return(plots)
  }
}

#' Print Method for htna Stability Groups
#'
#' S3 method for printing grouped htna stability results.
#'
#' @param x An object of class `htna_stability_group` from [centrality_stability_htna()].
#' @param ... Additional arguments passed to the print method.
#'
#' @return Invisibly returns the object, prints group stability information.
#'
#' @export
print.htna_stability_group <- function(x, ...) {
  cat("HTNA Centrality Stability Analysis (Grouped)\n")
  cat("============================================\n")

  for (group_name in names(x)) {
    cat("\n--- Group:", group_name, "---\n")
    print.htna_stability(x[[group_name]], ...)
  }

  invisible(x)
}