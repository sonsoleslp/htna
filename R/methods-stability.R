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
  Nestimate:::plot.net_stability(x, ...)
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
  Nestimate:::print.net_stability(x, ...)

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