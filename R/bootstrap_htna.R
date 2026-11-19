#' Bootstrap the edges of a heterogeneous transition network
#'
#' @description
#' `bootstrap_htna()` bootstraps the edges of a heterogeneous transition
#' network, or of every cohort of a grouped network, to show which
#' transitions would reappear in another sample of sessions. The
#' non-parametric bootstrap resamples the sessions with replacement,
#' re-estimates the network on each resample, and summarizes the
#' distribution of every edge weight: its mean, standard deviation,
#' percentile confidence interval, and a p-value. An edge weight estimated
#' from a sample of sessions is uncertain, and a plot of the network alone
#' does not show that uncertainty.
#'
#' Under the default `inference = "stability"`, the p-value is the share of
#' resamples (plus one, over the number of resamples plus one) whose weight
#' falls outside a consistency range around the observed weight (by default
#' 75 to 125 percent of it); an edge with a p-value below `ci_level` is
#' reproduced by the resamples and is marked significant. The bootstrap
#' works at the level of individual codes and runs Nestimate's
#' `bootstrap_network()` with the actor partition kept, so
#' [plot_htna_bootstrap()] draws the result with the actor layout and
#' colours. `bootstrap()` is the generic, with a method for htna networks.
#' It returns an `htna_bootstrap` object, or one per cohort for a grouped
#' network.
#'
#' For a closed-form estimate of the same quantities use
#' [certainty_htna()]; to remove unstable edges use [prune_htna()] with
#' `method = "bootstrap"`; for the stability of centralities use
#' [centrality_stability_htna()].
#'
#' @param x An htna network from [build_htna()], or an `htna_group`, in
#'   which case every cohort is bootstrapped.
#' @param ... Settings: `iter` (resamples, default 1000), `ci_level`
#'   (0.05), `inference` (`"stability"` or `"threshold"`),
#'   `consistency_range` (`c(0.75, 1.25)`), `edge_threshold`, `seed`,
#'   `boundary`, `ci_method` and `actor` (resample whole actors instead of
#'   sessions).
#'
#' @return For a network, an object of class
#'   `c("htna_bootstrap", "net_bootstrap")` with `print()`, `summary()` and
#'   `plot()` methods. `summary()` returns a data frame with one row per
#'   edge and columns `from`, `to`, `weight`, `mean`, `sd`, `p_value`,
#'   `sig`, `ci_lower`, `ci_upper`, `cr_lower` and `cr_upper`. For an
#'   `htna_group`, a named list of such objects with class
#'   `c("htna_bootstrap_group", "list")`.
#'
#' @section Conditions:
#' An error of class `htna_bad_input` is raised when `x` is neither an htna
#' network nor a list of them.
#'
#' @seealso [plot_htna_bootstrap()], [certainty_htna()], [prune_htna()],
#'   [reliability_htna()].
#'
#' @examples
#' data(human_ai)
#' net  <- build_htna(human_ai, actor_type = "actor_type")
#' boot <- bootstrap_htna(net, iter = 50, seed = 1)
#' boot
#' head(summary(boot))
#'
#' @export
#' @rdname bootstrap_htna
bootstrap_htna <- function(x, ...) {
  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    res <- lapply(x, bootstrap_htna, ...)
    names(res) <- names(x)
    class(res) <- c("htna_bootstrap_group", "list")
    return(res)
  }
  if (!inherits(x, "htna")) {
    .htna_stop("`x` must be an htna network produced by build_htna().")
  }

  boot <- Nestimate::bootstrap_network(x, ...)

  boot$model <- .restore_htna_model(boot$model, x)

  class(boot) <- c("htna_bootstrap", class(boot))
  boot
}

#' Bootstrap a network
#'
#' @description
#' `bootstrap()` bootstraps a fitted network: it is a generic for
#' resampling-based uncertainty of a fitted network. Its htna method runs
#' [bootstrap_htna()]: the sessions are resampled, the network is
#' re-estimated on each resample, and the distribution of every edge weight
#' is summarized. For an htna network it returns an `htna_bootstrap`.
#'
#' The generic has the same name as the `tna` generic; when both packages
#' are attached, the one attached last is found first. [bootstrap_htna()]
#' is unambiguous.
#'
#' @param x An object to bootstrap; for htna, a network from [build_htna()].
#' @param ... Passed to the method; see [bootstrap_htna()].
#'
#' @return The method's result; for an htna network an `htna_bootstrap`
#'   (see [bootstrap_htna()]).
#'
#' @seealso [bootstrap_htna()].
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' bootstrap(net, iter = 50, seed = 1)
#' @export
bootstrap <- function(x, ...) UseMethod("bootstrap")

#' @export
#' @rdname bootstrap_htna
bootstrap.htna <- function(x, ...) {
  settings <- list(...)
  # tna's bootstrap() argument names, when the call comes through tna's
  # generic, mapped onto Nestimate::bootstrap_network()'s
  tna_names <- c(level = "ci_level", method = "inference",
                 threshold = "edge_threshold")
  given <- intersect(names(settings), names(tna_names))
  names(settings)[match(given, names(settings))] <- tna_names[given]
  do.call(bootstrap_htna, c(list(x), settings))
}

# tna exports bootstrap() and plot_sequences() generics of the same names
# (tna is in Suggests). The htna methods are registered on those generics as
# well, so the calls reach htna whichever package is attached last. tna's
# bootstrap() has fixed arguments; they map onto Nestimate::bootstrap_network().
#' Bootstrap an htna network through tna's bootstrap() generic
#'
#' @param x An htna network.
#' @param iter,level,method,threshold,consistency_range,seed,actor tna's
#'   argument names, passed to [bootstrap_htna()] as `iter`, `ci_level`,
#'   `inference`, `edge_threshold`, `consistency_range`, `seed` and `actor`.
#' @return As [bootstrap_htna()].
#' @rawNamespace S3method(tna::bootstrap, htna, .bootstrap_htna_tna)
#' @rawNamespace S3method(tna::plot_sequences, htna)
#' @keywords internal
#' @noRd
.bootstrap_htna_tna <- function(x, iter = 1000L, level = 0.05,
                                method = "stability", threshold = NULL,
                                consistency_range = c(0.75, 1.25),
                                seed = NULL, actor = NULL) {
  settings <- list(iter = iter, level = level, method = method,
                   threshold = threshold,
                   consistency_range = consistency_range, seed = seed,
                   actor = actor)
  settings <- settings[!vapply(settings, is.null, logical(1))]
  do.call(bootstrap.htna, c(list(x), settings))
}

#' Print and summarize the bootstraps of a grouped network
#'
#' @description
#' `print()` and `summary()` read the bootstraps of a grouped network:
#' `print()` shows each cohort's report in turn under its name, and
#' `summary()` stacks the cohorts' edge tables into one data frame, so the
#' edges of the cohorts can be read and compared together. A grouped network
#' bootstrapped with [bootstrap_htna()] gives one bootstrap per cohort.
#'
#' @param x,object An `htna_bootstrap_group` from [bootstrap_htna()].
#' @param ... Passed to the method of each cohort.
#'
#' @return `print()` returns `x` invisibly. `summary()` returns a data
#'   frame with one row per cohort and edge: a first column `group`, then
#'   the columns of the per-cohort summary (`from`, `to`, `weight`, `mean`,
#'   `sd`, `p_value`, `sig`, `ci_lower`, `ci_upper`, `cr_lower`,
#'   `cr_upper`).
#'
#' @seealso [bootstrap_htna()], [plot_htna_bootstrap()].
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' boots <- bootstrap_htna(grp, iter = 20, seed = 1)
#' boots
#' head(summary(boots))
#' @export
print.htna_bootstrap_group <- function(x, ...) {
  cat("HTNA bootstrap, ", length(x), " cohort",
      if (length(x) != 1L) "s", "\n", sep = "")
  lapply(names(x) %||% as.character(seq_along(x)), function(cohort) {
    cat("\n--- ", cohort, " ---\n", sep = "")
    print(x[[cohort]], ...)
  })
  invisible(x)
}

#' @rdname print.htna_bootstrap_group
#' @export
summary.htna_bootstrap_group <- function(object, ...) {
  .stack_by(object, "group", function(item) {
    out <- summary(item, ...)
    class(out) <- "data.frame"
    out
  })
}
