#' Compare short sequence patterns between groups of sessions
#'
#' @description
#' `sequence_compare_htna()` compares short sequence patterns between the
#' cohorts of a grouped htna network, or between the groups of a single
#' network given by `group`. It counts every k-gram (a run of k consecutive
#' codes) in each group and finds the patterns that are more or less
#' frequent in a group than expected if pattern and group were independent.
#' Each pattern gets a standardized residual per group (Haberman, 1973) and,
#' optionally, a permutation test (the session is the unit) or a chi-square
#' test of whether its frequency differs between the groups, with p-values
#' adjusted across all patterns. Two groups of sessions can differ not only
#' in single transitions but in longer runs of codes, such as a human
#' request followed by an AI plan and an AI execution.
#'
#' With `level = "type"` every code is first replaced by its actor type, so
#' the patterns are meta-paths such as `Human->AI->Human` and the comparison
#' shows how the turn-taking between actors differs between the groups.
#' `sequence_compare_htna()` runs Nestimate's `sequence_compare()` on the
#' sequences and returns a `net_sequence_comparison` object.
#'
#' For single transitions use [permutation_htna()] or [mosaic_plot_htna()];
#' for descriptive counts of patterns in one network use
#' [extract_meta_paths()].
#'
#' @param x A grouped htna network (an `htna_group`), or an htna network
#'   together with `group`.
#' @param group The grouping of the sessions when `x` is a single network
#'   (default `NULL`).
#' @param level `"state"` (default) compares patterns of codes; `"type"`
#'   compares patterns of actor types.
#' @param ... Settings: `sub` (pattern lengths, default `3:5`), `min_freq`
#'   (minimum count in every group, 5), `test` (`"permutation"`,
#'   `"chisq"` or `"none"`), `iter` (permutations, 1000), `adjust`
#'   (multiplicity correction, `"fdr"`) and `actor` (needs Nestimate 1.0.19
#'   or later).
#'
#' @return An object of class `net_sequence_comparison` with `print()`,
#'   `summary()` and `plot()` methods. `summary()` returns a data frame
#'   with one row per pattern and columns `pattern`, `length`, one
#'   `freq_<group>`, `prop_<group>` and `resid_<group>` column per group,
#'   `effect_size` and, when a test is run, `p_value`.
#'
#' @section Conditions:
#' An error of class `htna_bad_input` when `level = "type"` and `x` has no
#' actor partition.
#'
#' @references
#' Haberman, S. J. (1973). The analysis of residuals in cross-classified
#' tables. *Biometrics*, 29(1), 205-220. \doi{10.2307/2529686}
#'
#' @seealso [extract_meta_paths()], [permutation_htna()],
#'   [mosaic_plot_htna()].
#' @examples
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' # iter = 50 keeps the example fast; use the default 1000 in practice.
#' sequence_compare_htna(grp, iter = 50)
#' sequence_compare_htna(grp, level = "type", iter = 50)
#' @export
sequence_compare_htna <- function(x,
                                  group = NULL,
                                  level = c("state", "type"),
                                  ...) {
  level <- match.arg(level)

  if (level == "type") {
    if (inherits(x, "htna_group")) {
      ng <- if (length(x)) x[[1L]]$node_groups else NULL
    } else {
      ng <- if (is.list(x)) x$node_groups else NULL
    }
    if (is.null(ng) || !nrow(ng)) {
      .htna_stop("`level = \"type\"` requires `x` to be an htna network with ",
                 "an actor partition (`$node_groups`). Build it with ",
                 "`build_htna()`.")
    }
    if (inherits(x, "htna_group")) {
      cls <- class(x)
      x <- lapply(x, .fold_states_to_types)
      class(x) <- cls
    } else {
      x <- .fold_states_to_types(x)
    }
  }

  Nestimate::sequence_compare(x = x, group = group, ...)
}

# Replace concrete state codes in $data with their actor types so that
# downstream sequence enumeration runs on meta-paths. Preserves the
# rest of the network structure (group columns, metadata, etc.) so
# Nestimate's input parser still recognises it as a network.
.fold_states_to_types <- function(net) {
  ng <- net$node_groups
  type_map <- stats::setNames(as.character(ng$group), as.character(ng$node))
  d <- net$data
  state_cols <- grep("^T\\d+$", names(d), value = TRUE)
  if (!length(state_cols)) {
    .htna_stop("Could not locate state columns (`T1`, `T2`, ...) in `x$data`.")
  }
  # NAs (padding) remain NA after the lookup.
  d[state_cols] <- lapply(d[state_cols], function(col) {
    unname(type_map[as.character(col)])
  })
  net$data <- d
  net
}
