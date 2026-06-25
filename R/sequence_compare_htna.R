#' Subsequence Pattern Comparison Across Groups
#'
#' Extends [Nestimate::sequence_compare()] with a `level` argument that
#' lets the comparison run on type-level (meta-path) sequences as well
#' as state-level sequences.
#'
#' Extracts all k-gram patterns (subsequences of length `k`) from the
#' sequences in each cohort, computes standardised residuals against
#' the independence model, and optionally runs a permutation or
#' chi-square test for differences in pattern rates between cohorts.
#'
#' Operates on grouped htna networks (built via
#' `build_htna(..., group = ...)`), single htna networks paired with a
#' `group` argument, or wide-format sequence data with an explicit
#' `group` argument. The actor partition itself is not consumed when
#' `level = "state"` — sequence comparison is between cohorts of
#' sessions, not between actors. When `level = "type"`, concrete codes
#' are folded into their actor type before pattern enumeration, so the
#' comparison runs on meta-paths.
#'
#' @inheritParams Nestimate::sequence_compare
#' @param level Character. `"state"` (default) compares concrete
#'   state-level k-grams; `"type"` first replaces each state with its
#'   actor type so the comparison runs on meta-paths
#'   (e.g. `Human->AI->Human`). Only meaningful when `x` is an htna
#'   network with an actor partition.
#'
#' @return An object of class `net_sequence_compare`. See
#'   [Nestimate::sequence_compare()] for full details and the
#'   corresponding `plot()` method.
#'
#' @seealso [permutation_htna()] for whole-network differences,
#'   [mosaic_plot_htna()] for single-step transition residuals,
#'   [extract_meta_paths()] for descriptive meta-path enumeration.
#' @examples
#' \donttest{
#' data(human_ai)
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#'
#' # State-level comparison (default)
#' sequence_compare_htna(grp, iter = 50)
#'
#' # Meta-path comparison
#' sequence_compare_htna(grp, level = "type", iter = 50)
#' }
#' @export
sequence_compare_htna <- function(x,
                                  group    = NULL,
                                  level    = c("state", "type"),
                                  sub      = 3:5,
                                  min_freq = 5L,
                                  test     = c("permutation", "chisq", "none"),
                                  iter     = 1000L,
                                  adjust   = "fdr") {
  level <- match.arg(level)
  test  <- match.arg(test)

  if (level == "type") {
    if (inherits(x, "htna_group")) {
      ng <- if (length(x)) x[[1L]]$node_groups else NULL
    } else {
      ng <- if (is.list(x)) x$node_groups else NULL
    }
    if (is.null(ng) || !nrow(ng)) {
      stop("`level = \"type\"` requires `x` to be an htna network with ",
           "an actor partition (`$node_groups`). Build it with ",
           "`build_htna()`.", call. = FALSE)
    }
    if (inherits(x, "htna_group")) {
      cls <- class(x)
      x <- lapply(x, .fold_states_to_types)
      class(x) <- cls
    } else {
      x <- .fold_states_to_types(x)
    }
  }

  Nestimate::sequence_compare(
    x        = x,
    group    = group,
    sub      = sub,
    min_freq = min_freq,
    test     = test,
    iter     = iter,
    adjust   = adjust
  )
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
    stop("Could not locate state columns (`T1`, `T2`, ...) in `x$data`.",
         call. = FALSE)
  }
  for (col in state_cols) {
    vals <- as.character(d[[col]])
    mapped <- unname(type_map[vals])
    # NAs in vals (padding) remain NA after the lookup.
    d[[col]] <- mapped
  }
  net$data <- d
  net
}
