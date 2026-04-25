#' Extract Meta-Paths from a Heterogeneous Transition Network
#'
#' Discovers type-level patterns (meta-paths) over the actor-typed sequences
#' that produced a heterogeneous transition network. A meta-path is a fixed
#' sequence of node types - for example `Human->AI->Human` - that abstracts
#' away from concrete states and asks how often a *typed* template appears in
#' the data. Two regimes are supported: enumerate every meta-path of given
#' lengths (default), or search for one specific schema (with optional
#' wildcards).
#'
#' Operates on the actor partition stored on the network by [build_htna()].
#'
#' @param x \[`htna_network`\]\cr
#'   A network built with [build_htna()]. Must have `$nodes$groups`,
#'   `$node_groups`, and `$data` populated.
#' @param length \[`integer()`: `2:4`\]\cr
#'   Meta-path lengths to enumerate. Ignored when `schema` is supplied.
#' @param schema \[`character(1)`\]\cr
#'   Optional. A meta-path template to search for, written as actor names
#'   separated by `"->"` (e.g. `"Human->AI->Human"`). Wildcards are
#'   supported: `"Human->*->Human"` matches every length-3 meta-path that
#'   starts and ends with Human, with any type between.
#' @param type \[`character(1)`: `"contiguous"`\]\cr
#'   `"contiguous"` (default) considers consecutive positions; `"gapped"`
#'   considers positions spaced by `gap + 1` apart.
#' @param gap \[`integer()`: `1L`\]\cr
#'   Gap size(s) used when `type = "gapped"`. Multiple values produce one row
#'   per (length, gap) combination, distinguished by the `gap` column.
#' @param min_count \[`integer(1)`: `1L`\]\cr
#'   Minimum total occurrences to retain.
#' @param min_support \[`numeric(1)`: `0`\]\cr
#'   Minimum proportion of sequences containing the meta-path at least once.
#' @param min_lift \[`numeric(1)`: `0`\]\cr
#'   Minimum lift (observed / expected under marginal independence). `0`
#'   disables the filter.
#' @param start \[`character()`\]\cr
#'   Keep only meta-paths whose first type is in this set.
#' @param end \[`character()`\]\cr
#'   Keep only meta-paths whose last type is in this set.
#' @param contain \[`character()`\]\cr
#'   Keep only meta-paths whose type sequence contains all elements of this
#'   set (in any order).
#'
#' @return An object of class `c("htna_meta_paths", "htna_paths",
#'   "data.frame")` with columns: `schema`, `length`, `gap`, `count`,
#'   `n_sequences`, `support`, `frequency`, `lift`. Attributes:
#'   `n_sequences`, `alphabet`, `level = "meta"`.
#'
#' @seealso [build_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#'
#' # All meta-paths of length 2..4
#' extract_meta_paths(net)
#'
#' # Wildcard family search: any length-3 path that returns to Human
#' extract_meta_paths(net, schema = "Human->*->Human")
#'
#' # Gapped meta-paths
#' extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
#'
#' # Lift threshold (over-represented relative to type marginals)
#' extract_meta_paths(net, length = 3, min_lift = 1.2)
#' }
#'
#' @export
extract_meta_paths <- function(x,
                               length      = 2:4,
                               schema      = NULL,
                               type        = c("contiguous", "gapped"),
                               gap         = 1L,
                               min_count   = 1L,
                               min_support = 0,
                               min_lift    = 0,
                               start       = NULL,
                               end         = NULL,
                               contain     = NULL) {

  type <- match.arg(type)
  .check_htna_input(x)
  type_map <- .build_type_map(x)
  alphabet <- unique(unname(type_map))

  int_seqs <- .encode_int_seqs(x, alphabet, type_map)

  out <- .discover_paths(
    int_seqs    = int_seqs,
    alphabet    = alphabet,
    len         = length,
    gap         = gap,
    type        = type,
    pattern     = schema,
    min_freq    = min_count,
    min_support = min_support,
    min_lift    = min_lift,
    start       = start,
    end         = end,
    contain     = contain
  )

  attr(out, "n_sequences") <- length(int_seqs)
  attr(out, "alphabet")    <- alphabet
  attr(out, "level")       <- "meta"
  class(out) <- c("htna_meta_paths", "htna_paths", "data.frame")
  out
}


.check_htna_input <- function(x) {
  if (!inherits(x, "cograph_network") && !inherits(x, "netobject")) {
    stop("`x` must be a network produced by build_htna() (or a netobject).",
         call. = FALSE)
  }
  if (is.null(x$node_groups) || !nrow(x$node_groups)) {
    stop("`x` has no actor partition (`$node_groups` is empty). ",
         "Build it with `build_htna()`.", call. = FALSE)
  }
  if (is.null(x$data) || !is.data.frame(x$data)) {
    stop("`x$data` is missing or not a data frame; cannot extract sequences.",
         call. = FALSE)
  }
}

.build_type_map <- function(x) {
  ng <- x$node_groups
  setNames(as.character(ng$group), as.character(ng$node))
}
