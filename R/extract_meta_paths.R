#' Extract Path Patterns from a Heterogeneous Transition Network
#'
#' Discovers recurring patterns in the actor-typed sequences that produced a
#' heterogeneous transition network. Operates at two levels:
#'
#' * `level = "state"` (default) - enumerate concrete state-level patterns
#'   (e.g. `Command->Execute->Command`) and annotate each row with the
#'   type-level template it instantiates (e.g. `Human->AI->Human`).
#' * `level = "type"` - enumerate type-level meta-paths only (one row per
#'   actor-type pattern, summed over its concrete instances).
#'
#' At either level, a `schema` can filter the search. Schema parts can be
#' type names (expand to every concrete code in that group), concrete
#' states, or `"*"` (any element). Parts can be mixed freely - e.g.
#' `"Human->Ask->Human"` means "any Human code, then `Ask`, then any Human
#' code". At `level = "type"` concrete codes resolve to their type, so the
#' same schema becomes `Human->AI->Human`.
#'
#' Operates on the actor partition stored on the network by [build_htna()].
#'
#' @param x \[`htna_network`\]\cr
#'   A network built with [build_htna()]. Must have `$nodes$groups`,
#'   `$node_groups`, and `$data` populated.
#' @param level \[`character(1)`: `"state"`\]\cr
#'   `"state"` returns concrete state-level patterns with a `meta_schema`
#'   rollup column. `"type"` returns the type-level meta-path summary.
#' @param length \[`integer()`: `2:4`\]\cr
#'   Pattern lengths to enumerate. Ignored when `schema` is supplied.
#' @param schema \[`character(1)`\]\cr
#'   Optional. A path template written as elements separated by `"->"`. Each
#'   element can be a type name, a concrete state, or `"*"`. See Details.
#' @param type \[`character(1)`: `"contiguous"`\]\cr
#'   `"contiguous"` (default) considers consecutive positions; `"gapped"`
#'   considers positions spaced by `gap + 1` apart.
#' @param gap \[`integer()`: `1L`\]\cr
#'   Gap size(s) used when `type = "gapped"`. Multiple values produce one
#'   row per (length, gap) combination.
#' @param min_count \[`integer(1)`: `1L`\]\cr
#'   Minimum total occurrences to retain.
#' @param min_support \[`numeric(1)`: `0`\]\cr
#'   Minimum proportion of sequences containing the pattern at least once.
#' @param min_lift \[`numeric(1)`: `0`\]\cr
#'   Minimum lift (observed / expected under marginal independence). `0`
#'   disables the filter.
#' @param start,end,contain \[`character()`\]\cr
#'   Filter rows whose first / last element is in `start` / `end`, or whose
#'   element sequence contains all of `contain`. Compared against the
#'   alphabet of the chosen level (types or concrete states).
#'
#' @return An object of class `c("htna_meta_paths", "htna_paths",
#'   "data.frame")`. At `level = "state"` the columns are `schema`,
#'   `meta_schema`, `length`, `gap`, `count`, `n_sequences`, `support`,
#'   `frequency`, `lift`. At `level = "type"` the `meta_schema` column is
#'   omitted (the `schema` column already holds the type pattern).
#'   Attributes: `n_sequences`, `alphabet`, `level`, and (when supplied)
#'   `schema`.
#'
#' @seealso [build_htna()].
#'
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#'
#' # Concrete state-level patterns of length 2..4 (default)
#' extract_meta_paths(net)
#'
#' # Type-level meta-path summary
#' extract_meta_paths(net, level = "type")
#'
#' # Concrete instances of a type-level template
#' extract_meta_paths(net, schema = "Human->AI->Human")
#'
#' # Mix types, concrete codes, and wildcards
#' extract_meta_paths(net, schema = "Human->Ask->Human")
#' extract_meta_paths(net, schema = "Human->*->Human")
#'
#' # Gapped patterns; lift threshold
#' extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
#' extract_meta_paths(net, length = 3, min_lift = 1.2)
#' }
#'
#' @export
extract_meta_paths <- function(x,
                               level       = c("state", "type"),
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

  level <- match.arg(level)
  type  <- match.arg(type)
  .check_htna_input(x)
  type_map <- .build_type_map(x)

  if (level == "type") {
    alphabet  <- unique(unname(type_map))
    int_seqs  <- .encode_int_seqs(x, alphabet, type_map)
    parts_idx <- if (is.null(schema)) NULL
                 else .parse_type_schema(schema, alphabet, type_map)
  } else {
    alphabet  <- as.character(x$node_groups$node)
    int_seqs  <- .encode_int_seqs(x, alphabet,
                                  stats::setNames(alphabet, alphabet))
    parts_idx <- if (is.null(schema)) NULL
                 else .expand_meta_schema(schema, alphabet, type_map)
  }

  out <- .discover_paths(
    int_seqs    = int_seqs,
    alphabet    = alphabet,
    len         = length,
    gap         = gap,
    type        = type,
    parts_idx   = parts_idx,
    min_freq    = min_count,
    min_support = min_support,
    min_lift    = min_lift,
    start       = start,
    end         = end,
    contain     = contain
  )

  if (level == "state") {
    if (nrow(out)) {
      meta <- vapply(strsplit(out$schema, "->", fixed = TRUE),
                     function(p) paste(type_map[p], collapse = "->"),
                     character(1L))
      out$meta_schema <- meta
      out <- out[, c("schema", "meta_schema",
                     setdiff(names(out), c("schema", "meta_schema")))]
    } else {
      out$meta_schema <- character(0L)
    }
  }

  attr(out, "n_sequences") <- base::length(int_seqs)
  attr(out, "alphabet")    <- alphabet
  attr(out, "level")       <- level
  if (!is.null(schema)) attr(out, "schema") <- schema

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
