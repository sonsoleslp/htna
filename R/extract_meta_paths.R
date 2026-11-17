#' Count recurring paths and meta-paths in the sessions
#'
#' @description
#' `extract_meta_paths()` counts the recurring paths and meta-paths in the
#' sessions of an htna network. In a heterogeneous network every code
#' belongs to an actor, so each run of codes in a session has two readings:
#' the concrete path (for example `Request->Plan->Execute`) and its
#' meta-path, the sequence of actor types it passes through
#' (`Human->AI->AI`). Meta-paths describe the turn-taking between the
#' actors, such as how often the human intervenes between two AI actions,
#' while the concrete paths say what is done in each turn. Counting both
#' shows which interaction patterns recur and whether they occur more often
#' than the frequencies of their codes would predict.
#'
#' `extract_meta_paths()` counts every path of the requested lengths in the
#' sessions of an htna network. With `level = "state"` (default) it returns
#' concrete paths, each tagged with its meta-path; with `level = "type"` it
#' returns the meta-paths, summed over their concrete instances. A
#' `schema` restricts the search to one template whose elements can be
#' actor types (any code of that actor), concrete codes, or `"*"` (any
#' element), freely mixed: `"Human->Ask->*"` is any human code, then the AI
#' code `Ask`, then anything. With `type = "gapped"`, the elements of a path
#' are `gap + 1` positions apart instead of adjacent.
#'
#' It returns a data frame with one row per path and gap. For each path the
#' table gives its count, the number and share (`support`) of sessions that
#' contain it, its share among the paths of the same length and gap
#' (`frequency`), and its `lift`: the count divided by the count expected if
#' the elements occurred independently with their overall frequencies.
#'
#' To compare paths between groups of sessions use
#' [sequence_compare_htna()]; for unordered co-occurrence use
#' [association_rules_htna()].
#'
#' @param x An htna network from [build_htna()].
#' @param level `"state"` (default) for concrete paths with their
#'   meta-path, or `"type"` for meta-paths only.
#' @param length Path lengths to count (default `2:4`); ignored when
#'   `schema` is given.
#' @param schema A template of elements separated by `"->"`; each element
#'   is an actor type, a code or `"*"`. `NULL` (default) counts all paths.
#' @param type `"contiguous"` (default) for adjacent elements, or
#'   `"gapped"` for elements `gap + 1` positions apart.
#' @param gap Gap or gaps for `type = "gapped"` (default 1); several values
#'   give one set of rows per length and gap.
#' @param min_count Minimum number of occurrences (default 1).
#' @param min_support Minimum share of sessions containing the path
#'   (default 0).
#' @param min_lift Minimum lift (default 0, no filter).
#' @param start,end,contain Keep only paths whose first element is in
#'   `start`, whose last element is in `end`, or that contain every element
#'   of `contain` (default `NULL`); elements are codes or, with
#'   `level = "type"`, actor types.
#'
#' @return A data frame of class
#'   `c("htna_meta_paths", "htna_paths", "data.frame")` with one row per
#'   path and gap, sorted by length, gap and decreasing count, and columns
#'   `schema` (the path), `meta_schema` (its meta-path; only with
#'   `level = "state"`), `length`, `gap`, `count`, `n_sequences`,
#'   `support`, `frequency` and `lift`. The number of sessions and the
#'   level are kept as attributes and shown by `print()`.
#'
#' @section Conditions:
#' Errors of class `htna_bad_input` for an unknown element in `schema` or a
#' zero gap with `type = "gapped"`, and `htna_unassigned_node` when `x` has
#' no actor partition.
#'
#' @seealso [print.htna_paths()], [sequence_compare_htna()],
#'   [association_rules_htna()].
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' extract_meta_paths(net, level = "type", length = 3)
#' extract_meta_paths(net, schema = "Human->AI->Human")
#' extract_meta_paths(net, schema = "Human->Ask->*", min_lift = 1)
#' extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
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
    .htna_stop("`x` must be a network produced by build_htna() (or a netobject).")
  }
  if (is.null(x$node_groups) || !nrow(x$node_groups)) {
    .htna_stop("`x` has no actor partition (`$node_groups` is empty). ",
               "Build it with `build_htna()`.",
               class = "htna_unassigned_node")
  }
  if (is.null(x$data) || !is.data.frame(x$data)) {
    .htna_stop("`x$data` is missing or not a data frame; cannot extract sequences.")
  }
}

.build_type_map <- function(x) {
  ng <- x$node_groups
  setNames(as.character(ng$group), as.character(ng$node))
}
