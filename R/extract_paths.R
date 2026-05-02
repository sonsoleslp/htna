#' Extract State-Level Paths from a Heterogeneous Transition Network
#'
#' State-level counterpart to [extract_meta_paths()]. Where
#' `extract_meta_paths()` discovers patterns over the *actor type* alphabet
#' (e.g. `Human->AI->Human`), `extract_paths()` discovers patterns over the
#' concrete *state* alphabet (e.g. `Ask->Plan->Verify`). Both share the
#' same path-discovery engine - this function just feeds it sequences
#' encoded over states instead of types.
#'
#' Operates on the actor partition stored on the network by [build_htna()]:
#' the node labels in `$node_groups$node` define the alphabet.
#'
#' @inheritParams extract_meta_paths
#'
#' @return An object of class `c("htna_state_paths", "htna_paths",
#'   "data.frame")` with columns: `schema`, `length`, `gap`, `count`,
#'   `n_sequences`, `support`, `frequency`, `lift`. Attributes:
#'   `n_sequences`, `alphabet`, `level = "state"`.
#'
#' @seealso [extract_meta_paths()] for the type-level counterpart.
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#'
#' # Top contiguous length-2..4 paths
#' extract_paths(net)
#'
#' # Specific schema with a wildcard
#' extract_paths(net, schema = "Specify->*->Verify")
#'
#' # Over-represented length-3 state paths
#' extract_paths(net, length = 3, min_lift = 1.2)
#' }
#'
#' @export
extract_paths <- function(x,
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

  states       <- as.character(x$node_groups$node)
  identity_map <- setNames(states, states)
  int_seqs     <- .encode_int_seqs(x, states, identity_map)

  out <- .discover_paths(
    int_seqs    = int_seqs,
    alphabet    = states,
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
  attr(out, "alphabet")    <- states
  attr(out, "level")       <- "state"
  class(out) <- c("htna_state_paths", "htna_paths", "data.frame")
  out
}
