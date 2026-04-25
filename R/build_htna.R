#' Build a Heterogeneous Transition Network (HTNA)
#'
#' Builds a transition network over a combined sequence of two or more actor
#' groups (e.g. Human and AI) and preserves the actor partition on the result
#' so downstream plotting and analysis can treat each actor's codes as a
#' distinct node group.
#'
#' @param data Either:
#'   \itemize{
#'     \item A named list of long-format data frames, one per actor
#'       (e.g. \code{list(Human = human_long, AI = ai_long)}). All frames
#'       must share the same column schema.
#'     \item A single long-format data frame with an actor column. In that
#'       case \code{actor_col} must be supplied.
#'   }
#' @param actor_col Character. Name of the actor column when \code{data} is
#'   a single data frame. Ignored when \code{data} is a named list.
#' @param action Character. Name of the action/code column. Default
#'   \code{"code"}.
#' @param session Character. Name of the session column. Default
#'   \code{"session_id"}.
#' @param order Character. Name of the within-session order column. Default
#'   \code{"order_in_session"}.
#' @param method Character. Transition method passed to
#'   \code{\link[Nestimate]{build_network}}: \code{"relative"} (default),
#'   \code{"frequency"}, or \code{"attention"}.
#' @param disambiguate Logical. If \code{FALSE} (default), the function errors
#'   when a code label appears in more than one actor group. If \code{TRUE},
#'   codes are prefixed with the actor name (\code{"Human:Ask"},
#'   \code{"AI:Ask"}) so they become distinct nodes.
#' @param ... Additional arguments forwarded to
#'   \code{\link[Nestimate]{build_network}}.
#'
#' @return A \code{netobject} with the actor partition stored in cograph's
#'   canonical schema, so \code{cograph::plot_htna(net)} auto-detects the
#'   groups with no further arguments:
#'   \itemize{
#'     \item \code{$nodes$groups} - actor label per node (the column name
#'       \code{cograph::plot_htna} auto-detects).
#'     \item \code{$node_groups} - data frame with columns \code{node} and
#'       \code{group} (canonical \code{cograph_network} schema, also
#'       readable by \code{cograph::get_groups}, \code{cluster_summary}, and
#'       the \code{print} method for \code{cograph_network}).
#'   }
#'   All other slots are exactly as returned by
#'   \code{\link[Nestimate]{build_network}}.
#'
#' @seealso \code{\link[Nestimate]{build_network}},
#'   \code{\link[Nestimate]{build_tna}},
#'   \code{\link[cograph]{plot_htna}}
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' net$node_groups            # canonical (node, group) schema
#' cograph::plot_htna(net)    # auto-detects $nodes$groups, no other args
#' }
#'
#' @export
build_htna <- function(data,
                       actor_col    = NULL,
                       action       = "code",
                       session      = "session_id",
                       order        = "order_in_session",
                       method       = "relative",
                       disambiguate = FALSE,
                       ...) {

  # ---- Resolve input form to (combined_df, actor_vec) ----
  if (is.list(data) && !is.data.frame(data)) {
    stopifnot(
      length(data) >= 2L,
      !is.null(names(data)),
      all(nzchar(names(data))),
      all(vapply(data, is.data.frame, logical(1L)))
    )
    schemas <- lapply(data, names)
    if (length(unique(schemas)) > 1L) {
      stop("All data frames in `data` must share the same column schema.",
           call. = FALSE)
    }
    actor_levels <- names(data)
    n_per        <- vapply(data, nrow, integer(1L))
    combined     <- do.call(rbind, unname(data))
    actor_vec    <- rep(actor_levels, times = n_per)
  } else {
    stopifnot(
      is.data.frame(data),
      is.character(actor_col), length(actor_col) == 1L,
      actor_col %in% names(data)
    )
    combined     <- data
    actor_vec    <- as.character(combined[[actor_col]])
    actor_levels <- unique(actor_vec)
    if (length(actor_levels) < 2L) {
      stop("`actor_col` must contain at least two distinct actors.",
           call. = FALSE)
    }
  }

  stopifnot(
    is.character(action),  length(action)  == 1L, action  %in% names(combined),
    is.character(session), length(session) == 1L, session %in% names(combined),
    is.character(order),   length(order)   == 1L, order   %in% names(combined),
    is.logical(disambiguate), length(disambiguate) == 1L
  )

  # ---- Build per-actor code dictionary; optionally disambiguate overlaps ----
  codes_by_actor <- lapply(
    split(as.character(combined[[action]]), actor_vec),
    unique
  )
  overlap <- Reduce(intersect, codes_by_actor)
  if (length(overlap) > 0L && !disambiguate) {
    stop(
      "Code(s) appear in more than one actor group: ",
      paste(overlap, collapse = ", "),
      ". Pass `disambiguate = TRUE` to prefix codes with the actor label.",
      call. = FALSE
    )
  }
  if (disambiguate) {
    combined[[action]] <- paste(actor_vec, combined[[action]], sep = ":")
    codes_by_actor <- lapply(
      split(as.character(combined[[action]]), actor_vec),
      unique
    )
  }

  # ---- Order within session so cross-actor transitions follow real time ----
  combined <- combined[
    order(combined[[session]], combined[[order]]), , drop = FALSE
  ]

  # ---- Delegate to Nestimate::build_network ----
  net <- Nestimate::build_network(
    combined,
    method  = method,
    action  = action,
    session = session,
    order   = order,
    format  = "long",
    ...
  )

  # ---- Inject actor partition using cograph's canonical schema ----
  # cograph's plot_htna auto-detects nodes columns named "groups"/"group"/
  # "clusters"/"cluster"/"community"/"module"/"layer" (see plot-htna.R).
  # Its node_groups slot expects columns "node" + one of "group"/"cluster"/
  # "layer". Using "groups" on $nodes and (node, group) on $node_groups
  # makes plot_htna(net), get_groups(net), and cluster_summary(net) all
  # work with no arguments.
  codes_flat   <- unlist(unname(codes_by_actor), use.names = FALSE)
  actor_flat   <- rep(names(codes_by_actor), lengths(codes_by_actor))
  actor_lookup <- setNames(actor_flat, codes_flat)
  net$nodes$groups <- unname(actor_lookup[net$nodes$label])
  net$node_groups <- data.frame(
    node  = net$nodes$label,
    group = net$nodes$groups,
    stringsAsFactors = FALSE
  )
  net
}
