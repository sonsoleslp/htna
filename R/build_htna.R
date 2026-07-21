#' Build a Heterogeneous Transition Network (HTNA)
#'
#' Builds a transition network over a combined sequence of two or more actor
#' groups (e.g. Human and AI) and preserves the actor partition on the result
#' so downstream plotting and analysis can treat each actor's codes as a
#' distinct node group.
#'
#' @param data Either:
#'   \itemize{
#'     \item A named list of long-format data frames, one per actor type
#'       (e.g. \code{list(Human = human_simplified, AI = ai_simplified)}).
#'       All frames must share the same column schema.
#'     \item A single long-format data frame. In that case either
#'       \code{actor_type} (row-level actor-type IDs) or \code{node_groups}
#'       (node-level actor-type lookup) must be supplied.
#'     \item A fitted clustering result from
#'       \code{Nestimate::build_clusters()},
#'       \code{Nestimate::cluster_mmm()} (a \code{net_mmm}), or
#'       \code{tna::cluster_sequences()}, or an already-materialized
#'       \code{netobject_group}. One HTNA network is returned per sequence
#'       cluster. The clustering fit is not rerun; assignments, diagnostics,
#'       fitted MMM weights, and actor metadata are preserved.
#'     \item A fitted \code{Nestimate::build_mcml()} object. Its node clusters
#'       become the HTNA actor groups and one full node-level network is rebuilt
#'       from the original source, including transitions between clusters.
#'   }
#' @param actor_type Character. Name of the column tagging each row's
#'   actor type / group (e.g. \code{"Human"} vs \code{"AI"}) when \code{data}
#'   is a single data frame. Ignored when \code{data} is a named list or
#'   when \code{node_groups} is supplied.
#' @param actor Character. Name of an optional column identifying the
#'   individual actor that performed each event (e.g. a learner / user id).
#'   Forwarded to \code{\link[Nestimate]{build_network}} with the same
#'   semantics; orthogonal to \code{actor_type}, which encodes the
#'   group/type partition over codes.
#' @param node_groups Node-to-actor-type lookup, in either of two forms:
#'   \itemize{
#'     \item A **named list** mapping actor-type labels to character vectors
#'       of code names, e.g. \code{list(Human = c("Specify", "Command"),
#'       AI = c("Plan", "Execute"))}.
#'     \item A **2-column data frame** with one column named after
#'       \code{action} (the codes) and one other column tagging each code
#'       with its actor type, e.g.
#'       \code{data.frame(code = c("Specify", "Plan"),
#'                        actor_type = c("Human", "AI"))}.
#'       The actor-type ordering follows the column's factor levels or, for
#'       character vectors, the order of first appearance.
#'   }
#'   Use \code{node_groups} when \code{data} is a single long-format frame
#'   with no actor-type column and you want to declare the node-to-type
#'   partition directly. Each code in \code{data[[action]]} must appear in
#'   exactly one entry. For clustering inputs, the canonical two-column
#'   \code{data.frame(node, group)} stored on an existing HTNA model is also
#'   accepted. The argument may be omitted when Nestimate preserved the
#'   partition from an HTNA input. Mutually exclusive with \code{actor_type}.
#'   For an \code{mcml} input, this optionally overrides
#'   \code{$cluster_members}; otherwise the fitted membership is used.
#' @param action Character. Name of the action/code column. Default
#'   \code{"code"}.
#' @param session Character. Name of the session column. Default
#'   \code{"session_id"}.
#' @param order Character. Name of the within-session order column. Default
#'   \code{"order_in_session"}.
#' @param method Character. Transition method passed to
#'   \code{\link[Nestimate]{build_network}}: \code{"relative"} (default),
#'   \code{"frequency"}, \code{"co_occurrence"}, or \code{"attention"}.
#'   Co-occurrence models are undirected and preserve the same actor partition
#'   as transition models.
#' @param group Character. Optional name of a column in \code{data} used to
#'   split sessions into cohorts (e.g. \code{"cluster"}). When supplied,
#'   one network is built per group level and the result is a named list of
#'   htna networks with class \code{c("htna_group", "netobject_group")}.
#' @param disambiguate Logical. If \code{FALSE} (default), the function errors
#'   when a code label appears in more than one actor-type group. If
#'   \code{TRUE}, codes are prefixed with the actor-type label
#'   (\code{"Human:Ask"}, \code{"AI:Ask"}) so they become distinct nodes.
#' @param source_data Original node-level data for an \code{mcml} input.
#'   Usually unnecessary when \code{Nestimate::build_mcml()} retained its
#'   sequence source. Required when the fitted object has no expandable source,
#'   such as an \code{mcml} built from a transition matrix or edge list. The
#'   fitted macro and within-cluster weights are deliberately not stitched
#'   together: rebuilding from this source is what preserves cross-cluster
#'   transitions.
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
#'     \item \code{$actor_levels} - declared actor ordering, also retained as
#'       metadata on \code{$node_groups} so the table can be passed back to
#'       \code{build_htna()} without losing that order.
#'   }
#'   All other slots are exactly as returned by
#'   \code{\link[Nestimate]{build_network}}. Clustering inputs return an
#'   \code{htna_group}; its children retain the actor partition and its outer
#'   clustering assignments, posterior probabilities, diagnostics, and other
#'   attributes are preserved. An \code{mcml} input instead returns one
#'   \code{htna}, equivalent to \code{Nestimate::as_htna()}.
#'
#' @seealso \code{\link[Nestimate]{build_network}},
#'   \code{\link[Nestimate]{build_tna}},
#'   \code{\link[cograph]{plot_htna}}
#'
#' @examples
#' data(human_ai, human_simplified, ai_simplified)
#'
#' # Form 1: named list of per-actor frames
#' net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#' head(net$node_groups)
#'
#' # Form 2: single combined frame with a row-level actor-type tag
#' net <- build_htna(human_ai, actor_type = "actor_type")
#'
#' @export
build_htna <- function(data,
                       actor_type   = NULL,
                       actor        = NULL,
                       node_groups  = NULL,
                       action       = "code",
                       session      = "session_id",
                       order        = "order_in_session",
                       method       = "relative",
                       group        = NULL,
                       disambiguate = FALSE,
                       source_data  = NULL,
                       ...) {

  dots <- list(...)
  if ("actor_col" %in% names(dots)) {
    stop("`actor_col` was renamed to `actor_type`. Use `actor_type = ",
         deparse(dots$actor_col), "`.", call. = FALSE)
  }

  # An MCML partition describes node groups, not separate fitted child
  # networks. Expand it to one global node-level HTNA so between-cluster
  # transitions are reconstructed from the original source.
  if (inherits(data, "mcml")) {
    if (!is.null(actor_type)) {
      stop("`actor_type` is not used for an `mcml` input; its node clusters ",
           "are the actor groups.", call. = FALSE)
    }
    if (!is.null(group)) {
      stop("An `mcml` expands to one global HTNA; do not also pass `group`.",
           call. = FALSE)
    }
    if (isTRUE(disambiguate)) {
      stop("`disambiguate` must be applied before fitting the `mcml`, not ",
           "while expanding it.", call. = FALSE)
    }
    return(.build_htna_from_mcml(
      data = data,
      source_data = source_data,
      node_groups = node_groups,
      actor = actor,
      action = action,
      session = session,
      order = order,
      method = method,
      dots = dots
    ))
  }

  if (!is.null(source_data)) {
    stop("`source_data` is only used when `data` is an `mcml` object.",
         call. = FALSE)
  }
  if (!is.null(actor_type) && !is.null(node_groups)) {
    stop("Pass either `actor_type` (row-level) or `node_groups` ",
         "(node-level), not both.", call. = FALSE)
  }

  # Nestimate and tna clustering results already encode the outer cohort
  # split. Convert/finalize those results without treating their list-like
  # objects as named lists of actor data frames.
  if (.is_htna_clustering_input(data)) {
    if (!is.null(actor_type)) {
      stop("`actor_type` is not available on a clustering result. Pass the ",
           "node partition with `node_groups`, or cluster an existing HTNA ",
           "object so Nestimate can preserve it.", call. = FALSE)
    }
    if (!is.null(group)) {
      stop("A clustering result already defines the outer groups; do not ",
           "also pass `group`.", call. = FALSE)
    }
    if (isTRUE(disambiguate)) {
      stop("`disambiguate` must be applied before clustering/fitting, not ",
           "while converting a clustering result.", call. = FALSE)
    }
    return(.build_htna_from_clustering(
      data = data,
      node_groups = node_groups,
      action = action,
      method = method,
      dots = dots
    ))
  }

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
  } else if (!is.null(node_groups)) {
    stopifnot(
      is.data.frame(data),
      is.character(action), length(action) == 1L,
      action %in% names(data)
    )
    # Accept node_groups either as a named list (canonical form) or as
    # a 2-column data frame whose `action`-named column lists the codes
    # and the other column tags each code with its actor type.
    if (is.data.frame(node_groups)) {
      if (ncol(node_groups) != 2L) {
        stop("If `node_groups` is a data frame, it must have exactly two ",
             "columns: the code column (named `", action,
             "`) and the actor-type column.", call. = FALSE)
      }
      if (!action %in% names(node_groups)) {
        stop("`node_groups` data frame must contain the code column `",
             action, "` (matching `action`).", call. = FALSE)
      }
      type_col   <- setdiff(names(node_groups), action)
      type_vals  <- node_groups[[type_col]]
      code_vals  <- as.character(node_groups[[action]])
      if (any(is.na(type_vals))) {
        stop("`node_groups` data frame's `", type_col,
             "` column has missing values; every code must be tagged ",
             "with an actor type.", call. = FALSE)
      }
      type_chr <- as.character(type_vals)
      # Drop unused factor levels so they don't become phantom actors.
      type_order <- if (is.factor(type_vals)) {
        levels(type_vals)[levels(type_vals) %in% type_chr]
      } else {
        unique(type_chr)
      }
      node_groups <- lapply(type_order, function(g)
        unique(code_vals[type_chr == g]))
      names(node_groups) <- type_order
    }
    stopifnot(
      is.list(node_groups), length(node_groups) >= 2L,
      !is.null(names(node_groups)), all(nzchar(names(node_groups))),
      all(vapply(node_groups, is.character, logical(1L)))
    )
    # De-duplicate codes within each actor before checking inter-actor
    # overlap, so the error message below only reports genuine
    # cross-actor collisions (not intra-actor typos / repeats).
    node_groups <- lapply(node_groups, unique)
    all_codes <- unlist(node_groups, use.names = FALSE)
    if (anyDuplicated(all_codes)) {
      dups <- unique(all_codes[duplicated(all_codes)])
      stop("Code(s) appear in more than one actor list in `node_groups`: ",
           paste(dups, collapse = ", "), call. = FALSE)
    }
    combined      <- data
    code_to_actor <- setNames(rep(names(node_groups), lengths(node_groups)), all_codes)
    raw_codes     <- as.character(combined[[action]])
    actor_vec     <- unname(code_to_actor[raw_codes])
    if (any(is.na(actor_vec))) {
      missing <- unique(raw_codes[is.na(actor_vec)])
      stop("Codes in `data` not assigned to any actor in `node_groups`: ",
           paste(missing, collapse = ", "),
           ". Add them to `node_groups` or filter them out.", call. = FALSE)
    }
    actor_levels <- names(node_groups)
  } else {
    stopifnot(
      is.data.frame(data),
      is.character(actor_type), length(actor_type) == 1L,
      actor_type %in% names(data)
    )
    combined  <- data
    raw_col   <- combined[[actor_type]]
    if (any(is.na(raw_col))) {
      stop("`actor_type` column has missing values; every row must be ",
           "tagged with an actor type.", call. = FALSE)
    }
    actor_vec <- as.character(raw_col)
    if (is.factor(raw_col)) {
      # Honour declared factor levels but drop levels that have no
      # rows — declared-but-absent levels would otherwise become
      # phantom actors with empty code sets.
      present      <- levels(raw_col)[levels(raw_col) %in% actor_vec]
      actor_levels <- present
    } else {
      actor_levels <- unique(actor_vec)
    }
    if (length(actor_levels) < 2L) {
      stop("`actor_type` must contain at least two distinct actor types.",
           call. = FALSE)
    }
  }

  stopifnot(
    is.character(action),  length(action)  == 1L, action  %in% names(combined),
    is.character(session), length(session) == 1L,
    length(order)   == 1L,
    is.logical(disambiguate), length(disambiguate) == 1L
  )

  # Check if required columns exist
  # Session is only required if no actor is provided
  if (is.null(actor) && !session %in% names(combined)) {
    stop("Column '", session, "' not found in data. ",
         "Available columns: ", paste(names(combined), collapse = ", "), ". ",
         "Either provide a data frame with a '", session, "' column or ",
         "specify an 'actor' parameter to calculate sessions automatically.", call. = FALSE)
  }

  # If actor is provided and session column doesn't exist, sessions will be auto-generated
  session_exists <- session %in% names(combined)

  # Order is only required if no actor is provided
  if (is.null(actor) && !order %in% names(combined)) {
    stop("Column '", order, "' not found in data. ",
         "Available columns: ", paste(names(combined), collapse = ", "), ". ",
         "Either provide a data frame with an '", order, "' column or ",
         "specify an 'actor' parameter for automatic ordering.", call. = FALSE)
  }

  order_exists <- order %in% names(combined)
  if (!is.null(group)) {
    stopifnot(
      is.character(group), length(group) == 1L, group %in% names(combined)
    )
  }
  if (!is.null(actor)) {
    stopifnot(
      is.character(actor), length(actor) == 1L, actor %in% names(combined)
    )
  }

  # ---- Build per-actor code dictionary; optionally disambiguate overlaps ----
  actor_fac <- factor(actor_vec, levels = actor_levels)
  codes_by_actor <- lapply(
    split(as.character(combined[[action]]), actor_fac),
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
      split(as.character(combined[[action]]), actor_fac),
      unique
    )
  }

  # ---- Order within session so cross-actor transitions follow real time ----
  if (session_exists && order_exists) {
    combined <- combined[
      order(combined[[session]], combined[[order]]), , drop = FALSE
    ]
  } else if (session_exists) {
    # Order by session only
    combined <- combined[order(combined[[session]]), , drop = FALSE]
  }

  # ---- Delegate to Nestimate::build_network ----
  nestimate_args <- list(data    = combined,
                        method  = method,
                        action  = action,
                        actor   = actor,
                        format  = "long",
                        group   = group)

  # Only include session if the column exists in the data
  if (session_exists) {
    nestimate_args$session <- session
  }

  # Only include order if the column exists in the data
  if (order_exists) {
    nestimate_args$order <- order
  }

  net <- do.call(Nestimate::build_network, c(nestimate_args, dots))

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

  if (!is.null(group)) {
    # When using node_groups, filter each group to only include codes that
    # actually appear in that group's data
    if (!is.null(node_groups)) {
      # Validate no NA values in group column before proceeding
      group_col <- combined[[group]]
      if (any(is.na(group_col))) {
        stop("Group column `", group, "` contains NA values. Every row must be ",
             "assigned to a group.", call. = FALSE)
      }

      group_codes <- split(as.character(combined[[action]]), group_col)
      net <- Map(function(subnet, group_name) {
        actual_codes <- unique(group_codes[[group_name]])
        .inject_htna_partition_filtered(subnet, actor_lookup, actor_levels, actual_codes)
      }, net, names(net))
    } else {
      net <- lapply(net, .inject_htna_partition, actor_lookup, actor_levels)
    }
    class(net) <- c("htna_group", "netobject_group", "list")
    attr(net, "actor_levels") <- actor_levels
    return(net)
  }

  .inject_htna_partition(net, actor_lookup, actor_levels)
}

#' @keywords internal
.inject_htna_partition <- function(net, actor_lookup, actor_levels) {
  node_actor <- unname(actor_lookup[net$nodes$label])
  net$nodes$groups <- factor(node_actor, levels = actor_levels)
  net$node_groups <- data.frame(
    node  = net$nodes$label,
    group = as.character(node_actor),
    stringsAsFactors = FALSE
  )
  attr(net$node_groups, "actor_levels") <- actor_levels
  net$actor_levels <- actor_levels
  if (!inherits(net, "htna")) class(net) <- c("htna", class(net))
  net
}

#' @keywords internal
.inject_htna_partition_filtered <- function(net, actor_lookup, actor_levels, actual_codes) {
  # Filter nodes to only include those that actually appear in this group's data
  nodes_to_keep <- net$nodes$label %in% actual_codes

  # Filter nodes dataframe
  net$nodes <- net$nodes[nodes_to_keep, , drop = FALSE]

  # Filter weights matrix
  codes_to_keep <- rownames(net$weights) %in% actual_codes
  net$weights <- net$weights[codes_to_keep, codes_to_keep, drop = FALSE]

  # Add actor group information for remaining nodes
  node_actor <- unname(actor_lookup[net$nodes$label])
  net$nodes$groups <- factor(node_actor, levels = actor_levels)
  net$node_groups <- data.frame(
    node  = net$nodes$label,
    group = as.character(node_actor),
    stringsAsFactors = FALSE
  )
  attr(net$node_groups, "actor_levels") <- actor_levels
  net$actor_levels <- actor_levels
  if (!inherits(net, "htna")) class(net) <- c("htna", class(net))
  net
}
