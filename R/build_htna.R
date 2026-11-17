#' Build a heterogeneous transition network
#'
#' @description
#' `build_htna()` builds a heterogeneous transition network: one transition
#' network over interaction sequences in which the events are produced by two
#' or more types of actor, such as a learner and an AI assistant. In
#' heterogeneous transition network analysis (HTNA) each node is a code (an
#' action or event) and belongs to exactly one actor type, and each directed
#' edge is the transition from one code to the next within a session
#' (Lopez-Pernas, Misiejuk and Saqr, 2026). Because the actor partition is
#' kept on the network, the within-actor transitions (Human to Human) and the
#' hand-overs between actors (Human to AI, AI to Human) can be estimated,
#' plotted and tested as distinct parts of one model.
#'
#' `build_htna()` orders the events of every session in time, estimates the
#' transition network over the combined sequence with the chosen `method`
#' (relative transition probabilities by default, or frequencies,
#' co-occurrences or attention weights), and attaches the actor type of every
#' node. The actor partition can be given in three ways: a named list of one
#' long data frame per actor type, a single data frame with a column naming
#' the actor type of each row (`actor_type`), or a single data frame together
#' with a code-to-actor lookup (`node_groups`). With `group`, one network is
#' estimated per level of a session-level variable (for example an early and
#' a late phase), in the order of the factor levels. A fitted sequence
#' clustering (from `Nestimate::build_clusters()`, `Nestimate::cluster_mmm()`
#' or `tna::cluster_sequences()`) is turned into one network per cluster
#' without refitting, and a fitted `Nestimate::build_mcml()` model into one
#' network whose actor types are its node clusters.
#'
#' It returns an `htna` network, or an `htna_group` with one network per
#' cohort or cluster. The grouped result is passed as it is to the
#' comparison, assessment and plotting functions, which work through every
#' cohort. Use [summary.htna()] to describe the result and
#' [as.data.frame.htna()] for its edges and nodes.
#'
#' @param data The event data, in one of these forms:
#'   * a named list of long data frames, one per actor type, with the same
#'     columns, e.g. `list(Human = human_simplified, AI = ai_simplified)`;
#'   * one long data frame, with `actor_type` or `node_groups`;
#'   * a fitted clustering (`net_clustering`, `net_mmm`, `tna_clustering`)
#'     or a `netobject_group`, giving one network per cluster;
#'   * a fitted `mcml` model, whose node clusters become the actor types.
#' @param actor_type Name of the column giving the actor type of each row,
#'   for a single data frame. A factor column sets the order of the actor
#'   types; otherwise they are ordered by first appearance.
#' @param actor Name of an optional column identifying the individual who
#'   produced each event (for example a learner id). Independent of
#'   `actor_type`. Default `NULL`.
#' @param node_groups Code-to-actor lookup for a single data frame without an
#'   actor-type column: a named list of code vectors, e.g.
#'   `list(Human = c("Specify", "Check"), AI = c("Plan", "Execute"))`, or a
#'   two-column data frame with the code column (named as `action`) and the
#'   actor-type column, such as [human_ai_codebook]. Every code must belong to
#'   exactly one actor type. For a clustering or `mcml` input it optionally
#'   supplies or overrides the partition. Use either `actor_type` or
#'   `node_groups`, not both.
#' @param action Name of the code column. Default `"code"`.
#' @param session Name of the session column. Default `"session_id"`.
#' @param order Name of the within-session order column. Default
#'   `"order_in_session"`.
#' @param method Network estimator: `"relative"` (default, transition
#'   probabilities), `"frequency"`, `"co_occurrence"` (undirected) or
#'   `"attention"`.
#' @param group Name of a session-level column defining cohorts; one network
#'   is built per cohort. Default `NULL` (one network).
#' @param disambiguate If `TRUE`, a code used by more than one actor type is
#'   split into one node per actor type, prefixed with the actor label
#'   (`"Human:Ask"`, `"AI:Ask"`). Default `FALSE` (a shared code is an error).
#' @param source_data The original event data for an `mcml` input whose fitted
#'   object did not keep it. Default `NULL`.
#' @param ... Further estimation arguments, such as `threshold` or `scaling`.
#'
#' @return An object of class `c("htna", "netobject", "cograph_network")`:
#'   a transition network with its weight matrix, nodes, sequence data and
#'   the actor partition (the actor type of every node and the ordered actor
#'   types). With `group` or a clustering input, an `htna_group`: a named list
#'   with one `htna` network per cohort or cluster, which keeps the clustering
#'   assignments and diagnostics as attributes. An `mcml` input returns one
#'   `htna`. [as.data.frame.htna()] returns the edges (one row per non-zero
#'   transition, with the actor types of its source and target) or the nodes.
#'
#' @section Conditions:
#' Errors inherit from `htna_error`. Specific classes:
#' `htna_missing_column` when the session, order or code column is absent;
#' `htna_actor_conflict` when a code belongs to more than one actor type and
#' `disambiguate = FALSE`; `htna_unassigned_node` when a code has no actor
#' type in `node_groups`; `htna_bad_input` for other invalid arguments.
#'
#' @references
#' Lopez-Pernas, S., Misiejuk, K., & Saqr, M. (2026). How AI-generated
#' feedback hinders or helps learning: A heterogeneous TNA study of learning
#' dynamics. *Journal of Computer Assisted Learning*, 42(4), e70285.
#' \doi{10.1002/jcal.70285}
#'
#' @seealso [summary.htna()] and [as.data.frame.htna()] to describe the
#'   network; [plot_htna()] to draw it; [compare_htna()],
#'   [permutation_htna()] and [bootstrap_htna()] for inference.
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' net
#'
#' # the same network from one data frame per actor type
#' build_htna(list(Human = human_simplified, AI = ai_simplified))
#'
#' # or from a code-to-actor codebook
#' build_htna(human_ai, node_groups = human_ai_codebook)
#'
#' # one network per phase, in the order of the factor levels
#' grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' summary(grp)
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
    .htna_stop("`actor_col` was renamed to `actor_type`. Use `actor_type = ",
               deparse(dots$actor_col), "`.")
  }

  # An MCML partition describes node groups, not separate fitted child
  # networks. Expand it to one global node-level HTNA so between-cluster
  # transitions are reconstructed from the original source.
  if (inherits(data, "mcml")) {
    if (!is.null(actor_type)) {
      .htna_stop("`actor_type` is not used for an `mcml` input; its node clusters ",
                 "are the actor groups.")
    }
    if (!is.null(group)) {
      .htna_stop("An `mcml` expands to one global HTNA; do not also pass `group`.")
    }
    if (isTRUE(disambiguate)) {
      .htna_stop("`disambiguate` must be applied before fitting the `mcml`, not ",
                 "while expanding it.")
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
    .htna_stop("`source_data` is only used when `data` is an `mcml` object.")
  }
  if (!is.null(actor_type) && !is.null(node_groups)) {
    .htna_stop("Pass either `actor_type` (row-level) or `node_groups` ",
               "(node-level), not both.")
  }

  # Nestimate and tna clustering results already encode the outer cohort
  # split. Convert/finalize those results without treating their list-like
  # objects as named lists of actor data frames.
  if (.is_htna_clustering_input(data)) {
    if (!is.null(actor_type)) {
      .htna_stop("`actor_type` is not available on a clustering result. Pass the ",
                 "node partition with `node_groups`, or cluster an existing HTNA ",
                 "object so Nestimate can preserve it.")
    }
    if (!is.null(group)) {
      .htna_stop("A clustering result already defines the outer groups; do not ",
                 "also pass `group`.")
    }
    if (isTRUE(disambiguate)) {
      .htna_stop("`disambiguate` must be applied before clustering/fitting, not ",
                 "while converting a clustering result.")
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
      .htna_stop("All data frames in `data` must share the same column schema.")
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
        .htna_stop("If `node_groups` is a data frame, it must have exactly two ",
                   "columns: the code column (named `", action,
                   "`) and the actor-type column.")
      }
      if (!action %in% names(node_groups)) {
        .htna_stop("`node_groups` data frame must contain the code column `",
                   action, "` (matching `action`).",
                   class = "htna_missing_column")
      }
      type_col   <- setdiff(names(node_groups), action)
      type_vals  <- node_groups[[type_col]]
      code_vals  <- as.character(node_groups[[action]])
      if (any(is.na(type_vals))) {
        .htna_stop("`node_groups` data frame's `", type_col,
                   "` column has missing values; every code must be tagged ",
                   "with an actor type.")
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
      .htna_stop("Code(s) appear in more than one actor list in `node_groups`: ",
                 paste(dups, collapse = ", "),
                 class = "htna_actor_conflict")
    }
    combined      <- data
    code_to_actor <- setNames(rep(names(node_groups), lengths(node_groups)), all_codes)
    raw_codes     <- as.character(combined[[action]])
    actor_vec     <- unname(code_to_actor[raw_codes])
    if (any(is.na(actor_vec))) {
      missing <- unique(raw_codes[is.na(actor_vec)])
      .htna_stop("Codes in `data` not assigned to any actor in `node_groups`: ",
                 paste(missing, collapse = ", "),
                 ". Add them to `node_groups` or filter them out.",
                 class = "htna_unassigned_node")
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
      .htna_stop("`actor_type` column has missing values; every row must be ",
                 "tagged with an actor type.")
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
      .htna_stop("`actor_type` must contain at least two distinct actor types.")
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
    .htna_stop("Column '", session, "' not found in data. ",
               "Available columns: ", paste(names(combined), collapse = ", "), ". ",
               "Either provide a data frame with a '", session, "' column or ",
               "specify an 'actor' parameter to calculate sessions automatically.",
               class = "htna_missing_column")
  }

  # If actor is provided and session column doesn't exist, sessions will be auto-generated
  session_exists <- session %in% names(combined)

  # Order is only required if no actor is provided
  if (is.null(actor) && !order %in% names(combined)) {
    .htna_stop("Column '", order, "' not found in data. ",
               "Available columns: ", paste(names(combined), collapse = ", "), ". ",
               "Either provide a data frame with an '", order, "' column or ",
               "specify an 'actor' parameter for automatic ordering.",
               class = "htna_missing_column")
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
  # A code is shared when it occurs under two or more actors (not only when
  # it occurs under all of them, which is what matters with 3+ actors).
  all_codes <- unlist(codes_by_actor, use.names = FALSE)
  overlap   <- unique(all_codes[duplicated(all_codes)])
  if (length(overlap) > 0L && !disambiguate) {
    .htna_stop("Code(s) appear in more than one actor group: ",
               paste(overlap, collapse = ", "),
               ". Pass `disambiguate = TRUE` to prefix codes with the actor ",
               "label.", class = "htna_actor_conflict")
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
        .htna_stop("Group column `", group, "` contains NA values. Every row must be ",
                   "assigned to a group.")
      }

      group_codes <- split(as.character(combined[[action]]), group_col)
      net <- Map(function(subnet, group_name) {
        actual_codes <- unique(group_codes[[group_name]])
        .inject_htna_partition_filtered(subnet, actor_lookup, actor_levels, actual_codes)
      }, net, names(net))
    } else {
      net <- lapply(net, .inject_htna_partition, actor_lookup, actor_levels)
    }
    net <- net[.cohort_order(names(net), combined[[group]])]
    class(net) <- c("htna_group", "netobject_group", "list")
    attr(net, "actor_levels") <- actor_levels
    return(net)
  }

  .inject_htna_partition(net, actor_lookup, actor_levels)
}

#' Order cohorts by the levels of a factor grouping column
#'
#' The estimation engine returns cohorts in order of first appearance in the
#' data. A factor `group` column declares its own order (e.g. Early, Late),
#' which is then used for the cohorts and for the pair names of every
#' grouped comparison. Character columns keep the engine's order.
#' @keywords internal
#' @noRd
.cohort_order <- function(cohort_names, group_col) {
  if (!is.factor(group_col)) return(cohort_names)
  declared <- levels(group_col)
  c(declared[declared %in% cohort_names],
    setdiff(cohort_names, declared))
}

#' @keywords internal
#' @noRd
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
#' @noRd
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
