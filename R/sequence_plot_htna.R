#' Plot Per-Actor Sequences From an htna Network
#'
#' Extracts each actor's events from the combined wide sequence in
#' `net$data` (using `net$node_groups` as the node-to-actor lookup),
#' compresses each session into the actor's own ordered events, pads to a
#' common width, and renders with [Nestimate::sequence_plot()] grouped by
#' actor. Each session contributes one row per actor that had at least one
#' event in it.
#'
#' @param net An htna network from [build_htna()]. Must have `$data` and
#'   `$node_groups` populated.
#' @param by `"state"` (default) keeps state-level colouring with one
#'   row per (session, actor) extracted from `net$data`; `"group"`
#'   re-colours the original combined session matrix by actor (each cell
#'   = the actor that acted at that time step).
#' @param type Sequence plot layout: `"index"` (default; one panel per
#'   actor, vertically stacked), `"heatmap"` (single carpet with a white
#'   separator at the actor boundary, controllable via `k_line_width`), or
#'   `"distribution"` (one stacked-area panel per actor).
#' @param ... Forwarded to [Nestimate::sequence_plot()].
#'
#' @return Invisibly, the list returned by [Nestimate::sequence_plot()].
#'
#' @seealso [Nestimate::sequence_plot()], [build_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#'
#' sequence_plot_htna(net)                       # index, faceted
#' sequence_plot_htna(net, type = "heatmap")     # single carpet, white gulf
#' sequence_plot_htna(net, type = "distribution")
#' }
#'
#' @export
sequence_plot_htna <- function(net,
                               by   = c("state", "group"),
                               type = c("index", "heatmap", "distribution"),
                               ...) {
  by   <- match.arg(by)
  type <- match.arg(type)

  if (!inherits(net, "htna")) {
    stop("`net` must be an htna network produced by build_htna().",
         call. = FALSE)
  }
  if (is.null(net$data) || !is.data.frame(net$data)) {
    stop("`net$data` is missing or not a data frame; rebuild with ",
         "`build_htna()`.", call. = FALSE)
  }
  if (is.null(net$node_groups) || !nrow(net$node_groups)) {
    stop("`net` has no actor partition (`$node_groups` is empty).",
         call. = FALSE)
  }

  type_map  <- setNames(as.character(net$node_groups$group),
                        as.character(net$node_groups$node))
  actors    <- sort(unique(net$node_groups$group))

  if (by == "group") {
    wide <- net$data
    wide[] <- lapply(wide,
                     function(col) unname(type_map[as.character(col)]))
    user_args <- list(...)
    if (type == "distribution" && is.null(user_args$na)) {
      user_args$na <- FALSE
    }
    return(do.call(Nestimate::sequence_plot,
                   c(list(x = wide, type = type), user_args)))
  }

  state_mat <- as.matrix(net$data)

  pieces <- lapply(actors, function(a) {
    rows <- lapply(seq_len(nrow(state_mat)), function(i) {
      r <- state_mat[i, ]
      r <- r[!is.na(r)]
      r[unname(type_map[r]) == a]
    })
    keep   <- lengths(rows) > 0L
    rows   <- rows[keep]
    if (!length(rows)) return(matrix(NA_character_, 0L, 0L))
    n_cols <- max(lengths(rows))
    out    <- t(vapply(rows, function(v) {
      length(v) <- n_cols
      v
    }, character(n_cols)))
    colnames(out) <- paste0("T", seq_len(n_cols))
    out
  })

  n_cols_all <- max(vapply(pieces, ncol, integer(1L)))
  pieces <- lapply(pieces, function(m) {
    if (ncol(m) >= n_cols_all) return(m)
    extra <- matrix(NA_character_, nrow(m), n_cols_all - ncol(m))
    colnames(extra) <- paste0("T", (ncol(m) + 1L):n_cols_all)
    cbind(m, extra)
  })
  stacked <- do.call(rbind, pieces)
  group   <- rep(actors, vapply(pieces, nrow, integer(1L)))

  user_args <- list(...)

  if (type == "heatmap") {
    hc <- .htna_combined_hclust(pieces)
    if (is.null(user_args$tree))         user_args$tree         <- hc
    if (is.null(user_args$k))            user_args$k            <- length(actors)
    if (is.null(user_args$k_color))      user_args$k_color      <- "white"
    if (is.null(user_args$k_line_width)) user_args$k_line_width <- 12
  } else {
    user_args$group <- group
    if (type == "index" &&
        is.null(user_args$ncol) && is.null(user_args$nrow)) {
      user_args$ncol <- 1L
    }
    if (type == "distribution" && is.null(user_args$na)) {
      user_args$na <- FALSE
    }
  }

  do.call(Nestimate::sequence_plot,
          c(list(x = stacked, type = type), user_args))
}


# Build a combined hclust that has one real per-actor subtree, joined at
# the root. Each per-actor hclust is computed independently on that
# actor's compressed sequences (Euclidean distance over integer-coded
# states, ward.D2 linkage). A single trivial actor (n < 2) becomes a
# synthetic single-leaf "tree" that still composes correctly.
.htna_combined_hclust <- function(pieces) {
  per_actor <- lapply(pieces, .htna_actor_hclust)
  Reduce(.htna_join_hclust, per_actor)
}

.htna_actor_hclust <- function(mat) {
  n <- nrow(mat)
  if (n == 0L) return(NULL)
  if (n == 1L) {
    return(list(merge = matrix(integer(0L), 0L, 2L),
                height = numeric(0L), order = 1L,
                labels = NULL, method = "ward.D2",
                call = sys.call(), dist.method = "euclidean",
                class = "hclust"))
  }
  vals  <- as.vector(mat)
  lvls  <- sort(unique(vals[!is.na(vals)]))
  codes <- matrix(match(mat, lvls), nrow = n)
  codes[is.na(codes)] <- 0L
  hc <- stats::hclust(stats::dist(codes, method = "euclidean"),
                      method = "ward.D2")
  hc
}

.htna_join_hclust <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  n_a  <- length(a$order); n_b <- length(b$order)
  m_a  <- nrow(a$merge);   m_b <- nrow(b$merge)

  b_merge <- b$merge
  b_merge[b_merge < 0] <- b_merge[b_merge < 0] - n_a
  b_merge[b_merge > 0] <- b_merge[b_merge > 0] + m_a

  if (n_a == 1L) a_root_id <- -1L else a_root_id <- m_a
  if (n_b == 1L) b_root_id <- -(n_a + 1L) else b_root_id <- m_a + m_b

  joined_merge  <- rbind(a$merge, b_merge,
                         matrix(c(a_root_id, b_root_id), 1L, 2L))
  max_h         <- max(c(a$height, b$height), 0)
  joined_height <- c(a$height, b$height, max_h * 1.5 + 1)
  joined_order  <- c(a$order, b$order + n_a)

  structure(list(merge = joined_merge, height = joined_height,
                 order = joined_order, labels = NULL,
                 method = "ward.D2", call = sys.call(),
                 dist.method = "euclidean"),
            class = "hclust")
}
