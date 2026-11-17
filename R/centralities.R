#' Default htna state colour palette
#'
#' Okabe-Ito palette, matching the per-state colours used by
#' [sequence_plot_htna()] (and `Nestimate::sequence_plot()` internally).
#' Used by [plot_centralities()] when `by = "state"`.
#'
#' @noRd
htna_state_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"
)

#' Compute node centralities of an htna network
#'
#' @description
#' `centralities_htna()` computes the node centralities of an htna network,
#' with the actor type of every code kept. Centrality measures rank the
#' codes of a transition network by their position in it: how much
#' transition probability a code sends and receives (out- and in-strength),
#' how close it is to the other codes (closeness), how often it lies on the
#' shortest paths between other codes (betweenness), and how far its
#' influence spreads (diffusion). Because the actor type is kept, the table
#' shows whether the human or the AI codes hold the central positions, for
#' example which actor's codes broker the hand-overs.
#'
#' `centralities_htna()` computes nine measures: `OutStrength`,
#' `InStrength`, `ClosenessIn`, `ClosenessOut`, `Closeness`, `Betweenness`,
#' `BetweennessRSP` (randomized shortest-path betweenness), `Diffusion` and
#' `Clustering` (weighted clustering coefficient). Path-based measures treat
#' a larger transition weight as a shorter distance (the weights are
#' inverted); strength, diffusion and clustering use the weights as they
#' are. For a grouped network the measures are computed per cohort. It
#' returns a data frame with one row per code (per cohort and code for a
#' grouped network) and one column per measure.
#'
#' Draw the result with [plot_centralities()]. To check whether the ranking
#' of the codes would survive dropping sessions use
#' [centrality_stability_htna()].
#'
#' @param x An htna network from [build_htna()] or an `htna_group`.
#' @param measures Names of the measures to compute. Default: all nine.
#' @param ... Further arguments for the centrality computations.
#'
#' @return A data frame of class `c("htna_centralities", "data.frame")`,
#'   one row per node, with columns `node`, `actor` (its actor type) and one
#'   numeric column per measure. For an `htna_group`, the rows of all cohorts
#'   are stacked (one row per cohort and node) with a leading `group` column.
#'
#' @section Conditions:
#' `htna_bad_input` when `x` is not an htna network or a measure name is
#' unknown; `htna_empty_input` for a grouped network without cohorts;
#' `htna_unnamed_nodes` when the nodes of the network have no names.
#'
#' @seealso [plot_centralities()], [centrality_stability_htna()],
#'   [edge_betweenness_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' centralities_htna(net)
#' centralities_htna(net, measures = c("InStrength", "Betweenness"))
#'
#' @export
centralities_htna <- function(x,
                         measures = c("OutStrength", "InStrength",
                                      "ClosenessIn", "ClosenessOut",
                                      "Closeness", "Betweenness",
                                      "BetweennessRSP", "Diffusion",
                                      "Clustering"),
                         ...) {
  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    if (length(x) == 0L) .htna_stop("Empty htna_group.",
                                    class = "htna_empty_input")
    nms  <- names(x) %||% as.character(seq_along(x))
    rows <- lapply(seq_along(x), function(i) {
      df <- centralities_htna(x[[i]], measures = measures, ...)
      cbind(group = nms[i], df, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }
  if (!inherits(x, "htna")) {
    .htna_stop("`x` must be an htna network from build_htna() (or htna_group).")
  }

  unknown <- setdiff(measures, names(.htna_centrality_specs))
  if (length(unknown) > 0L) {
    .htna_stop("Unknown measure(s): ", paste(unknown, collapse = ", "),
               ". Available: ", paste(names(.htna_centrality_specs),
                                      collapse = ", "))
  }

  W <- .weights_of(x)
  diag(W) <- 0   # match tna's loops = FALSE default for matrix-fed measures

  # cograph's get_labels() requires a `label`/`labels` column and errors without
  # one, so a network whose labels were dropped has to be repaired BEFORE any
  # measure is delegated -- not just when the result is assembled. Under
  # cograph >= 2.5 the unrepaired network makes centrality() build its result
  # from a zero-length node vector, which then dies inside data.frame() with
  # "arguments imply differing number of rows: 0, n".
  nodes <- x$nodes$label %||% rownames(W)
  if (length(nodes) != nrow(W)) {
    .htna_stop(sprintf(paste("cannot name the %d nodes of this htna network:",
                             "`nodes$label` is absent and the weight matrix",
                             "has no rownames"), nrow(W)),
               class = "htna_unnamed_nodes")
  }
  x$nodes$label <- nodes

  vals <- lapply(measures, function(m) {
    spec <- .htna_centrality_specs[[m]]
    if (isTRUE(spec$matrix)) {
      return(as.numeric(spec$fn(W)))
    }
    extra <- if (isTRUE(spec$path_based)) list(invert_weights = TRUE)
             else list()
    
    extra$tna_network <- TRUE
    as.numeric(do.call(spec$fn, c(list(x), extra, list(...))))
  })
  names(vals) <- measures

  out   <- data.frame(node = nodes, stringsAsFactors = FALSE)
  if (!is.null(x$node_groups)) {
    actor_lookup <- setNames(as.character(x$node_groups$group),
                             as.character(x$node_groups$node))
    out$actor <- unname(actor_lookup[as.character(nodes)])
  }
  out[measures] <- vals
  class(out) <- c("htna_centralities", class(out))
  out
}

#' @keywords internal
#' @noRd
# Onnela's weighted clustering coefficient on the symmetrised matrix,
# ported from tna::wcc. cograph::centrality_transitivity has a formal
# `transitivity_type = "local"`, which appears as explicit and so
# blocks the `tna_network = TRUE` Onnela override; we compute it
# ourselves to guarantee equivalence with tna.
.weighted_clustering <- function(mat) {
  diag(mat) <- 0
  M <- mat + t(mat)
  n <- ncol(M)
  num <- diag(M %*% M %*% M)
  den <- .colSums(M, n, n) ^ 2 - .colSums(M ^ 2, n, n)
  num / den
}

#' @keywords internal
#' @noRd
# Randomized shortest-paths betweenness, ported from tna::rsp_bet so the
# value matches tna's `BetweennessRSP` (cograph's current_flow_betweenness
# uses a different formula).
.rsp_betweenness <- function(mat, beta = 0.01) {
  n <- ncol(mat)
  D <- .rowSums(mat, m = n, n = n)
  if (any(D == 0)) return(rep(NA_real_, n))
  P_ref <- diag(1 / D, n, n) %*% mat
  C <- mat ^ -1
  C[is.infinite(C)] <- 0
  W <- P_ref * exp(-beta * C)
  Z <- solve(diag(1, n, n) - W)
  Z_recip <- Z ^ -1
  Z_recip[is.infinite(Z_recip)] <- 0
  Z_recip_diag <- diag(diag(Z_recip), n, n)
  out <- diag(tcrossprod(Z, Z_recip - n * Z_recip_diag) %*% Z)
  out <- round(out)
  out - min(out) + 1
}

#' @keywords internal
#' @noRd
.htna_centrality_specs <- list(
  OutStrength    = list(fn = cograph::centrality_outstrength,              path_based = FALSE),
  InStrength     = list(fn = cograph::centrality_instrength,               path_based = FALSE),
  ClosenessIn    = list(fn = cograph::centrality_incloseness,              path_based = TRUE),
  ClosenessOut   = list(fn = cograph::centrality_outcloseness,             path_based = TRUE),
  Closeness      = list(fn = cograph::centrality_closeness,                path_based = TRUE),
  Betweenness    = list(fn = cograph::centrality_betweenness,              path_based = TRUE),
  # BetweennessRSP follows tna's RSP-betweenness (cograph's
  # current_flow_betweenness is a different formula).
  BetweennessRSP = list(fn = .rsp_betweenness, matrix = TRUE),
  Diffusion      = list(fn = cograph::centrality_diffusion,                path_based = FALSE),
  # Clustering uses Onnela's wcc on (M + Mᵀ); cograph routes its own
  # local transitivity here when called via centrality_transitivity().
  Clustering     = list(fn = .weighted_clustering, matrix = TRUE)
)

#' Plot node centralities of an htna network
#'
#' @description
#' `plot_centralities()` plots the node centralities of an htna network: the
#' value of each centrality measure for every code, one panel per measure,
#' so the codes that hold the central positions of the interaction can be
#' read at a glance and compared across measures. Colouring the bars by
#' actor type shows whether the central codes belong to one actor or are
#' shared between them.
#'
#' `plot_centralities()` draws horizontal bars, one per code, in one facet per
#' measure, sorted by value within each facet by default and labelled with
#' the value. It accepts an htna network, a grouped network or a table from
#' [centralities_htna()]. For a grouped network each panel shows one
#' line-and-point profile per cohort over the same codes, so differences
#' between cohorts read along one axis; `by` is then not used. The chart is
#' a 'ggplot2' chart and needs that package; it is returned as a `ggplot`
#' object.
#'
#' The values themselves are returned by [centralities_htna()].
#'
#' @param x An htna network, an `htna_group`, or a table from
#'   [centralities_htna()].
#' @param measures Measures to plot. Default: all nine.
#' @param by `"state"` (default) gives every code its own colour; `"group"`
#'   colours the bars by actor type.
#' @param reorder Sort the codes by value within each measure. Default
#'   `TRUE`.
#' @param ncol Number of facet columns. Default `3`.
#' @param scales Facet scales: `"free_x"` (default) or `"fixed"`.
#' @param colors Fill colours, recycled over codes or actor types. Default
#'   `NULL` (Okabe-Ito for codes, [htna_palette] for actor types).
#' @param labels Print the value next to each bar. Default `TRUE`.
#' @param ... Further arguments for [centralities_htna()] when `x` is a
#'   network.
#'
#' @return A `ggplot` object, drawn when printed.
#'
#' @section Conditions:
#' `htna_missing_package` when 'ggplot2' is not installed; `htna_bad_input`
#' when none of `measures` is in the table or `by = "group"` is asked for a
#' table without an `actor` column.
#'
#' @seealso [centralities_htna()], [centrality_stability_htna()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(human_ai)
#'   net <- build_htna(human_ai, actor_type = "actor_type")
#'   plot_centralities(net, measures = c("InStrength", "OutStrength",
#'                                       "Betweenness"), by = "group")
#'
#'   grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#'   plot_centralities(grp, measures = c("InStrength", "Betweenness"))
#' }
#' @export
plot_centralities <- function(x,
                              measures = c("OutStrength", "InStrength",
                                           "ClosenessIn", "ClosenessOut",
                                           "Closeness", "Betweenness",
                                           "BetweennessRSP", "Diffusion",
                                           "Clustering"),
                              by      = c("state", "group"),
                              reorder = TRUE,
                              ncol    = 3,
                              scales  = c("free_x", "fixed"),
                              colors  = NULL,
                              labels  = TRUE,
                              ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {  # nocov start
    .htna_stop("`plot_centralities()` needs the 'ggplot2' package. ",
               "Install it with `install.packages(\"ggplot2\")`.",
               class = "htna_missing_package")
  }                                                    # nocov end
  by     <- match.arg(by)
  scales <- match.arg(scales)

  # Capture the canonical actor order from the network (if available)
  # so colours follow the same Human/AI/... ordering as plot_htna()
  # rather than the alphabetical node-encounter order in the
  # centralities data frame.
  actor_levels <- NULL
  if (inherits(x, "htna")) {
    actor_levels <- x$actor_levels
  } else if (inherits(x, "htna_group") && length(x) > 0L) {
    actor_levels <- x[[1L]]$actor_levels
  }

  if (!inherits(x, "htna_centralities") && !is.data.frame(x)) {
    df <- centralities_htna(x, measures = measures, ...)
  } else {
    df <- x
    have <- intersect(measures, names(df))
    if (length(have) == 0L) {
      .htna_stop("None of `measures` found in the data frame.")
    }
    keep <- c("node",
              if ("actor" %in% names(df)) "actor",
              if ("group" %in% names(df)) "group",
              have)
    df <- df[, keep, drop = FALSE]
    measures <- have
  }

  if (by == "group" && !"actor" %in% names(df)) {
    .htna_stop("`by = \"group\"` needs an `actor` column. Pass an htna network ",
               "(or a data frame from centralities_htna()) so the actor partition is ",
               "available.")
  }

  has_cluster <- "group" %in% names(df)
  long <- do.call(rbind, lapply(measures, function(m) {
    data.frame(
      node    = df$node,
      actor   = if ("actor" %in% names(df)) df$actor else NA_character_,
      cluster = if (has_cluster) df$group else NA_character_,
      measure = m,
      value   = df[[m]],
      stringsAsFactors = FALSE
    )
  }))
  long$measure <- factor(long$measure, levels = measures)

  scales_arg <- if (scales == "free_x") "free" else "free_y"

  if (has_cluster) {
    cluster_levels <- unique(long$cluster)
    long$node <- factor(long$node)
    p <- ggplot2::ggplot(long,
           ggplot2::aes(x = .data$value, y = .data$node,
                        color = .data$cluster, fill = .data$cluster,
                        group = .data$cluster)) +
      ggplot2::facet_wrap(~ measure, ncol = ncol, scales = scales_arg) +
      ggplot2::geom_path() +
      ggplot2::geom_point(size = 2, shape = 21, stroke = NA) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        strip.text      = ggplot2::element_text(face = "bold", size = 12),
        axis.text.y     = ggplot2::element_text(size = 8, vjust = 0.2),
        panel.spacing   = ggplot2::unit(1, "lines")
      ) +
      ggplot2::xlab("Centrality") + ggplot2::ylab("") +
      ggplot2::labs(color = "", fill = "")
    if (!is.null(colors)) {
      colors <- rep(colors, length.out = length(cluster_levels))
      p <- p +
        ggplot2::scale_color_manual(
          values = stats::setNames(colors, cluster_levels)) +
        ggplot2::scale_fill_manual(
          values = stats::setNames(colors, cluster_levels))
    } else {
      p <- p +
        ggplot2::scale_color_brewer(palette = "Set2") +
        ggplot2::scale_fill_brewer(palette = "Set2")
    }
    return(p)
  }

  if (isTRUE(reorder)) {
    long$node_within <- factor(paste(long$node, long$measure, sep = "___"))
    long$node_within <- stats::reorder(long$node_within, long$value,
                                       FUN = identity)
  } else {
    long$node_within <- factor(long$node)
  }

  fill_var <- if (by == "group") "actor" else "node"
  if (by == "group" && !is.null(actor_levels)) {
    # Use the network's canonical actor order so palette[1] always
    # binds to the first actor (and matches plot_htna's colour map).
    fill_levels <- actor_levels
    long$actor  <- factor(long$actor, levels = fill_levels)
  } else {
    fill_levels <- unique(long[[fill_var]])
  }
  if (is.null(colors)) {
    colors <- if (by == "group") {
      unname(.htna_actor_colors(length(fill_levels)))
    } else {
      rep_len(htna_state_palette, length(fill_levels))
    }
  } else {
    colors <- rep(colors, length.out = length(fill_levels))
  }

  show_legend <- by == "group"

  p <- ggplot2::ggplot(long,
         ggplot2::aes(x = .data$node_within, y = .data$value,
                      fill = .data[[fill_var]])) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = stats::setNames(colors, fill_levels)) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_x_discrete(labels = function(y) gsub("___.+$", "", y)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position    = if (show_legend) "bottom" else "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y        = ggplot2::element_text(size = 8, vjust = 0.2),
      panel.spacing      = ggplot2::unit(2, "lines")
    ) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::labs(fill = if (by == "group") "Actor" else NULL) +
    ggplot2::facet_wrap(~ measure, ncol = ncol, scales = scales_arg)

  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(" ", round(.data$value, 2), " ")),
      hjust = -0.05, size = 3)
  }
  p
}
