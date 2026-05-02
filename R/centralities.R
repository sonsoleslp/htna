#' Default htna state colour palette
#'
#' Okabe-Ito palette, matching the per-state colours used by
#' [sequence_plot_htna()] (and `Nestimate::sequence_plot()` internally).
#' Used by [plot_centralities()] when `by = "state"`.
#'
#' @keywords internal
htna_state_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"
)

#' Compute Centrality Measures for an htna Network
#'
#' Computes a fixed panel of centrality measures from the network's
#' weight matrix and returns them as a tidy data frame: one row per
#' node, one column per measure (plus a `node` column, an `actor`
#' column if the htna partition is set, and a `group` column when the
#' input is an `htna_group`).
#'
#' The measures match `tna::centralities()` exactly: `OutStrength`,
#' `InStrength`, `ClosenessIn`, `ClosenessOut`, `Closeness`,
#' `Betweenness`, `BetweennessRSP`, `Diffusion`, `Clustering`. Igraph
#' is used for the strength, closeness, and betweenness measures;
#' `BetweennessRSP`, `Diffusion`, and `Clustering` use the same
#' closed-form expressions tna uses (randomized shortest paths
#' betweenness, geometric-series diffusion, weighted clustering
#' coefficient).
#'
#' @param x An htna network from [build_htna()] or an `htna_group` (list of
#'   htna networks).
#' @param measures Character vector of measure names to compute. Defaults
#'   to all nine.
#' @param loops If `FALSE` (default), self-loops in the weight matrix
#'   are zeroed before computing centralities.
#' @param normalize If `TRUE`, each measure is rescaled to `[0, 1]`.
#' @param invert If `TRUE` (default), igraph's distance-based measures
#'   (closeness, betweenness) interpret edge weights as costs and use
#'   `1 / weight` so larger transition probabilities mean shorter
#'   distances.
#' @param ... Reserved for future extensions.
#'
#' @return A data frame with one row per node and columns `node`,
#'   `actor`, the requested measures, and (for grouped input) `group`.
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' centralities(net)
#' }
#'
#' @export
centralities <- function(x,
                         measures  = c("OutStrength", "InStrength",
                                       "ClosenessIn", "ClosenessOut",
                                       "Closeness", "Betweenness",
                                       "BetweennessRSP", "Diffusion",
                                       "Clustering"),
                         loops     = FALSE,
                         normalize = FALSE,
                         invert    = TRUE,
                         ...) {
  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    if (length(x) == 0L) stop("Empty htna_group.", call. = FALSE)
    nms  <- names(x) %||% as.character(seq_along(x))
    rows <- lapply(seq_along(x), function(i) {
      df <- centralities(x[[i]], measures = measures,
                         loops = loops, normalize = normalize,
                         invert = invert, ...)
      cbind(group = nms[i], df, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network from build_htna() (or htna_group).",
         call. = FALSE)
  }

  unknown <- setdiff(measures, names(.htna_centrality_funs))
  if (length(unknown) > 0L) {
    stop("Unknown measure(s): ", paste(unknown, collapse = ", "),
         ". Available: ", paste(names(.htna_centrality_funs),
                                collapse = ", "), call. = FALSE)
  }

  if (!requireNamespace("igraph", quietly = TRUE)) {  # nocov start
    stop("`centralities()` needs the 'igraph' package. ",
         "Install it with `install.packages(\"igraph\")`.", call. = FALSE)
  }                                                    # nocov end

  W <- .weights_of(x)
  if (!isTRUE(loops)) diag(W) <- 0

  g <- igraph::graph_from_adjacency_matrix(W, mode = "directed",
                                           weighted = TRUE, diag = TRUE)
  w <- if (isTRUE(invert)) 1 / igraph::E(g)$weight else igraph::E(g)$weight

  vals <- lapply(measures, function(m) {
    as.numeric(.htna_centrality_funs[[m]](g = g, x = W, w = w))
  })
  names(vals) <- measures

  if (isTRUE(normalize)) {
    vals <- lapply(vals, function(v) {
      r <- range(v, na.rm = TRUE)
      if (diff(r) == 0) rep(0, length(v)) else (v - r[1]) / diff(r)
    })
  }

  nodes <- x$nodes$label %||% rownames(W)
  out   <- data.frame(node = nodes, stringsAsFactors = FALSE)
  if (!is.null(x$node_groups)) {
    actor_lookup <- setNames(as.character(x$node_groups$group),
                             as.character(x$node_groups$node))
    out$actor <- unname(actor_lookup[as.character(nodes)])
  }
  for (m in measures) out[[m]] <- vals[[m]]
  class(out) <- c("htna_centralities", class(out))
  out
}

# ---- Centrality engine (mirrors tna::centralities_) ------------------------

#' @keywords internal
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
.diffusion_centrality <- function(mat) {
  n <- ncol(mat)
  s <- 0; p <- diag(1, n, n)
  for (i in seq_len(n)) { p <- p %*% mat; s <- s + p }
  .rowSums(s, n, n)
}

#' @keywords internal
.weighted_clustering <- function(mat) {
  diag(mat) <- 0
  n <- ncol(mat)
  num <- diag(mat %*% mat %*% mat)
  den <- .colSums(mat, n, n) ^ 2 - .colSums(mat ^ 2, n, n)
  num / den
}

#' @keywords internal
.htna_centrality_funs <- list(
  OutStrength    = function(g, ...) igraph::strength(g, mode = "out"),
  InStrength     = function(g, ...) igraph::strength(g, mode = "in"),
  ClosenessIn    = function(g, w, ...)
                     igraph::closeness(g, mode = "in",  weights = w),
  ClosenessOut   = function(g, w, ...)
                     igraph::closeness(g, mode = "out", weights = w),
  Closeness      = function(g, w, ...)
                     igraph::closeness(g, mode = "all", weights = w),
  Betweenness    = function(g, w, ...) igraph::betweenness(g, weights = w),
  BetweennessRSP = function(x, ...) .rsp_betweenness(x),
  Diffusion      = function(x, ...) .diffusion_centrality(x),
  Clustering     = function(x, ...) .weighted_clustering(x + t(x))
)

#' Plot Centrality Measures
#'
#' Faceted bar plot of node-level centralities, one panel per measure.
#' Mirrors the look of [tna::plot.tna_centralities()].
#'
#' Accepts an htna network, an `htna_group`, or a data frame produced by
#' [centralities()]. For groups, bars are coloured by group within each
#' panel.
#'
#' @param x An htna network, `htna_group`, or `htna_centralities` data
#'   frame from [centralities()].
#' @param measures Centralities to plot. Default: all nine.
#' @param by `"state"` (default) gives each node its own colour;
#'   `"group"` colours by actor group (Human, AI, …) using
#'   [htna_palette].
#' @param reorder If `TRUE`, sort nodes by value within each measure.
#' @param ncol Number of facet columns. Default `3`.
#' @param scales Facet scaling: `"free_x"` (default) or `"fixed"`.
#' @param colors Optional fill colours, recycled per group/node.
#' @param labels If `TRUE` (default), draw the value next to each bar.
#' @param ... Forwarded to [centralities()] when computing on the fly.
#'
#' @return A ggplot object.
#' @seealso [centralities()].
#' @examples
#' \dontrun{
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' plot_centralities(net)
#'
#' grp <- build_htna(list(Human = human_long, AI = ai_long),
#'                   group = "cluster")
#' plot_centralities(grp)
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
    stop("`plot_centralities()` needs the 'ggplot2' package. ",
         "Install it with `install.packages(\"ggplot2\")`.", call. = FALSE)
  }                                                    # nocov end
  by     <- match.arg(by)
  scales <- match.arg(scales)

  if (!inherits(x, "htna_centralities") && !is.data.frame(x)) {
    df <- centralities(x, measures = measures, ...)
  } else {
    df <- x
    have <- intersect(measures, names(df))
    if (length(have) == 0L) {
      stop("None of `measures` found in the data frame.", call. = FALSE)
    }
    keep <- c("node",
              if ("actor" %in% names(df)) "actor",
              if ("group" %in% names(df)) "group",
              have)
    df <- df[, keep, drop = FALSE]
    measures <- have
  }

  if (by == "group" && !"actor" %in% names(df)) {
    stop("`by = \"group\"` needs an `actor` column. Pass an htna network ",
         "(or a data frame from centralities()) so the actor partition is ",
         "available.", call. = FALSE)
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
  fill_levels <- unique(long[[fill_var]])
  if (is.null(colors)) {
    colors <- if (by == "group") {
      htna_palette[seq_along(fill_levels)]
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
