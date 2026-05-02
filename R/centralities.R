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
#' Computes a fixed panel of centrality measures via [cograph] and returns
#' them as a single tidy data frame: one row per node, one column per
#' measure (plus a `node` column, and a `group` column if the input is an
#' `htna_group`).
#'
#' The measures are: `OutStrength`, `InStrength`, `ClosenessIn`,
#' `ClosenessOut`, `Closeness`, `Betweenness`, `BetweennessRSP`,
#' `Diffusion`, `Clustering`. Each is computed by the corresponding
#' [cograph] function — see Details for the mapping.
#'
#' @details
#' Mapping from measure name to cograph implementation:
#' * `OutStrength`     → [cograph::centrality_outstrength()]
#' * `InStrength`      → [cograph::centrality_instrength()]
#' * `ClosenessIn`     → [cograph::centrality_incloseness()]
#' * `ClosenessOut`    → [cograph::centrality_outcloseness()]
#' * `Closeness`       → [cograph::centrality_closeness()]
#' * `Betweenness`     → [cograph::centrality_betweenness()]
#' * `BetweennessRSP`  → [cograph::centrality_current_flow_betweenness()]
#' * `Diffusion`       → [cograph::centrality_diffusion()]
#' * `Clustering`      → [cograph::centrality_transitivity()]
#'
#' @param x An htna network from [build_htna()] or an `htna_group` (list of
#'   htna networks).
#' @param measures Character vector of measure names to compute. Defaults
#'   to all nine.
#' @param ... Forwarded to the underlying centrality functions.
#'
#' @return A data frame with one row per node and columns `node`, the
#'   requested measures, and (for `htna_group` input) `group`.
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' centralities(net)
#'
#' grp <- build_htna(list(Human = human_long, AI = ai_long),
#'                   group = "cluster")
#' centralities(grp)
#' }
#'
#' @export
centralities <- function(x,
                         measures = c("OutStrength", "InStrength",
                                      "ClosenessIn", "ClosenessOut",
                                      "Closeness", "Betweenness",
                                      "BetweennessRSP", "Diffusion",
                                      "Clustering"),
                         ...) {
  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    if (length(x) == 0L) stop("Empty htna_group.", call. = FALSE)
    nms  <- names(x) %||% as.character(seq_along(x))
    rows <- lapply(seq_along(x), function(i) {
      df <- centralities(x[[i]], measures = measures, ...)
      cbind(group = nms[i], df, stringsAsFactors = FALSE)
    })
    return(do.call(rbind, rows))
  }
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network from build_htna() (or htna_group).",
         call. = FALSE)
  }

  fns <- list(
    OutStrength    = cograph::centrality_outstrength,
    InStrength     = cograph::centrality_instrength,
    ClosenessIn    = cograph::centrality_incloseness,
    ClosenessOut   = cograph::centrality_outcloseness,
    Closeness      = cograph::centrality_closeness,
    Betweenness    = cograph::centrality_betweenness,
    BetweennessRSP = cograph::centrality_current_flow_betweenness,
    Diffusion      = cograph::centrality_diffusion,
    Clustering     = cograph::centrality_transitivity
  )
  unknown <- setdiff(measures, names(fns))
  if (length(unknown) > 0L) {
    stop("Unknown measure(s): ", paste(unknown, collapse = ", "),
         ". Available: ", paste(names(fns), collapse = ", "), call. = FALSE)
  }

  vals  <- lapply(measures, function(m) as.numeric(fns[[m]](x, ...)))
  nodes <- x$nodes$label %||% rownames(.weights_of(x))
  out   <- data.frame(node = nodes, stringsAsFactors = FALSE)
  if (!is.null(x$node_groups)) {
    actor_lookup <- setNames(as.character(x$node_groups$group),
                             as.character(x$node_groups$node))
    out$actor <- unname(actor_lookup[as.character(nodes)])
  }
  for (i in seq_along(measures)) out[[measures[i]]] <- vals[[i]]
  class(out) <- c("htna_centralities", class(out))
  out
}

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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`plot_centralities()` needs the 'ggplot2' package. ",
         "Install it with `install.packages(\"ggplot2\")`.", call. = FALSE)
  }
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
