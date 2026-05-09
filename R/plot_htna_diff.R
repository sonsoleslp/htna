#' Plot the Difference Between Two htna Networks
#'
#' Renders the elementwise edge-weight difference `x - y` as a heterogeneous
#' transition network. Accepts:
#'
#' * Two htna networks (`x`, `y`): plots `x - y` via [cograph::splot()] with
#'   positive/negative edge coloring.
#' * A `net_permutation` result: routes through
#'   [cograph::splot.net_permutation()] so the significance overlay
#'   (significant pos/neg edges, dashed non-significant edges, stars) is
#'   drawn upstream.
#' * A `net_permutation_group`: iterates over all pairwise comparisons and
#'   plots each (no `par(mfrow=...)` management - user controls layout).
#'
#' In all cases the layout, node colours and donut match [plot_htna()].
#'
#' @param x One of: an htna network, a `net_permutation` result, or a
#'   `net_permutation_group`.
#' @param y htna network (only used when `x` is also an htna network).
#' @param pos_color Edge colour for positive differences. Default
#'   `"#009900"`.
#' @param neg_color Edge colour for negative differences. Default
#'   `"#C62828"`.
#' @param group_colors Character vector of colours, one per actor group.
#'   Defaults to the built-in [htna_palette].
#' @param ... Forwarded to the underlying splot dispatcher
#'   ([cograph::splot()] or [cograph::splot.net_permutation()]).
#'
#' @return The value of the underlying splot call (invisibly).
#' @seealso [plot_htna()], [build_htna()], [permutation()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net1 <- build_htna(list(Human = human_long,        AI = ai_long))
#' net2 <- build_htna(list(Human = human_long[1:50,], AI = ai_long))
#' plot_htna_diff(net1, net2)
#'
#' perm <- permutation(net1, net2, iter = 200)
#' plot_htna_diff(perm)
#' plot_htna_diff(perm, show_nonsig = TRUE)
#' }
#'
#' @export
plot_htna_diff <- function(x, y = NULL,
                           pos_color    = "#009900",
                           neg_color    = "#C62828",
                           group_colors = htna_palette,
                           ...) {
  if (inherits(x, "net_permutation_group")) {
    if (length(x) == 0L) stop("Empty net_permutation_group.", call. = FALSE)
    for (nm in names(x)) {
      .plot_htna_diff_perm(x[[nm]], pos_color, neg_color, group_colors,
                           title = nm, ...)
    }
    return(invisible(x))
  }
  if (inherits(x, "net_permutation")) {
    return(invisible(.plot_htna_diff_perm(
      x, pos_color, neg_color, group_colors, ...)))
  }

  if (!inherits(x, "htna") || !inherits(y, "htna")) {
    stop("Both `x` and `y` must be htna networks, or `x` a permutation result.",
         call. = FALSE)
  }
  # Both networks must use the same global actor partition (same actor
  # types in the same canonical ordering). Their node sets are allowed
  # to differ — partial coverage is aligned to the union below.
  if (!identical(x$actor_levels, y$actor_levels)) {
    stop("x and y must share the same actor partition (`actor_levels`).",
         call. = FALSE)
  }

  aligned  <- .align_diff_inputs(x, y)
  diff_mat <- aligned$diff
  ng_styling <- .htna_node_styling(aligned$net, group_colors)

  result <- do.call(cograph::splot, c(
    list(x = diff_mat,
         layout              = ng_styling$layout,
         node_fill           = ng_styling$node_colors,
         node_shape          = ng_styling$node_shapes,
         donut_color         = ng_styling$dark,
         edge_positive_color = pos_color,
         edge_negative_color = neg_color,
         node_size           = 12,
         curvature           = 0.4,
         layout_margin       = 0.15,
         rescale             = FALSE,
         layout_scale        = 1,
         edge_labels         = TRUE,
         edge_label_size     = 0.55,
         edge_label_position = 0.7,
         edge_label_leading_zero = FALSE,
         label_size          = 0.8,
         arrow_size          = 0.61,
         minimum             = 0.01,
         legend              = FALSE),
    list(...)
  ))
  if (ng_styling$n_groups >= 2L) {
    .htna_legend(ng_styling$groups, ng_styling$colors)
  }
  invisible(result)
}

# Align two htna networks whose node sets may differ (e.g. two cohorts
# of an htna_group built from data where some codes appear in one
# cohort but not the other). Returns:
#   $diff: x - y on the union node set, with zero-padding for missing
#          rows/cols on either side.
#   $net : a synthetic htna network on the union nodes that
#          `.htna_node_styling()` can consume for layout/colours/shapes.
.align_diff_inputs <- function(x, y) {
  rn_x <- rownames(x$weights); rn_y <- rownames(y$weights)
  if (identical(rn_x, rn_y) && identical(colnames(x$weights),
                                          colnames(y$weights))) {
    diff_mat <- cograph::to_matrix(x) - cograph::to_matrix(y)
    rownames(diff_mat) <- colnames(diff_mat) <- x$nodes$label
    return(list(diff = diff_mat, net = x))
  }
  # Union of nodes. Use both networks' node_groups to derive the actor
  # tag for every union node — they must agree on shared nodes.
  ng_combined <- rbind(x$node_groups, y$node_groups)
  ng_combined <- ng_combined[!duplicated(as.character(ng_combined$node)), ,
                             drop = FALSE]
  # Order union nodes by canonical actor_levels then alphabetically
  # within each actor, so the layout puts each actor's nodes together.
  ord <- order(match(as.character(ng_combined$group), x$actor_levels),
               as.character(ng_combined$node))
  ng_combined <- ng_combined[ord, , drop = FALSE]
  rownames(ng_combined) <- NULL
  union_nodes <- as.character(ng_combined$node)

  pad <- function(net) {
    W <- cograph::to_matrix(net)
    rownames(W) <- colnames(W) <- net$nodes$label
    out <- matrix(0, length(union_nodes), length(union_nodes),
                  dimnames = list(union_nodes, union_nodes))
    rn <- rownames(W); cn <- colnames(W)
    out[rn, cn] <- W
    out
  }
  W_x <- pad(x); W_y <- pad(y)
  diff_mat <- W_x - W_y

  # Synthetic htna network for the styling helper. We only need
  # $weights, $nodes, $node_groups, $actor_levels.
  synthetic <- list(
    weights      = matrix(0, length(union_nodes), length(union_nodes),
                          dimnames = list(union_nodes, union_nodes)),
    nodes        = data.frame(label = union_nodes,
                              groups = ng_combined$group,
                              stringsAsFactors = FALSE),
    node_groups  = ng_combined,
    actor_levels = x$actor_levels
  )
  class(synthetic) <- c("htna", "netobject", "cograph_network")
  list(diff = diff_mat, net = synthetic)
}

.plot_htna_diff_perm <- function(perm, pos_color, neg_color, group_colors, title = "",
                                 ...) {
  net <- perm$x
  if (!inherits(net, "htna") || is.null(net$node_groups)) {
    stop("perm$x is not an htna network. Build inputs with build_htna() ",
         "before calling permutation().", call. = FALSE)
  }

  ng_styling <- .htna_node_styling(net, group_colors)

  result <- do.call(cograph::splot.net_permutation, c(
    list(x = perm,
         layout              = ng_styling$layout,
         node_fill           = ng_styling$node_colors,
         node_shape          = ng_styling$node_shapes,
         node_size           = 12,
         donut_color         = ng_styling$dark,
         edge_positive_color = pos_color,
         edge_negative_color = neg_color,
         curvature           = 0.4,
         layout_margin       = 0.15,
         rescale             = FALSE,
         layout_scale        = 1,
         edge_labels         = TRUE,
         edge_label_size     = 0.55,
         edge_label_position = 0.7,
         edge_label_leading_zero = FALSE,
         label_size          = 0.8,
         arrow_size          = 0.61,
         minimum             = 0.01,
         title               = title,
         legend              = FALSE),
    list(...)
  ))
  if (ng_styling$n_groups >= 2L) {
    .htna_legend(ng_styling$groups, ng_styling$colors)
  }
  invisible(result)
}
