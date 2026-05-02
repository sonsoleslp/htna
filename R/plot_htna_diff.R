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
  if (!identical(sort(unique(x$node_groups$group)),
                 sort(unique(y$node_groups$group)))) {
    stop("x and y must share the same actor partition.", call. = FALSE)
  }

  diff_mat <- cograph::to_matrix(x) - cograph::to_matrix(y)
  rownames(diff_mat) <- colnames(diff_mat) <- x$nodes$label

  ng_styling <- .htna_node_styling(x, group_colors)

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
