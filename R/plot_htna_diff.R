#' Plot the difference between two htna networks
#'
#' @description
#' `plot_htna_diff()` plots the difference between two htna networks: for
#' every transition, how much more (or less) likely it is in one network
#' than in another, for example in the late sessions compared with the
#' early ones. Drawn in the layout of the two heterogeneous networks, the
#' difference network shows whether the change lies within an actor type or
#' in the hand-overs between actors. When the difference comes from a
#' permutation test or a Bayesian comparison, only the differences that the
#' test supports are emphasized.
#'
#' `plot_htna_diff()` accepts two htna networks and draws `x - y`, with
#' positive differences in `pos_color` and negative differences in
#' `neg_color`; networks whose node sets differ are aligned on the union of
#' their nodes. It also accepts the result of [permutation_htna()] or
#' [bayes_compare_htna()] for two networks, which adds the significance
#' overlay (significant edges solid, the others dashed or hidden with
#' `show_nonsig`), and the grouped results of either test, drawn as one plot
#' per cohort pair titled with the pair name. It is called for the plot and
#' returns, invisibly, the value of the drawing call, or `x` for a grouped
#' test result.
#'
#' For a table of the differences by actor pair use [compare_htna()].
#'
#' @param x An htna network, or a result of [permutation_htna()] or
#'   [bayes_compare_htna()] (for two networks or for a grouped network).
#' @param y A second htna network, when `x` is an htna network. Default
#'   `NULL`.
#' @param pos_color Colour of positive differences. Default `"#009900"`.
#' @param neg_color Colour of negative differences. Default `"#C62828"`.
#' @param group_colors Colours of the actor types, in actor order. Default
#'   [htna_palette].
#' @param ... Further drawing arguments of [cograph::splot()], such as
#'   `title`, `minimum` or, for a test result, `show_nonsig`.
#'
#' @return Called for the plot. Invisibly, the value of the drawing call, or
#'   `x` for a grouped test result.
#'
#' @section Conditions:
#' `htna_bad_input` when `x` and `y` are not two htna networks with the same
#' actor types, or a test result was not computed on htna networks;
#' `htna_empty_input` for an empty grouped result.
#'
#' @seealso [compare_htna()], [permutation_htna()], [bayes_compare_htna()],
#'   [plot_htna()]
#'
#' @examples
#' data(human_ai)
#' early <- build_htna(subset(human_ai, phase == "Early"),
#'                     actor_type = "actor_type")
#' late  <- build_htna(subset(human_ai, phase == "Late"),
#'                     actor_type = "actor_type")
#' plot_htna_diff(late, early)
#'
#' # a permutation test on the grouped network: one plot per pair of phases
#' grp  <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#' perm <- permutation_htna(grp, iter = 50, seed = 1)
#' plot_htna_diff(perm)
#'
#' @export
plot_htna_diff <- function(x, y = NULL,
                           pos_color    = "#009900",
                           neg_color    = "#C62828",
                           group_colors = htna_palette,
                           ...) {
  # A grouped permutation or Bayesian comparison: one plot per cohort pair,
  # titled with the pair name (the caller controls the device layout).
  if (inherits(x, c("net_permutation_group", "net_bayes_group"))) {
    if (length(x) == 0L) {
      .htna_stop("Empty grouped comparison `x`.",
                 class = "htna_empty_input")
    }
    lapply(names(x), function(pair) {
      .plot_htna_diff_perm(x[[pair]], pos_color, neg_color, group_colors,
                           title = pair, ...)
    })
    return(invisible(x))
  }
  if (inherits(x, "net_permutation")) {
    return(invisible(.plot_htna_diff_perm(
      x, pos_color, neg_color, group_colors, ...)))
  }

  if (!inherits(x, "htna") || !inherits(y, "htna")) {
    .htna_stop("Both `x` and `y` must be htna networks, or `x` a permutation result.")
  }
  # Both networks must use the same global actor partition (same actor
  # types in the same canonical ordering). Their node sets are allowed
  # to differ — partial coverage is aligned to the union below.
  if (!identical(x$actor_levels, y$actor_levels)) {
    .htna_stop("x and y must share the same actor partition (`actor_levels`).")
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
    actor_levels = x$actor_levels,
    boundary     = x$boundary %||% y$boundary
  )
  class(synthetic) <- c("htna", "netobject", "cograph_network")
  list(diff = diff_mat, net = synthetic)
}

.plot_htna_diff_perm <- function(perm, pos_color, neg_color, group_colors, title = "",
                                 ...) {
  net <- perm$x
  if (!inherits(net, "htna") || is.null(net$node_groups)) {
    .htna_stop("perm$x is not an htna network. Build inputs with build_htna() ",
               "before calling permutation().")
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
