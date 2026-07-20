test_that("compare_htna() is numerically identical to compare_model()", {
  pair <- make_htna_pair(seed = 41L)
  aligned <- htna:::.align_htna_comparison_pair(pair$ctrl, pair$exp)

  for (scaling in c("none", "minmax", "max", "rank", "zscore", "robust",
                    "log1p", "softmax", "quantile", "frobenius", "row")) {
    actual <- suppressMessages(suppressWarnings(compare_htna(
      pair$ctrl, pair$exp, scaling = scaling,
      measures = c("InStrength", "OutStrength"), network = TRUE
    )))
    expected <- suppressMessages(suppressWarnings(Nestimate::compare_model(
      aligned$x, aligned$y, scaling = scaling,
      measures = c("InStrength", "OutStrength"), network = TRUE
    )))

    for (slot in c("matrices", "difference_matrix", "summary_metrics",
                   "network_metrics", "centrality_differences",
                   "centrality_correlations")) {
      expect_equal(actual[[slot]], expected[[slot]],
                   info = paste(scaling, slot))
    }
    base_edge_names <- names(expected$edge_metrics)
    expect_equal(actual$edge_metrics[base_edge_names], expected$edge_metrics,
                 info = paste(scaling, "edge_metrics"))
  }
})

test_that("compare_htna() supports every engine scaling on positive weights", {
  pair <- make_htna_pair(seed = 51L)
  set.seed(5101L)
  for (net_name in names(pair)) {
    n <- nrow(pair[[net_name]]$weights)
    W <- matrix(runif(n * n, 0.05, 1), n, n,
                dimnames = dimnames(pair[[net_name]]$weights))
    pair[[net_name]]$weights <- W
  }
  aligned <- htna:::.align_htna_comparison_pair(pair$ctrl, pair$exp)

  scalings <- c("none", "minmax", "max", "rank", "zscore", "robust",
                "log", "log1p", "softmax", "quantile", "frobenius", "row")
  for (scaling in scalings) {
    actual <- suppressWarnings(compare_htna(
      pair$ctrl, pair$exp, scaling = scaling, network = FALSE
    ))
    expected <- suppressWarnings(Nestimate::compare_model(
      aligned$x, aligned$y, scaling = scaling, network = FALSE
    ))
    expect_equal(actual$difference_matrix, expected$difference_matrix,
                 info = scaling)
    expect_equal(actual$summary_metrics, expected$summary_metrics,
                 info = scaling)
  }
})

test_that("compare_htna() retains actors, models, and actor-block summaries", {
  pair <- make_htna_pair(seed = 61L)
  ctrl_before <- pair$ctrl
  exp_before <- pair$exp
  result <- compare_htna(pair$ctrl, pair$exp, network = FALSE)

  expect_s3_class(result, "htna_comparison")
  expect_s3_class(result, "net_comparison")
  expect_identical(result$actor_levels, c("Human", "AI"))
  expect_identical(attr(result$node_groups, "actor_levels"),
                   result$actor_levels)
  expect_identical(result$models$x, pair$ctrl)
  expect_identical(result$models$y, pair$exp)
  expect_identical(pair$ctrl, ctrl_before)
  expect_identical(pair$exp, exp_before)

  lookup <- setNames(result$node_groups$group, result$node_groups$node)
  expect_identical(
    as.character(result$edge_metrics$source_actor),
    unname(lookup[as.character(result$edge_metrics$source)])
  )
  expect_identical(
    as.character(result$edge_metrics$target_actor),
    unname(lookup[as.character(result$edge_metrics$target)])
  )
  expect_identical(levels(result$edge_metrics$source_actor),
                   result$actor_levels)
  expect_identical(levels(result$edge_metrics$target_actor),
                   result$actor_levels)

  blocks <- result$actor_pair_metrics
  expect_equal(nrow(blocks), length(result$actor_levels)^2L)
  expect_equal(sum(blocks$n_cells), nrow(result$edge_metrics))
  expect_equal(sum(blocks$n_changed),
               sum(result$edge_metrics$absolute_difference > 0))
  for (i in seq_len(nrow(blocks))) {
    keep <- as.character(result$edge_metrics$source_actor) ==
      as.character(blocks$source_actor[i]) &
      as.character(result$edge_metrics$target_actor) ==
      as.character(blocks$target_actor[i])
    edge_block <- result$edge_metrics[keep, , drop = FALSE]
    expect_equal(blocks$n_cells[i], nrow(edge_block))
    expect_equal(blocks$mean_difference[i],
                 mean(edge_block$raw_difference))
    expect_equal(blocks$rms_difference[i],
                 sqrt(mean(edge_block$squared_difference)))
  }
})

test_that("compare_htna() zero-aligns partial node coverage by actor", {
  pair <- make_htna_pair(seed = 71L)
  trim <- function(net, keep) {
    net$weights <- net$weights[keep, keep, drop = FALSE]
    net$nodes <- net$nodes[match(keep, net$nodes$label), , drop = FALSE]
    net$node_groups <- net$node_groups[
      match(keep, net$node_groups$node), , drop = FALSE
    ]
    attr(net$node_groups, "actor_levels") <- net$actor_levels
    net
  }
  x <- trim(pair$ctrl, c("H1", "H2", "A1"))
  y <- trim(pair$exp, c("H2", "H3", "A1", "A2", "A3"))
  result <- compare_htna(x, y, network = FALSE)

  expected_nodes <- c("H1", "H2", "H3", "A1", "A2", "A3")
  expect_identical(rownames(result$matrices$x), expected_nodes)
  expect_identical(colnames(result$matrices$x), expected_nodes)
  expect_identical(rownames(result$matrices$y), expected_nodes)
  expect_true(all(result$matrices$x[c("H3", "A2", "A3"), ] == 0))
  expect_true(all(result$matrices$x[, c("H3", "A2", "A3")] == 0))
  expect_true(all(result$matrices$y["H1", ] == 0))
  expect_true(all(result$matrices$y[, "H1"] == 0))
  expect_identical(result$node_groups$node, expected_nodes)
  expect_identical(result$node_groups$group,
                   c("Human", "Human", "Human", "AI", "AI", "AI"))
})

test_that("compare_htna(htna_group) computes every pair without subsetting", {
  pair <- make_htna_pair(seed = 81L)
  third <- pair$ctrl
  third$weights <- (pair$ctrl$weights + pair$exp$weights) / 2
  group <- structure(
    list(Control = pair$ctrl, Experimental = pair$exp, Middle = third),
    class = c("htna_group", "netobject_group", "list"),
    actor_levels = pair$ctrl$actor_levels
  )

  result <- suppressWarnings(compare_htna(
    group, scaling = "robust", network = FALSE
  ))
  expect_s3_class(result, "htna_comparison_group")
  expect_identical(
    names(result),
    c("Control vs Experimental", "Control vs Middle",
      "Experimental vs Middle")
  )
  expect_equal(length(result), choose(length(group), 2L))
  expect_identical(attr(result, "actor_levels"), c("Human", "AI"))
  expect_identical(
    attr(result, "comparisons")$comparison,
    names(result)
  )
  expect_identical(result[[1L]]$comparison,
                   c(x = "Control", y = "Experimental"))
  expect_identical(result[[1L]]$models$Control, group$Control)
  expect_identical(result[[1L]]$models$Experimental, group$Experimental)
})

test_that("compare_htna() validates incompatible and malformed inputs", {
  pair <- make_htna_pair(seed = 91L)
  expect_error(compare_htna(pair$ctrl), "both be `htna`")
  expect_error(compare_htna(pair$ctrl$weights, pair$exp), "both be `htna`")

  group <- structure(
    list(Control = pair$ctrl, Experimental = pair$exp),
    class = c("htna_group", "netobject_group", "list")
  )
  expect_error(compare_htna(group, pair$ctrl), "leave `y = NULL`")
  one_group <- structure(
    list(Control = pair$ctrl),
    class = c("htna_group", "netobject_group", "list")
  )
  expect_error(compare_htna(one_group), "at least two cohorts")

  bad_levels <- pair$exp
  bad_levels$actor_levels <- rev(bad_levels$actor_levels)
  expect_error(compare_htna(pair$ctrl, bad_levels), "identical `actor_levels`")

  bad_assignment <- pair$exp
  idx <- match("H1", bad_assignment$node_groups$node)
  bad_assignment$node_groups$group[idx] <- "AI"
  expect_error(compare_htna(pair$ctrl, bad_assignment),
               "assigned to different actors")

  missing_assignment <- pair$exp
  missing_assignment$node_groups <- missing_assignment$node_groups[-1L, ]
  expect_error(compare_htna(pair$ctrl, missing_assignment),
               "without actor assignments")

  duplicated_assignment <- pair$exp
  duplicated_assignment$node_groups <- rbind(
    duplicated_assignment$node_groups,
    duplicated_assignment$node_groups[1L, ]
  )
  expect_error(compare_htna(pair$ctrl, duplicated_assignment), "must be unique")
})

test_that("comparison plots dispatch for single and grouped results", {
  skip_if_not_installed("ggplot2")
  pair <- make_htna_pair(seed = 101L)
  single <- compare_htna(pair$ctrl, pair$exp, network = FALSE)
  expect_s3_class(plot(single, type = "heatmap"), "ggplot")

  group <- structure(
    list(Control = pair$ctrl, Experimental = pair$exp),
    class = c("htna_group", "netobject_group", "list")
  )
  plots <- plot(compare_htna(group, network = FALSE), type = "scatter")
  expect_named(plots, "Control vs Experimental")
  expect_true(all(vapply(plots, inherits, logical(1L), what = "ggplot")))

  expect_error(
    plot(structure(list(), class = c("htna_comparison_group", "list"))),
    "empty"
  )
})
