# LOCAL ONLY (moved 2026-11-08 from tests/testthat/: test-casedrop-reliability.R,
# test-permutation.R, test-markov-order-test.R, test-sequence-compare.R,
# test-reliability.R, test-reexports-viz.R, test-tna-verbs.R,
# test-compare-htna.R): a shipped test may not compare
# htna's output against another package (Nestimate / tna). These blocks do,
# so they live here, outside the built package (^equivalence$ in .Rbuildignore).
# Run from the htna root:
#   Rscript -e 'pkgload::load_all("."); source("tests/testthat/helper-fixtures.R");
#               testthat::test_file("equivalence/test-equiv-delegation.R")'


# ---- from tests/testthat/test-casedrop-reliability.R ----

test_that("casedrop_reliability_htna() is a stable ...-forwarding wrapper", {
  # Durable-wrapper design: documented formals stay `x, ...` (see NEWS 0.1.2).
  expect_identical(names(formals(htna::casedrop_reliability_htna)), c("x", "..."))

  net <- make_htna()
  via_htna <- casedrop_reliability_htna(net, iter = 5L, seed = 1L)
  direct   <- Nestimate::casedrop_reliability(net, iter = 5L, seed = 1L)
  expect_equal(via_htna$cs, direct$cs)
})

# ---- from tests/testthat/test-permutation.R ----

test_that("permutation_htna() is a stable ...-forwarding wrapper for Nestimate::permutation()", {
  # Durable-wrapper design: documented formals stay `x, y, ...` regardless of
  # upstream signature changes, so codoc can never regress (see NEWS 0.1.2).
  expect_identical(names(formals(htna::permutation_htna)), c("x", "y", "..."))

  pair <- make_htna_pair()
  via_htna <- permutation_htna(pair$ctrl, pair$exp, iter = 20, seed = 1)
  direct   <- Nestimate::permutation(pair$ctrl, pair$exp, iter = 20, seed = 1)
  expect_equal(via_htna$p_values, direct$p_values)
})

# ---- from tests/testthat/test-markov-order-test.R ----

test_that("markov_order_test_htna() is a stable ...-forwarding wrapper", {
  # Durable-wrapper design: documented formals stay `data, ...` (see NEWS 0.1.2).
  expect_identical(names(formals(htna::markov_order_test_htna)), c("data", "..."))

  data(human_long, package = "Nestimate", envir = environment())
  seqs <- split(human_long$code, human_long$session_id)
  seqs <- seqs[lengths(seqs) >= 3L]

  via_htna <- markov_order_test_htna(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  direct   <- Nestimate::markov_order_test(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  expect_equal(via_htna$optimal_order, direct$optimal_order)
})

# ---- from tests/testthat/test-sequence-compare.R ----

test_that("sequence_compare_htna() default (level='state') matches Nestimate::sequence_compare()", {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  net <- suppressWarnings(build_htna(list(Human = human_long, AI = ai_long)))
  n   <- nrow(net$data)
  grp <- rep(c("early", "late"), length.out = n)

  set.seed(1L)
  res_htna <- suppressWarnings(
    sequence_compare_htna(net, group = grp, iter = 10, min_freq = 5)
  )
  set.seed(1L)
  res_nest <- suppressWarnings(
    Nestimate::sequence_compare(net, group = grp, iter = 10, min_freq = 5)
  )
  expect_identical(res_htna$patterns, res_nest$patterns)
  expect_identical(res_htna$groups,   res_nest$groups)
})

# ---- from tests/testthat/test-reliability.R ----

test_that("reliability_htna() matches Nestimate::network_reliability() numerically", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  set.seed(99)
  rel_h <- reliability_htna(net, iter = 5L, seed = 11L)
  rel_n <- Nestimate::network_reliability(net, iter = 5L, seed = 11L)

  # Same seed → identical iteration metrics; the htna wrapper only
  # post-processes $models, so the reliability calculations are
  # bit-equal.
  expect_equal(rel_h$iterations, rel_n$iterations)
  expect_equal(rel_h$summary,    rel_n$summary)
  expect_equal(rel_h$iter,  rel_n$iter)
  expect_equal(rel_h$split, rel_n$split)
  expect_equal(rel_h$scale, rel_n$scale)
})

# ---- from tests/testthat/test-reexports-viz.R ----

test_that("state_distribution_htna() forwards to Nestimate::state_distribution and keeps S3 dispatch", {
  expect_identical(names(formals(htna::state_distribution_htna)), c("x", "..."))
  net <- make_htna()
  expect_equal(state_distribution_htna(net), Nestimate::state_distribution(net))
})

test_that("state_frequencies_htna() forwards to Nestimate::state_frequencies", {
  expect_identical(names(formals(htna::state_frequencies_htna)), c("data", "..."))
  net <- make_htna()
  expect_equal(state_frequencies_htna(net$data),
               Nestimate::state_frequencies(net$data))
})

test_that("association_rules_htna() forwards to Nestimate::association_rules", {
  expect_identical(names(formals(htna::association_rules_htna)), c("x", "..."))
  net <- make_htna()
  expect_equal(association_rules_htna(net, max_length = 3L),
               Nestimate::association_rules(net, max_length = 3L))
})

# ---- from tests/testthat/test-tna-verbs.R ----

test_that("prune_htna() prunes and preserves the htna partition", {
  net    <- make_htna()
  pruned <- prune_htna(net, method = "threshold", threshold = 0.05)

  expect_s3_class(pruned, "htna")
  expect_identical(pruned$actor_levels, net$actor_levels)
  expect_true(!is.null(pruned$node_groups) && nrow(pruned$node_groups) > 0)

  # identical to calling the engine directly
  direct <- Nestimate::net_prune(net, method = "threshold", threshold = 0.05)
  expect_equal(pruned$weights, direct$weights)
})

test_that("deprune_htna() / reprune_htna() round-trip and stay htna-aware", {
  net    <- make_htna()
  pruned <- prune_htna(net, method = "threshold", threshold = 0.05)

  depruned <- deprune_htna(pruned)
  expect_s3_class(depruned, "htna")
  expect_equal(depruned$weights, Nestimate::net_deprune(pruned)$weights)

  # reprune re-applies the recorded rule to a depruned network
  repruned <- reprune_htna(depruned)
  expect_s3_class(repruned, "htna")
  expect_equal(repruned$weights, Nestimate::net_reprune(depruned)$weights)
  # round-trip: reprune(deprune(prune(net))) == prune(net)
  expect_equal(repruned$weights, pruned$weights)
})

test_that("pruning_details_htna() returns a tidy report equal to the engine", {
  net    <- make_htna()
  pruned <- prune_htna(net, method = "threshold", threshold = 0.05)

  det <- pruning_details_htna(pruned)
  expect_s3_class(det, "data.frame")
  expect_equal(det, Nestimate::net_pruning_details(pruned))
})

test_that("bayes_compare_htna() forwards, keeps the partition, and is plot-compatible", {
  pair <- make_htna_pair()

  bc <- bayes_compare_htna(pair$ctrl, pair$exp, draws = 100, seed = 1)
  expect_s3_class(bc, "net_bayes")
  expect_s3_class(bc, "net_permutation")           # so plot_htna_diff() dispatches
  expect_s3_class(bc$x, "htna")                    # partition preserved
  expect_identical(bc$x$actor_levels, c("Human", "AI"))

  direct <- Nestimate::bayes_compare(pair$ctrl, pair$exp, draws = 100, seed = 1)
  expect_equal(summary(bc), summary(direct))
})

test_that("certainty_htna() forwards to Nestimate::certainty()", {
  net <- make_htna()
  ce  <- certainty_htna(net)
  expect_s3_class(ce, "net_certainty")
  expect_equal(ce, Nestimate::certainty(net))
})

# ---- from tests/testthat/test-compare-htna.R ----

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

# ---- from tests/testthat/test-summary-network-metrics.R ----
# `summary(plain)` on a class-stripped netobject dispatches to
# Nestimate::summary.netobject(), so these blocks compare against Nestimate.

test_that("summary.htna includes the exact Nestimate network metric table", {
  for (seed in c(151L, 157L, 163L, 167L)) {
    net <- make_htna(seed)
    plain <- net
    class(plain) <- setdiff(class(plain), "htna")
    expected <- summary(plain)
    net_before <- net

    output <- capture.output(actual <- summary(net))
    expect_true(any(grepl("Network metrics:", output, fixed = TRUE)))
    expect_equal(actual$network_metrics, expected, info = paste("seed", seed))
    expect_identical(net, net_before)
    expect_equal(actual$n_nodes,
                 expected$value[expected$metric == "Node Count"])
    expect_equal(actual$n_edges,
                 expected$value[expected$metric == "Edge Count"])
  }
})

test_that("summary.htna network metrics work for every construction method", {
  d <- make_grouped_data(seed = 171L)
  node_groups <- list(Human = c("H1", "H2", "H3"),
                      AI = c("A1", "A2", "A3"))
  for (method in c("relative", "frequency", "co_occurrence", "attention")) {
    net <- suppressWarnings(build_htna(d, node_groups = node_groups,
                                       method = method))
    plain <- net
    class(plain) <- setdiff(class(plain), "htna")
    actual <- capture.output(result <- summary(net))
    expected <- summary(plain)

    expect_equal(result$network_metrics, expected, info = method)
    expect_equal(result$network_metrics$value[
      result$network_metrics$metric == "Node Count"
    ], nrow(net$weights), info = method)
    # Nestimate counts an undirected edge once since 1.0.7, every cell before.
    each_pair_once <- !net$directed &&
      utils::packageVersion("Nestimate") >= "1.0.7"
    expect_equal(result$network_metrics$value[
      result$network_metrics$metric == "Edge Count"
    ], sum(net$weights[if (each_pair_once) upper.tri(net$weights, diag = TRUE)
                       else TRUE] > 0),
    info = method)
    expect_true(any(grepl("Network Density", actual, fixed = TRUE)),
                info = method)
  }
})

test_that("summary.htna_group includes independent metrics for every cohort", {
  group <- make_htna_group(seed = 181L)
  output <- capture.output(result <- summary(group))

  expect_named(result, names(group))
  expect_equal(sum(grepl("Network metrics:", output, fixed = TRUE)),
               length(group))
  for (cohort in names(group)) {
    plain <- group[[cohort]]
    class(plain) <- setdiff(class(plain), "htna")
    expect_equal(result[[cohort]]$network_metrics, summary(plain),
                 info = cohort)
    expect_equal(result[[cohort]]$n_nodes, nrow(group[[cohort]]$weights))
    expect_equal(result[[cohort]]$n_edges,
                 sum(group[[cohort]]$weights != 0))
  }
})
