# Behaviour tests for the tna-coverage verbs added in 0.2.0: the pruning
# family, bayes_compare_htna(), and certainty_htna(). Each is a
# ...-forwarding wrapper over Nestimate; we check that it keeps htna networks
# htna-aware where the engine does. The checks that each one forwards
# identically to the engine are local-only (equivalence/test-equiv-delegation.R).

test_that("prune_htna() prunes and preserves the htna partition", {
  net    <- make_htna()
  pruned <- prune_htna(net, method = "threshold", threshold = 0.05)

  expect_s3_class(pruned, "htna")
  expect_identical(pruned$actor_levels, net$actor_levels)
  expect_true(!is.null(pruned$node_groups) && nrow(pruned$node_groups) > 0)
})

test_that("deprune_htna() / reprune_htna() round-trip and stay htna-aware", {
  net    <- make_htna()
  pruned <- prune_htna(net, method = "threshold", threshold = 0.05)

  depruned <- deprune_htna(pruned)
  expect_s3_class(depruned, "htna")

  # reprune re-applies the recorded rule to a depruned network
  repruned <- reprune_htna(depruned)
  expect_s3_class(repruned, "htna")
  # round-trip: reprune(deprune(prune(net))) == prune(net)
  expect_equal(repruned$weights, pruned$weights)
})

test_that("pruning_details_htna() returns a tidy report", {
  net    <- make_htna()
  pruned <- prune_htna(net, method = "threshold", threshold = 0.05)

  det <- pruning_details_htna(pruned)
  expect_s3_class(det, "data.frame")
})

test_that("bayes_compare_htna() keeps the partition and is plot-compatible", {
  pair <- make_htna_pair()

  bc <- bayes_compare_htna(pair$ctrl, pair$exp, draws = 100, seed = 1)
  expect_s3_class(bc, "net_bayes")
  expect_s3_class(bc, "net_permutation")           # so plot_htna_diff() dispatches
  expect_s3_class(bc$x, "htna")                    # partition preserved
  expect_identical(bc$x$actor_levels, c("Human", "AI"))
})

test_that("certainty_htna() returns a net_certainty object", {
  net <- make_htna()
  ce  <- certainty_htna(net)
  expect_s3_class(ce, "net_certainty")
})
