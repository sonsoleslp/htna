# Behaviour + forwarding-equivalence tests for the tna-coverage verbs added
# in 0.2.0: the pruning family, bayes_compare_htna(), and certainty_htna().
# Each is a ...-forwarding wrapper over Nestimate; we check that it (a) forwards
# identically to the engine and (b) keeps htna networks htna-aware where the
# engine does.

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
