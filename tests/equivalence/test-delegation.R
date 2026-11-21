# The wrappers that forward an htna network to one Nestimate function and add no computation of their own:
# casedrop_reliability_htna(), certainty_htna(), bayes_compare_htna(), markov_order_test_htna() and
# sequence_compare_htna(). Each is compared with the Nestimate function it calls, on the same input and seed
# (authority internal: the owner's package, the call htna makes). The Nestimate functions are themselves checked in
# Nestimate's own suite (exports.via in verify.json). sequence_compare_htna(level = "type") first rewrites every
# code as its actor; that step is htna's, and its reference is Nestimate::sequence_compare() of the sessions read
# as actors in the test. Re-expressed from htna's equivalence/test-equiv-delegation.R.

data(human_ai)
phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
early <- subset(human_ai, session_id %in% names(phase_of)[phase_of == "Early"])
late <- subset(human_ai, session_id %in% names(phase_of)[phase_of == "Late"])
net <- build_htna(human_ai, actor_type = "actor_type")

test_that("casedrop_reliability_htna() equals Nestimate::casedrop_reliability() under one seed", {
  expect_reference(summary(casedrop_reliability_htna(net, iter = 10L, seed = 1L)),
                   summary(Nestimate::casedrop_reliability(net, iter = 10L, seed = 1L)), reference = "Nestimate",
                   covers = "casedrop_reliability_htna()", quantity = "human_ai, per metric and drop proportion",
                   tolerance = 0, key = c("metric", "drop_prop"))
})

test_that("certainty_htna() equals Nestimate::certainty()", {
  expect_reference(summary(certainty_htna(net)), summary(Nestimate::certainty(net)), reference = "Nestimate",
                   covers = "certainty_htna()", quantity = "human_ai, per edge", tolerance = 0, key = c("from", "to"))
})

test_that("bayes_compare_htna() equals Nestimate::bayes_compare() under one seed", {
  x <- build_htna(early, actor_type = "actor_type")
  y <- build_htna(late, actor_type = "actor_type")
  expect_reference(summary(bayes_compare_htna(x, y, draws = 200L, seed = 1L)),
                   summary(Nestimate::bayes_compare(x, y, draws = 200L, seed = 1L)), reference = "Nestimate",
                   covers = "bayes_compare_htna()", quantity = "human_ai Early vs Late, per edge", tolerance = 0,
                   key = c("from", "to"))
})

test_that("markov_order_test_htna() equals Nestimate::markov_order_test() under one seed", {
  sessions <- ref_sequences(ref_wide(human_ai))
  expect_reference(summary(markov_order_test_htna(sessions, max_order = 2L, n_perm = 20L, seed = 1L)),
                   summary(Nestimate::markov_order_test(sessions, max_order = 2L, n_perm = 20L, seed = 1L)),
                   reference = "Nestimate", covers = "markov_order_test_htna()", quantity = "human_ai, per order",
                   tolerance = 0, key = "order")
})

test_that("sequence_compare_htna() equals Nestimate::sequence_compare(), by code and by actor", {
  group <- unname(phase_of[sort(names(phase_of))])
  set.seed(1)
  found <- sequence_compare_htna(net, group = group, iter = 20L, min_freq = 5L)
  set.seed(1)
  expected <- Nestimate::sequence_compare(net, group = group, iter = 20L, min_freq = 5L)
  expect_reference(summary(found), summary(expected), reference = "Nestimate", covers = "sequence_compare_htna()",
                   quantity = "human_ai Early vs Late, patterns of codes", tolerance = 0, key = "pattern")
  actor_of <- ref_actor_of(lapply(split(human_ai$code, human_ai$actor_type), unique))
  as_actors <- as.data.frame(lapply(ref_wide(human_ai), \(column) unname(actor_of[column])), stringsAsFactors = FALSE)
  set.seed(2)
  found <- sequence_compare_htna(net, group = group, level = "type", iter = 20L, min_freq = 5L)
  set.seed(2)
  expected <- Nestimate::sequence_compare(as_actors, group = group, iter = 20L, min_freq = 5L)
  expect_reference(summary(found), summary(expected), reference = "Nestimate", covers = "sequence_compare_htna()",
                   quantity = "human_ai Early vs Late, patterns of actors", tolerance = 0, key = "pattern")
})
