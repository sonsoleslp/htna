# reliability_htna(): split-half reliability of an htna network against tna::reliability() on the same sessions.
# htna hands the network to Nestimate::network_reliability(), which splits the sessions in halves from the same
# random stream as tna, so under one seed the four metrics it reports (mean, median and maximum absolute
# difference of the two halves' weights, and their Pearson correlation) have the same mean and SD over the draws.
# Re-expressed from htna's equivalence/test-equiv-tna.R ("network_reliability is bit-identical between htna and
# Nestimate"): that compared htna with the call it makes; tna is the reference here (tolerance 1e-8 as in
# Nestimate's suite: a mean and SD over draws).

data(human_ai)

test_that("split-half metrics of human_ai equal tna::reliability()", {
  skip_without_reference("tna")
  set.seed(1)
  reference <- tna::reliability(tna::tna(ref_wide(human_ai)), iter = 30L)
  found <- reliability_htna(build_htna(human_ai, actor_type = "actor_type"), iter = 30L, seed = 1)
  expect_reference(summary(found), ref_reliability_summary(reference), reference = "tna", covers = "reliability_htna()",
                   quantity = "human_ai", tolerance = 1e-8, key = c("model", "metric"))
})

test_that("every data set: split-half metrics equal tna::reliability()", {
  skip_without_reference("tna")
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    set.seed(11)
    reference <- tna::reliability(tna::tna(ref_dataset_wide(dataset)), iter = 10L)
    found <- reliability_htna(build_htna(dataset$actors), iter = 10L, seed = 11)
    expect_reference(summary(found), ref_reliability_summary(reference), reference = "tna",
                     covers = "reliability_htna()", quantity = dataset$name, tolerance = 1e-8, key = c("model", "metric"))
  }))
})
