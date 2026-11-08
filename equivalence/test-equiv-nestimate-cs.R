# LOCAL ONLY (moved from tests/testthat/test-centrality-stability.R,
# 2026-11-08): a shipped test may not compare against another package.
# Run from the htna root:
#   Rscript -e 'pkgload::load_all("."); source("tests/testthat/helper-fixtures.R");
#               testthat::test_file("equivalence/test-equiv-nestimate-cs.R")'

test_that("centrality_stability_htna() matches Nestimate::centrality_stability()", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  cs_h <- centrality_stability_htna(net, iter = 5L, seed = 11L)
  # Same measures on both sides: Nestimate's default set differs from the one
  # centrality_stability_htna() requests.
  cs_n <- Nestimate::centrality_stability(net, measures = htna_cs_measures(),
                                          iter = 5L, seed = 11L)
  # Same seed → bit-equal results (the wrapper only adds a class tag)
  expect_equal(cs_h$cs,           cs_n$cs)
  expect_equal(cs_h$correlations, cs_n$correlations)
  expect_equal(cs_h$measures,     cs_n$measures)
})
