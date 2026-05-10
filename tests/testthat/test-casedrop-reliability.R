test_that("casedrop_reliability_htna() is the same function as Nestimate::casedrop_reliability()", {
  expect_identical(htna::casedrop_reliability_htna, Nestimate::casedrop_reliability)
})

test_that("casedrop_reliability_htna() runs on an htna network", {
  net <- make_htna()
  cd  <- casedrop_reliability_htna(net, iter = 5L,
                                   drop_prop = c(0.3, 0.5), seed = 1L)

  expect_s3_class(cd, "net_casedrop_reliability")
  expect_named(cd,
               c("cs", "summary", "metrics", "correlations",
                 "drop_prop", "threshold", "certainty", "iter",
                 "method", "include_diag", "n_cases", "n_edges"),
               ignore.order = TRUE)
  expect_equal(cd$iter, 5L)
  expect_equal(cd$drop_prop, c(0.3, 0.5))
  expect_true(is.numeric(cd$cs))
  expect_s3_class(cd$summary, "data.frame")
})

test_that("casedrop_reliability_htna() dispatches on htna_group", {
  gnet <- make_htna_group()
  cdg  <- casedrop_reliability_htna(gnet, iter = 5L,
                                    drop_prop = c(0.3, 0.5), seed = 1L)

  expect_s3_class(cdg, "net_casedrop_reliability_group")
  expect_named(cdg, names(gnet))
  for (entry in cdg) expect_s3_class(entry, "net_casedrop_reliability")
})
