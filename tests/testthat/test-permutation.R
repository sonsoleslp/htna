test_that("permutation_htna() preserves htna class on $x and $y", {
  pair <- make_htna_pair()
  perm <- permutation_htna(pair$ctrl, pair$exp, iter = 20)

  expect_s3_class(perm, "net_permutation")
  expect_s3_class(perm$x, "htna")
  expect_s3_class(perm$y, "htna")
  expect_identical(perm$x$actor_levels, c("Human", "AI"))

  n <- length(perm$x$nodes$label)
  expect_equal(dim(perm$diff),     c(n, n))
  expect_equal(dim(perm$diff_sig), c(n, n))
  expect_equal(dim(perm$p_values), c(n, n))
})

test_that("permutation_htna() is a stable ...-forwarding wrapper for Nestimate::permutation()", {
  # Durable-wrapper design: documented formals stay `x, y, ...` regardless of
  # upstream signature changes, so codoc can never regress (see NEWS 0.1.2).
  expect_identical(names(formals(htna::permutation_htna)), c("x", "y", "..."))

  pair <- make_htna_pair()
  via_htna <- permutation_htna(pair$ctrl, pair$exp, iter = 20, seed = 1)
  direct   <- Nestimate::permutation(pair$ctrl, pair$exp, iter = 20, seed = 1)
  expect_equal(via_htna$p_values, direct$p_values)
})
