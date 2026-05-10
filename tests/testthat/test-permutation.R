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

test_that("permutation_htna() is the same function as Nestimate::permutation()", {
  expect_identical(htna::permutation_htna, Nestimate::permutation)
})
