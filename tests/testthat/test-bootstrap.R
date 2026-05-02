test_that("bootstrap_htna() preserves htna info on the bootstrap object", {
  net  <- make_htna()
  boot <- bootstrap_htna(net, iter = 10)

  expect_s3_class(boot, "htna_bootstrap")
  expect_s3_class(boot, "net_bootstrap")
  # boot$model retains htna class and partition
  expect_s3_class(boot$model, "htna")
  expect_identical(boot$model$actor_levels, net$actor_levels)
  expect_identical(boot$model$node_groups, net$node_groups)
})

test_that("bootstrap_htna() rejects non-htna scalar input", {
  expect_error(
    bootstrap_htna("not a network"),
    regexp = "htna"
  )
})

test_that("bootstrap_htna() iterates over htna_group", {
  grp   <- make_htna_group()
  boots <- bootstrap_htna(grp, iter = 10)

  expect_s3_class(boots, "htna_bootstrap_group")
  expect_identical(names(boots), names(grp))
  for (b in boots) {
    expect_s3_class(b, "htna_bootstrap")
  }
})

test_that("bootstrap() generic dispatches to .htna", {
  net <- make_htna()
  expect_true(is.function(bootstrap))
  expect_s3_class(bootstrap(net, iter = 10), "htna_bootstrap")
})
