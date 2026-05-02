test_that("extract_meta_paths() returns type-level paths", {
  net <- make_htna()
  mp  <- extract_meta_paths(net, length = 2:3)

  expect_s3_class(mp, "htna_meta_paths")
  expect_s3_class(mp, "htna_paths")
  expect_s3_class(mp, "data.frame")
  expect_true(all(c("schema", "length", "count", "support",
                    "frequency", "lift") %in% names(mp)))
  expect_identical(attr(mp, "level"), "meta")
  # alphabet is the actor types
  expect_setequal(attr(mp, "alphabet"), c("Human", "AI"))
})

test_that("extract_paths() returns state-level paths", {
  net <- make_htna()
  sp  <- extract_paths(net, length = 2:3)

  expect_s3_class(sp, "htna_state_paths")
  expect_s3_class(sp, "htna_paths")
  expect_identical(attr(sp, "level"), "state")
  # state alphabet matches node_groups
  expect_setequal(attr(sp, "alphabet"),
                  as.character(net$node_groups$node))
})

test_that("schema with wildcard filters meta-paths correctly", {
  net <- make_htna()
  mp  <- extract_meta_paths(net, schema = "Human->*->Human")
  expect_true(all(grepl("^Human->", mp$schema)))
  expect_true(all(grepl("->Human$", mp$schema)))
  expect_true(all(mp$length == 3L))
})

test_that("min_lift filter keeps only over-represented paths", {
  net <- make_htna()
  mp  <- extract_meta_paths(net, length = 3, min_lift = 1.2)
  expect_true(all(mp$lift >= 1.2))
})

test_that("extract_*_paths() error on non-htna input", {
  expect_error(extract_meta_paths(list()),       regexp = "build_htna")
  expect_error(extract_paths(list()),            regexp = "build_htna")
})
