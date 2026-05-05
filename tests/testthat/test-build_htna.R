test_that("build_htna() with named-list input builds an htna network", {
  net <- make_htna()

  expect_s3_class(net, "htna")
  expect_s3_class(net, "netobject")
  expect_true(!is.null(net$node_groups))
  expect_setequal(as.character(net$node_groups$group), c("Human", "AI"))
  expect_identical(net$actor_levels, c("Human", "AI"))
  # node_groups order matches nodes order
  expect_identical(as.character(net$node_groups$node), net$nodes$label)
})

test_that("build_htna() with single data frame + node_groups works", {
  d <- make_grouped_data()
  net <- build_htna(
    d,
    node_groups = list(Human = c("H1", "H2", "H3"),
                       AI    = c("A1", "A2", "A3"))
  )
  expect_s3_class(net, "htna")
  expect_setequal(as.character(net$node_groups$group), c("Human", "AI"))
  # Verify each code is bound to the correct actor — without this a
  # flipped mapping would still pass the set-equality check above.
  ng <- net$node_groups
  group_of <- function(node) as.character(ng$group[ng$node == node])
  for (h in c("H1","H2","H3")) expect_identical(group_of(h), "Human")
  for (a in c("A1","A2","A3")) expect_identical(group_of(a), "AI")
})

test_that("build_htna(group = ...) returns an htna_group", {
  grp <- make_htna_group()

  expect_s3_class(grp, "htna_group")
  expect_s3_class(grp, "netobject_group")
  # `make_htna_group()` produces exactly 2 cohorts; tighter than `>= 2`
  # so an extra/missing cohort would fail.
  expect_equal(length(grp), 2L)
  expect_setequal(names(grp), c("Control", "Experimental"))
  for (g in grp) {
    expect_s3_class(g, "htna")
    expect_true(!is.null(g$node_groups))
    expect_identical(g$actor_levels, c("Human", "AI"))
  }
})

test_that("build_htna() rejects single-actor input", {
  d <- make_grouped_data()
  expect_error(
    build_htna(list(Only = d)),
    regexp = "length"
  )
})

test_that("build_htna() rejects unknown group column", {
  d <- make_grouped_data()
  expect_error(
    build_htna(d,
               node_groups = list(Human = c("H1","H2","H3"),
                                  AI    = c("A1","A2","A3")),
               group = "missing_col"),
    regexp = "names\\(combined\\)|missing_col"
  )
})

test_that("build_htna() errors on overlapping codes without disambiguate", {
  d <- data.frame(
    session_id = rep("S1", 4),
    code = c("X", "Y", "X", "Y"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, node_groups = list(A = "X", B = c("X","Y"))),
    regexp = "more than one"
  )
})
