test_that("centralities() returns one row per node and the requested measures", {
  net <- make_htna()
  ct  <- centralities(net)

  expect_s3_class(ct, "htna_centralities")
  expect_s3_class(ct, "data.frame")
  expect_equal(nrow(ct), nrow(net$node_groups))
  expect_true(all(c("node", "actor",
                    "OutStrength", "InStrength",
                    "ClosenessIn", "ClosenessOut", "Closeness",
                    "Betweenness", "BetweennessRSP",
                    "Diffusion", "Clustering") %in% names(ct)))
  expect_setequal(ct$node, as.character(net$node_groups$node))
  expect_setequal(ct$actor, c("Human", "AI"))
})

test_that("centralities() honours `measures =` subset", {
  net <- make_htna()
  ct  <- centralities(net, measures = c("OutStrength", "InStrength"))
  expect_true(all(c("node", "actor", "OutStrength", "InStrength") %in% names(ct)))
  expect_false("Closeness" %in% names(ct))
})

test_that("centralities() rejects unknown measure", {
  net <- make_htna()
  expect_error(
    centralities(net, measures = "Bogus"),
    regexp = "Unknown measure"
  )
})

test_that("centralities() on htna_group stacks per-group rows", {
  grp <- make_htna_group()
  ct  <- centralities(grp)

  expect_s3_class(ct, "data.frame")
  expect_true("group" %in% names(ct))
  expect_setequal(unique(ct$group), names(grp))
  # at least one row per (group, node) pair
  expect_gte(nrow(ct), sum(vapply(grp, function(g) nrow(g$node_groups), integer(1))))
})

test_that("plot_centralities() requires ggplot2 (install error otherwise)", {
  skip_if_not_installed("ggplot2")
  net <- make_htna()
  p   <- plot_centralities(net)
  expect_s3_class(p, "ggplot")
})

test_that("plot_centralities(by = 'group') colours by actor for single network", {
  skip_if_not_installed("ggplot2")
  net <- make_htna()
  p   <- plot_centralities(net, by = "group")
  expect_s3_class(p, "ggplot")
})

test_that("plot_centralities() on htna_group returns ggplot (line+point style)", {
  skip_if_not_installed("ggplot2")
  grp <- make_htna_group()
  p   <- plot_centralities(grp, by = "group")
  expect_s3_class(p, "ggplot")
})
