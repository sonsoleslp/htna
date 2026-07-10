# Delegation tests: the htna-named re-exports are `...`-forwarding
# wrappers over Nestimate's source functions (NEWS 0.1.2). They pin
# stable documented formals -- guarding against codoc regressions when
# Nestimate changes a signature -- while still delegating unchanged. We
# assert both the stable formals and that a call forwards to Nestimate.
#
# `mosaic_plot_htna()` additionally colours axis-tick labels by actor
# group using `htna_palette`, so it is checked for delegation only.

test_that("mosaic_plot_htna() delegates to Nestimate::mosaic_plot", {
  expect_true(any(grepl("Nestimate::mosaic_plot",
                        deparse(body(htna::mosaic_plot_htna)),
                        fixed = TRUE)))
})

test_that("state_distribution_htna() forwards to Nestimate::state_distribution and keeps S3 dispatch", {
  expect_identical(names(formals(htna::state_distribution_htna)), c("x", "..."))
  net <- make_htna()
  expect_equal(state_distribution_htna(net), Nestimate::state_distribution(net))
})

test_that("state_frequencies_htna() forwards to Nestimate::state_frequencies", {
  expect_identical(names(formals(htna::state_frequencies_htna)), c("data", "..."))
  net <- make_htna()
  expect_equal(state_frequencies_htna(net$data),
               Nestimate::state_frequencies(net$data))
})

test_that("frequencies_htna() returns tidy per-actor frequency table from an htna network", {
  net <- make_htna()
  fr  <- frequencies_htna(net)
  expect_s3_class(fr, "data.frame")
  expect_true(all(c("group", "state", "count", "proportion") %in% names(fr)))
  expect_setequal(unique(fr$group), net$actor_levels)
  expect_true(is.numeric(fr$count) && all(fr$count >= 0))
})

test_that("frequencies_htna() rejects non-htna input", {
  expect_error(frequencies_htna(42L), "htna network")
})

test_that("association_rules_htna() forwards to Nestimate::association_rules", {
  expect_identical(names(formals(htna::association_rules_htna)), c("x", "..."))
  net <- make_htna()
  expect_equal(association_rules_htna(net, max_length = 3L),
               Nestimate::association_rules(net, max_length = 3L))
})

# Behavior tests for the three aliases backed by explicit .htna S3
# methods in Nestimate: confirm dispatch on htna inputs still works
# through the renamed entry point.

test_that("plot_frequencies_htna(view = 'treemap') renders and returns state_freq invisibly", {
  net <- make_htna()
  with_null_device({
    sf <- plot_frequencies_htna(net)
  })
  expect_s3_class(sf, "state_freq")
  expect_true(all(c("plot", "table") %in% names(sf)))
})

test_that("plot_frequencies_htna(view = 'bars') returns a ggplot", {
  net <- make_htna()
  p <- plot_frequencies_htna(net, view = "bars")
  expect_s3_class(p, "ggplot")
})

test_that("plot_frequencies_htna(view = 'facet') returns a ggplot", {
  net <- make_htna()
  p <- plot_frequencies_htna(net, view = "facet")
  expect_s3_class(p, "ggplot")
})

test_that("mosaic_plot_htna() dispatches on htna input", {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  freq_net <- build_htna(list(Human = human_long, AI = ai_long),
                         method = "frequency")
  with_null_device({
    mp <- mosaic_plot_htna(freq_net, n_perm = 20L, seed = 1L)
  })
  expect_true(inherits(mp, "ggplot") || inherits(mp, "gtable") ||
              inherits(mp, "grob"))
})

test_that("state_distribution_htna() dispatches on htna input", {
  net <- make_htna()
  sd  <- state_distribution_htna(net)
  expect_true(is.data.frame(sd) || is.list(sd))
})
