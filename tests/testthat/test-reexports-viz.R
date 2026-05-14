# Identity tests: every htna-named alias must be the exact same object
# as Nestimate's source function. Guards against accidental drift -- if
# a future change introduces validation/coercion logic, these tests
# fire.
#
# `mosaic_plot_htna()` is intentionally NOT a pure alias: it wraps
# Nestimate::mosaic_plot() to colour axis-tick labels by actor group
# using `htna_palette`. The identity check is therefore replaced by a
# behavioural check that the wrapper still delegates to Nestimate.

test_that("mosaic_plot_htna() delegates to Nestimate::mosaic_plot", {
  expect_true(any(grepl("Nestimate::mosaic_plot",
                        deparse(body(htna::mosaic_plot_htna)),
                        fixed = TRUE)))
})

test_that("state_distribution_htna() is Nestimate::state_distribution", {
  expect_identical(htna::state_distribution_htna,
                   Nestimate::state_distribution)
})

test_that("state_frequencies_htna() is Nestimate::state_frequencies", {
  expect_identical(htna::state_frequencies_htna,
                   Nestimate::state_frequencies)
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

test_that("association_rules_htna() is Nestimate::association_rules", {
  expect_identical(htna::association_rules_htna,
                   Nestimate::association_rules)
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
