# Regression guard for the durable ...-forwarding wrapper design (NEWS 0.1.2).
#
# The *_htna functions below all delegate to Nestimate. Six were direct aliases
# (`X_htna <- Nestimate::Y`), which froze their Rd at htna build time while their
# code formals tracked the *installed* Nestimate; two more (sequence_compare,
# mosaic_plot) were wrappers that hardcoded Nestimate's argument list. Either
# way, an argument Nestimate added/renamed produced a code/documentation-mismatch
# WARNING in `R CMD check` (surfacing in Nestimate's CRAN reverse-dependency
# check). Making each forward through `...` pins the documented formals so
# upstream signature changes can never reproduce that WARNING.
#
# This test fails loudly if anyone reverts a wrapper to a bare alias or re-adds
# Nestimate's args as explicit formals: `"..."` then drops out of the formals.

test_that("every *_htna wrapper exposes `...` so codoc stays stable", {
  expected <- list(
    permutation_htna          = c("x", "y", "..."),
    association_rules_htna     = c("x", "..."),
    casedrop_reliability_htna  = c("x", "..."),
    markov_order_test_htna     = c("data", "..."),
    state_distribution_htna    = c("x", "..."),
    state_frequencies_htna     = c("data", "..."),
    sequence_compare_htna      = c("x", "group", "level", "..."),
    mosaic_plot_htna           = c("x", "..."),
    prune_htna                 = c("x", "..."),
    deprune_htna               = c("x", "..."),
    reprune_htna               = c("x", "..."),
    pruning_details_htna       = c("x", "..."),
    bayes_compare_htna         = c("x", "y", "..."),
    certainty_htna             = c("x", "...")
  )

  invisible(Map(function(nm, formals_names) {
    fn <- get(nm, envir = asNamespace("htna"))
    expect_true(is.function(fn), info = nm)
    expect_identical(names(formals(fn)), formals_names, info = nm)
    expect_true("..." %in% names(formals(fn)), info = nm)
  }, names(expected), expected))
})
