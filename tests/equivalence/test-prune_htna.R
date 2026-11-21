# prune_htna(), pruning_details_htna(), deprune_htna(), reprune_htna(): pruning of an htna network against
# tna::prune(), tna::pruning_details(), tna::deprune() and tna::reprune() on the same sessions (threshold, lowest
# share, disparity filter). htna forwards to Nestimate's net_prune() family, which never prunes a self-loop; tna
# prunes the diagonal like any other edge, so the off-diagonal edges are compared as a reference and the diagonal
# as a known difference (as in Nestimate's suite). Re-expressed from htna's equivalence/test-equiv-delegation.R,
# which compared each wrapper with the Nestimate call it makes.

data(human_ai)
wide <- ref_wide(human_ai)
settings <- list(list(method = "threshold", threshold = 0.05), list(method = "threshold", threshold = 0.1),
                 list(method = "lowest", lowest = 0.2), list(method = "disparity", level = 0.5))
setting_label <- function(setting) paste(names(setting), unlist(setting), sep = " = ", collapse = ", ")

test_that("pruned off-diagonal weights equal tna::prune() for three methods", {
  skip_without_reference("tna")
  model <- tna::tna(wide)
  net <- build_htna(human_ai, actor_type = "actor_type")
  invisible(lapply(settings, \(setting) {
    found <- do.call(prune_htna, c(list(net), setting))
    expected <- do.call(tna::prune, c(list(model), setting))
    expect_reference(ref_edge_table(weight = found$weights, drop_diagonal = TRUE),
                     ref_edge_table(weight = expected$weights, drop_diagonal = TRUE), reference = "tna",
                     covers = "prune_htna()", quantity = sprintf("human_ai, %s, off-diagonal", setting_label(setting)),
                     tolerance = 1e-12, key = c("from", "to"))
  }))
})

test_that("self-loops are kept by prune_htna() and pruned by tna::prune()", {
  skip_without_reference("tna")
  found <- prune_htna(build_htna(human_ai, actor_type = "actor_type"), method = "threshold", threshold = 0.1)
  expected <- tna::prune(tna::tna(wide), method = "threshold", threshold = 0.1)
  expect_known_difference(diag(found$weights), diag(expected$weights), reference = "tna", covers = "prune_htna()",
                          quantity = "human_ai, threshold = 0.1, self-loops", tolerance = 1e-12,
                          reason = "Nestimate (which htna calls) never removes a self-loop when it prunes; tna::prune() prunes the diagonal like any other edge.")
})

test_that("removed off-diagonal edges equal tna::pruning_details()", {
  skip_without_reference("tna")
  model <- tna::tna(wide)
  net <- build_htna(human_ai, actor_type = "actor_type")
  invisible(lapply(settings[c(2L, 3L, 4L)], \(setting) {
    found <- do.call(prune_htna, c(list(net), setting))
    removed <- as.data.frame(tna::pruning_details(do.call(tna::prune, c(list(model), setting))))
    removed$from <- as.character(removed$from)
    removed$to <- as.character(removed$to)
    expect_reference(pruning_details_htna(found), subset(removed, from != to), reference = "tna",
                     covers = "pruning_details_htna()",
                     quantity = sprintf("human_ai, %s, removed edges", setting_label(setting)), tolerance = 1e-12,
                     key = c("from", "to"))
  }))
})

test_that("deprune_htna() restores tna::deprune()'s weights; reprune_htna() those of tna::reprune()", {
  skip_without_reference("tna")
  pruned <- prune_htna(build_htna(human_ai, actor_type = "actor_type"), method = "threshold", threshold = 0.1)
  pruned_tna <- tna::prune(tna::tna(wide), method = "threshold", threshold = 0.1)
  restored <- deprune_htna(pruned)
  expect_reference(restored$weights, tna::deprune(pruned_tna)$weights, reference = "tna", covers = "deprune_htna()",
                   quantity = "human_ai, depruned weights", tolerance = 1e-12)
  expect_reference(ref_edge_table(weight = reprune_htna(restored)$weights, drop_diagonal = TRUE),
                   ref_edge_table(weight = tna::reprune(tna::deprune(pruned_tna))$weights, drop_diagonal = TRUE),
                   reference = "tna", covers = "reprune_htna()", quantity = "human_ai, repruned weights, off-diagonal",
                   tolerance = 1e-12, key = c("from", "to"))
})
