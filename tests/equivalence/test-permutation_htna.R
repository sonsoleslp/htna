# permutation_htna(): the permutation test of two htna networks, a forwarding wrapper of Nestimate::permutation().
# The observed differences of the edge weights are deterministic and are compared with tna::permutation_test() on
# the same two sets of sessions. The permutation p-values are a known difference with tna: tna permutes whole
# sequences, Nestimate permutes the per-sequence transition counts (Nestimate's suite and FINDINGS.md); they are
# compared with Nestimate::permutation() under one seed, which is the call htna makes (internal; Nestimate's own
# permutation() is verified against tna in its suite: exports.via in verify.json).
# Re-expressed from htna's equivalence/test-equiv-tna.R (permutation against Nestimate and against tna on padded
# matrices, eight data sets) and test-equiv-delegation.R. tna needs the two sets of sessions in tables of one
# width: both are padded to the longest session.

data(human_ai)
phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
early <- subset(human_ai, session_id %in% names(phase_of)[phase_of == "Early"])
late <- subset(human_ai, session_id %in% names(phase_of)[phase_of == "Late"])

test_that("observed edge differences of Early and Late equal tna::permutation_test(); p-values differ by the unit", {
  skip_without_reference("tna")
  width <- max(table(human_ai$session_id))
  model_x <- tna::tna(ref_wide(early, width = width))
  model_y <- tna::tna(ref_wide(late, width = width))
  set.seed(1)
  reference <- ref_tna_edge_stats(tna::permutation_test(model_x, model_y, iter = 100L), model_x, model_y)
  found <- permutation_htna(build_htna(early, actor_type = "actor_type"), build_htna(late, actor_type = "actor_type"),
                            iter = 100L, seed = 1)
  expect_reference(summary(found), subset(reference, select = c(from, to, weight_x, weight_y, diff)),
                   reference = "tna", covers = "permutation_htna()", quantity = "human_ai Early vs Late, observed",
                   tolerance = 1e-12, key = c("from", "to"))
  expect_known_difference(summary(found), subset(reference, select = c(from, to, p_value)), reference = "tna",
                          covers = "permutation_htna()", quantity = "human_ai Early vs Late, permutation p-values",
                          tolerance = 1e-10, key = c("from", "to"),
                          reason = "tna permutes whole sequences between the groups; Nestimate (which htna calls) permutes the per-sequence transition counts. The null distributions are drawn differently under one seed, so the p-values agree only up to Monte Carlo error.")
})

test_that("every data set: observed differences of two halves of the sessions equal tna::permutation_test()", {
  skip_without_reference("tna")
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    sessions <- sort(unique(unlist(lapply(dataset$actors, \(part) part$session_id))))
    # Alternate sessions, so that both halves hold every actor (one data set has each actor in its own sessions).
    half <- seq_along(sessions) %% 2L == 1L
    halves <- lapply(list(x = sessions[half], y = sessions[!half]), \(chosen)
      lapply(dataset$actors, \(part) part[part$session_id %in% chosen, , drop = FALSE]))
    width <- max(table(unlist(lapply(dataset$actors, \(part) part$session_id))))
    model_x <- tna::tna(do.call(ref_wide, c(unname(halves$x), list(width = width))))
    model_y <- tna::tna(do.call(ref_wide, c(unname(halves$y), list(width = width))))
    set.seed(42)
    reference <- ref_tna_edge_stats(tna::permutation_test(model_x, model_y, iter = 30L), model_x, model_y)
    found <- permutation_htna(build_htna(halves$x), build_htna(halves$y), iter = 30L, seed = 42)
    expect_reference(summary(found), subset(reference, select = c(from, to, weight_x, weight_y, diff)),
                     reference = "tna", covers = "permutation_htna()", quantity = sprintf("%s, observed", dataset$name),
                     tolerance = 1e-12, key = c("from", "to"))
  }))
})

test_that("p-values, effect sizes and flags equal Nestimate::permutation() under one seed", {
  x <- build_htna(early, actor_type = "actor_type")
  y <- build_htna(late, actor_type = "actor_type")
  expect_reference(summary(permutation_htna(x, y, iter = 100L, seed = 5)),
                   summary(Nestimate::permutation(x, y, iter = 100L, seed = 5)), reference = "Nestimate",
                   covers = "permutation_htna()", quantity = "human_ai Early vs Late, every column", tolerance = 0,
                   key = c("from", "to"))
})
