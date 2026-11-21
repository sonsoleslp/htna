# centrality_stability_htna(): the case-dropping stability of centralities (CS coefficient, Epskamp, Borsboom &
# Fried 2018) of an htna network against tna::estimate_cs() on the same sessions. htna hands the network to
# Nestimate::centrality_stability(), which drops sessions from the same random stream as tna, so under one seed
# the correlation of every draw is the same number, and with it the CS coefficients and the summary per drop
# proportion (mean, SD and share of draws at or above the threshold, written here from tna's correlations).
# Re-expressed from htna's equivalence/test-equiv-tna.R ("centrality_stability cs values match across htna, tna,
# Nestimate") and test-equiv-nestimate-cs.R (htna against Nestimate: the call htna makes, not carried over).

data(human_ai)
measures <- c("InStrength", "OutStrength", "Betweenness")

test_that("CS coefficients and per-drop correlations of human_ai equal tna::estimate_cs()", {
  skip_without_reference("tna")
  set.seed(1)
  reference <- tna::estimate_cs(tna::tna(ref_wide(human_ai)), iter = 30L, measures = measures)
  found <- centrality_stability_htna(build_htna(human_ai, actor_type = "actor_type"), iter = 30L, seed = 1)
  expect_reference(as.data.frame(found), ref_cs_coefficients(reference), reference = "tna",
                   covers = c("centrality_stability_htna()", "as.data.frame.htna_stability()"),
                   quantity = "human_ai, CS coefficients", tolerance = 1e-10, key = "measure")
  expect_reference(as.data.frame(found, what = "correlations"), ref_stability_summary(reference), reference = "tna",
                   covers = c("centrality_stability_htna()", "as.data.frame.htna_stability()"),
                   quantity = "human_ai, per drop proportion", tolerance = 1e-10, key = c("measure", "drop_prop"))
})

test_that("every data set with at least ten sessions: CS coefficients equal tna::estimate_cs()", {
  skip_without_reference("tna")
  # With fewer than ten sessions the smallest drop proportion drops no session; tna then skips that proportion
  # (with a warning) and its CS coefficient moves by one step. All data sets here have ten or more sessions.
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    set.seed(7)
    reference <- tna::estimate_cs(tna::tna(ref_dataset_wide(dataset)), iter = 10L, measures = measures)
    found <- centrality_stability_htna(build_htna(dataset$actors), iter = 10L, seed = 7)
    expect_reference(as.data.frame(found, what = "correlations"), ref_stability_summary(reference), reference = "tna",
                     covers = "centrality_stability_htna()", quantity = sprintf("%s, per drop proportion", dataset$name),
                     tolerance = 1e-10, key = c("measure", "drop_prop"))
  }))
})

test_that("a grouped network gives tna::estimate_cs() of each group under the same seed", {
  skip_without_reference("tna")
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  references <- lapply(stats::setNames(nm = c("Early", "Late")), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    set.seed(2)
    tna::estimate_cs(tna::tna(ref_wide(subset(human_ai, session_id %in% sessions))), iter = 10L, measures = measures)
  })
  found <- centrality_stability_htna(build_htna(human_ai, actor_type = "actor_type", group = "phase"),
                                     iter = 10L, seed = 2)
  expect_reference(summary(found),
                   do.call(rbind, lapply(names(references), \(g) cbind(group = g, ref_stability_summary(references[[g]])))),
                   reference = "tna", covers = c("centrality_stability_htna()", "summary.htna_stability_group()"),
                   quantity = "human_ai by phase, per drop proportion", tolerance = 1e-10,
                   key = c("group", "measure", "drop_prop"))
  expect_reference(as.data.frame(found),
                   do.call(rbind, lapply(names(references), \(g) cbind(group = g, ref_cs_coefficients(references[[g]])))),
                   reference = "tna", covers = c("centrality_stability_htna()", "as.data.frame.htna_stability_group()"),
                   quantity = "human_ai by phase, CS coefficients", tolerance = 1e-10, key = c("group", "measure"))
})
