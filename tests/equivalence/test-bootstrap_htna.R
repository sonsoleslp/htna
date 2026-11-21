# bootstrap_htna() and the bootstrap() generic: the stability bootstrap of an htna network against tna::bootstrap()
# on the same sessions. htna hands the network to Nestimate::bootstrap_network(), which resamples whole sessions
# with replacement from the same random stream as tna (Nestimate's suite: "Bootstrap and case-dropping stability
# do share tna's random stream"), so under one seed every bootstrap mean, SD, percentile interval, consistency
# range, p-value and significance flag is the same number. Re-expressed from htna's equivalence/test-equiv-tna.R
# ("bootstrap weights and CIs match across the three engines", 30 draws on eight data sets); its comparison with
# Nestimate::bootstrap_network() is the call htna makes and is not carried over.

data(human_ai)

test_that("bootstrap_htna() equals tna::bootstrap() on human_ai, 100 draws", {
  skip_without_reference("tna")
  set.seed(42)
  reference <- tna::bootstrap(tna::tna(ref_wide(human_ai)), iter = 100L)
  found <- bootstrap_htna(build_htna(human_ai, actor_type = "actor_type"), iter = 100L, seed = 42)
  # Same draws, same arithmetic: only the last bits of a mean or a quantile can differ.
  expect_reference(summary(found), ref_tna_bootstrap(reference), reference = "tna", covers = "bootstrap_htna()",
                   quantity = "human_ai, 100 draws", tolerance = 1e-10, key = c("from", "to"),
                   note = "tna::bootstrap(tna::tna(sessions), iter = 100) after set.seed(42)")
})

test_that("the bootstrap() generic gives tna::bootstrap() on human_ai", {
  skip_without_reference("tna")
  set.seed(7)
  reference <- tna::bootstrap(tna::tna(ref_wide(human_ai)), iter = 50L)
  found <- bootstrap(build_htna(human_ai, actor_type = "actor_type"), iter = 50L, seed = 7)
  expect_reference(summary(found), ref_tna_bootstrap(reference), reference = "tna",
                   covers = c("bootstrap()", "bootstrap.htna()"), quantity = "human_ai, 50 draws", tolerance = 1e-10,
                   key = c("from", "to"))
})

test_that("every data set: bootstrap_htna() equals tna::bootstrap(), 30 draws", {
  skip_without_reference("tna")
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    set.seed(123)
    reference <- tna::bootstrap(tna::tna(ref_dataset_wide(dataset)), iter = 30L)
    found <- bootstrap_htna(build_htna(dataset$actors), iter = 30L, seed = 123)
    expect_reference(summary(found), ref_tna_bootstrap(reference), reference = "tna", covers = "bootstrap_htna()",
                     quantity = sprintf("%s, 30 draws", dataset$name), tolerance = 1e-10, key = c("from", "to"))
  }))
})

test_that("a grouped network gives tna::bootstrap() of each group's sessions under the same seed", {
  skip_without_reference("tna")
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  # Each group is drawn from the same seed (bootstrap_htna() passes `seed` to every group).
  expected <- do.call(rbind, lapply(c("Early", "Late"), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    set.seed(3)
    cbind(group = phase, ref_tna_bootstrap(tna::bootstrap(tna::tna(ref_wide(subset(human_ai, session_id %in% sessions))),
                                                          iter = 30L)))
  }))
  found <- bootstrap_htna(build_htna(human_ai, actor_type = "actor_type", group = "phase"), iter = 30L, seed = 3)
  expect_reference(summary(found), expected, reference = "tna",
                   covers = c("bootstrap_htna()", "summary.htna_bootstrap_group()"),
                   quantity = "human_ai by phase, 30 draws", tolerance = 1e-10, key = c("group", "from", "to"))
})
