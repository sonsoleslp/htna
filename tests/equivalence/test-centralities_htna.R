# centralities_htna(): nine node centralities of an htna network against tna::centralities() on the same sessions
# (loops = FALSE, path lengths 1 / weight, the defaults of both). OutStrength, InStrength, the three closeness
# measures and Betweenness come from cograph, BetweennessRSP and Clustering are htna's own code; all eight are
# compared as tna computes them. Diffusion is range-normalised to 0-1 by cograph, where tna 1.3.1 returns it raw:
# it is compared with tna's value rescaled in the test, and with the raw value as a known difference.
# Re-expressed from htna's equivalence/test-equiv-tna.R ("shared centrality measures", six measures on eight data
# sets). BetweennessRSP, Diffusion and Clustering were left out there; they are compared here.

data(human_ai)
measures <- c("OutStrength", "InStrength", "ClosenessIn", "ClosenessOut", "Closeness", "Betweenness",
              "BetweennessRSP", "Clustering")

test_that("eight centralities of human_ai equal tna::centralities()", {
  skip_without_reference("tna")
  reference <- tna::centralities(tna::tna(ref_wide(human_ai)))
  expect_reference(centralities_htna(build_htna(human_ai, actor_type = "actor_type")),
                   ref_tna_centralities(reference, measures), reference = "tna", covers = "centralities_htna()",
                   quantity = "human_ai", tolerance = 1e-10, key = "node")
})

test_that("Diffusion is tna's Diffusion range-normalised to 0-1", {
  skip_without_reference("tna")
  reference <- ref_tna_centralities(tna::centralities(tna::tna(ref_wide(human_ai))), "Diffusion")
  found <- centralities_htna(build_htna(human_ai, actor_type = "actor_type"), measures = "Diffusion")
  rescaled <- data.frame(node = reference$node, Diffusion = ref_range_normalise(reference$Diffusion))
  expect_reference(found, rescaled, reference = "tna", covers = "centralities_htna()",
                   quantity = "human_ai, Diffusion rescaled", tolerance = 1e-10, key = "node",
                   note = "tna 1.3.1's raw Diffusion, (d - min) / (max - min) in the test")
  expect_known_difference(found, reference, reference = "tna", covers = "centralities_htna()",
                          quantity = "human_ai, Diffusion raw", tolerance = 1e-10, key = "node",
                          reason = "centralities_htna() returns Diffusion range-normalised to 0-1 (cograph::centrality_diffusion under tna_network = TRUE); tna 1.3.1 returns it raw. tna >= 1.5 rescales it too (FINDINGS.md).")
})

test_that("every data set: eight centralities equal tna::centralities()", {
  skip_without_reference("tna")
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    reference <- tna::centralities(tna::tna(ref_dataset_wide(dataset)))
    expect_reference(centralities_htna(build_htna(dataset$actors), measures = measures),
                     ref_tna_centralities(reference, measures), reference = "tna", covers = "centralities_htna()",
                     quantity = dataset$name, tolerance = 1e-10, key = "node")
  }))
})

test_that("a grouped network gives each group the centralities tna computes from its sessions", {
  skip_without_reference("tna")
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  expected <- do.call(rbind, lapply(c("Early", "Late"), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    model <- tna::tna(ref_wide(subset(human_ai, session_id %in% sessions)))
    cbind(group = phase, ref_tna_centralities(tna::centralities(model), measures))
  }))
  found <- centralities_htna(build_htna(human_ai, actor_type = "actor_type", group = "phase"), measures = measures)
  expect_reference(found, expected, reference = "tna", covers = "centralities_htna()",
                   quantity = "human_ai by phase", tolerance = 1e-10, key = c("group", "node"))
})

test_that("the actor of every node is the actor its code was given", {
  actor_of <- ref_actor_of(lapply(split(human_ai$code, human_ai$actor_type), unique))
  found <- centralities_htna(build_htna(human_ai, actor_type = "actor_type"), measures = "InStrength")
  expect_reference(found, data.frame(node = names(actor_of), actor = unname(actor_of)),
                   reference = "independent: the actor_type column of the data, base R",
                   covers = "centralities_htna()", quantity = "human_ai, actor of each node", tolerance = 0,
                   key = "node")
})
