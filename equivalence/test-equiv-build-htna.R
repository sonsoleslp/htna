# LOCAL ONLY (moved 2026-11-08 from tests/testthat/test-build_htna-mcml.R and
# tests/testthat/test-build_htna-clustering.R): a shipped test may not compare
# htna's output against another package (Nestimate / tna). These blocks do,
# so they live here, outside the built package (^equivalence$ in .Rbuildignore).
# Run from the htna root:
#   Rscript -e 'pkgload::load_all("."); source("tests/testthat/helper-fixtures.R");
#               testthat::test_file("equivalence/test-equiv-build-htna.R")'


# ---- from tests/testthat/test-build_htna-mcml.R ----

.mcml_htna_fixture <- function() {
  sequences <- data.frame(
    T1 = c("A", "A", "C", "E", "B", "D"),
    T2 = c("C", "B", "E", "A", "D", "F"),
    T3 = c("B", "D", "F", "C", "A", "E"),
    T4 = c("D", "E", "A", "F", "C", "B"),
    stringsAsFactors = FALSE
  )
  clusters <- list(
    Human = c("A", "B"),
    AI = c("C", "D"),
    Tool = c("E", "F")
  )
  list(
    sequences = sequences,
    clusters = clusters,
    mcml = Nestimate::build_mcml(sequences, clusters)
  )
}

test_that("build_htna(mcml) equals as_htna(mcml) and rebuilds globally", {
  fixture <- .mcml_htna_fixture()

  # Newer Nestimate returns an `htna` from as_htna(<mcml>); older versions
  # return a plain `netobject`, which htna's mcml guard rejects.
  probe <- try(
    suppressWarnings(Nestimate::as_htna(fixture$mcml, method = "relative")),
    silent = TRUE
  )
  if (inherits(probe, "try-error") || !inherits(probe, "htna")) {
    skip(paste0(
      "Installed Nestimate does not return an `htna` from as_htna(<mcml>); ",
      "this test requires that shape."
    ))
  }

  for (method in c("relative", "frequency", "co_occurrence", "attention")) {
    actual <- suppressWarnings(build_htna(fixture$mcml, method = method))
    expected <- suppressWarnings(Nestimate::as_htna(
      fixture$mcml, method = method
    ))
    direct <- suppressWarnings(Nestimate::build_network(
      fixture$sequences, method = method
    ))

    expect_s3_class(actual, "htna")
    expect_false(inherits(actual, "htna_group"))
    expect_identical(actual, expected,
                     info = paste("complete as_htna equivalence:", method))
    expect_equal(actual$weights, expected$weights,
                 info = paste("as_htna equivalence:", method))
    expect_equal(actual$weights, direct$weights,
                 info = paste("global source rebuild:", method))
    expect_equal(actual$inits, expected$inits,
                 info = paste("initial distribution:", method))
  }

  net <- build_htna(fixture$mcml)
  mcml_clusters <- fixture$mcml$cluster_members
  expect_gt(net$weights["A", "C"], 0)
  expect_identical(net$actor_levels, names(mcml_clusters))
  expect_type(net$node_groups$group, "character")
  expect_s3_class(net$nodes$groups, "factor")
  expect_identical(levels(net$nodes$groups), names(mcml_clusters))
  expect_identical(attr(net, "cluster_members"), mcml_clusters)
  expect_identical(as.character(net$nodes$groups), net$node_groups$group)
})

test_that("build_htna(mcml) accepts an explicit source and group override", {
  fixture <- .mcml_htna_fixture()
  direct <- Nestimate::build_network(fixture$sequences, method = "relative")
  matrix_mcml <- Nestimate::build_mcml(direct$weights, fixture$clusters)

  expect_error(build_htna(matrix_mcml), "no expandable node-level source")

  # Newer Nestimate returns an `htna` from as_htna(matrix mcml + source_data);
  # older versions return a plain `netobject`, which htna's mcml guard rejects.
  probe <- try(
    Nestimate::as_htna(matrix_mcml, data = fixture$sequences,
                       method = "relative"),
    silent = TRUE
  )
  if (inherits(probe, "try-error") || !inherits(probe, "htna")) {
    skip(paste0(
      "Installed Nestimate does not return an `htna` from ",
      "as_htna(matrix mcml + source_data); this test requires that shape."
    ))
  }

  rebuilt <- build_htna(matrix_mcml, source_data = fixture$sequences)
  expected <- Nestimate::as_htna(
    matrix_mcml, data = fixture$sequences, method = "relative"
  )
  expect_equal(rebuilt$weights, expected$weights)

  override <- list(
    First = c("A", "C", "E"),
    Second = c("B", "D", "F")
  )
  regrouped <- build_htna(
    fixture$mcml,
    node_groups = override,
    source_data = fixture$sequences
  )
  expect_identical(regrouped$actor_levels, names(override))
  expect_identical(attr(regrouped, "cluster_members"), override)
  expect_equal(
    regrouped$weights,
    Nestimate::as_htna(
      fixture$mcml,
      clusters = override,
      data = fixture$sequences
    )$weights
  )
})

# ---- from tests/testthat/test-build_htna-clustering.R ----

.clustering_htna_fixture <- function() {
  data <- make_grouped_data(seed = 17L)
  node_groups <- list(
    Human = c("H1", "H2", "H3"),
    AI = c("A1", "A2", "A3")
  )
  net <- suppressWarnings(build_htna(data, node_groups = node_groups))
  list(net = net, node_groups = node_groups)
}

.expect_htna_clustering_group <- function(x, actor_levels = c("Human", "AI")) {
  expect_s3_class(x, "htna_group")
  expect_s3_class(x, "netobject_group")
  expect_identical(attr(x, "actor_levels"), actor_levels)
  for (net in x) {
    expect_s3_class(net, "htna")
    expect_identical(net$actor_levels, actor_levels)
    expect_identical(as.character(net$node_groups$node),
                     as.character(net$nodes$label))
    expect_false(anyNA(net$node_groups$group))
  }
}

test_that("build_htna consumes fitted cluster_mmm output without refitting", {
  fixture <- .clustering_htna_fixture()
  fit <- Nestimate::cluster_mmm(
    fixture$net,
    k = 2,
    n_starts = 1,
    max_iter = 20,
    seed = 8
  )
  if (!inherits(fit, "net_mmm")) {
    skip(paste0(
      "Installed Nestimate materialises clusters directly from ",
      "cluster_mmm(<htna>); this test targets the raw `net_mmm` form."
    ))
  }
  expect_s3_class(fit, "net_mmm")
  weights_before <- lapply(fit$models, function(net) net$weights)

  result <- build_htna(fit)

  .expect_htna_clustering_group(result)
  expect_equal(lapply(result, function(net) net$weights), weights_before)
  expect_identical(attr(result, "clustering")$assignments,
                   fit$assignments)
  expect_equal(attr(result, "clustering")$posterior,
               fit$posterior)
  expect_equal(attr(result, "clustering")$mixing, fit$mixing)

  via_generic <- Nestimate::as_htna(fit)
  .expect_htna_clustering_group(via_generic)
  expect_equal(lapply(via_generic, function(net) net$weights), weights_before)
  expect_identical(attr(via_generic, "clustering")$assignments,
                   fit$assignments)
})
