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

test_that("build_htna(mcml) returns one global htna with the mcml clusters as actors", {
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

    expect_s3_class(actual, "htna")
    expect_false(inherits(actual, "htna_group"))
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
})

test_that("build_htna(mcml) validates incompatible constructor arguments", {
  fixture <- .mcml_htna_fixture()

  expect_error(
    build_htna(fixture$mcml, actor_type = "actor_type"),
    "node clusters are the actor groups"
  )
  expect_error(
    build_htna(fixture$mcml, group = "condition"),
    "one global HTNA"
  )
  expect_error(
    build_htna(fixture$mcml, disambiguate = TRUE),
    "before fitting"
  )
  expect_error(
    build_htna(data.frame(x = 1), source_data = fixture$sequences),
    "only used when.*mcml"
  )
})
