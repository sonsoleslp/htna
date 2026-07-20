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
