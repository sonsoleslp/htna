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

test_that("build_htna consumes Nestimate distance-clustering results", {
  fixture <- .clustering_htna_fixture()
  clustering <- Nestimate::build_clusters(fixture$net, k = 2, seed = 4)

  result <- build_htna(clustering, node_groups = fixture$node_groups)

  .expect_htna_clustering_group(result)
  expect_s3_class(attr(result, "clustering"), "net_clustering")
  expect_identical(attr(result, "clustering")$assignments,
                   clustering$assignments)
  expect_identical(vapply(result, function(net) nrow(net$data), integer(1L)),
                   setNames(as.integer(clustering$sizes), names(result)))
})

test_that("build_htna consumes fitted cluster_mmm output without refitting", {
  fixture <- .clustering_htna_fixture()
  fit <- Nestimate::cluster_mmm(
    fixture$net,
    k = 2,
    n_starts = 1,
    max_iter = 20,
    seed = 8
  )
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

test_that("build_htna consumes legacy tna::cluster_sequences output", {
  skip_if_not_installed("tna")
  fixture <- .clustering_htna_fixture()
  sequence_data <- Nestimate::build_clusters(
    fixture$net, k = 2, seed = 2
  )$data
  clustering <- tna::cluster_sequences(sequence_data, k = 2)

  result <- build_htna(clustering, node_groups = fixture$node_groups)

  .expect_htna_clustering_group(result)
  expect_s3_class(attr(result, "clustering"), "tna_clustering")
  expect_identical(attr(result, "clustering")$assignments,
                   clustering$assignments)
})

test_that("build_htna uses a partition preserved by Nestimate", {
  fixture <- .clustering_htna_fixture()
  clustered <- Nestimate::cluster_network(fixture$net, k = 2, seed = 3)

  if (!inherits(clustered, "htna_group")) {
    skip("Installed Nestimate predates HTNA partition preservation.")
  }
  result <- build_htna(clustered)

  .expect_htna_clustering_group(result)
  expect_s3_class(attr(result, "clustering"), "net_clustering")
})

test_that("cluster conversion requires an actor partition", {
  fixture <- .clustering_htna_fixture()
  clustering <- Nestimate::build_clusters(fixture$net$data, k = 2, seed = 1)

  expect_error(
    build_htna(clustering),
    "no preserved HTNA actor partition"
  )
})

test_that("cluster conversion rejects a second outer grouping", {
  fixture <- .clustering_htna_fixture()
  clustering <- Nestimate::build_clusters(fixture$net, k = 2, seed = 1)

  expect_error(
    build_htna(clustering, node_groups = fixture$node_groups,
               group = "condition"),
    "already defines the outer groups"
  )
})
