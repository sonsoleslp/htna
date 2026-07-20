test_that("as.igraph.htna preserves weights, names, and actor metadata", {
  net <- make_htna(seed = 111L)
  graph <- igraph::as.igraph(net)

  expect_s3_class(graph, "igraph")
  expect_true(igraph::is_directed(graph))
  expect_equal(igraph::vcount(graph), nrow(net$weights))
  expect_equal(igraph::ecount(graph), sum(net$weights != 0))
  expect_identical(igraph::V(graph)$name, rownames(net$weights))

  recovered <- as.matrix(igraph::as_adjacency_matrix(
    graph, attr = "weight", sparse = FALSE
  ))
  expect_equal(recovered, net$weights[rownames(recovered), colnames(recovered)])

  lookup <- setNames(net$node_groups$group, net$node_groups$node)
  expect_identical(
    igraph::vertex_attr(graph, "actor_type"),
    unname(lookup[igraph::V(graph)$name])
  )
  expect_identical(igraph::graph_attr(graph, "actor_levels"),
                   net$actor_levels)
  expect_identical(igraph::graph_attr(graph, "htna_method"), net$method)
})

test_that("as.igraph.htna respects loop, weight, and mode controls", {
  net <- make_htna(seed = 121L)
  diag(net$weights) <- seq_len(nrow(net$weights)) / 10

  without_loops <- igraph::as.igraph(net, diag = FALSE)
  expect_false(igraph::any_loop(without_loops))
  expect_equal(igraph::ecount(without_loops),
               sum(net$weights != 0) - sum(diag(net$weights) != 0))

  unweighted <- igraph::as.igraph(net, weighted = FALSE)
  expect_false("weight" %in% igraph::edge_attr_names(unweighted))

  undirected <- igraph::as.igraph(net, mode = "plus")
  expect_false(igraph::is_directed(undirected))
  expect_identical(igraph::vertex_attr(undirected, "actor_type"),
                   igraph::vertex_attr(unweighted, "actor_type"))
})

test_that("co-occurrence HTNA converts to an undirected actor-aware graph", {
  d <- data.frame(
    session_id = rep(paste0("S", 1:4), each = 3L),
    code = c("H1", "A1", "H2", "A1", "H2", "A2",
             "H2", "A2", "H1", "A2", "H1", "A1"),
    order_in_session = rep(1:3, 4L),
    stringsAsFactors = FALSE
  )
  net <- suppressWarnings(build_htna(
    d,
    node_groups = list(Human = c("H1", "H2"), AI = c("A1", "A2")),
    method = "co_occurrence"
  ))
  graph <- igraph::as.igraph(net)

  expect_false(net$directed)
  expect_true(isSymmetric(net$weights))
  expect_false(igraph::is_directed(graph))
  expect_setequal(igraph::vertex_attr(graph, "actor_type"), c("Human", "AI"))
})

test_that("as.igraph.htna_group converts every cohort", {
  group <- make_htna_group(seed = 131L)
  graphs <- igraph::as.igraph(group)

  expect_s3_class(graphs, "htna_igraph_group")
  expect_named(graphs, names(group))
  expect_equal(length(graphs), length(group))
  expect_identical(attr(graphs, "actor_levels"), c("Human", "AI"))
  for (i in seq_along(graphs)) {
    expect_s3_class(graphs[[i]], "igraph")
    expect_identical(igraph::graph_attr(graphs[[i]], "cohort"), names(group)[i])
    expect_identical(igraph::graph_attr(graphs[[i]], "actor_levels"),
                     group[[i]]$actor_levels)
    expect_identical(
      igraph::vertex_attr(graphs[[i]], "actor_type"),
      unname(setNames(group[[i]]$node_groups$group,
                      group[[i]]$node_groups$node)[igraph::V(graphs[[i]])$name])
    )
  }

  empty <- structure(list(), class = c("htna_group", "netobject_group", "list"),
                     actor_levels = c("Human", "AI"))
  empty_graphs <- igraph::as.igraph(empty)
  expect_s3_class(empty_graphs, "htna_igraph_group")
  expect_length(empty_graphs, 0L)
  expect_identical(attr(empty_graphs, "actor_levels"), c("Human", "AI"))
})

test_that("as.igraph.htna validates malformed network state and arguments", {
  net <- make_htna(seed = 141L)

  missing_partition <- net
  missing_partition$node_groups <- NULL
  expect_error(igraph::as.igraph(missing_partition), "actor partition")

  missing_node <- net
  missing_node$node_groups <- missing_node$node_groups[-1L, ]
  expect_error(igraph::as.igraph(missing_node), "without actor assignments")

  unnamed <- net
  dimnames(unnamed$weights) <- NULL
  expect_error(igraph::as.igraph(unnamed), "named numeric square matrix")

  expect_error(igraph::as.igraph(net, mode = NA_character_), "`mode`")
  expect_error(igraph::as.igraph(net, weighted = NA), "`weighted`")
  expect_error(igraph::as.igraph(net, diag = NA), "`diag`")
})
