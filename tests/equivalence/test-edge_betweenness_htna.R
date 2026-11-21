# edge_betweenness_htna(): the betweenness of every edge of an htna network (path length 1 / weight) against
# tna::betweenness_network() on the same sessions. Re-expressed from htna's equivalence/test-equiv-tna.R ("edge
# betweenness matches between htna and tna", eight data sets). The result is a network whose weights are the
# scores; they are read from the field `weights` (the zero cells are not in the edge list) and from the edge list
# of as.data.frame().

data(human_ai)

test_that("edge betweenness of human_ai equals tna::betweenness_network()", {
  skip_without_reference("tna")
  reference <- tna::betweenness_network(tna::tna(ref_wide(human_ai)))
  found <- edge_betweenness_htna(build_htna(human_ai, actor_type = "actor_type"))
  expect_reference(found$weights, reference$weights, reference = "tna", covers = "edge_betweenness_htna()",
                   quantity = "human_ai, edge betweenness", tolerance = 1e-10)
  expect_reference(as.data.frame(found), subset(ref_edge_table(weight = reference$weights), weight != 0),
                   reference = "tna", covers = c("edge_betweenness_htna()", "as.data.frame.htna()"),
                   quantity = "human_ai, edge list of the scores", tolerance = 1e-10, key = c("from", "to"))
})

test_that("every data set: edge betweenness equals tna::betweenness_network()", {
  skip_without_reference("tna")
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    reference <- tna::betweenness_network(tna::tna(ref_dataset_wide(dataset)))
    expect_reference(edge_betweenness_htna(build_htna(dataset$actors))$weights, reference$weights, reference = "tna",
                     covers = "edge_betweenness_htna()", quantity = dataset$name, tolerance = 1e-10)
  }))
})
