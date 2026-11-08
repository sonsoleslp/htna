test_that("summary.htna prints a network metric table and leaves its input unchanged", {
  for (seed in c(151L, 157L, 163L, 167L)) {
    net <- make_htna(seed)
    net_before <- net

    output <- capture.output(actual <- summary(net))
    expect_true(any(grepl("Network metrics:", output, fixed = TRUE)))
    expect_identical(net, net_before)
  }
})

test_that("summary.htna network metrics work for every construction method", {
  d <- make_grouped_data(seed = 171L)
  node_groups <- list(Human = c("H1", "H2", "H3"),
                      AI = c("A1", "A2", "A3"))
  for (method in c("relative", "frequency", "co_occurrence", "attention")) {
    net <- suppressWarnings(build_htna(d, node_groups = node_groups,
                                       method = method))
    actual <- capture.output(result <- summary(net))

    expect_equal(result$network_metrics$value[
      result$network_metrics$metric == "Node Count"
    ], nrow(net$weights), info = method)
    # Nestimate counts an undirected edge once since 1.0.7, every cell before.
    each_pair_once <- !net$directed &&
      utils::packageVersion("Nestimate") >= "1.0.7"
    expect_equal(result$network_metrics$value[
      result$network_metrics$metric == "Edge Count"
    ], sum(net$weights[if (each_pair_once) upper.tri(net$weights, diag = TRUE)
                       else TRUE] > 0),
    info = method)
    expect_true(any(grepl("Network Density", actual, fixed = TRUE)),
                info = method)
  }
})

test_that("summary.htna_group includes independent metrics for every cohort", {
  group <- make_htna_group(seed = 181L)
  output <- capture.output(result <- summary(group))

  expect_named(result, names(group))
  expect_equal(sum(grepl("Network metrics:", output, fixed = TRUE)),
               length(group))
  for (cohort in names(group)) {
    expect_equal(result[[cohort]]$n_nodes, nrow(group[[cohort]]$weights))
    expect_equal(result[[cohort]]$n_edges,
                 sum(group[[cohort]]$weights != 0))
  }
})

test_that("summary network metrics remain correct with partial actor coverage", {
  d <- data.frame(
    session_id = c(rep("A", 4), rep("B", 5)),
    code = c("H1", "H2", "H1", "H2",
             "H1", "A1", "A2", "H2", "A1"),
    order_in_session = c(1:4, 1:5),
    cohort = c(rep("HumanOnly", 4), rep("Mixed", 5)),
    stringsAsFactors = FALSE
  )
  group <- suppressWarnings(build_htna(
    d,
    node_groups = list(Human = c("H1", "H2"), AI = c("A1", "A2")),
    group = "cohort"
  ))
  result <- capture.output(summaries <- summary(group))

  expect_identical(group$HumanOnly$actor_levels, c("Human", "AI"))
  expect_setequal(summaries$HumanOnly$actors$actor, c("Human", "AI"))
  expect_equal(
    summaries$HumanOnly$network_metrics$value[
      summaries$HumanOnly$network_metrics$metric == "Node Count"
    ],
    nrow(group$HumanOnly$weights)
  )
  expect_equal(summaries$HumanOnly$actors$n_nodes, c(2L, 0L))
})
