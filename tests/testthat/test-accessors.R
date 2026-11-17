# Tidy accessors, summaries and print methods added in 0.3.5.

# ---- as.data.frame.htna ------------------------------------------------------

test_that("as.data.frame(net) lists every non-zero edge with its actors", {
  net <- make_htna()
  edges <- as.data.frame(net)

  expect_s3_class(edges, "data.frame")
  expect_named(edges, c("from", "to", "weight", "source_actor",
                        "target_actor"))
  # one row per non-zero cell, diagonal included
  expect_equal(nrow(edges), sum(net$weights != 0))
  # weights read back from the matrix, cell by cell
  expect_equal(edges$weight,
               net$weights[cbind(edges$from, edges$to)])
  # actors follow the partition
  lookup <- setNames(net$node_groups$group, net$node_groups$node)
  expect_equal(as.character(edges$source_actor), unname(lookup[edges$from]))
  expect_equal(as.character(edges$target_actor), unname(lookup[edges$to]))
  expect_identical(levels(edges$source_actor), net$actor_levels)
})

test_that("as.data.frame(net) on a hand-built network matches a hand count", {
  d <- data.frame(session_id = rep("S", 5),
                  code = c("H1", "A1", "H1", "A1", "A1"),
                  order_in_session = 1:5, stringsAsFactors = FALSE)
  net <- build_htna(d, node_groups = list(Human = "H1", AI = "A1"),
                    method = "frequency")
  edges <- as.data.frame(net)
  # transitions: H1->A1 (2), A1->H1 (1), A1->A1 (1)
  expected <- data.frame(from = c("A1", "A1", "H1"),
                         to = c("A1", "H1", "A1"),
                         weight = c(1, 1, 2))
  ord <- order(edges$from, edges$to)
  expect_equal(edges$from[ord], expected$from)
  expect_equal(edges$to[ord], expected$to)
  expect_equal(edges$weight[ord], expected$weight)
})

test_that("as.data.frame(net, what = 'nodes') returns one row per node", {
  net <- make_htna()
  nodes <- as.data.frame(net, what = "nodes")
  expect_named(nodes, c("node", "actor"))
  expect_equal(nrow(nodes), nrow(net$weights))
  expect_setequal(nodes$node, rownames(net$weights))
})

test_that("as.data.frame() on an htna_group stacks cohorts with a group column", {
  grp <- make_htna_group()
  stacked <- as.data.frame(grp)
  expect_identical(names(stacked)[1L], "group")
  expect_equal(nrow(stacked),
               sum(vapply(grp, function(n) sum(n$weights != 0), numeric(1))))
  expect_setequal(unique(stacked$group), names(grp))
  expect_error(as.data.frame(grp, what = "bogus"))
})

# ---- summary.htna -------------------------------------------------------------

test_that("summary() returns an object and prints nothing until printed", {
  net <- make_htna()
  expect_silent(s <- summary(net))
  expect_s3_class(s, "summary.htna")
  out <- capture.output(print(s))
  expect_true(any(grepl("Actor types (2)", out, fixed = TRUE)))
})

test_that("as.data.frame(summary(net)) tables match the network", {
  net <- make_htna()
  s <- summary(net)

  pairs <- as.data.frame(s, what = "actor_pairs")
  expect_named(pairs, c("source_actor", "target_actor", "n_edges"))
  expect_equal(nrow(pairs), length(net$actor_levels)^2)
  # invariant: the blocks partition the non-zero edges
  expect_equal(sum(pairs$n_edges), sum(net$weights != 0))
  # block counts equal the counts of the edge table
  edges <- as.data.frame(net)
  by_hand <- as.vector(table(edges$source_actor, edges$target_actor))
  expect_equal(pairs$n_edges[order(pairs$target_actor, pairs$source_actor)],
               by_hand)

  actors <- as.data.frame(s)
  expect_named(actors, c("actor", "n_nodes", "nodes"))
  metrics <- as.data.frame(s, what = "metrics")
  expect_identical(class(metrics), "data.frame")
  expect_true(all(c("metric", "value") %in% names(metrics)))
})

test_that("summary() of an htna_group stacks with a group column", {
  grp <- make_htna_group()
  s <- summary(grp)
  expect_s3_class(s, "summary.htna_group")
  pairs <- as.data.frame(s, what = "actor_pairs")
  expect_identical(names(pairs)[1L], "group")
  expect_equal(nrow(pairs), length(grp) * 4L)
})

test_that("summary() without an actor partition raises a classed error", {
  net <- make_htna()
  net$node_groups <- net$node_groups[0, ]
  expect_error(summary(net), class = "htna_unassigned_node")
  expect_error(summary(net), class = "htna_error")
})

# ---- htna_comparison ----------------------------------------------------------

test_that("compare_htna() results have print, summary and as.data.frame", {
  pair <- make_htna_pair()
  cmp <- compare_htna(pair$ctrl, pair$exp)

  out <- capture.output(print(cmp))
  expect_true(any(grepl("Actor-pair blocks", out, fixed = TRUE)))
  expect_false(any(grepl("$", out, fixed = TRUE)))

  pairs <- summary(cmp)
  expect_identical(class(pairs), "data.frame")
  expect_equal(nrow(pairs), 4L)
  expect_identical(as.data.frame(cmp), pairs)

  edges <- as.data.frame(cmp, what = "edges")
  expect_true(all(c("source", "target", "weight_x", "weight_y",
                    "source_actor", "target_actor") %in% names(edges)))
  # invariant: the actor-pair blocks partition the aligned cells
  expect_equal(sum(pairs$n_cells), nrow(edges))
  # mean difference of each block recomputed from the edge table
  key <- paste(edges$source_actor, edges$target_actor)
  by_hand <- tapply(edges$weight_x - edges$weight_y, key, mean)
  expect_equal(as.vector(by_hand[paste(pairs$source_actor, pairs$target_actor)]),
               pairs$mean_difference)

  metrics <- as.data.frame(cmp, what = "metrics")
  expect_named(metrics, c("category", "metric", "value"))
  network <- as.data.frame(cmp, what = "network")
  expect_named(network, c("metric", "x", "y"))
})

test_that("a grouped comparison prints per pair and stacks its tables", {
  grp <- make_htna_group()
  cmp <- compare_htna(grp)
  out <- capture.output(print(cmp))
  expect_true(any(grepl("--- Control vs Experimental ---", out, fixed = TRUE)))
  expect_false(any(grepl("attr(", out, fixed = TRUE)))

  stacked <- summary(cmp)
  expect_identical(names(stacked)[1L], "comparison")
  expect_equal(nrow(stacked), 4L)
  expect_identical(unique(stacked$comparison), "Control vs Experimental")
  stacked_edges <- as.data.frame(cmp, what = "edges")
  expect_equal(nrow(stacked_edges), nrow(as.data.frame(cmp[[1]],
                                                       what = "edges")))
})

# ---- htna_stability -----------------------------------------------------------

test_that("as.data.frame() returns the CS coefficients and correlations", {
  net <- make_htna()
  cs <- centrality_stability_htna(net, iter = 10, seed = 1)

  coef <- as.data.frame(cs)
  expect_named(coef, c("measure", "cs"))
  expect_equal(coef$cs, unname(cs$cs))

  cors <- as.data.frame(cs, what = "correlations")
  expect_identical(class(cors), "data.frame")
  expect_named(cors, c("measure", "drop_prop", "mean_cor", "sd_cor",
                       "prop_above"))
  expect_equal(nrow(cors), length(cs$cs) * length(cs$drop_prop))
})

test_that("grouped stability results stack with a group column", {
  grp <- make_htna_group()
  cs <- centrality_stability_htna(grp, iter = 10, seed = 1)
  coef <- as.data.frame(cs)
  expect_identical(names(coef)[1L], "group")
  expect_equal(nrow(coef), length(grp) * 3L)
  cors <- summary(cs)
  expect_identical(names(cors)[1L], "group")
  expect_identical(cors, as.data.frame(cs, what = "correlations"))
  out <- capture.output(print(cs))
  expect_equal(sum(grepl("CS-coefficients", out, fixed = TRUE)), length(grp))
})
