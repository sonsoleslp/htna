# summary() of an htna network and of a grouped network, and their as.data.frame() tables: the codes of each actor,
# the number of edges in each actor block, and the network metrics. The actor tables are counted from the data
# and from tna's weights in base R; the metric table is compared with tna's summary() of the same model. tna 1.3.1
# reports the SD of the column sums as "SD Out-Strength" and of the row sums as "SD In-Strength" (Nestimate's
# suite, FINDINGS.md); htna's table (Nestimate's summary) has them the other way round, which is the definition,
# so those two rows are compared with the row and column sums computed in the test, and with tna as a known
# difference. Re-expressed from htna's equivalence/test-equiv-delegation.R (summary.htna against Nestimate's
# summary of the same network: the call htna makes, not carried over).

data(human_ai)
codes <- lapply(split(human_ai$code, human_ai$actor_type), \(c) sort(unique(c)))
actor_of <- ref_actor_of(codes)
by_hand <- "independent: counts from the data and tna's weights, base R"

# Edges in each (source actor, target actor) block of a weight matrix.
ref_actor_blocks <- function(weights, actor_of, actors) {
  edges <- subset(ref_edge_table(weight = weights), weight != 0)
  blocks <- expand.grid(target_actor = actors, source_actor = actors, stringsAsFactors = FALSE)
  blocks$n_edges <- vapply(seq_len(nrow(blocks)), \(i)
    sum(actor_of[edges$from] == blocks$source_actor[[i]] & actor_of[edges$to] == blocks$target_actor[[i]]),
    numeric(1))
  blocks
}

test_that("the actors' codes and the edges of each actor block are counted from the data and tna's weights", {
  skip_without_reference("tna")
  weights <- tna::tna(ref_wide(human_ai))$weights
  found <- summary(build_htna(human_ai, actor_type = "actor_type"))
  actors <- data.frame(actor = names(codes), n_nodes = lengths(codes),
                       nodes = vapply(codes, paste, character(1), collapse = ", "), stringsAsFactors = FALSE)
  expect_reference(as.data.frame(found), actors, reference = by_hand,
                   covers = c("summary.htna()", "as.data.frame.summary.htna()"), quantity = "human_ai, actors",
                   tolerance = 0, key = "actor")
  expect_reference(as.data.frame(found, what = "actor_pairs"), ref_actor_blocks(weights, actor_of, names(codes)),
                   reference = by_hand, covers = c("summary.htna()", "as.data.frame.summary.htna()"),
                   quantity = "human_ai, edges per actor block", tolerance = 0, key = c("source_actor", "target_actor"))
})

test_that("network metrics equal tna's summary(); the SD of the strengths follows the definition", {
  skip_without_reference("tna")
  model <- tna::tna(ref_wide(human_ai))
  reference <- as.data.frame(summary(model))
  found <- as.data.frame(summary(build_htna(human_ai, actor_type = "actor_type")), what = "metrics")
  sd_rows <- c("SD Out-Strength", "SD In-Strength")
  expect_reference(subset(found, !metric %in% sd_rows), subset(reference, !metric %in% sd_rows), reference = "tna",
                   covers = c("summary.htna()", "as.data.frame.summary.htna()"), quantity = "human_ai, network metrics",
                   tolerance = 1e-10, key = "metric")
  strengths <- data.frame(metric = sd_rows, value = c(stats::sd(rowSums(model$weights)),
                                                      stats::sd(colSums(model$weights))))
  expect_reference(subset(found, metric %in% sd_rows), strengths,
                   reference = "independent: SD of the row (out) and column (in) sums of tna's weights",
                   covers = "summary.htna()", quantity = "human_ai, SD of the strengths", tolerance = 1e-10, key = "metric")
  expect_known_difference(subset(found, metric %in% sd_rows), subset(reference, metric %in% sd_rows),
                          reference = "tna", covers = "summary.htna()", quantity = "human_ai, SD of the strengths",
                          tolerance = 1e-10, key = "metric",
                          reason = "tna 1.3.1 reports the SD of the column sums as out-strength and of the row sums as in-strength; htna (Nestimate's summary) reports the SD of the row sums as out-strength.")
})

test_that("a grouped network: each group's actor blocks are counted from tna's weights of its sessions", {
  skip_without_reference("tna")
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  expected <- do.call(rbind, lapply(c("Early", "Late"), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    weights <- tna::tna(ref_wide(subset(human_ai, session_id %in% sessions)))$weights
    cbind(group = phase, ref_actor_blocks(weights, actor_of, names(codes)))
  }))
  found <- summary(build_htna(human_ai, actor_type = "actor_type", group = "phase"))
  expect_reference(as.data.frame(found, what = "actor_pairs"), expected, reference = by_hand,
                   covers = c("summary.htna_group()", "as.data.frame.summary.htna_group()"),
                   quantity = "human_ai by phase, edges per actor block", tolerance = 0,
                   key = c("group", "source_actor", "target_actor"))
})

test_that("as.igraph() holds tna's weights as edge weights and each code's actor as a vertex attribute", {
  skip_without_reference("tna")
  model <- tna::tna(ref_wide(human_ai))
  graph <- igraph::as.igraph(build_htna(human_ai, actor_type = "actor_type"))
  expect_reference(igraph::as_adjacency_matrix(graph, attr = "weight", sparse = FALSE), model$weights,
                   reference = "tna", covers = "as.igraph.htna()", quantity = "human_ai, adjacency weights",
                   tolerance = 1e-12)
  expect_reference(stats::setNames(igraph::V(graph)$actor_type, igraph::V(graph)$name), actor_of,
                   reference = by_hand, covers = "as.igraph.htna()", quantity = "human_ai, actor of each vertex",
                   tolerance = 0)
})

test_that("as.igraph() of a grouped network holds each group's tna weights", {
  skip_without_reference("tna")
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  expected <- do.call(rbind, lapply(c("Early", "Late"), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    cbind(group = phase, ref_edge_table(weight = tna::tna(ref_wide(subset(human_ai, session_id %in% sessions)))$weights))
  }))
  graphs <- igraph::as.igraph(build_htna(human_ai, actor_type = "actor_type", group = "phase"))
  found <- do.call(rbind, lapply(names(graphs), \(phase)
    cbind(group = phase, ref_edge_table(weight = igraph::as_adjacency_matrix(graphs[[phase]], attr = "weight",
                                                                             sparse = FALSE)))))
  expect_reference(found, expected, reference = "tna", covers = "as.igraph.htna_group()",
                   quantity = "human_ai by phase, adjacency weights", tolerance = 1e-12, key = c("group", "from", "to"))
})
