# compare_htna(): the comparison of two htna networks (edge-level differences, 22 summary metrics, network
# metrics) with htna's own table of the differences per actor block. The two weight matrices are tna's models of
# the same sessions. The edge differences and the actor-block table are written from their definitions in base R;
# the summary metrics are compared with tna::compare(), with the two conventions Nestimate's suite established for
# compare_model() (which htna calls) as known differences: "Distance" (Nestimate dCor, tna 1.3.1 dCor squared) and
# Canberra (Nestimate counts every cell but 0 / 0, tna leaves out cells that are zero in either network). Rank
# Agreement depends on the order of the nodes, and compare_htna() orders them by actor: the summary metrics are
# compared in htna's order, and in tna's alphabetical order as a known difference.
# Re-expressed from htna's equivalence/test-equiv-delegation.R (compare_htna() against Nestimate::compare_model()
# through an unexported helper; a test of the call htna makes), which is kept as one internal comparison.

data(human_ai)
phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
early <- subset(human_ai, session_id %in% names(phase_of)[phase_of == "Early"])
late <- subset(human_ai, session_id %in% names(phase_of)[phase_of == "Late"])
actor_of <- ref_actor_of(lapply(split(human_ai$code, human_ai$actor_type), unique))
by_hand <- "independent: differences of tna's two weight matrices, base R"
conventions <- c("Distance", "Canberra")

ref_differences <- function(model_x, model_y) {
  edges <- ref_edge_table(weight_x = model_x$weights, weight_y = model_y$weights)
  names(edges)[names(edges) == "from"] <- "source"
  names(edges)[names(edges) == "to"] <- "target"
  edges$raw_difference <- edges$weight_x - edges$weight_y
  edges$absolute_difference <- abs(edges$raw_difference)
  edges$squared_difference <- edges$raw_difference^2
  edges$source_actor <- unname(actor_of[edges$source])
  edges$target_actor <- unname(actor_of[edges$target])
  edges
}

ref_actor_pair_table <- function(edges, actors) {
  blocks <- expand.grid(target_actor = actors, source_actor = actors, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(blocks)), \(i) {
    block <- subset(edges, source_actor == blocks$source_actor[[i]] & target_actor == blocks$target_actor[[i]])
    data.frame(source_actor = blocks$source_actor[[i]], target_actor = blocks$target_actor[[i]],
               n_cells = nrow(block), n_nonzero_x = sum(block$weight_x != 0), n_nonzero_y = sum(block$weight_y != 0),
               n_changed = sum(block$absolute_difference > 0), mean_difference = mean(block$raw_difference),
               mean_absolute_difference = mean(block$absolute_difference),
               rms_difference = sqrt(mean(block$squared_difference)),
               max_absolute_difference = max(block$absolute_difference), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

test_that("edge differences and the actor-block table follow from tna's two weight matrices", {
  skip_without_reference("tna")
  model_x <- tna::tna(ref_wide(early))
  model_y <- tna::tna(ref_wide(late))
  edges <- ref_differences(model_x, model_y)
  found <- compare_htna(build_htna(early, actor_type = "actor_type"), build_htna(late, actor_type = "actor_type"))
  expect_reference(as.data.frame(found, what = "edges"), edges, reference = by_hand, covers = "compare_htna()",
                   quantity = "human_ai Early vs Late, edge differences", tolerance = 1e-12,
                   key = c("source", "target"))
  expected <- ref_actor_pair_table(edges, c("Human", "AI"))
  expect_reference(as.data.frame(found), expected, reference = by_hand,
                   covers = c("compare_htna()", "as.data.frame.htna_comparison()"),
                   quantity = "human_ai Early vs Late, actor blocks", tolerance = 1e-12,
                   key = c("source_actor", "target_actor"))
  expect_reference(summary(found), expected, reference = by_hand, covers = "summary.htna_comparison()",
                   quantity = "human_ai Early vs Late, actor blocks, summary()", tolerance = 1e-12,
                   key = c("source_actor", "target_actor"))
})

test_that("a grouped network compares its two groups the same way", {
  skip_without_reference("tna")
  edges <- ref_differences(tna::tna(ref_wide(early)), tna::tna(ref_wide(late)))
  expected <- cbind(comparison = "Early vs Late", ref_actor_pair_table(edges, c("Human", "AI")))
  found <- compare_htna(build_htna(human_ai, actor_type = "actor_type", group = "phase"))
  expect_reference(summary(found), expected, reference = by_hand,
                   covers = c("compare_htna()", "summary.htna_comparison_group()"),
                   quantity = "human_ai by phase, actor blocks", tolerance = 1e-12,
                   key = c("comparison", "source_actor", "target_actor"))
  expect_reference(as.data.frame(found), expected, reference = by_hand,
                   covers = c("compare_htna()", "as.data.frame.htna_comparison_group()"),
                   quantity = "human_ai by phase, actor blocks, as.data.frame()", tolerance = 1e-12,
                   key = c("comparison", "source_actor", "target_actor"))
})

test_that("summary metrics equal tna::compare() of the matrices in htna's node order", {
  skip_without_reference("tna")
  model_x <- tna::tna(ref_wide(early))
  model_y <- tna::tna(ref_wide(late))
  # compare_htna() orders the nodes by actor type (in the order of the actor levels), then by name in the C locale.
  in_htna_order <- unlist(lapply(c("Human", "AI"), \(actor) sort(names(actor_of)[actor_of == actor], method = "radix")))
  reference <- as.data.frame(tna::compare(model_x$weights[in_htna_order, in_htna_order],
                                          model_y$weights[in_htna_order, in_htna_order])$summary_metrics)
  found <- as.data.frame(compare_htna(build_htna(early, actor_type = "actor_type"),
                                      build_htna(late, actor_type = "actor_type")), what = "metrics")
  expect_reference(subset(found, !metric %in% conventions, select = c(metric, value)),
                   subset(reference, !metric %in% conventions, select = c(metric, value)), reference = "tna",
                   covers = "compare_htna()", quantity = "human_ai Early vs Late, summary metrics", tolerance = 1e-10,
                   key = "metric")
  expect_known_difference(subset(found, metric %in% conventions, select = c(metric, value)),
                          subset(reference, metric %in% conventions, select = c(metric, value)), reference = "tna",
                          covers = "compare_htna()", quantity = "human_ai Early vs Late, Distance and Canberra",
                          tolerance = 1e-10, key = "metric",
                          reason = "Nestimate's compare_model() (which htna calls) returns the distance correlation dCor where tna 1.3.1 returns dCor squared, and sums the Canberra terms over every cell but the 0 / 0 ones where tna leaves out every cell that is zero in either network (Nestimate's suite).")
})

test_that("Rank Agreement depends on the order of the nodes; htna's order is by actor", {
  skip_without_reference("tna")
  model_x <- tna::tna(ref_wide(early))
  model_y <- tna::tna(ref_wide(late))
  reference <- as.data.frame(tna::compare(model_x, model_y)$summary_metrics)
  found <- as.data.frame(compare_htna(build_htna(early, actor_type = "actor_type"),
                                      build_htna(late, actor_type = "actor_type")), what = "metrics")
  expect_known_difference(subset(found, metric == "Rank Agreement", select = c(metric, value)),
                          subset(reference, metric == "Rank Agreement", select = c(metric, value)), reference = "tna",
                          covers = "compare_htna()", quantity = "human_ai Early vs Late, Rank Agreement, tna's order",
                          tolerance = 1e-10, key = "metric",
                          reason = "Rank Agreement is the share of agreeing signs of consecutive differences of the vectorised matrices (tna's definition, kept by Nestimate), so it depends on the order of the nodes. compare_htna() orders them by actor type, tna::compare() of two models keeps their (alphabetical) order.",
                          agrees_with = "tna::compare() of the matrices in htna's node order")
})

test_that("every table equals Nestimate::compare_model() of the two weight matrices in htna's node order", {
  x <- build_htna(early, actor_type = "actor_type")
  y <- build_htna(late, actor_type = "actor_type")
  in_htna_order <- unlist(lapply(c("Human", "AI"), \(actor) sort(names(actor_of)[actor_of == actor], method = "radix")))
  expected <- Nestimate::compare_model(x$weights[in_htna_order, in_htna_order], y$weights[in_htna_order, in_htna_order])
  found <- compare_htna(x, y)
  expect_reference(as.data.frame(found, what = "metrics"), as.data.frame(expected$summary_metrics),
                   reference = "Nestimate", covers = "compare_htna()",
                   quantity = "human_ai Early vs Late, summary metrics", tolerance = 1e-12, key = "metric")
})
