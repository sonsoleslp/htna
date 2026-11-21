# build_htna(): the heterogeneous transition network of two or more actors' codes. Its weights and initial
# probabilities are a transition network over all codes, so they must be the model tna estimates from the same
# sessions in wide format (tna::tna / ftna / ctna / atna), whatever the actor partition; the partition itself is
# checked against the codes each actor was given. Also the other inputs build_htna() takes: a grouped build
# (tna::group_tna per group), a tna clustering (tna per cluster) and a Nestimate mcml (rebuilt at node level).
# Re-expressed from htna's equivalence/test-equiv-tna.R (weights, co-occurrence, initial distribution on eight data
# sets, htna against tna and Nestimate) and test-equiv-build-htna.R (mcml, clustering). The comparisons of htna
# with Nestimate::build_network() there are the call htna makes (internal) and are not carried over; the input
# forms of one data set compared with each other stay unit tests (tests/testthat/test-input-forms.R).
# The weight matrix and the initial probabilities have no htna accessor besides the edge list of as.data.frame(),
# which leaves out the zero cells; they are read as the fields `weights` and `initial` (API gap, FINDINGS.md).

data(human_ai)

test_that("transition probabilities and initial probabilities of human_ai equal tna::tna()", {
  skip_without_reference("tna")
  wide <- ref_wide(human_ai)
  reference <- tna::tna(wide)
  net <- build_htna(human_ai, actor_type = "actor_type")
  expect_reference(net$weights, reference$weights, reference = "tna", covers = "build_htna()",
                   quantity = "human_ai, transition probability", tolerance = 1e-12)
  expect_reference(net$initial, reference$inits, reference = "tna", covers = "build_htna()",
                   quantity = "human_ai, initial probability", tolerance = 1e-12)
  expect_reference(as.data.frame(net), subset(ref_edge_table(weight = reference$weights), weight != 0),
                   reference = "tna", covers = c("build_htna()", "as.data.frame.htna()"),
                   quantity = "human_ai, edge list", tolerance = 1e-12, key = c("from", "to"))
})

test_that("as.data.frame() tags every edge with the actors of its two codes", {
  actor_of <- ref_actor_of(split(human_ai$code, human_ai$actor_type) |> lapply(unique))
  edges <- as.data.frame(build_htna(human_ai, actor_type = "actor_type"))
  expected <- data.frame(from = edges$from, to = edges$to, source_actor = unname(actor_of[edges$from]),
                         target_actor = unname(actor_of[edges$to]), stringsAsFactors = FALSE)
  expect_reference(edges, expected, reference = "independent: the actor_type column of the data, base R",
                   covers = c("build_htna()", "as.data.frame.htna()"), quantity = "human_ai, actor of each end",
                   tolerance = 0, key = c("from", "to"))
})

test_that("transition probabilities equal the row shares of the table of consecutive pairs", {
  sequences <- ref_sequences(ref_wide(human_ai))
  from <- unlist(lapply(sequences, \(codes) utils::head(codes, -1L)))
  to <- unlist(lapply(sequences, \(codes) utils::tail(codes, -1L)))
  states <- sort(unique(human_ai$code))
  counts <- unclass(table(factor(from, states), factor(to, states)))
  firsts <- table(factor(vapply(sequences, \(codes) codes[[1L]], character(1)), states))
  net <- build_htna(human_ai, actor_type = "actor_type")
  by_hand <- "independent: table of consecutive pairs, base R"
  expect_reference(net$weights, prop.table(counts, margin = 1L), reference = by_hand, covers = "build_htna()",
                   quantity = "human_ai, transition probability", tolerance = 1e-12)
  expect_reference(build_htna(human_ai, actor_type = "actor_type", method = "frequency")$weights, counts,
                   reference = by_hand, covers = "build_htna()", quantity = "human_ai, transition count",
                   tolerance = 0)
  expect_reference(net$initial, stats::setNames(as.vector(firsts) / length(sequences), states),
                   reference = by_hand, covers = "build_htna()", quantity = "human_ai, initial probability",
                   tolerance = 1e-12)
})

test_that("every data set: transition probabilities and initial probabilities equal tna::tna()", {
  skip_without_reference("tna")
  datasets <- c(ref_datasets(), list(ref_group_regulation()))
  invisible(lapply(datasets, \(dataset) {
    reference <- tna::tna(ref_dataset_wide(dataset))
    net <- build_htna(dataset$actors)
    expect_reference(net$weights, reference$weights, reference = "tna", covers = "build_htna()",
                     quantity = sprintf("%s, transition probability", dataset$name), tolerance = 1e-12)
    expect_reference(net$initial, reference$inits, reference = "tna", covers = "build_htna()",
                     quantity = sprintf("%s, initial probability", dataset$name), tolerance = 1e-12)
  }))
})

test_that("frequency, co-occurrence and attention weights equal tna::ftna(), ctna() and atna()", {
  skip_without_reference("tna")
  datasets <- c(list(list(name = "human_ai", actors = human_ai, wide = ref_wide(human_ai))),
                lapply(c(ref_datasets(), list(ref_group_regulation())), \(dataset)
                  c(dataset, list(wide = ref_dataset_wide(dataset)))))
  methods <- list(frequency = tna::ftna, co_occurrence = tna::ctna, attention = tna::atna)
  invisible(lapply(datasets, \(dataset) lapply(names(methods), \(method) {
    net <- if (is.data.frame(dataset$actors)) {
      build_htna(dataset$actors, actor_type = "actor_type", method = method)
    } else {
      build_htna(dataset$actors, method = method)
    }
    # Attention weights are sums of exponentials of the lag (exp(-beta * lag)); the two implementations add them in
    # a different order, so their last digits differ (2.7e-12 on human_ai): 1e-10.
    expect_reference(net$weights, methods[[method]](dataset$wide)$weights, reference = "tna",
                     covers = "build_htna()", quantity = sprintf("%s, %s weight", dataset$name, method),
                     tolerance = 1e-10)
  })))
})

test_that("a grouped build gives each group the model tna::tna() estimates from that group's sessions", {
  skip_without_reference("tna")
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  per_group <- lapply(c("Early", "Late"), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    model <- tna::tna(ref_wide(subset(human_ai, session_id %in% sessions)))
    list(edges = cbind(group = phase, subset(ref_edge_table(weight = model$weights), weight != 0)),
         inits = cbind(group = phase, ref_named(model$inits, key = "state", value = "initial")))
  })
  grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
  expect_reference(as.data.frame(grp), do.call(rbind, lapply(per_group, \(g) g$edges)), reference = "tna",
                   covers = c("build_htna()", "as.data.frame.htna_group()"),
                   quantity = "human_ai by phase, edge list", tolerance = 1e-12, key = c("group", "from", "to"))
  found_inits <- do.call(rbind, lapply(names(grp), \(phase)
    cbind(group = phase, ref_named(grp[[phase]]$initial, key = "state", value = "initial"))))
  expect_reference(found_inits, do.call(rbind, lapply(per_group, \(g) g$inits)), reference = "tna",
                   covers = "build_htna()", quantity = "human_ai by phase, initial probability", tolerance = 1e-12,
                   key = c("group", "state"))
})

test_that("a tna clustering gives each cluster the model tna::tna() estimates from its sessions", {
  skip_without_reference("tna")
  wide <- ref_wide(human_ai)
  clustering <- tna::cluster_sequences(wide, k = 2)
  codes <- lapply(split(human_ai$code, human_ai$actor_type), unique)
  expected <- do.call(rbind, lapply(sort(unique(clustering$assignments)), \(k) {
    model <- tna::tna(wide[clustering$assignments == k, , drop = FALSE])
    cbind(group = paste("Cluster", k), subset(ref_edge_table(weight = model$weights), weight != 0))
  }))
  expect_reference(as.data.frame(build_htna(clustering, node_groups = codes)), expected, reference = "tna",
                   covers = c("build_htna()", "as.data.frame.htna_group()"),
                   quantity = "human_ai, two tna clusters, edge list", tolerance = 1e-12, key = c("group", "from", "to"))
})

test_that("a Nestimate mcml is rebuilt as the node-level model of tna::tna()", {
  skip_without_reference("tna")
  wide <- ref_wide(human_ai)
  codes <- lapply(split(human_ai$code, human_ai$actor_type), unique)
  reference <- tna::tna(wide)
  net <- build_htna(Nestimate::build_mcml(wide, codes))
  expect_reference(net$weights, reference$weights, reference = "tna", covers = "build_htna()",
                   quantity = "human_ai mcml, transition probability", tolerance = 1e-12)
  expect_reference(net$initial, reference$inits, reference = "tna", covers = "build_htna()",
                   quantity = "human_ai mcml, initial probability", tolerance = 1e-12)
})
