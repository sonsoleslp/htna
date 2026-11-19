# Start and end states, cohorts that lack a code, grouped frequencies, a
# grouping column in sequence_compare_htna(), and tied latency times.

node_groups_6 <- list(Human = c("H1", "H2", "H3"), AI = c("A1", "A2", "A3"))

# Two cohorts; the Control cohort never uses the code A3.
make_uneven_data <- function(seed = 1L) {
  d <- make_grouped_data(seed)
  d <- d[!(d$condition == "Control" & d$code == "A3"), ]
  d$order_in_session <- stats::ave(d$order_in_session, d$session_id,
                                   FUN = seq_along)
  d$actor_type <- ifelse(d$code %in% node_groups_6$Human, "Human", "AI")
  d
}

# ---- 1. start and end states ----------------------------------------------

test_that("start and end states form the actor type Boundary", {
  d <- make_grouped_data(seed = 61L)
  net <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                     start = TRUE, end = "Finish"))
  expect_identical(net$actor_levels, c("Human", "AI", "Boundary"))
  expect_false(anyNA(net$node_groups$group))
  expect_false(anyNA(net$nodes$groups))
  nodes <- as.data.frame(net, what = "nodes")
  expect_identical(
    as.character(subset(nodes, node %in% c("Start", "Finish"))$actor),
    c("Boundary", "Boundary")
  )
  expect_setequal(subset(nodes, actor == "Boundary")$node,
                  c("Start", "Finish"))

  # every session starts once and ends once
  edges <- as.data.frame(net)
  expect_equal(sum(subset(edges, from == "Start")$weight), 1)
  expect_true(all(subset(edges, to == "Finish")$source_actor != "Boundary"))

  s <- summary(net)
  expect_identical(s$actors$actor, c("Human", "AI", "Boundary"))
  expect_equal(s$actors$n_nodes, c(3L, 3L, 2L))
  expect_equal(sum(s$edges_by_actor), s$n_edges)

  with_null_device({
    expect_no_error(plot_htna(net))
    expect_no_error(sequence_plot_htna(net))
    expect_no_error(sequence_plot_htna(net, by = "group"))
    expect_no_error(plot_htna_bootstrap(bootstrap_htna(net, iter = 10L,
                                                       seed = 1L)))
  })
  expect_s3_class(igraph::as.igraph(net), "igraph")
})

test_that("start and end states are kept in every cohort", {
  d <- make_uneven_data(seed = 67L)
  grp <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                     group = "condition", start = TRUE))
  expect_identical(attr(grp, "actor_levels"), c("Human", "AI", "Boundary"))
  expect_true("Start" %in% grp$Control$nodes$label)
  expect_true("Start" %in% grp$Experimental$nodes$label)
  expect_false("A3" %in% grp$Control$nodes$label)
  expect_s3_class(compare_htna(grp), "htna_comparison_group")
})

test_that("a start state named like a code or actor type is refused", {
  d <- make_grouped_data(seed = 71L)
  expect_error(build_htna(d, node_groups = node_groups_6, start = "H1"),
               class = "htna_actor_conflict")
  expect_error(
    build_htna(d, node_groups = list(Human = c("H1", "H2", "H3"),
                                     Boundary = c("A1", "A2", "A3")),
               start = TRUE),
    class = "htna_actor_conflict"
  )
})

# ---- 2. cohorts that lack a code -------------------------------------------

test_that("a cohort without a code is consistent in every field", {
  d <- make_uneven_data(seed = 73L)
  grp <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                     group = "condition"))
  ctrl <- grp$Control
  expect_false("A3" %in% ctrl$nodes$label)
  expect_equal(ctrl$n_nodes, 5L)
  expect_equal(ctrl$n_nodes, nrow(ctrl$nodes))
  expect_equal(ctrl$n_nodes, nrow(ctrl$weights))
  expect_identical(ctrl$nodes$id, seq_len(5L))
  expect_equal(ctrl$n_edges, sum(ctrl$weights != 0))
  expect_equal(nrow(ctrl$edges), ctrl$n_edges)
  expect_true(all(c(ctrl$edges$from, ctrl$edges$to) %in% ctrl$nodes$id))
  expect_equal(ctrl$edges$weight,
               ctrl$weights[cbind(ctrl$edges$from, ctrl$edges$to)])
  expect_identical(dim(ctrl$frequency_matrix), c(5L, 5L))
  expect_identical(names(ctrl$initial), ctrl$nodes$label)
  expect_equal(grp$Experimental$n_nodes, 6L)
})

test_that("permutation and Bayesian comparison work across uneven cohorts", {
  d <- make_uneven_data(seed = 79L)
  uneven <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                        group = "condition"))
  # the same cohorts built over the codes of both (no code is dropped)
  full <- suppressMessages(build_htna(d, actor_type = "actor_type",
                                      group = "condition"))
  expect_equal(full$Control$n_nodes, 6L)

  perm <- permutation_htna(uneven, iter = 20L, seed = 1L)
  expect_s3_class(perm, "net_permutation_group")
  expect_equal(summary(perm),
               summary(permutation_htna(full, iter = 20L, seed = 1L)))

  pair <- permutation_htna(uneven$Control, uneven$Experimental,
                           iter = 20L, seed = 1L)
  expect_identical(dim(pair$diff), c(6L, 6L))
  # A3 has no transitions in Control: the difference is minus Experimental's
  expect_equal(pair$diff["A3", ], -uneven$Experimental$weights["A3", ])

  bayes <- bayes_compare_htna(uneven, seed = 1L)
  expect_equal(summary(bayes), summary(bayes_compare_htna(full, seed = 1L)))

  # the cohorts themselves are not changed by the comparison
  expect_equal(uneven$Control$n_nodes, 5L)
})

test_that("networks whose shared codes belong to different actors are refused", {
  d <- make_uneven_data(seed = 83L)
  uneven <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                        group = "condition"))
  other <- uneven$Experimental
  other$node_groups$group[other$node_groups$node == "H1"] <- "AI"
  expect_error(permutation_htna(uneven$Control, other, iter = 10L),
               class = "htna_actor_conflict")
})

# ---- 3. frequencies of a grouped network -----------------------------------

test_that("frequencies_htna() stacks the cohorts of a grouped network", {
  d <- make_uneven_data(seed = 89L)
  grp <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                     group = "condition"))
  freq <- frequencies_htna(grp)
  expect_s3_class(freq, "data.frame")
  expect_named(freq, c("group", "actor", "state", "count", "proportion"))
  expect_setequal(freq$group, c("Control", "Experimental"))

  # hand count: every (cohort, code) count is the number of its events
  events <- as.data.frame(table(group = d$condition, state = d$code),
                          responseName = "n", stringsAsFactors = FALSE)
  events <- events[events$n > 0, ]
  both <- merge(freq, events, by = c("group", "state"))
  expect_equal(nrow(both), nrow(events))
  expect_equal(both$count, both$n)
  # proportions sum to one within each cohort and actor
  shares <- stats::aggregate(proportion ~ group + actor, data = freq,
                             FUN = sum)
  expect_equal(shares$proportion, rep(1, nrow(shares)))

  single <- frequencies_htna(grp$Control)
  expect_named(single, c("group", "state", "count", "proportion"))
  expect_error(frequencies_htna(42L), class = "htna_bad_input")
})

# ---- 4. a grouping column in sequence_compare_htna() -----------------------

test_that("sequence_compare_htna() reads `group` as a session-level column", {
  d <- make_grouped_data(seed = 97L)
  net <- suppressMessages(build_htna(d, node_groups = node_groups_6))
  by_name <- sequence_compare_htna(net, group = "condition", sub = 2L,
                                   min_freq = 1L, test = "none")
  by_vector <- sequence_compare_htna(net, group = net$metadata$condition,
                                     sub = 2L, min_freq = 1L, test = "none")
  expect_s3_class(by_name, "net_sequence_comparison")
  expect_equal(summary(by_name), summary(by_vector))
  expect_true(all(c("freq_Control", "freq_Experimental") %in%
                    names(summary(by_name))))

  by_type <- sequence_compare_htna(net, group = "condition", level = "type",
                                   sub = 2L, min_freq = 1L, test = "none")
  expect_setequal(summary(by_type)$pattern,
                  c("Human->Human", "Human->AI", "AI->Human", "AI->AI"))

  expect_error(sequence_compare_htna(net, group = "absent"),
               class = "htna_missing_column")
})

# ---- 5. tied latency times ----------------------------------------------------

test_that("method = \"latency\" refuses times tied within sessions", {
  d <- make_grouped_data(seed = 101L)
  d$session_time <- 1000 * match(d$session_id, unique(d$session_id))
  err <- expect_error(
    build_htna(d, node_groups = node_groups_6, method = "latency",
               time = "session_time"),
    class = "htna_tied_times"
  )
  expect_s3_class(err, "htna_error")
  expect_match(conditionMessage(err), "tied within sessions", fixed = TRUE)

  # the bundled data have one time per session
  expect_error(
    build_htna(human_ai, actor_type = "actor_type", method = "latency",
               time = "timestamp"),
    class = "htna_tied_times"
  )
})

test_that("the tied-times check passes event times and other methods", {
  d <- make_grouped_data(seed = 103L)
  d$session_time <- 1000 * match(d$session_id, unique(d$session_id))
  d$event_time <- d$session_time + d$order_in_session
  expect_true(.check_latency_times(d, time = "event_time", action = "code",
                                   unit = "session_id"))
  expect_error(.check_latency_times(d, time = "session_time",
                                    action = "code", unit = "session_id"),
               class = "htna_tied_times")
  # tied times are no obstacle to the other methods
  net <- suppressMessages(suppressWarnings(
    build_htna(d, node_groups = node_groups_6, time = "session_time")
  ))
  expect_s3_class(net, "htna")
})

# ---- start and end states in the plots ----------------------------------------

# x, y of every node of a drawn network, by node label
drawn_layout <- function(net, ...) {
  drawn <- with_null_device(plot_htna(net, ...))
  stats::setNames(drawn$nodes[c("x", "y")], c("x", "y")) |>
    as.matrix() |>
    structure(dimnames = list(drawn$nodes$label, c("x", "y")))
}

test_that("plot_htna() keeps the actor-type positions with start and end", {
  d <- make_grouped_data(seed = 107L)
  plain <- suppressMessages(build_htna(d, node_groups = node_groups_6))
  marked <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                        start = TRUE, end = "Finish"))
  expect_identical(marked$boundary, c(start = "Start", end = "Finish"))

  without <- drawn_layout(plain)
  with <- drawn_layout(marked)
  actor_nodes <- rownames(without)
  expect_setequal(setdiff(rownames(with), actor_nodes), c("Start", "Finish"))
  expect_equal(with[actor_nodes, ], without)

  # Start at the top centre, End at the bottom centre
  expect_equal(unname(with["Start", "x"]), 0)
  expect_equal(unname(with["Finish", "x"]), 0)
  expect_gt(with["Start", "y"], max(with[actor_nodes, "y"]))
  expect_lt(with["Finish", "y"], min(with[actor_nodes, "y"]))
  expect_equal(unname(with["Start", "y"]), -unname(with["Finish", "y"]))
})

test_that("three actor types keep their positions; anchors lie outside", {
  d <- make_grouped_data(seed = 109L)
  three <- list(Human = c("H1", "H2"), AI = c("A1", "A2"),
                Tool = c("H3", "A3"))
  plain <- suppressMessages(build_htna(d, node_groups = three))
  marked <- suppressMessages(build_htna(d, node_groups = three,
                                        start = TRUE, end = TRUE))
  without <- drawn_layout(plain)
  with <- drawn_layout(marked)
  actor_nodes <- rownames(without)
  expect_equal(with[actor_nodes, ], without)
  expect_gt(with["Start", "y"], max(with[actor_nodes, "y"]))
  expect_lt(with["End", "y"], min(with[actor_nodes, "y"]))
  expect_equal(unname(with["Start", "x"]),
               mean(range(with[actor_nodes, "x"])))
  expect_equal(unname(with["End", "x"]), unname(with["Start", "x"]))
})

test_that("bootstrap and difference plots use the same anchored layout", {
  d <- make_grouped_data(seed = 113L)
  plain <- suppressMessages(build_htna(d, node_groups = node_groups_6))
  marked <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                        start = TRUE, end = TRUE))
  base <- .htna_node_styling(plain, htna_palette)
  anchored <- .htna_node_styling(marked, htna_palette)
  coords <- function(styling, net) {
    structure(styling$layout, dimnames = list(net$nodes$label, c("x", "y")))
  }
  at <- coords(anchored, marked)
  expect_equal(at[plain$nodes$label, ], coords(base, plain))
  expect_equal(unname(at["Start", ]), c(0, 2))
  expect_equal(unname(at["End", ]), c(0, -2))
  # Boundary is no arc, but it is in the legend with its own colour
  expect_named(anchored$node_list, c("AI", "Human"))
  expect_identical(anchored$groups, c("Human", "AI", "Boundary"))
  expect_identical(anchored$colors, htna_palette[1:3])

  # the legend colours are those of the nodes
  expect_identical(base$groups, c("Human", "AI"))
  expect_identical(base$colors, htna_palette[1:2])
  human <- plain$node_groups$group == "Human"
  expect_identical(unique(base$node_colors[human]), htna_palette[1])

  boot <- bootstrap_htna(marked, iter = 10L, seed = 1L)
  expect_identical(boot$model$boundary, marked$boundary)
  grp <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                     group = "condition", start = TRUE,
                                     end = TRUE))
  expect_identical(grp$Control$boundary, c(start = "Start", end = "End"))
  with_null_device({
    expect_no_error(plot_htna_bootstrap(boot))
    expect_no_error(plot_htna_diff(grp$Control, grp$Experimental))
    expect_no_error(plot_htna_diff(permutation_htna(grp, iter = 10L,
                                                    seed = 1L)))
    expect_no_error(plot_htna(grp))
    expect_no_error(plot_htna(marked, layout = "polygon"))
  })
})

test_that("the anchored drawing equals cograph's for a plain network", {
  net <- suppressMessages(build_htna(make_grouped_data(seed = 127L),
                                     node_groups = node_groups_6))
  usual <- with_null_device(plot_htna(net))
  anchored <- with_null_device(.plot_htna_anchored(
    net, group_colors = htna_palette, group_shapes = htna_shape_palette,
    minimum = 0.05
  ))
  expect_equal(anchored$nodes, usual$nodes)
  expect_equal(anchored$edges, usual$edges)
})
