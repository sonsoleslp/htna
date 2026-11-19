# Behaviour that follows the estimation engine (Nestimate) across its 0.9.x
# and 1.0.x releases: undirected edge counts, whole-actor case-dropping, the
# `time` column and the latency method, and closed-form certainty results.

node_groups_6 <- list(Human = c("H1", "H2", "H3"), AI = c("A1", "A2", "A3"))

# Sessions nested in students, with an event time one second apart.
make_timed_data <- function(seed = 1L) {
  d <- make_grouped_data(seed)
  sessions <- unique(d$session_id)
  student <- stats::setNames(paste0("student_", rep(seq_len(10L), each = 3L)),
                             sessions)
  d$student <- unname(student[d$session_id])
  d$event_time <- 1000 * match(d$session_id, sessions) + d$order_in_session
  d
}

engine_at_least <- function(version) {
  utils::packageVersion("Nestimate") >= version
}

# ---- summary() of an undirected network ---------------------------------

test_that("summary.htna counts each edge of an undirected network once", {
  d <- make_grouped_data(seed = 11L)
  net <- suppressWarnings(build_htna(d, node_groups = node_groups_6,
                                     method = "co_occurrence"))
  expect_false(net$directed)
  s <- summary(net)
  W <- net$weights
  pairs_once <- sum(W != 0 & upper.tri(W, diag = TRUE))

  expect_false(s$directed)
  expect_equal(s$n_edges, pairs_once)
  expect_equal(s$n_edges, nrow(as.data.frame(net)))
  expect_true(isSymmetric(s$edges_by_actor))

  blocks <- as.data.frame(s, what = "actor_pairs")
  expect_equal(nrow(blocks), 3L)
  expect_equal(sum(blocks$n_edges), s$n_edges)

  edges <- as.data.frame(net)
  between <- sum(edges$source_actor != edges$target_actor)
  expect_equal(s$edges_by_actor["Human", "AI"], between)
  expect_equal(s$edges_by_actor["AI", "Human"], between)
  expect_equal(s$edges_by_actor["Human", "Human"],
               sum(edges$source_actor == "Human" &
                     edges$target_actor == "Human"))

  printed <- capture.output(print(s))
  expect_true(any(grepl(sprintf("Edges: +%d / 21 \\(non-zero, undirected\\)",
                                pairs_once), printed)))
  expect_true(any(grepl("each edge once", printed, fixed = TRUE)))

  skip_if_not(engine_at_least("1.0.7"),
              "Nestimate before 1.0.7 counted an undirected edge twice")
  expect_equal(
    subset(as.data.frame(s, what = "metrics"), metric == "Edge Count")$value,
    s$n_edges
  )
})

test_that("summary.htna of a directed network keeps every directed block", {
  net <- make_htna_group(seed = 13L)$Control
  s <- summary(net)
  expect_true(s$directed)
  expect_equal(s$n_edges, sum(net$weights != 0))
  blocks <- as.data.frame(s, what = "actor_pairs")
  expect_equal(nrow(blocks), 4L)
  expect_equal(sum(blocks$n_edges), s$n_edges)
  printed <- capture.output(print(s))
  expect_true(any(grepl("rows = source, cols = target", printed,
                        fixed = TRUE)))
  expect_false(any(grepl("undirected", printed, fixed = TRUE)))
})

# ---- centrality stability with whole actors dropped -----------------------

test_that("print and plot of htna_stability say when whole actors are dropped", {
  net <- make_htna_group(seed = 17L)$Control
  cs <- centrality_stability_htna(net, iter = 10L, seed = 1L,
                                  drop_prop = c(0.3, 0.5))
  by_session <- capture.output(print(cs))
  expect_false(any(grepl("whole actors", by_session, fixed = TRUE)))

  # the fields the engine adds when `actor` is given (Nestimate >= 1.0.19)
  cs$actor <- "student"
  cs$n_actors <- 10L
  by_actor <- capture.output(print(cs))
  expect_true(any(grepl("whole actors dropped (student, 10 actors)",
                        by_actor, fixed = TRUE)))

  skip_if_not_installed("ggplot2")
  expect_identical(.htna_stability_ggplot(cs)$labels$x,
                   "Proportion of actors dropped")
  cs$actor <- NULL
  expect_identical(.htna_stability_ggplot(cs)$labels$x,
                   "Proportion of sessions dropped")
})

test_that("centrality_stability_htna(actor = ) reports the actors it dropped", {
  skip_if_not(engine_at_least("1.0.19"),
              "`actor` in centrality_stability() needs Nestimate 1.0.19")
  d <- make_timed_data(seed = 19L)
  net <- suppressWarnings(build_htna(d, node_groups = node_groups_6,
                                     actor = "student"))
  cs <- centrality_stability_htna(net, iter = 10L, seed = 1L,
                                  drop_prop = c(0.3, 0.5),
                                  actor = "student")
  expect_s3_class(cs, "htna_stability")
  expect_identical(cs$actor, "student")
  expect_equal(cs$n_actors, 10L)
  expect_true(any(grepl("whole actors dropped (student, 10 actors)",
                        capture.output(print(cs)), fixed = TRUE)))
})

# ---- time column and the latency method -----------------------------------

test_that("build_htna(time = ) orders events by time without an order column", {
  d <- make_timed_data(seed = 23L)
  by_order <- suppressWarnings(build_htna(d, node_groups = node_groups_6))

  set.seed(29L)
  shuffled <- d[sample(nrow(d)), ]
  shuffled$order_in_session <- NULL
  by_time <- suppressMessages(suppressWarnings(
    build_htna(shuffled, node_groups = node_groups_6, time = "event_time")
  ))

  expect_s3_class(by_time, "htna")
  expect_equal(by_time$weights, by_order$weights)
  expect_equal(by_time$node_groups, by_order$node_groups,
               ignore_attr = TRUE)
})

test_that("build_htna() raises classed errors for a bad time or order column", {
  d <- make_timed_data(seed = 31L)
  expect_error(build_htna(d, node_groups = node_groups_6, time = "absent"),
               class = "htna_missing_column")
  expect_error(build_htna(d, node_groups = node_groups_6, time = 1L),
               class = "htna_bad_input")
  d$order_in_session <- NULL
  expect_error(build_htna(d, node_groups = node_groups_6),
               class = "htna_missing_column")
})

test_that("build_htna(method = \"latency\") keeps the actor partition", {
  skip_if_not(engine_at_least("1.0.20"),
              "method = \"latency\" needs Nestimate 1.0.20")
  d <- make_timed_data(seed = 37L)
  set.seed(41L)
  d$event_time <- d$event_time + stats::runif(nrow(d), 0, 0.5)
  d$actor_type <- ifelse(d$code %in% node_groups_6$Human, "Human", "AI")

  net <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                     method = "latency",
                                     time = "event_time"))
  expect_s3_class(net, "htna")
  expect_identical(net$method, "latency")
  expect_identical(net$actor_levels, c("Human", "AI"))
  expect_setequal(net$node_groups$node, unlist(node_groups_6))
  # a reply weighs between the floor (0.5) and 1, so a pair's summed weight
  # is at most its number of replies
  expect_true(all(net$weights >= 0))
  expect_true(all(net$weights <= net$n_replies + 1e-8))

  # the three input forms give one network
  from_column <- suppressMessages(build_htna(
    d, actor_type = "actor_type", method = "latency", time = "event_time"
  ))
  from_list <- suppressMessages(build_htna(
    list(Human = subset(d, actor_type == "Human"),
         AI = subset(d, actor_type == "AI")),
    method = "latency", time = "event_time"
  ))
  expect_equal(from_column$weights, net$weights)
  expect_equal(from_list$weights, net$weights)

  # without an order column
  d$order_in_session <- NULL
  no_order <- suppressMessages(build_htna(d, node_groups = node_groups_6,
                                          method = "latency",
                                          time = "event_time"))
  expect_equal(no_order$weights, net$weights)

  s <- summary(net)
  expect_identical(s$method, "latency")
  boot <- bootstrap_htna(net, iter = 10L, seed = 1L)
  expect_s3_class(boot, "htna_bootstrap")
  expect_s3_class(boot$model, "htna")
})

# ---- closed-form certainty --------------------------------------------------

test_that("certainty_htna() keeps the actor partition on its model", {
  net <- make_htna_group(seed = 43L)$Control
  cert <- certainty_htna(net)
  expect_s3_class(cert, "net_certainty")
  expect_s3_class(cert$model, "htna")
  expect_equal(cert$model$node_groups, net$node_groups)
  expect_identical(cert$model$actor_levels, net$actor_levels)
  with_null_device(expect_no_error(plot_htna_bootstrap(cert)))
})

test_that("certainty_htna() of a grouped network is drawn per cohort", {
  grp <- make_htna_group(seed = 47L)
  cert <- certainty_htna(grp)
  expect_named(cert, names(grp))
  expect_true(all(vapply(cert, function(item) inherits(item$model, "htna"),
                         logical(1L))))
  expect_equal(cert$Experimental$model$node_groups,
               grp$Experimental$node_groups)
  with_null_device(expect_no_error(plot_htna_bootstrap(cert)))
})

test_that("plot_htna_bootstrap() refuses what is not a bootstrap or certainty", {
  net <- make_htna_group(seed = 53L)$Control
  expect_error(plot_htna_bootstrap(net), class = "htna_bad_input")
})
