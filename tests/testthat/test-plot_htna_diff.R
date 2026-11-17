test_that("plot_htna_diff() runs on two htna networks", {
  pair <- make_htna_pair()
  with_null_device(
    expect_no_error(plot_htna_diff(pair$ctrl, pair$exp))
  )
})

test_that("plot_htna_diff() runs on a net_permutation result", {
  pair <- make_htna_pair()
  perm <- permutation_htna(pair$ctrl, pair$exp, iter = 20)
  with_null_device(
    expect_no_error(plot_htna_diff(perm))
  )
  with_null_device(
    expect_no_error(plot_htna_diff(perm, show_nonsig = TRUE))
  )
})

test_that("plot_htna_diff() aligns networks whose node sets only partially overlap", {
  # Two networks built from disjoint code sets but the SAME actor
  # partition (Human / AI). The plot should align them on the union
  # (zero-padding missing rows/cols), not error.
  d_a <- data.frame(session_id = rep("S", 4),
                    code = c("H1","A1","H2","A2"),
                    order_in_session = 1:4, stringsAsFactors = FALSE)
  d_b <- data.frame(session_id = rep("S", 4),
                    code = c("H1","A1","H3","A3"),
                    order_in_session = 1:4, stringsAsFactors = FALSE)
  net_a <- build_htna(d_a, node_groups = list(Human = c("H1","H2","H3"),
                                              AI    = c("A1","A2","A3")))
  net_b <- build_htna(d_b, node_groups = list(Human = c("H1","H2","H3"),
                                              AI    = c("A1","A2","A3")))
  # Sanity: the two have different node sets but the same actor_levels.
  expect_false(setequal(rownames(net_a$weights),
                        rownames(net_b$weights)))
  expect_identical(net_a$actor_levels, net_b$actor_levels)
  with_null_device(
    expect_no_error(plot_htna_diff(net_a, net_b))
  )
})

test_that("plot_htna_diff() rejects networks with truly different actor partitions", {
  pair <- make_htna_pair()
  bad  <- pair$ctrl
  # Make `bad`'s actor partition genuinely different from pair$exp's
  # (different actor_levels, not just renamed factor labels).
  bad$actor_levels <- c("Human", "OTHER")
  levels(bad$node_groups$group) <- c("Human", "OTHER")
  expect_error(plot_htna_diff(bad, pair$exp),
               regexp = "actor partition")
})

test_that("plot_htna_diff() rejects non-htna inputs", {
  expect_error(
    plot_htna_diff(list(), list()),
    regexp = "htna"
  )
})

test_that("plot_htna_diff() draws a grouped Bayesian comparison (0.3.5)", {
  grp <- make_htna_group()
  bc <- bayes_compare_htna(grp, draws = 50, seed = 1)
  expect_s3_class(bc, "net_bayes_group")
  drawn <- character(0)
  trace_title <- function(perm, pos_color, neg_color, group_colors,
                          title = "", ...) {
    drawn <<- c(drawn, title)
    invisible(NULL)
  }
  local_mocked_bindings(.plot_htna_diff_perm = trace_title)
  expect_invisible(plot_htna_diff(bc))
  expect_identical(drawn, names(bc))
})

test_that("plot_htna_diff() renders a grouped Bayesian comparison", {
  grp <- make_htna_group()
  bc <- bayes_compare_htna(grp, draws = 50, seed = 1)
  with_null_device(expect_no_error(plot_htna_diff(bc)))
})

test_that("an empty grouped comparison raises a classed error", {
  empty <- structure(list(), class = c("net_bayes_group", "list"))
  expect_error(plot_htna_diff(empty), class = "htna_empty_input")
})
