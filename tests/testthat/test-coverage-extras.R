# Targeted tests to push coverage to 100% by exercising error branches,
# fallback paths, and overlay/legend code that the happy-path tests miss.

# ---- print method -----------------------------------------------------------

test_that("print.htna_paths emits a header and the rows", {
  net <- make_htna()
  mp  <- extract_meta_paths(net, length = 3)
  out <- capture.output(print(mp))
  expect_true(any(grepl("Meta-paths", out)))
  expect_true(any(grepl("schema", out)))
})

test_that("print.htna_paths handles state-level and empty path objects", {
  net <- make_htna()
  sp  <- extract_paths(net, length = 2)
  out <- capture.output(print(sp))
  expect_true(any(grepl("state-level|Patterns", out)))

  # Empty result: a deliberately impossible pattern
  empty <- extract_meta_paths(net, length = 3, min_count = 1e9)
  expect_no_error(capture.output(print(empty)))
})

# ---- discover_paths internal branches ---------------------------------------

test_that("extract_meta_paths() with type='gapped' triggers the gapped branch", {
  net <- make_htna()
  res <- extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2)
  expect_s3_class(res, "htna_paths")
  expect_true(all(res$gap >= 1L))
})

test_that("extract_meta_paths() with start/end/contain filters works", {
  net <- make_htna()
  res <- extract_meta_paths(net, length = 3,
                            start = "Human", end = "Human", contain = "AI")
  for (p in strsplit(res$schema, "->", fixed = TRUE)) {
    expect_equal(p[1], "Human")
    expect_equal(p[length(p)], "Human")
    expect_true("AI" %in% p)
  }
})

test_that("extract_meta_paths() rejects an unknown alphabet element", {
  net <- make_htna()
  expect_error(
    extract_meta_paths(net, schema = "Bogus->Human"),
    regexp = "Unknown alphabet"
  )
})

test_that("extract_meta_paths() rejects a single-element schema", {
  net <- make_htna()
  expect_error(
    extract_meta_paths(net, schema = "Human"),
    regexp = "at least two"
  )
})

test_that("extract_*_paths() return an empty data frame when nothing passes filters", {
  net <- make_htna()
  empty <- extract_meta_paths(net, length = 5, min_count = 1e9)
  expect_s3_class(empty, "htna_paths")
  expect_equal(nrow(empty), 0L)
})

test_that(".check_htna_input rejects networks with empty node_groups / data", {
  net <- make_htna()
  bad <- net
  bad$node_groups <- bad$node_groups[0, ]
  expect_error(extract_meta_paths(bad), regexp = "actor partition")

  bad <- net
  bad$data <- NULL
  expect_error(extract_meta_paths(bad), regexp = "data")
})

# ---- build_htna actor_col path ---------------------------------------------

test_that("build_htna() supports actor_col (row-level actor IDs)", {
  set.seed(7)
  d <- data.frame(
    session_id = rep(paste0("S", 1:5), each = 8),
    code       = rep(c("X1", "X2", "Y1", "Y2"), 10L),
    actor      = rep(c("A", "A", "B", "B"), 10L),
    order_in_session = rep(1:8, 5),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_col = "actor")
  expect_s3_class(net, "htna")
  expect_setequal(as.character(net$node_groups$group), c("A", "B"))
})

test_that("build_htna() rejects single-actor `actor_col`", {
  d <- data.frame(
    session_id = "S1",
    code = c("X","Y"),
    actor = c("A","A"),
    order_in_session = 1:2,
    stringsAsFactors = FALSE
  )
  expect_error(build_htna(d, actor_col = "actor"),
               regexp = "at least two")
})

test_that("build_htna() with disambiguate=TRUE prefixes overlapping codes", {
  d <- data.frame(
    session_id = rep("S1", 6),
    code       = c("X","Y","X","Y","X","Y"),
    actor      = c("A","A","A","B","B","B"),
    order_in_session = 1:6,
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_col = "actor", disambiguate = TRUE)
  expect_s3_class(net, "htna")
  # codes are prefixed with the actor name
  expect_true(any(grepl("^A:", net$nodes$label)))
  expect_true(any(grepl("^B:", net$nodes$label)))
})

test_that("build_htna() rejects mutually exclusive actor_col + node_groups", {
  d <- data.frame(
    session_id = "S", code = "X", actor = "A", order_in_session = 1L,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, actor_col = "actor", node_groups = list(A = "X")),
    regexp = "either"
  )
})

# ---- bootstrap_htna model partition restoration -----------------------------

test_that("bootstrap_htna() iterates over a plain list of htna nets", {
  pair  <- make_htna_pair()
  boots <- bootstrap_htna(list(C = pair$ctrl, E = pair$exp), iter = 10)
  expect_s3_class(boots, "htna_bootstrap_group")
  expect_setequal(names(boots), c("C", "E"))
})

# ---- plot_htna empty-group error -------------------------------------------

test_that("plot_htna() and friends error on an empty htna_group", {
  empty <- structure(list(),
                     class = c("htna_group", "netobject_group", "list"))
  expect_error(plot_htna(empty),                regexp = "Empty")
  expect_error(plot_htna_bootstrap(structure(list(),
                                             class = c("htna_bootstrap_group",
                                                       "list"))),
               regexp = "Empty")
  expect_error(sequence_plot_htna(empty),       regexp = "Empty")
})

# ---- plot_htna_diff: net_permutation_group iteration -----------------------

test_that("plot_htna_diff() iterates a net_permutation_group", {
  pair <- make_htna_pair()
  perm <- permutation(pair$ctrl, pair$exp, iter = 20)
  grp_perm <- structure(list(`Ctrl vs Exp` = perm),
                        class = c("net_permutation_group", "list"))
  with_null_device(
    expect_no_error(plot_htna_diff(grp_perm))
  )
})

test_that("plot_htna_diff() errors on empty permutation group", {
  empty <- structure(list(),
                     class = c("net_permutation_group", "list"))
  expect_error(plot_htna_diff(empty), regexp = "Empty")
})

test_that(".plot_htna_diff_perm rejects perm$x without htna info", {
  pair <- make_htna_pair()
  perm <- permutation(pair$ctrl, pair$exp, iter = 10)
  perm$x$node_groups <- NULL
  class(perm$x) <- setdiff(class(perm$x), "htna")
  expect_error(plot_htna_diff(perm),
               regexp = "htna network")
})

# ---- sequence_plot: heatmap path + grouped legend overlay ------------------

test_that("sequence_plot_htna() type='heatmap' draws (exercises hclust helpers)", {
  net <- make_htna()
  with_null_device(
    expect_no_error(sequence_plot_htna(net, by = "state", type = "heatmap"))
  )
})

test_that("sequence_plot_htna() with legend='bottom' uses bottom gutter overlay", {
  net <- make_htna()
  with_null_device(
    expect_no_error(sequence_plot_htna(net, by = "state",
                                       type = "index", legend = "bottom"))
  )
})

test_that("sequence_plot_htna() with legend='none' suppresses grouped overlay", {
  net <- make_htna()
  with_null_device(
    expect_no_error(sequence_plot_htna(net, by = "state",
                                       type = "index", legend = "none"))
  )
})

test_that("sequence_plot_htna() type='distribution' for by='state'", {
  net <- make_htna()
  with_null_device(
    expect_no_error(sequence_plot_htna(net, by = "state",
                                       type = "distribution"))
  )
})

test_that("sequence_plot_htna() error: missing $node_groups", {
  net <- make_htna()
  net$node_groups <- net$node_groups[0, ]
  expect_error(sequence_plot_htna(net), regexp = "actor partition")
})

# ---- centralities: data frame input branch ----------------------------------

test_that("plot_centralities() accepts an htna_centralities data frame", {
  skip_if_not_installed("ggplot2")
  net <- make_htna()
  ct  <- centralities(net)
  p   <- plot_centralities(ct)
  expect_s3_class(p, "ggplot")
})

test_that("plot_centralities() rejects a data frame with no measures", {
  skip_if_not_installed("ggplot2")
  bad <- data.frame(node = "x", actor = "A", stringsAsFactors = FALSE)
  expect_error(plot_centralities(bad), regexp = "None of `measures`")
})

test_that("plot_centralities(by = 'group') errors when actor column missing", {
  skip_if_not_installed("ggplot2")
  bad <- data.frame(node = c("a","b"), OutStrength = c(1, 2),
                    stringsAsFactors = FALSE)
  expect_error(plot_centralities(bad, by = "group"),
               regexp = "actor")
})

test_that("centralities() falls back to rownames(weights) when nodes$label is NULL", {
  net <- make_htna()
  net$nodes$label <- NULL
  ct <- centralities(net, measures = "OutStrength")
  expect_true(all(nzchar(ct$node)))
  expect_equal(nrow(ct), nrow(net$weights))
})

# ---- single-node-per-actor exercises .htna_circular's else branch ----------

test_that(".htna_circular handles single-node groups without error", {
  set.seed(11)
  d <- data.frame(
    session_id = rep("S1", 4),
    code = c("X1", "Y1", "X1", "Y1"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  net <- build_htna(
    d,
    node_groups = list(A = "X1", B = "Y1")
  )
  with_null_device(
    expect_no_error(plot_htna(net))
  )
})

# ---- generic dispatch sanity ------------------------------------------------

test_that("plot_sequences.htna direct call works", {
  net <- make_htna()
  with_null_device(
    expect_no_error(plot_sequences.htna(net))
  )
})

# ---- More targeted edge cases ----------------------------------------------

test_that("centralities() rejects empty htna_group and non-htna input", {
  empty <- structure(list(),
                     class = c("htna_group", "netobject_group", "list"))
  expect_error(centralities(empty), regexp = "Empty")
  expect_error(centralities(list(foo = 1)), regexp = "htna")
  expect_error(centralities("nope"),         regexp = "htna")
})

test_that("plot_centralities() with reorder = FALSE keeps node order", {
  skip_if_not_installed("ggplot2")
  net <- make_htna()
  p <- plot_centralities(net, reorder = FALSE, labels = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_centralities() honours user-supplied colors (single network)", {
  skip_if_not_installed("ggplot2")
  net <- make_htna()
  p <- plot_centralities(net, by = "group", colors = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_centralities() honours user-supplied colors (grouped)", {
  skip_if_not_installed("ggplot2")
  grp <- make_htna_group()
  p   <- plot_centralities(grp, colors = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_htna_bootstrap() rejects bootstrap whose model lost its partition", {
  net  <- make_htna()
  boot <- bootstrap_htna(net, iter = 10)
  boot$model$node_groups <- NULL
  expect_error(plot_htna_bootstrap(boot), regexp = "node_groups")
})

test_that("plot_htna_diff() rejects htna nets with different actor partitions", {
  pair <- make_htna_pair()
  bad  <- pair$ctrl
  # rename one actor so partition differs
  levels(bad$node_groups$group) <- c("Human", "OTHER")
  expect_error(plot_htna_diff(bad, pair$exp),
               regexp = "actor partition")
})

test_that(".htna_circular handles a single-node group via plot_htna_diff", {
  set.seed(2)
  d <- data.frame(
    session_id = rep(c("S1", "S2"), each = 4),
    code = c("X1","Y1","X1","Y1","X1","Y1","X1","Y1"),
    order_in_session = rep(1:4, 2),
    stringsAsFactors = FALSE
  )
  net1 <- build_htna(d, node_groups = list(A = "X1", B = "Y1"))
  net2 <- build_htna(d, node_groups = list(A = "X1", B = "Y1"))
  with_null_device(
    expect_no_error(plot_htna_diff(net1, net2))
  )
})

test_that("extract_paths() handles a network whose $data lacks T-pattern columns", {
  net <- make_htna()
  # Rename the wide columns so ^T[0-9]+$ doesn't match
  names(net$data) <- paste0("step_", seq_along(net$data))
  res <- extract_paths(net, length = 2)
  expect_s3_class(res, "htna_paths")
})

test_that("extract_paths() handles a network with only NA/empty cells", {
  net <- make_htna()
  net$data[] <- NA_character_
  res <- extract_paths(net, length = 2)
  expect_equal(nrow(res), 0L)
})

test_that("extract_meta_paths() with type='gapped' on minimal-length sequence", {
  net <- make_htna()
  # very large gap forces .enumerate_paths / .search_pattern to find no codes
  res <- extract_meta_paths(net, length = 3, type = "gapped", gap = 1000L)
  expect_equal(nrow(res), 0L)
})

test_that("build_htna() rejects mismatched column schemas across data frames", {
  d1 <- data.frame(session_id = "S", code = "X", order_in_session = 1L,
                   stringsAsFactors = FALSE)
  d2 <- data.frame(session_id = "S", code = "Y", order_in_session = 1L,
                   extra = 1, stringsAsFactors = FALSE)
  expect_error(
    build_htna(list(A = d1, B = d2)),
    regexp = "same column schema"
  )
})

test_that("build_htna() rejects codes not in any node_groups entry", {
  d <- data.frame(
    session_id = "S1",
    code = c("X", "UNKNOWN_CODE"),
    order_in_session = 1:2,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, node_groups = list(A = "X", B = "Y")),
    regexp = "UNKNOWN_CODE"
  )
})

test_that("build_htna(actor_col=...) preserves factor levels order", {
  d <- data.frame(
    session_id = rep(paste0("S", 1:3), each = 4),
    code  = c("X1","Y1","X1","Y1","X2","Y2","X2","Y2","X1","Y2","X2","Y1"),
    actor = factor(c("A","B","A","B","A","B","A","B","A","B","A","B"),
                   levels = c("B", "A")),
    order_in_session = rep(1:4, 3),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_col = "actor")
  expect_identical(net$actor_levels, c("B", "A"))
})

test_that("build_htna() errors on overlapping codes via actor_col without disambiguate", {
  d <- data.frame(
    session_id = rep("S1", 4),
    code  = c("X","Y","X","Y"),     # X appears for both A and B
    actor = c("A","A","B","B"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, actor_col = "actor"),
    regexp = "more than one actor group"
  )
})

test_that("extract_meta_paths(schema=..., type='gapped', gap huge) yields empty", {
  net <- make_htna()
  res <- extract_meta_paths(net, schema = "Human->AI",
                            type = "gapped", gap = 1000L)
  expect_equal(nrow(res), 0L)
})

test_that("sequence_plot_htna() rejects non-htna with data validation message", {
  expect_error(sequence_plot_htna("nope"), regexp = "htna network")
})

test_that("sequence_plot_htna() handles network with no usable codes", {
  net <- make_htna()
  net$data[] <- NA_character_
  with_null_device(
    expect_error(sequence_plot_htna(net))
  )
})

test_that(".htna_grouped_legend short-circuits when result has no levels/palette", {
  fake_res <- list()
  with_null_device({
    out <- htna:::.htna_grouped_legend(fake_res, c(A="A"), "A")
    expect_null(out)
  })
})

test_that(".htna_grouped_legend short-circuits when no actors keep states", {
  fake_res <- list(levels = c("A","B"), palette = c("red","blue"))
  with_null_device({
    out <- htna:::.htna_grouped_legend(
      fake_res,
      type_map = c(A = "X", B = "X"),
      actors   = "Z"  # actor Z owns nothing
    )
    expect_null(out)
  })
})

test_that(".htna_legend_gutter_fig handles legend_ncol and bottom position", {
  with_null_device({
    f <- htna:::.htna_legend_gutter_fig(
      levels = c("A","B","C","D"), position = "right", legend_ncol = 2L)
    expect_length(f, 4L)
    f2 <- htna:::.htna_legend_gutter_fig(
      levels = c("A","B"), position = "bottom",
      legend_size = 0.8, legend_title = "T")
    expect_length(f2, 4L)
  })
})

test_that(".htna_actor_hclust handles n=0 and n=1 matrices", {
  expect_null(htna:::.htna_actor_hclust(matrix(NA_character_, 0L, 0L)))
  out1 <- htna:::.htna_actor_hclust(matrix("X", 1L, 3L))
  expect_s3_class(out1, NA)              # raw list with class attr
  expect_identical(out1$labels, NULL)
})

test_that(".htna_join_hclust composes pieces including null and singletons", {
  empty   <- htna:::.htna_actor_hclust(matrix(NA_character_, 0L, 0L))
  single  <- htna:::.htna_actor_hclust(matrix("X", 1L, 3L))
  multi   <- htna:::.htna_actor_hclust(matrix(c("A","B","A","B","A","C"),
                                              nrow = 3L))
  # null short-circuits
  expect_identical(htna:::.htna_join_hclust(NULL, multi),  multi)
  expect_identical(htna:::.htna_join_hclust(multi, NULL),  multi)
  # both single
  out <- htna:::.htna_join_hclust(single, single)
  expect_true(length(out$order) == 2L)
})

test_that("sequence_plot_htna() type='heatmap' on a single-actor cluster", {
  # Build a synthetic htna network where one actor has just one node.
  set.seed(3)
  d <- data.frame(
    session_id = rep(paste0("S", 1:5), each = 6),
    code = sample(c("Solo", "B1", "B2"), 30, replace = TRUE),
    order_in_session = rep(1:6, 5),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, node_groups = list(A = "Solo", B = c("B1", "B2")))
  with_null_device(
    expect_no_error(sequence_plot_htna(net, by = "state", type = "heatmap"))
  )
})
