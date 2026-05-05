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

# ---- build_htna actor_type path --------------------------------------------

test_that("build_htna() supports actor_type (row-level actor-type IDs)", {
  set.seed(7)
  d <- data.frame(
    session_id = rep(paste0("S", 1:5), each = 8),
    code       = rep(c("X1", "X2", "Y1", "Y2"), 10L),
    actor_type = rep(c("A", "A", "B", "B"), 10L),
    order_in_session = rep(1:8, 5),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_type = "actor_type")
  expect_s3_class(net, "htna")
  expect_setequal(as.character(net$node_groups$group), c("A", "B"))
  # Verify the row-level tagging actually assigned each code to its
  # tagged actor — the test data has X1/X2 always tagged "A" and
  # Y1/Y2 always tagged "B", so a flipped mapping should fail.
  ng <- net$node_groups
  group_of <- function(node) as.character(ng$group[ng$node == node])
  expect_identical(group_of("X1"), "A")
  expect_identical(group_of("X2"), "A")
  expect_identical(group_of("Y1"), "B")
  expect_identical(group_of("Y2"), "B")
})

test_that("build_htna() rejects single-actor-type `actor_type`", {
  d <- data.frame(
    session_id = "S1",
    code = c("X","Y"),
    actor_type = c("A","A"),
    order_in_session = 1:2,
    stringsAsFactors = FALSE
  )
  expect_error(build_htna(d, actor_type = "actor_type"),
               regexp = "at least two")
})

test_that("build_htna() with disambiguate=TRUE prefixes overlapping codes", {
  d <- data.frame(
    session_id = rep("S1", 6),
    code       = c("X","Y","X","Y","X","Y"),
    actor_type = c("A","A","A","B","B","B"),
    order_in_session = 1:6,
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_type = "actor_type", disambiguate = TRUE)
  expect_s3_class(net, "htna")
  # The two original codes (X, Y) appear in BOTH actor types, so
  # disambiguation must produce four distinct prefixed nodes:
  # `A:X`, `A:Y`, `B:X`, `B:Y` — and each must be tagged with the
  # corresponding actor (a flipped prefix would otherwise pass).
  expect_setequal(net$nodes$label, c("A:X", "A:Y", "B:X", "B:Y"))
  ng <- net$node_groups
  group_of <- function(node) as.character(ng$group[ng$node == node])
  expect_identical(group_of("A:X"), "A")
  expect_identical(group_of("A:Y"), "A")
  expect_identical(group_of("B:X"), "B")
  expect_identical(group_of("B:Y"), "B")
})

test_that("build_htna() rejects mutually exclusive actor_type + node_groups", {
  d <- data.frame(
    session_id = "S", code = "X", actor_type = "A", order_in_session = 1L,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, actor_type = "actor_type", node_groups = list(A = "X")),
    regexp = "either"
  )
})

# ---- build_htna node_groups data-frame form ---------------------------------

test_that("build_htna() accepts node_groups as a 2-column data frame", {
  d <- data.frame(
    session_id = rep(paste0("S", 1:4), each = 4),
    code       = rep(c("X1", "X2", "Y1", "Y2"), 4L),
    order_in_session = rep(1:4, 4),
    stringsAsFactors = FALSE
  )
  cb <- data.frame(
    code       = c("X1", "X2", "Y1", "Y2"),
    actor_type = c("A",  "A",  "B",  "B"),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, node_groups = cb)
  expect_s3_class(net, "htna")
  expect_identical(net$actor_levels, c("A", "B"))
  # Check the node-to-actor mapping is correct, not just that both
  # actor labels are present (a flipped mapping would otherwise pass).
  ng <- net$node_groups
  group_of <- function(node) as.character(ng$group[ng$node == node])
  expect_identical(group_of("X1"), "A")
  expect_identical(group_of("X2"), "A")
  expect_identical(group_of("Y1"), "B")
  expect_identical(group_of("Y2"), "B")
})

test_that("node_groups df: factor actor-type column preserves level order", {
  d <- data.frame(
    session_id = rep("S", 4),
    code  = c("X1", "X2", "Y1", "Y2"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  cb <- data.frame(
    code       = c("X1", "X2", "Y1", "Y2"),
    actor_type = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, node_groups = cb)
  expect_identical(net$actor_levels, c("B", "A"))
})

test_that("node_groups df: action col may be 1st or 2nd; networks are equivalent", {
  d <- data.frame(
    session_id = rep("S", 4),
    code  = c("X1", "X2", "Y1", "Y2"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  cb_code_first  <- data.frame(code       = c("X1","X2","Y1","Y2"),
                               actor_type = c("A","A","B","B"),
                               stringsAsFactors = FALSE)
  cb_actor_first <- data.frame(actor_type = c("A","A","B","B"),
                               code       = c("X1","X2","Y1","Y2"),
                               stringsAsFactors = FALSE)
  net1 <- build_htna(d, node_groups = cb_code_first)
  net2 <- build_htna(d, node_groups = cb_actor_first)
  # Both column orders must produce the same network (not just the
  # same actor_levels — verify the full node-to-actor mapping too).
  expect_identical(net1$actor_levels, net2$actor_levels)
  ng1 <- net1$node_groups[order(net1$node_groups$node), ]
  ng2 <- net2$node_groups[order(net2$node_groups$node), ]
  rownames(ng1) <- rownames(ng2) <- NULL
  expect_identical(ng1, ng2)
  expect_equal(unname(net1$weights[rownames(net1$weights),
                                   colnames(net1$weights)]),
               unname(net2$weights[rownames(net1$weights),
                                   colnames(net1$weights)]))
})

test_that("node_groups df errors when it has != 2 columns", {
  d <- data.frame(session_id = "S", code = "X", order_in_session = 1L,
                  stringsAsFactors = FALSE)
  expect_error(
    build_htna(d, node_groups = data.frame(code = "X", actor_type = "A",
                                           extra = 1L)),
    regexp = "exactly two columns"
  )
  expect_error(
    build_htna(d, node_groups = data.frame(code = "X")),
    regexp = "exactly two columns"
  )
})

test_that("node_groups df errors when the action column is missing", {
  d <- data.frame(session_id = "S", code = "X", order_in_session = 1L,
                  stringsAsFactors = FALSE)
  cb <- data.frame(c = "X", a = "A", stringsAsFactors = FALSE)
  expect_error(
    build_htna(d, node_groups = cb),
    regexp = "must contain the code column"
  )
})

test_that("named list: actor_levels follows list-definition order", {
  d_h <- data.frame(session_id = "S", code = "X1", order_in_session = 1L,
                    stringsAsFactors = FALSE)
  d_a <- data.frame(session_id = "S", code = "Y1", order_in_session = 2L,
                    stringsAsFactors = FALSE)
  expect_identical(build_htna(list(Human = d_h, AI = d_a))$actor_levels,
                   c("Human", "AI"))
  expect_identical(build_htna(list(AI = d_a, Human = d_h))$actor_levels,
                   c("AI", "Human"))
})

test_that("actor_type character column: order = first appearance in rows", {
  d_h <- data.frame(session_id = "S", code = "X1",
                    actor_type = "Human", order_in_session = 1L,
                    stringsAsFactors = FALSE)
  d_a <- data.frame(session_id = "S", code = "Y1",
                    actor_type = "AI",    order_in_session = 2L,
                    stringsAsFactors = FALSE)
  expect_identical(build_htna(rbind(d_h, d_a),
                              actor_type = "actor_type")$actor_levels,
                   c("Human", "AI"))
  expect_identical(build_htna(rbind(d_a, d_h),
                              actor_type = "actor_type")$actor_levels,
                   c("AI", "Human"))
})

test_that("actor_type factor column: factor levels override row order (both directions)", {
  d_rows_human_first <- data.frame(
    session_id = c("S","S"), code = c("X1","Y1"),
    actor_type = c("Human","AI"),
    order_in_session = 1:2, stringsAsFactors = FALSE
  )
  d_rows_ai_first <- data.frame(
    session_id = c("S","S"), code = c("Y1","X1"),
    actor_type = c("AI","Human"),
    order_in_session = 1:2, stringsAsFactors = FALSE
  )
  for (df in list(d_rows_human_first, d_rows_ai_first)) {
    df_h_lvl <- df; df_h_lvl$actor_type <- factor(df_h_lvl$actor_type,
                                                  levels = c("Human","AI"))
    df_a_lvl <- df; df_a_lvl$actor_type <- factor(df_a_lvl$actor_type,
                                                  levels = c("AI","Human"))
    expect_identical(build_htna(df_h_lvl, actor_type = "actor_type")$actor_levels,
                     c("Human","AI"))
    expect_identical(build_htna(df_a_lvl, actor_type = "actor_type")$actor_levels,
                     c("AI","Human"))
  }
})

test_that("node_groups list: order follows list names", {
  d <- data.frame(session_id = "S", code = c("X1","Y1"),
                  order_in_session = 1:2, stringsAsFactors = FALSE)
  expect_identical(
    build_htna(d, node_groups = list(Human = "X1", AI = "Y1"))$actor_levels,
    c("Human", "AI"))
  expect_identical(
    build_htna(d, node_groups = list(AI = "Y1", Human = "X1"))$actor_levels,
    c("AI", "Human"))
})

test_that("node_groups df (character): order = first appearance in codebook", {
  d <- data.frame(session_id = "S", code = c("X1","Y1"),
                  order_in_session = 1:2, stringsAsFactors = FALSE)
  cb1 <- data.frame(code = c("X1","Y1"),
                    actor_type = c("Human","AI"),
                    stringsAsFactors = FALSE)
  cb2 <- data.frame(code = c("Y1","X1"),
                    actor_type = c("AI","Human"),
                    stringsAsFactors = FALSE)
  expect_identical(build_htna(d, node_groups = cb1)$actor_levels,
                   c("Human", "AI"))
  expect_identical(build_htna(d, node_groups = cb2)$actor_levels,
                   c("AI", "Human"))
})

test_that("node_groups df (factor): factor levels override row order (both directions)", {
  d <- data.frame(session_id = "S", code = c("X1","Y1"),
                  order_in_session = 1:2, stringsAsFactors = FALSE)
  cb_rows_human_first <- data.frame(code = c("X1","Y1"),
                                    actor_type = c("Human","AI"),
                                    stringsAsFactors = FALSE)
  cb_rows_ai_first    <- data.frame(code = c("Y1","X1"),
                                    actor_type = c("AI","Human"),
                                    stringsAsFactors = FALSE)
  for (cb in list(cb_rows_human_first, cb_rows_ai_first)) {
    cb_h_lvl <- cb; cb_h_lvl$actor_type <- factor(cb_h_lvl$actor_type,
                                                  levels = c("Human","AI"))
    cb_a_lvl <- cb; cb_a_lvl$actor_type <- factor(cb_a_lvl$actor_type,
                                                  levels = c("AI","Human"))
    expect_identical(build_htna(d, node_groups = cb_h_lvl)$actor_levels,
                     c("Human","AI"))
    expect_identical(build_htna(d, node_groups = cb_a_lvl)$actor_levels,
                     c("AI","Human"))
  }
})

test_that("all input forms with the same declared order yield identical actor_levels (both directions)", {
  d_h <- data.frame(session_id = "S", code = c("X1","X2"),
                    order_in_session = 1:2, stringsAsFactors = FALSE)
  d_a <- data.frame(session_id = "S", code = c("Y1","Y2"),
                    order_in_session = 3:4, stringsAsFactors = FALSE)
  combined <- rbind(transform(d_h, actor_type = "Human"),
                    transform(d_a, actor_type = "AI"))
  combined_3col <- combined[, c("session_id","code","order_in_session")]

  for (declared in list(c("Human","AI"), c("AI","Human"))) {
    combined_fac <- combined
    combined_fac$actor_type <- factor(combined_fac$actor_type,
                                      levels = declared)
    cb <- data.frame(
      code       = c("X1","X2","Y1","Y2"),
      actor_type = factor(c("Human","Human","AI","AI"), levels = declared),
      stringsAsFactors = FALSE
    )
    list_arg <- if (identical(declared, c("Human","AI"))) {
      list(Human = d_h, AI = d_a)
    } else {
      list(AI = d_a, Human = d_h)
    }
    list_codes_arg <- if (identical(declared, c("Human","AI"))) {
      list(Human = c("X1","X2"), AI = c("Y1","Y2"))
    } else {
      list(AI = c("Y1","Y2"), Human = c("X1","X2"))
    }
    forms <- list(
      list_form        = build_htna(list_arg),
      actor_type_form  = build_htna(combined_fac, actor_type = "actor_type"),
      node_groups_list = build_htna(combined_3col, node_groups = list_codes_arg),
      node_groups_df   = build_htna(combined_3col, node_groups = cb)
    )
    for (nm in names(forms)) {
      expect_identical(forms[[nm]]$actor_levels, declared,
                       info = paste0("declared: ", paste(declared, collapse=","),
                                     " | form: ", nm))
    }
  }
})

test_that("build_htna() rejects NA in actor_type column", {
  d <- data.frame(session_id = "S", code = c("X","Y","Z"),
                  actor_type = c("A", NA, "B"),
                  order_in_session = 1:3, stringsAsFactors = FALSE)
  expect_error(build_htna(d, actor_type = "actor_type"),
               regexp = "missing values")
})

test_that("build_htna() drops unused factor levels in actor_type", {
  d <- data.frame(
    session_id = rep("S", 4), code = c("X1","X2","Y1","Y2"),
    actor_type = factor(c("A","A","B","B"), levels = c("A","B","C")),
    order_in_session = 1:4, stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_type = "actor_type")
  # `C` is declared but absent in the data — it should not appear as a
  # phantom actor.
  expect_identical(net$actor_levels, c("A", "B"))
  expect_false("C" %in% as.character(net$node_groups$group))
})

test_that("build_htna() rejects NA in node_groups df actor_type column", {
  d <- data.frame(session_id = "S", code = c("X","Y"),
                  order_in_session = 1:2, stringsAsFactors = FALSE)
  cb <- data.frame(code = c("X","Y"),
                   actor_type = c("A", NA),
                   stringsAsFactors = FALSE)
  expect_error(build_htna(d, node_groups = cb),
               regexp = "missing values")
})

test_that("build_htna() drops unused factor levels in node_groups df", {
  d <- data.frame(session_id = rep("S", 2), code = c("X","Y"),
                  order_in_session = 1:2, stringsAsFactors = FALSE)
  cb <- data.frame(code = c("X","Y"),
                   actor_type = factor(c("A","B"),
                                        levels = c("A","B","C")),
                   stringsAsFactors = FALSE)
  net <- build_htna(d, node_groups = cb)
  expect_identical(net$actor_levels, c("A", "B"))
})

test_that("build_htna() silently dedupes within-actor codes (list form, consistent with df)", {
  d <- data.frame(session_id = rep("S", 4), code = c("X","Y","X","Y"),
                  order_in_session = 1:4, stringsAsFactors = FALSE)
  net_list <- build_htna(d, node_groups = list(A = c("X","X"), B = "Y"))
  net_df   <- build_htna(d, node_groups = data.frame(
                              code = c("X","X","Y"),
                              actor_type = c("A","A","B"),
                              stringsAsFactors = FALSE))
  expect_identical(net_list$weights, net_df$weights)
  expect_identical(net_list$actor_levels, net_df$actor_levels)
})

test_that("node_groups df honours non-default `action` column name", {
  d <- data.frame(
    session_id = rep("S", 4),
    action_label = c("X1", "X2", "Y1", "Y2"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  cb <- data.frame(
    action_label = c("X1", "X2", "Y1", "Y2"),
    actor_type   = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, action = "action_label", node_groups = cb)
  expect_s3_class(net, "htna")
  expect_setequal(rownames(net$weights), c("X1", "X2", "Y1", "Y2"))
  # Verify codes are mapped to the correct actor (not just present).
  ng <- net$node_groups
  group_of <- function(node) as.character(ng$group[ng$node == node])
  expect_identical(group_of("X1"), "A")
  expect_identical(group_of("X2"), "A")
  expect_identical(group_of("Y1"), "B")
  expect_identical(group_of("Y2"), "B")
})

# ---- build_htna(method = "frequency" / "attention") ------------------------

test_that("build_htna(method = 'frequency') produces non-negative integer counts", {
  net <- make_htna()
  net_freq <- build_htna(list(Human = {
    data(human_long, package = "Nestimate", envir = environment())
    human_long
  }, AI = {
    data(ai_long, package = "Nestimate", envir = environment())
    ai_long
  }), method = "frequency")
  expect_s3_class(net_freq, "htna")
  expect_identical(net_freq$method, "frequency")
  expect_identical(net_freq$actor_levels, c("Human", "AI"))
  # Raw counts: weights are >= 0 and integer-valued
  expect_true(all(net_freq$weights >= 0))
  expect_true(all(net_freq$weights == round(net_freq$weights)))
  # Same node set as the relative-method default (the partition is
  # method-independent).
  expect_setequal(rownames(net_freq$weights), rownames(net$weights))
  # Same node-to-actor mapping
  ng_freq <- net_freq$node_groups
  ng_rel  <- net$node_groups
  expect_identical(
    setNames(as.character(ng_freq$group), as.character(ng_freq$node))[
      sort(as.character(ng_freq$node))],
    setNames(as.character(ng_rel$group), as.character(ng_rel$node))[
      sort(as.character(ng_rel$node))])
})

test_that("build_htna(method = 'attention') produces a sensible network", {
  data(human_long, ai_long, package = "Nestimate")
  net_att <- build_htna(list(Human = human_long, AI = ai_long),
                        method = "attention")
  expect_s3_class(net_att, "htna")
  expect_identical(net_att$method, "attention")
  expect_identical(net_att$actor_levels, c("Human", "AI"))
  # Attention weights are non-negative and not all zero.
  expect_true(all(net_att$weights >= 0))
  expect_gt(sum(net_att$weights != 0), 0L)
  # Actor partition preserved.
  expect_setequal(as.character(net_att$node_groups$group),
                  c("Human", "AI"))
})

test_that("build_htna() rejects an unknown method via Nestimate", {
  d <- data.frame(session_id = "S", code = c("X","Y"),
                  actor_type = c("A","B"),
                  order_in_session = 1:2, stringsAsFactors = FALSE)
  # Nestimate raises the error; we just verify it surfaces, not its text.
  expect_error(build_htna(d, actor_type = "actor_type", method = "bogus"))
})

# ---- summary.htna -----------------------------------------------------------

test_that("summary.htna() returns structured info and prints actor partition", {
  net <- make_htna()
  out <- capture.output(s <- summary(net))

  # Header lines
  expect_true(any(grepl("htna network", out)))
  expect_true(any(grepl("Method:", out)))
  expect_true(any(grepl("Sessions:", out)))
  expect_true(any(grepl("Actor types \\(2\\)", out)))

  # Each actor type's nodes are listed
  human_nodes <- as.character(net$node_groups$node[
    as.character(net$node_groups$group) == "Human"])
  ai_nodes    <- as.character(net$node_groups$node[
    as.character(net$node_groups$group) == "AI"])
  expect_true(any(grepl(paste0("Human.*", human_nodes[1L]), out)))
  expect_true(any(grepl(paste0("AI.*",    ai_nodes[1L]),    out)))

  # Returned structure
  expect_named(s, c("actors", "edges_by_actor", "n_nodes", "n_edges",
                    "n_sessions", "n_timesteps", "method"))
  expect_s3_class(s$actors, "data.frame")
  expect_setequal(s$actors$actor, c("Human", "AI"))
  expect_equal(sum(s$actors$n_nodes), nrow(net$node_groups))
  expect_equal(s$n_nodes, nrow(net$weights))
  expect_equal(s$n_edges, sum(net$weights != 0))
  expect_equal(dim(s$edges_by_actor), c(2L, 2L))
  expect_setequal(rownames(s$edges_by_actor), c("Human", "AI"))
  expect_equal(sum(s$edges_by_actor), sum(net$weights != 0))
})

test_that("summary.htna() truncates long node lists with ellipsis", {
  human_codes <- paste0("H", 1:20)
  ai_codes    <- paste0("A", 1:20)
  # Make sure every code appears at least once so the partition has 40
  # nodes total — otherwise the truncation count would be data-driven.
  d <- data.frame(
    session_id = rep("S", 40L),
    code = c(human_codes, ai_codes),
    order_in_session = 1:40,
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, node_groups = list(Human = human_codes,
                                          AI    = ai_codes))
  out <- capture.output(summary(net, max_nodes = 5L))
  # Each actor has 20 nodes, max_nodes = 5 → should report "+15 more".
  expect_true(any(grepl("\\(\\+15 more\\)", out)))
})

test_that("summary.htna() preserves the canonical actor order in printed lines", {
  set.seed(2)
  d <- data.frame(
    session_id = rep("S", 4), code = c("X1","Y1","X2","Y2"),
    order_in_session = 1:4, stringsAsFactors = FALSE
  )
  # AI listed first → its actor-list line must appear before Human's
  net <- build_htna(d, node_groups = list(AI    = c("Y1","Y2"),
                                          Human = c("X1","X2")))
  out <- capture.output(summary(net))
  # Match the actor-list lines specifically (they include "(N nodes):"
  # which the edge-matrix rows don't).
  ai_line    <- grep("^  AI .*\\(.*nodes\\):",    out)
  human_line <- grep("^  Human .*\\(.*nodes\\):", out)
  expect_length(ai_line, 1L)
  expect_length(human_line, 1L)
  expect_lt(ai_line, human_line)
})

test_that("summary.htna_group() iterates each cohort", {
  grp <- make_htna_group()
  out <- capture.output(s <- summary(grp))
  expect_true(any(grepl("htna_group: 2 cohorts", out)))
  expect_true(any(grepl("=== Control ===", out)))
  expect_true(any(grepl("=== Experimental ===", out)))
  expect_named(s, names(grp))
  for (cohort_summary in s) {
    expect_setequal(cohort_summary$actors$actor, c("Human", "AI"))
  }
})

test_that("summary.htna_group() handles empty group gracefully", {
  empty <- structure(list(),
                     class = c("htna_group", "netobject_group", "list"))
  out <- capture.output(s <- summary(empty))
  expect_true(any(grepl("empty", out)))
  expect_length(s, 0L)
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
  # Verify the user's colours actually reach the rendered fill mapping
  # — checking class alone wouldn't catch a bug that drops `colors=`.
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  expect_true("red"  %in% fills)
  expect_true("blue" %in% fills)
})

test_that("plot_centralities() honours user-supplied colors (grouped)", {
  skip_if_not_installed("ggplot2")
  grp <- make_htna_group()
  p   <- plot_centralities(grp, colors = c("red", "blue"))
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  fills <- unique(unlist(lapply(built$data, `[[`, "fill")))
  expect_true("red"  %in% fills)
  expect_true("blue" %in% fills)
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

test_that("build_htna(actor_type=...) preserves factor levels order", {
  d <- data.frame(
    session_id = rep(paste0("S", 1:3), each = 4),
    code  = c("X1","Y1","X1","Y1","X2","Y2","X2","Y2","X1","Y2","X2","Y1"),
    actor_type = factor(c("A","B","A","B","A","B","A","B","A","B","A","B"),
                        levels = c("B", "A")),
    order_in_session = rep(1:4, 3),
    stringsAsFactors = FALSE
  )
  net <- build_htna(d, actor_type = "actor_type")
  expect_identical(net$actor_levels, c("B", "A"))
})

test_that("build_htna() errors on overlapping codes via actor_type without disambiguate", {
  d <- data.frame(
    session_id = rep("S1", 4),
    code  = c("X","Y","X","Y"),     # X appears for both A and B
    actor_type = c("A","A","B","B"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, actor_type = "actor_type"),
    regexp = "more than one actor"
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
