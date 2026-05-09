test_that("build_htna() with named-list input builds an htna network", {
  net <- make_htna()

  expect_s3_class(net, "htna")
  expect_s3_class(net, "netobject")
  expect_true(!is.null(net$node_groups))
  expect_setequal(as.character(net$node_groups$group), c("Human", "AI"))
  expect_identical(net$actor_levels, c("Human", "AI"))
  # node_groups order matches nodes order
  expect_identical(as.character(net$node_groups$node), net$nodes$label)
})

test_that("build_htna() with single data frame + node_groups works", {
  d <- make_grouped_data()
  net <- build_htna(
    d,
    node_groups = list(Human = c("H1", "H2", "H3"),
                       AI    = c("A1", "A2", "A3"))
  )
  expect_s3_class(net, "htna")
  expect_setequal(as.character(net$node_groups$group), c("Human", "AI"))
  # Verify each code is bound to the correct actor — without this a
  # flipped mapping would still pass the set-equality check above.
  ng <- net$node_groups
  group_of <- function(node) as.character(ng$group[ng$node == node])
  for (h in c("H1","H2","H3")) expect_identical(group_of(h), "Human")
  for (a in c("A1","A2","A3")) expect_identical(group_of(a), "AI")
})

test_that("build_htna(group = ...) returns an htna_group", {
  grp <- make_htna_group()

  expect_s3_class(grp, "htna_group")
  expect_s3_class(grp, "netobject_group")
  # `make_htna_group()` produces exactly 2 cohorts; tighter than `>= 2`
  # so an extra/missing cohort would fail.
  expect_equal(length(grp), 2L)
  expect_setequal(names(grp), c("Control", "Experimental"))
  for (g in grp) {
    expect_s3_class(g, "htna")
    expect_true(!is.null(g$node_groups))
    expect_identical(g$actor_levels, c("Human", "AI"))
  }
})

test_that("build_htna() rejects single-actor input", {
  d <- make_grouped_data()
  expect_error(
    build_htna(list(Only = d)),
    regexp = "length"
  )
})

test_that("build_htna() rejects unknown group column", {
  d <- make_grouped_data()
  expect_error(
    build_htna(d,
               node_groups = list(Human = c("H1","H2","H3"),
                                  AI    = c("A1","A2","A3")),
               group = "missing_col"),
    regexp = "names\\(combined\\)|missing_col"
  )
})

test_that("build_htna() errors on overlapping codes without disambiguate", {
  d <- data.frame(
    session_id = rep("S1", 4),
    code = c("X", "Y", "X", "Y"),
    order_in_session = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_htna(d, node_groups = list(A = "X", B = c("X","Y"))),
    regexp = "more than one"
  )
})

# ---- Thorough htna_group coverage ------------------------------------------

# Helpers used across the htna_group tests below.
.split_grouped <- function() {
  d <- make_grouped_data()
  hum_codes <- c("H1", "H2", "H3"); ai_codes <- c("A1", "A2", "A3")
  d$actor_type <- ifelse(d$code %in% hum_codes, "Human", "AI")
  d$actor_type <- factor(d$actor_type, levels = c("Human", "AI"))
  list(combined  = d[order(d$session_id, d$order_in_session), ],
       hum_codes = hum_codes,
       ai_codes  = ai_codes)
}

test_that("htna_group: every cohort shares the global actor_levels", {
  grp <- make_htna_group()
  expect_identical(attr(grp, "actor_levels"), c("Human", "AI"))
  for (g in grp) {
    expect_identical(g$actor_levels, c("Human", "AI"))
  }
})

test_that("htna_group: each cohort's $node_groups maps every node correctly", {
  grp <- make_htna_group()
  hum_codes <- c("H1", "H2", "H3"); ai_codes <- c("A1", "A2", "A3")
  for (cohort_name in names(grp)) {
    g  <- grp[[cohort_name]]
    ng <- g$node_groups
    for (n in as.character(ng$node)) {
      expected <- if (n %in% hum_codes) "Human" else "AI"
      observed <- as.character(ng$group[ng$node == n])
      expect_identical(observed, expected,
                       info = paste0(cohort_name, " | node=", n))
    }
  }
})

test_that("htna_group: each cohort's nodes are a subset of the global alphabet", {
  grp <- make_htna_group()
  global_codes <- c("H1","H2","H3","A1","A2","A3")
  for (cohort_name in names(grp)) {
    cohort_nodes <- as.character(grp[[cohort_name]]$node_groups$node)
    expect_true(all(cohort_nodes %in% global_codes),
                info = cohort_name)
  }
})

test_that("htna_group: per-cohort weights match a manual filter+build", {
  d <- make_grouped_data()
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "condition")
  for (cohort_name in names(grp)) {
    d_c <- d[d$condition == cohort_name, ]
    manual <- build_htna(d_c,
                         node_groups = list(Human = c("H1","H2","H3"),
                                            AI    = c("A1","A2","A3")))
    ref <- grp[[cohort_name]]$weights
    aligned <- manual$weights[rownames(ref), colnames(ref), drop = FALSE]
    expect_equal(unname(ref), unname(aligned), info = cohort_name)
  }
})

test_that("htna_group: cohort session counts partition the original data", {
  d <- make_grouped_data()
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "condition")
  # Per-condition session count must match the wide-matrix row count
  # for that cohort (each row in $data corresponds to one session).
  expected <- vapply(split(d$session_id, d$condition),
                     function(x) length(unique(x)), integer(1L))
  for (cohort in names(grp)) {
    expect_equal(nrow(grp[[cohort]]$data), expected[[cohort]],
                 info = cohort)
  }
  # The cohorts together cover every session exactly once.
  expect_equal(sum(vapply(grp, function(g) nrow(g$data), integer(1L))),
               length(unique(d$session_id)))
})

test_that("htna_group: all four input forms produce equivalent cohort networks", {
  parts <- .split_grouped()
  d  <- parts$combined
  hc <- parts$hum_codes; ac <- parts$ai_codes

  hum <- d[d$actor_type == "Human", ]
  ai  <- d[d$actor_type == "AI",    ]
  cb  <- data.frame(
    code       = c(hc, ac),
    actor_type = factor(c(rep("Human", length(hc)), rep("AI", length(ac))),
                        levels = c("Human", "AI")),
    stringsAsFactors = FALSE
  )

  forms <- list(
    list_form        = build_htna(list(Human = hum, AI = ai),
                                  group = "condition"),
    actor_type_form  = build_htna(d, actor_type = "actor_type",
                                  group = "condition"),
    node_groups_list = build_htna(
      d[, c("session_id","code","order_in_session","condition")],
      node_groups = list(Human = hc, AI = ac), group = "condition"),
    node_groups_df   = build_htna(
      d[, c("session_id","code","order_in_session","condition")],
      node_groups = cb, group = "condition")
  )

  ref_grp <- forms$list_form
  expect_setequal(names(ref_grp), c("Control", "Experimental"))
  for (form_name in setdiff(names(forms), "list_form")) {
    expect_setequal(names(forms[[form_name]]), names(ref_grp))
    for (cohort in names(ref_grp)) {
      a <- ref_grp[[cohort]]$weights
      b <- forms[[form_name]][[cohort]]$weights[rownames(a),
                                                colnames(a), drop = FALSE]
      expect_equal(unname(a), unname(b),
                   info = paste0(form_name, " | ", cohort))
    }
  }
})

test_that("htna_group: works with method = 'frequency' and 'attention'", {
  for (m in c("frequency", "attention")) {
    grp <- build_htna(make_grouped_data(),
                      node_groups = list(Human = c("H1","H2","H3"),
                                         AI    = c("A1","A2","A3")),
                      group = "condition", method = m)
    expect_s3_class(grp, "htna_group")
    expect_equal(length(grp), 2L)
    for (g in grp) {
      expect_s3_class(g, "htna")
      expect_identical(g$method, m)
      expect_identical(g$actor_levels, c("Human", "AI"))
      expect_true(all(g$weights >= 0))
    }
  }
})

test_that("htna_group: factor group column produces all declared cohorts", {
  d <- make_grouped_data()
  d$condition <- factor(d$condition, levels = c("Experimental", "Control"))
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "condition")
  # Note: factor level order on `group =` isn't currently honoured —
  # Nestimate alphabetises group keys internally — so we only assert
  # the cohort set, not its order.
  expect_setequal(names(grp), c("Control", "Experimental"))
})

test_that("htna_group: single-level group still returns htna_group (1 cohort)", {
  d <- make_grouped_data()
  d$condition <- "only"
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "condition")
  expect_s3_class(grp, "htna_group")
  expect_equal(length(grp), 1L)
  expect_identical(names(grp), "only")
})

test_that("htna_group: rejects a group column with NA", {
  d <- make_grouped_data()
  d$condition[1L] <- NA
  expect_error(
    build_htna(d,
               node_groups = list(Human = c("H1","H2","H3"),
                                  AI    = c("A1","A2","A3")),
               group = "condition")
  )
})

test_that("htna_group: a cohort with a partial code subset only contains its own codes", {
  # Cohort A uses {H1,H2,A1,A2}; Cohort B uses {H1,H3,A1,A3}.
  d <- data.frame(
    session_id = c(rep("sA1",4), rep("sA2",4),
                   rep("sB1",4), rep("sB2",4)),
    code = c("H1","A1","H2","A2",  "H1","A2","H1","A1",
             "H1","A1","H3","A3",  "H1","A3","H1","A1"),
    order_in_session = rep(1:4, 4),
    cohort = c(rep("A", 8), rep("B", 8)),
    stringsAsFactors = FALSE
  )
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "cohort")
  # Each cohort's $weights only contains the codes that appeared in
  # that cohort's rows — not the union, not zero-padded.
  expect_setequal(rownames(grp$A$weights), c("H1","H2","A1","A2"))
  expect_setequal(rownames(grp$B$weights), c("H1","H3","A1","A3"))
  # The global `actor_levels` is preserved on every cohort, even if a
  # cohort doesn't exercise every actor — this makes cohorts share a
  # canonical actor ordering for downstream plotting.
  expect_identical(grp$A$actor_levels, c("Human", "AI"))
  expect_identical(grp$B$actor_levels, c("Human", "AI"))
  # Node-to-actor mapping remains correct on each cohort's subset.
  for (cohort in names(grp)) {
    ng <- grp[[cohort]]$node_groups
    for (n in as.character(ng$node)) {
      observed <- as.character(ng$group[ng$node == n])
      expected <- if (n %in% c("H1","H2","H3")) "Human" else "AI"
      expect_identical(observed, expected,
                       info = paste0(cohort, " | ", n))
    }
  }
})

test_that("htna_group: plot_htna_diff() works on cohorts with partial code overlap", {
  d <- data.frame(
    session_id = c(rep("sA1",4), rep("sA2",4),
                   rep("sB1",4), rep("sB2",4)),
    code = c("H1","A1","H2","A2",  "H1","A2","H1","A1",
             "H1","A1","H3","A3",  "H1","A3","H1","A1"),
    order_in_session = rep(1:4, 4),
    cohort = c(rep("A", 8), rep("B", 8)),
    stringsAsFactors = FALSE
  )
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "cohort")
  # Cohorts have different node sets — A misses {H3,A3}, B misses {H2,A2}.
  expect_false(setequal(rownames(grp$A$weights),
                        rownames(grp$B$weights)))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(plot_htna_diff(grp$A, grp$B))
})

test_that("htna_group: a cohort with zero codes for an actor still builds", {
  # Cohort A has only Human events; cohort B has both.
  d <- data.frame(
    session_id = c(rep("sA1", 3), rep("sB1", 4)),
    code       = c("H1","H2","H1",  "H1","A1","H2","A2"),
    order_in_session = c(1:3, 1:4),
    cohort = c(rep("A", 3), rep("B", 4)),
    stringsAsFactors = FALSE
  )
  grp <- build_htna(d,
                    node_groups = list(Human = c("H1","H2","H3"),
                                       AI    = c("A1","A2","A3")),
                    group = "cohort")
  # Cohort A only has Human nodes — but `actor_levels` keeps the
  # global ordering so cross-cohort comparison/plotting still makes
  # sense.
  expect_setequal(rownames(grp$A$weights), c("H1","H2"))
  expect_setequal(as.character(grp$A$node_groups$group), "Human")
  expect_identical(grp$A$actor_levels, c("Human", "AI"))
  # Cohort B is unaffected.
  expect_setequal(rownames(grp$B$weights), c("H1","H2","A1","A2"))
  expect_setequal(as.character(grp$B$node_groups$group), c("Human","AI"))
})

test_that("htna_group: per-cohort summary() reports per-cohort partition", {
  grp <- make_htna_group()
  out <- capture.output(s <- summary(grp))
  # One header per cohort
  expect_true(any(grepl("=== Control ===",       out)))
  expect_true(any(grepl("=== Experimental ===",  out)))
  expect_named(s, names(grp))
  for (cohort_name in names(grp)) {
    expect_setequal(s[[cohort_name]]$actors$actor, c("Human", "AI"))
  }
})
