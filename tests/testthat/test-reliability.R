# Thorough tests for reliability_htna(). Each block focuses on a
# distinct property: structure, partition preservation, equivalence to
# the underlying Nestimate call, downstream usability, parameter
# forwarding (every supported `scale =`), and method-independence.

# ---- Structure --------------------------------------------------------

test_that("reliability_htna() returns the documented structure", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  rel <- reliability_htna(net, iter = 5L, seed = 1L)

  expect_s3_class(rel, "htna_reliability")
  expect_s3_class(rel, "net_reliability")
  expect_named(rel,
               c("iterations", "summary", "models",
                 "iter", "split", "scale"),
               ignore.order = TRUE)
  expect_equal(rel$iter,  5L)
  expect_equal(rel$split, 0.5)
  expect_equal(rel$scale, "none")

  # iterations: one row per (model × iteration), columns =
  # model + the four metrics.
  expect_s3_class(rel$iterations, "data.frame")
  expect_equal(nrow(rel$iterations), 5L)
  expect_setequal(names(rel$iterations),
                  c("model", "mean_dev", "median_dev", "cor", "max_dev"))
  # All metric columns are numeric and finite for a non-degenerate
  # network.
  for (col in c("mean_dev", "median_dev", "cor", "max_dev")) {
    expect_true(is.numeric(rel$iterations[[col]]))
    expect_true(all(is.finite(rel$iterations[[col]])))
  }

  # summary: one row per (model × metric) with mean+sd.
  expect_s3_class(rel$summary, "data.frame")
  expect_setequal(names(rel$summary), c("model", "metric", "mean", "sd"))
  expect_equal(nrow(rel$summary), 4L)  # 4 metrics × 1 model
  expect_setequal(rel$summary$metric,
                  c("mean_dev", "median_dev", "cor", "max_dev"))
})

# ---- Partition preservation ------------------------------------------

test_that("reliability_htna() preserves the htna partition on every model", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  rel <- reliability_htna(net, iter = 5L, seed = 1L)

  expect_length(rel$models, 1L)
  for (m in rel$models) {
    expect_s3_class(m, "htna")
    expect_identical(m$actor_levels, net$actor_levels)
    # Per-node mapping is identical to the input
    map_in  <- setNames(as.character(net$node_groups$group),
                        as.character(net$node_groups$node))
    map_out <- setNames(as.character(m$node_groups$group),
                        as.character(m$node_groups$node))
    expect_identical(map_out[names(map_in)], map_in)
    # nodes$groups is preserved (the cograph-canonical form)
    expect_identical(m$nodes$groups, net$nodes$groups)
  }
})

# ---- Multiple networks ------------------------------------------------

test_that("reliability_htna() accepts multiple htna networks via ...", {
  skip_if_not_installed("Nestimate")
  pair <- make_htna_pair()
  rel  <- reliability_htna(pair$ctrl, pair$exp, iter = 5L, seed = 2L)

  expect_length(rel$models, 2L)
  for (m in rel$models) expect_s3_class(m, "htna")

  expect_setequal(unique(rel$iterations$model), unique(rel$summary$model))
  expect_equal(nrow(rel$iterations), 2L * 5L)  # iter rows per model
  expect_equal(nrow(rel$summary),    2L * 4L)  # 4 metrics per model
})

# ---- Equivalence to direct Nestimate call -----------------------------

test_that("reliability_htna() matches Nestimate::network_reliability() numerically", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  set.seed(99)
  rel_h <- reliability_htna(net, iter = 5L, seed = 11L)
  rel_n <- Nestimate::network_reliability(net, iter = 5L, seed = 11L)

  # Same seed → identical iteration metrics; the htna wrapper only
  # post-processes $models, so the reliability calculations are
  # bit-equal.
  expect_equal(rel_h$iterations, rel_n$iterations)
  expect_equal(rel_h$summary,    rel_n$summary)
  expect_equal(rel_h$iter,  rel_n$iter)
  expect_equal(rel_h$split, rel_n$split)
  expect_equal(rel_h$scale, rel_n$scale)
})

# ---- Downstream usability of $models ---------------------------------

test_that("reliability_htna() $models can be fed back into htna pipelines", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  rel <- reliability_htna(net, iter = 3L, seed = 1L)
  m   <- rel$models[[1L]]

  # centralities() should accept the recovered model without erroring
  ct <- centralities(m, measures = c("OutStrength", "InStrength"))
  expect_s3_class(ct, "data.frame")
  expect_setequal(ct$node, as.character(net$node_groups$node))

  # Plot calls should not error
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(plot_htna(m))
})

# ---- Parameter forwarding (iter / split / scale / seed) --------------

test_that("reliability_htna() forwards iter, split, scale, seed", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  rel1 <- reliability_htna(net, iter = 3L, split = 0.3,
                           scale = "minmax", seed = 42L)
  rel2 <- reliability_htna(net, iter = 3L, split = 0.3,
                           scale = "minmax", seed = 42L)
  expect_equal(rel1$iterations, rel2$iterations)
  expect_equal(rel1$iter,  3L)
  expect_equal(rel1$split, 0.3)
  expect_equal(rel1$scale, "minmax")
})

test_that("reliability_htna() supports every documented `scale` value", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  for (s in c("none", "minmax", "standardize", "proportion")) {
    rel <- reliability_htna(net, iter = 3L, scale = s, seed = 1L)
    expect_equal(rel$scale, s)
    expect_s3_class(rel, "htna_reliability")
    expect_equal(nrow(rel$iterations), 3L)
  }
})

test_that("reliability_htna() with different seeds yields different iterations", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  rel_a <- reliability_htna(net, iter = 5L, seed = 1L)
  rel_b <- reliability_htna(net, iter = 5L, seed = 2L)
  # Same network + same iter, different seed → at least one metric column
  # must differ across some iteration.
  diffs <- vapply(c("mean_dev","median_dev","cor","max_dev"),
                  function(col) any(rel_a$iterations[[col]] !=
                                    rel_b$iterations[[col]]),
                  logical(1L))
  expect_true(any(diffs))
})

# ---- Validation -------------------------------------------------------

test_that("reliability_htna() rejects empty input and non-htna input", {
  skip_if_not_installed("Nestimate")
  expect_error(reliability_htna(),
               regexp = "needs at least one htna network")
  expect_error(reliability_htna("nope"),
               regexp = "must be htna networks")
  expect_error(reliability_htna(list()),
               regexp = "must be htna networks")
  # Mixed: one htna, one not — still rejected.
  net <- make_htna()
  expect_error(reliability_htna(net, "still_bad"),
               regexp = "must be htna networks")
})

# ---- Method independence (relative / frequency / attention) ----------

test_that("reliability_htna() works for every build_htna() method", {
  skip_if_not_installed("Nestimate")
  d   <- make_grouped_data()
  ng  <- list(Human = c("H1","H2","H3"), AI = c("A1","A2","A3"))
  for (m in c("relative", "frequency", "attention")) {
    net <- build_htna(d, node_groups = ng, method = m)
    rel <- reliability_htna(net, iter = 3L, seed = 1L)
    expect_s3_class(rel, "htna_reliability")
    expect_length(rel$models, 1L)
    expect_identical(rel$models[[1L]]$method, m)
    expect_identical(rel$models[[1L]]$actor_levels, c("Human", "AI"))
  }
})

# ---- htna_group: cohorts can be passed via splat ---------------------

test_that("reliability_htna() works on htna_group cohorts via do.call(...)", {
  skip_if_not_installed("Nestimate")
  grp <- make_htna_group()
  rel <- do.call(reliability_htna, c(unname(grp),
                                     list(iter = 3L, seed = 1L)))
  expect_s3_class(rel, "htna_reliability")
  expect_length(rel$models, length(grp))
  for (m in rel$models) expect_s3_class(m, "htna")
})

# ---- Cross-product: input form × method --------------------------------
#
# All 4 input forms × all 3 methods produce the same network, so the
# reliability summary metrics must agree across forms (same seed).

test_that("reliability_htna() invariant across input form × method", {
  skip_if_not_installed("Nestimate")
  d  <- make_grouped_data()
  hum <- d[d$code %in% c("H1","H2","H3"), ]
  ai  <- d[d$code %in% c("A1","A2","A3"), ]
  combined <- d
  combined$actor_type <- ifelse(d$code %in% c("H1","H2","H3"),
                                 "Human", "AI")
  combined$actor_type <- factor(combined$actor_type,
                                levels = c("Human","AI"))
  ng_list <- list(Human = c("H1","H2","H3"), AI = c("A1","A2","A3"))
  ng_df   <- data.frame(
    code = c("H1","H2","H3","A1","A2","A3"),
    actor_type = factor(c(rep("Human",3), rep("AI",3)),
                        levels = c("Human","AI")),
    stringsAsFactors = FALSE
  )

  for (m in c("relative", "frequency", "attention")) {
    forms <- list(
      list_form        = build_htna(list(Human = hum, AI = ai),
                                    method = m),
      actor_type_form  = build_htna(combined, actor_type = "actor_type",
                                    method = m),
      node_groups_list = build_htna(d[, c("session_id","code","order_in_session")],
                                    node_groups = ng_list, method = m),
      node_groups_df   = build_htna(d[, c("session_id","code","order_in_session")],
                                    node_groups = ng_df, method = m)
    )
    rels <- lapply(forms, function(net) {
      reliability_htna(net, iter = 5L, seed = 1L)
    })
    ref <- rels$list_form$summary
    for (form_name in setdiff(names(rels), "list_form")) {
      expect_equal(rels[[form_name]]$summary, ref,
                   info = paste0("method=", m, " | form=", form_name))
    }
  }
})

# ---- Cross-product: scale × multi-input -------------------------------

test_that("reliability_htna(): every (scale × n_inputs) combination works", {
  skip_if_not_installed("Nestimate")
  pair <- make_htna_pair()
  for (s in c("none", "minmax", "standardize", "proportion")) {
    for (inputs in list(list(pair$ctrl),
                        list(pair$ctrl, pair$exp))) {
      rel <- do.call(reliability_htna,
                     c(inputs,
                       list(iter = 3L, scale = s, seed = 1L)))
      expect_s3_class(rel, "htna_reliability")
      expect_equal(rel$scale, s)
      expect_length(rel$models, length(inputs))
      expect_equal(nrow(rel$iterations), 3L * length(inputs))
    }
  }
})

# ---- Cross-product: split sweep × multi-input -------------------------

test_that("reliability_htna(): split sweep round-trips correctly", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  for (sp in c(0.2, 0.4, 0.5, 0.6, 0.8)) {
    rel <- reliability_htna(net, iter = 3L, split = sp, seed = 1L)
    expect_equal(rel$split, sp)
    # iterations must still have 3 rows for the single input
    expect_equal(nrow(rel$iterations), 3L)
  }
})

# ---- Cross-product: iter × scale --------------------------------------

test_that("reliability_htna(): iter × scale combinations all work", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  for (it in c(2L, 5L, 25L)) {
    for (s in c("none", "minmax", "standardize", "proportion")) {
      rel <- reliability_htna(net, iter = it, scale = s, seed = 1L)
      expect_equal(rel$iter,  it)
      expect_equal(rel$scale, s)
      expect_equal(nrow(rel$iterations), it)
    }
  }
})

# ---- Cross-product: method × multi-network ----------------------------

test_that("reliability_htna(): method × multi-network works for all methods", {
  skip_if_not_installed("Nestimate")
  d  <- make_grouped_data()
  ng <- list(Human = c("H1","H2","H3"), AI = c("A1","A2","A3"))
  for (m in c("relative", "frequency", "attention")) {
    n1 <- build_htna(d[d$condition == "Control",      ],
                     node_groups = ng, method = m)
    n2 <- build_htna(d[d$condition == "Experimental", ],
                     node_groups = ng, method = m)
    rel <- reliability_htna(n1, n2, iter = 3L, seed = 1L)
    expect_s3_class(rel, "htna_reliability")
    expect_length(rel$models, 2L)
    for (mod in rel$models) {
      expect_s3_class(mod, "htna")
      expect_identical(mod$method, m,
                       info = paste0("method=", m))
    }
  }
})
