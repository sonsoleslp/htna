# Thorough tests for centrality_stability_htna(). The wrapper itself is
# thin (no embedded models to repair) so the focus is on: input
# validation, htna_group iteration, parameter forwarding, structure of
# the returned `net_stability` object, and equivalence to the
# underlying Nestimate call.

# ---- Structure & class -----------------------------------------------

test_that("centrality_stability_htna() returns the documented structure", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  cs  <- centrality_stability_htna(net, iter = 5L, seed = 1L)

  expect_s3_class(cs, "htna_stability")
  expect_s3_class(cs, "net_stability")
  expect_named(cs,
               c("cs", "correlations", "measures",
                 "drop_prop", "threshold", "certainty",
                 "iter", "method"),
               ignore.order = TRUE)

  # cs is a named numeric, one entry per requested measure
  expect_true(is.numeric(cs$cs))
  expect_setequal(names(cs$cs),
                  c("InStrength", "OutStrength", "Betweenness"))
  # all stability coefficients are in [0, 1]
  expect_true(all(cs$cs >= 0 & cs$cs <= 1))

  # correlations: one entry per measure
  expect_setequal(names(cs$correlations),
                  c("InStrength", "OutStrength", "Betweenness"))

  # parameters round-trip
  expect_equal(cs$iter, 5L)
  expect_equal(cs$threshold, 0.7)
  expect_equal(cs$certainty, 0.95)
  expect_equal(cs$method, "pearson")
})

# ---- Equivalence to the underlying Nestimate call --------------------

test_that("centrality_stability_htna() matches Nestimate::centrality_stability()", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  cs_h <- centrality_stability_htna(net, iter = 5L, seed = 11L)
  cs_n <- Nestimate::centrality_stability(net, iter = 5L, seed = 11L)
  # Same seed → bit-equal results (the wrapper only adds a class tag)
  expect_equal(cs_h$cs,           cs_n$cs)
  expect_equal(cs_h$correlations, cs_n$correlations)
  expect_equal(cs_h$measures,     cs_n$measures)
})

# ---- Parameter forwarding --------------------------------------------

test_that("centrality_stability_htna() forwards every documented parameter", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  cs <- centrality_stability_htna(
    net,
    measures   = c("InStrength", "OutStrength"),
    iter       = 4L,
    drop_prop  = c(0.25, 0.5, 0.75),
    threshold  = 0.6,
    certainty  = 0.9,
    method     = "spearman",
    seed       = 42L
  )
  expect_equal(cs$iter,      4L)
  expect_equal(cs$drop_prop, c(0.25, 0.5, 0.75))
  expect_equal(cs$threshold, 0.6)
  expect_equal(cs$certainty, 0.9)
  expect_equal(cs$method,    "spearman")
  expect_setequal(names(cs$cs), c("InStrength", "OutStrength"))
})

test_that("centrality_stability_htna(): same seed → identical output", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  a <- centrality_stability_htna(net, iter = 5L, seed = 1L)
  b <- centrality_stability_htna(net, iter = 5L, seed = 1L)
  expect_equal(a$cs,           b$cs)
  expect_equal(a$correlations, b$correlations)
})

test_that("centrality_stability_htna(): different seed → different output", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  a <- centrality_stability_htna(net, iter = 5L, seed = 1L)
  b <- centrality_stability_htna(net, iter = 5L, seed = 2L)
  # At least one correlation column should differ across seeds.
  any_diff <- vapply(names(a$correlations), function(m)
    !isTRUE(all.equal(a$correlations[[m]], b$correlations[[m]])),
    logical(1L))
  expect_true(any(any_diff))
})

# ---- htna_group iteration --------------------------------------------

test_that("centrality_stability_htna() iterates over an htna_group", {
  skip_if_not_installed("Nestimate")
  grp <- make_htna_group()
  res <- centrality_stability_htna(grp, iter = 3L, seed = 1L)

  expect_s3_class(res, "htna_stability_group")
  expect_named(res, names(grp))
  for (cs in res) {
    expect_s3_class(cs, "htna_stability")
    expect_setequal(names(cs$cs),
                    c("InStrength", "OutStrength", "Betweenness"))
  }
})

test_that("centrality_stability_htna() rejects an empty htna_group", {
  skip_if_not_installed("Nestimate")
  empty <- structure(list(),
                     class = c("htna_group", "netobject_group", "list"))
  expect_error(centrality_stability_htna(empty), regexp = "Empty")
})

# ---- Validation -------------------------------------------------------

test_that("centrality_stability_htna() rejects non-htna scalar input", {
  skip_if_not_installed("Nestimate")
  expect_error(centrality_stability_htna("nope"),  regexp = "htna")
  expect_error(centrality_stability_htna(1L),      regexp = "htna")
})

# ---- Subset of measures ----------------------------------------------

test_that("centrality_stability_htna() honours `measures =`", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  cs  <- centrality_stability_htna(net, measures = "OutStrength",
                                   iter = 3L, seed = 1L)
  expect_setequal(names(cs$cs), "OutStrength")
  expect_setequal(names(cs$correlations), "OutStrength")
})

# ---- Method-independence (relative / frequency / attention) ---------

test_that("centrality_stability_htna() works for every build_htna() method", {
  skip_if_not_installed("Nestimate")
  d  <- make_grouped_data()
  ng <- list(Human = c("H1","H2","H3"), AI = c("A1","A2","A3"))
  for (m in c("relative", "frequency", "attention")) {
    net <- build_htna(d, node_groups = ng, method = m)
    cs  <- centrality_stability_htna(net, iter = 3L, seed = 1L)
    expect_s3_class(cs, "htna_stability")
    expect_true(all(cs$cs >= 0 & cs$cs <= 1))
  }
})

# ---- Cross-product: input form × method ------------------------------
#
# Same data, four input shapes, three methods → twelve builds. For each
# the stability check should run, return a valid `htna_stability`, and
# the per-measure `cs` value should match across forms (input shape
# doesn't change the network, so it can't change the stability either).

test_that("centrality_stability_htna() invariant across input form × method", {
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
    code       = c("H1","H2","H3","A1","A2","A3"),
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
    cs_list <- lapply(forms, function(net) {
      centrality_stability_htna(net, iter = 5L, seed = 1L)
    })
    ref <- cs_list$list_form$cs
    for (form_name in setdiff(names(cs_list), "list_form")) {
      expect_equal(cs_list[[form_name]]$cs, ref,
                   info = paste0("method=", m, " | form=", form_name))
    }
  }
})

# ---- Cross-product: cor method × measure ------------------------------
#
# Three correlation methods × the three default agreement measures.
# All 9 combinations should produce valid stability output.

test_that("centrality_stability_htna(): every (cor method × measure) combination works", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  for (cor_m in c("pearson", "spearman", "kendall")) {
    for (msr in c("InStrength", "OutStrength", "Betweenness")) {
      cs <- centrality_stability_htna(net, measures = msr,
                                      method = cor_m, iter = 3L,
                                      seed = 1L)
      expect_setequal(names(cs$cs), msr)
      expect_true(cs$cs[[msr]] >= 0 && cs$cs[[msr]] <= 1,
                  info = paste0("cor=", cor_m, " | measure=", msr))
      expect_equal(cs$method, cor_m)
    }
  }
})

# ---- Cross-product: drop_prop sweep -----------------------------------

test_that("centrality_stability_htna(): different drop_prop vectors round-trip cleanly", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  drop_props <- list(
    single  = 0.5,
    coarse  = c(0.25, 0.5, 0.75),
    fine    = seq(0.1, 0.9, by = 0.1),
    extreme = c(0.05, 0.95)
  )
  for (nm in names(drop_props)) {
    dp <- drop_props[[nm]]
    cs <- centrality_stability_htna(net, drop_prop = dp,
                                    iter = 3L, seed = 1L)
    expect_equal(cs$drop_prop, dp, info = nm)
    # `correlations[[measure]]` is an iter × |drop_prop| matrix.
    for (m in cs$measures) {
      expect_equal(ncol(cs$correlations[[m]]), length(dp),
                   info = paste0("drop_prop=", nm,
                                  " | measure=", m, " (cols)"))
      expect_equal(nrow(cs$correlations[[m]]), 3L,
                   info = paste0("drop_prop=", nm,
                                  " | measure=", m, " (iter rows)"))
    }
  }
})

# ---- Cross-product: threshold × certainty -----------------------------

test_that("centrality_stability_htna(): threshold/certainty are honoured (high → lower cs)", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  cs_low  <- centrality_stability_htna(net, threshold = 0.5,
                                       certainty = 0.5,
                                       iter = 5L, seed = 1L)
  cs_high <- centrality_stability_htna(net, threshold = 0.95,
                                       certainty = 0.99,
                                       iter = 5L, seed = 1L)
  # Higher bar → cs must be at most as high as the lenient bar
  # (the same correlation set just clears fewer proportions).
  for (m in cs_low$measures) {
    expect_lte(cs_high$cs[[m]], cs_low$cs[[m]],
               label = paste0("measure=", m,
                              " (high cs ≤ low cs)"))
  }
})

# ---- Cross-product: htna_group × method -------------------------------

test_that("centrality_stability_htna(): htna_group × method produces per-cohort results", {
  skip_if_not_installed("Nestimate")
  d  <- make_grouped_data()
  ng <- list(Human = c("H1","H2","H3"), AI = c("A1","A2","A3"))
  # Each cohort has only 10 sessions, so restrict drop_prop to keep
  # at least 2 sessions in every drop step (avoids Nestimate's
  # internal minimum-sessions error).
  small_dp <- c(0.25, 0.5)
  for (m in c("relative", "frequency", "attention")) {
    grp <- build_htna(d, node_groups = ng, group = "condition",
                      method = m)
    res <- centrality_stability_htna(grp, iter = 3L,
                                     drop_prop = small_dp, seed = 1L)
    expect_s3_class(res, "htna_stability_group")
    expect_named(res, names(grp))
    for (cs in res) {
      expect_s3_class(cs, "htna_stability")
      expect_true(all(cs$cs >= 0 & cs$cs <= 1),
                  info = paste0("method=", m))
    }
  }
})

# ---- Each individual measure independently ----------------------------

test_that("centrality_stability_htna(): every default measure works on its own", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  for (msr in c("InStrength", "OutStrength", "Betweenness")) {
    cs <- centrality_stability_htna(net, measures = msr,
                                    iter = 3L, seed = 1L)
    expect_setequal(names(cs$cs), msr)
    expect_setequal(cs$measures, msr)
  }
})

# ---- iter sweep: minimum, small, medium -------------------------------

test_that("centrality_stability_htna(): iter sweep round-trips without error", {
  skip_if_not_installed("Nestimate")
  net <- make_htna()
  for (it in c(2L, 5L, 25L)) {
    cs <- centrality_stability_htna(net, iter = it, seed = 1L)
    expect_equal(cs$iter, it)
    expect_s3_class(cs, "htna_stability")
  }
})
