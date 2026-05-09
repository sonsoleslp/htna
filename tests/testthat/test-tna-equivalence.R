# Mirrors the equivalence checks in
# `vignettes/articles/tna-equivalence.Rmd` and runs them across several
# diverse datasets, so the htna <-> tna <-> Nestimate equivalence
# guarantees are continuously verified.
#
# Skipped on CRAN and whenever `tna` or `Nestimate` is missing.

skip_if_missing_eq_deps <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tna")
  testthat::skip_if_not_installed("Nestimate")
}

# --- Comparison helpers -----------------------------------------------
#
# `expect_mat_equiv()` asserts two matrices represent the same quantity
# regardless of internal node ordering: it verifies node sets match,
# aligns `b` to `a`'s row/column ordering, requires nontrivial signal
# (otherwise an all-zero match would silently "pass"), and only then
# checks cell-level equality. `expect_vec_equiv()` is the same for
# named vectors.

expect_mat_equiv <- function(a, b, info = NULL,
                             require_signal = TRUE) {
  expect_setequal(rownames(a), rownames(b))
  expect_setequal(colnames(a), colnames(b))
  b <- b[rownames(a), colnames(a), drop = FALSE]
  if (require_signal) {
    nz <- sum(a != 0, na.rm = TRUE)
    expect_gt(nz, 0L, label = paste0("nonzero entries in `", info, "`"))
  }
  expect_equal(a, b, info = info, ignore_attr = "dimnames")
}

expect_vec_equiv <- function(a, b, info = NULL,
                             require_signal = TRUE) {
  expect_setequal(names(a), names(b))
  b <- b[names(a)]
  if (require_signal) {
    # "Not all zero" is the meaningful nontriviality bar. A constant
    # nonzero vector (e.g. centrality on a perfectly symmetric graph)
    # is still a real equivalence claim.
    nz <- sum(a != 0, na.rm = TRUE)
    expect_gt(nz, 0L, label = paste0("nonzero entries in `", info, "`"))
  }
  expect_equal(a, b, info = info, ignore_attr = "names")
}

# --- Dataset providers -------------------------------------------------
#
# Each provider returns a list with:
#   $name   character label used in test names
#   $actors named list of long frames, one per actor (Human/AI-style)
#
# Each long frame must carry `session_id`, `code`, `order_in_session`.

ds_nestimate <- function() {
  e <- new.env()
  utils::data(human_long, ai_long, package = "Nestimate", envir = e)
  list(
    name   = "Nestimate human_long + ai_long",
    actors = list(Human = e$human_long, AI = e$ai_long)
  )
}

# Synthetic two-actor sequences with biased transitions; small but
# non-trivial alphabet (3 + 3 codes), modest session count.
ds_synthetic_balanced <- function(seed = 1L) {
  set.seed(seed)
  human_codes <- c("H1", "H2", "H3")
  ai_codes    <- c("A1", "A2", "A3")
  alphabet    <- c(human_codes, ai_codes)

  trans <- matrix(rep(1 / length(alphabet), length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans["H1", ] <- c(0.05, 0.05, 0.05, 0.70, 0.10, 0.05) # H1 -> A1 bias
  trans["A2", ] <- c(0.30, 0.10, 0.10, 0.10, 0.10, 0.30) # A2 -> H1/A3 bias

  sim <- function(sid) {
    n <- sample(8:14, 1)
    s <- character(n)
    s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("S", 1:20), sim))
  list(
    name   = "synthetic balanced 3+3 codes",
    actors = list(Human = d[d$code %in% human_codes, ],
                  AI    = d[d$code %in% ai_codes, ])
  )
}

# Synthetic with a wider alphabet (5 + 4 codes) and more sessions, to
# exercise larger transition matrices and a non-square actor split.
ds_synthetic_wide <- function(seed = 2L) {
  set.seed(seed)
  human_codes <- c("Hask", "Hexplain", "Hcorrect", "Hverify", "Hreflect")
  ai_codes    <- c("Aplan", "Aexec", "Adebug", "Aexplain")
  alphabet    <- c(human_codes, ai_codes)
  trans <- matrix(stats::runif(length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans <- trans / rowSums(trans)

  sim <- function(sid) {
    n <- sample(15:25, 1)
    s <- character(n)
    s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("W", 1:30), sim))
  list(
    name   = "synthetic wide 5+4 codes",
    actors = list(Human = d[d$code %in% human_codes, ],
                  AI    = d[d$code %in% ai_codes, ])
  )
}

# Tiny sample: minimum-viable amount of data (few short sessions),
# 2+2 alphabet. Stresses bootstrap/permutation behaviour where many
# resamples will be near-degenerate.
ds_tiny <- function(seed = 11L) {
  set.seed(seed)
  human_codes <- c("h1", "h2"); ai_codes <- c("a1", "a2")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(rep(0.25, 16), 4, 4, dimnames = list(alphabet, alphabet))
  sim <- function(sid) {
    n <- sample(5:7, 1)
    s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("T", 1:6), sim))
  list(
    name   = "tiny 6 sessions, 2+2 codes",
    actors = list(Human = d[d$code %in% human_codes, ],
                  AI    = d[d$code %in% ai_codes, ])
  )
}

# Heavily imbalanced actor sizes: one actor with a single code, the
# other with 5 codes. Degenerate row in the transition matrix (the
# 1-code actor can only transition out of one state).
ds_imbalanced <- function(seed = 12L) {
  set.seed(seed)
  human_codes <- c("h_only")
  ai_codes    <- c("a1", "a2", "a3", "a4", "a5")
  alphabet    <- c(human_codes, ai_codes)
  trans <- matrix(stats::runif(length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans <- trans / rowSums(trans)
  sim <- function(sid) {
    n <- sample(10:18, 1)
    s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("I", 1:20), sim))
  list(
    name   = "imbalanced 1 vs 5 codes",
    actors = list(Human = d[d$code %in% human_codes, ],
                  AI    = d[d$code %in% ai_codes, ])
  )
}

# Block-diagonal: actors never transition between each other (Human ->
# Human, AI -> AI). Yields a transition matrix with two disconnected
# components, which can produce NaN/Inf for path-based centralities --
# both engines should produce the *same* NaN/Inf values.
ds_disconnected <- function(seed = 13L) {
  set.seed(seed)
  human_codes <- c("h1", "h2", "h3"); ai_codes <- c("a1", "a2", "a3")
  trans_h <- matrix(stats::runif(9), 3, 3,
                    dimnames = list(human_codes, human_codes))
  trans_h <- trans_h / rowSums(trans_h)
  trans_a <- matrix(stats::runif(9), 3, 3,
                    dimnames = list(ai_codes, ai_codes))
  trans_a <- trans_a / rowSums(trans_a)
  sim <- function(sid, codes, trans) {
    n <- sample(10:14, 1)
    s <- character(n); s[1] <- sample(codes, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(codes, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  human <- do.call(rbind, lapply(paste0("Hd", 1:12), sim,
                                 human_codes, trans_h))
  ai    <- do.call(rbind, lapply(paste0("Ad", 1:12), sim,
                                 ai_codes,    trans_a))
  list(
    name   = "disconnected actors (block-diagonal)",
    actors = list(Human = human, AI = ai)
  )
}

# Highly variable session lengths: a few short sessions and a few very
# long ones. The wide matrix becomes mostly NA, so engines have to
# agree on how to ignore padding.
ds_variable_lengths <- function(seed = 14L) {
  set.seed(seed)
  human_codes <- c("h1", "h2", "h3"); ai_codes <- c("a1", "a2", "a3")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(stats::runif(length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans <- trans / rowSums(trans)
  sim <- function(sid, n) {
    s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  lengths <- c(rep(4, 8), rep(80, 4))
  d <- do.call(rbind, Map(sim, paste0("V", seq_along(lengths)), lengths))
  list(
    name   = "variable session lengths (4 vs 80)",
    actors = list(Human = d[d$code %in% human_codes, ],
                  AI    = d[d$code %in% ai_codes, ])
  )
}

# Nearly-deterministic transitions: most rows of the transition matrix
# concentrate >90% mass on a single successor. Produces a very sparse
# weight matrix.
ds_near_deterministic <- function(seed = 15L) {
  set.seed(seed)
  human_codes <- c("h1", "h2", "h3"); ai_codes <- c("a1", "a2", "a3")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(0.02, length(alphabet), length(alphabet),
                  dimnames = list(alphabet, alphabet))
  # Concentrated successors: H1 -> A1, A1 -> H2, H2 -> A2, ...
  preferred <- c(h1 = "a1", a1 = "h2", h2 = "a2", a2 = "h3",
                 h3 = "a3", a3 = "h1")
  for (from in names(preferred)) {
    trans[from, preferred[[from]]] <- 0.9
  }
  trans <- trans / rowSums(trans)
  sim <- function(sid) {
    n <- sample(12:18, 1)
    s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("D", 1:25), sim))
  list(
    name   = "near-deterministic 3+3 codes",
    actors = list(Human = d[d$code %in% human_codes, ],
                  AI    = d[d$code %in% ai_codes, ])
  )
}

# tna::group_regulation_long re-cast as a two-actor partition, splitting
# the action vocabulary into Cognitive vs Social pseudo-actors. This
# exercises the equivalence on a *real*, larger dataset with different
# code names and a different sequence-length distribution.
ds_tna_group_regulation <- function() {
  testthat::skip_if_not_installed("tna")
  e <- new.env()
  utils::data(group_regulation_long, package = "tna", envir = e)
  d <- as.data.frame(e$group_regulation_long)
  cognitive <- c("plan", "monitor", "discuss", "synthesis", "adapt")
  social    <- c("cohesion", "consensus", "emotion", "coregulate")
  d <- d[d$Action %in% c(cognitive, social), , drop = FALSE]
  d$session_id <- paste(d$Group, d$Course, d$Actor, sep = "_")
  d <- d[order(d$session_id, d$Time), ]
  d$order_in_session <- ave(seq_len(nrow(d)), d$session_id,
                            FUN = seq_along)
  d$code <- d$Action
  long <- d[, c("session_id", "code", "order_in_session")]
  list(
    name   = "tna::group_regulation_long (Cognitive/Social split)",
    actors = list(
      Cognitive = long[long$code %in% cognitive, ],
      Social    = long[long$code %in% social, ]
    )
  )
}

# --- Pipeline builder --------------------------------------------------
#
# Build the htna / tna / nest triple from a dataset descriptor.

build_triple <- function(ds) {
  htna_net <- build_htna(ds$actors)
  tna_net  <- tna::tna(htna_net$data)
  combined <- do.call(rbind, ds$actors)
  combined <- combined[order(combined$session_id,
                             combined$order_in_session), ]
  nest_net <- Nestimate::build_network(combined,
                                       method  = "relative",
                                       action  = "code",
                                       session = "session_id",
                                       order   = "order_in_session",
                                       format  = "long")
  list(htna = htna_net, tna = tna_net, nest = nest_net)
}

# Centrality measures whose underlying algorithms are the same in
# cograph and tna and so should match cell-for-cell. The remaining three
# (BetweennessRSP, Diffusion, Clustering) are intentionally skipped --
# the vignette explains the divergence.
shared_centrality_measures <- c("OutStrength", "InStrength",
                                "ClosenessIn", "ClosenessOut",
                                "Closeness", "Betweenness")

# --- Dataset list ------------------------------------------------------

equivalence_datasets <- function() {
  out <- list(ds_nestimate(),
              ds_synthetic_balanced(),
              ds_synthetic_wide(),
              ds_tiny(),
              ds_imbalanced(),
              ds_disconnected(),
              ds_variable_lengths(),
              ds_near_deterministic())
  if (requireNamespace("tna", quietly = TRUE)) {
    out <- c(out, list(ds_tna_group_regulation()))
  }
  out
}

# --- Build a dataset via every supported input form -------------------

# Returns a named list with one htna network per input form. Skips the
# `node_groups` paths if any actor's code set overlaps with another's
# (none of the test datasets do, but the guard keeps the helper safe).
# `actor_order` is the desired actor-type ordering (a permutation of
# `names(ds$actors)`); every form is built so its `actor_levels`
# matches `actor_order`.
build_all_input_forms <- function(ds, actor_order = names(ds$actors),
                                  method = "relative") {
  actors <- ds$actors[actor_order]
  actor_levels <- actor_order

  # 1. Named list — list-name order encodes the desired ordering
  net_list <- build_htna(actors, method = method)

  # 2. Single combined frame + actor_type column (factor with explicit levels)
  combined <- do.call(rbind, lapply(actor_levels, function(a) {
    df <- actors[[a]][, c("session_id", "code", "order_in_session")]
    df$actor_type <- a
    df
  }))
  combined$actor_type <- factor(combined$actor_type, levels = actor_levels)
  combined <- combined[order(combined$session_id,
                             combined$order_in_session), ]
  net_at <- build_htna(combined, actor_type = "actor_type", method = method)

  # 3. Single combined frame + node_groups list — list-name order encodes ordering
  codes_per_actor <- lapply(actors, function(df) unique(df$code))
  net_ng_list <- build_htna(
    combined[, c("session_id", "code", "order_in_session")],
    node_groups = codes_per_actor,
    method = method
  )

  # 4. Single combined frame + node_groups data frame (codebook with factor)
  codebook <- do.call(rbind, lapply(actor_levels, function(a) {
    data.frame(code = codes_per_actor[[a]],
               actor_type = a,
               stringsAsFactors = FALSE)
  }))
  codebook$actor_type <- factor(codebook$actor_type, levels = actor_levels)
  net_ng_df <- build_htna(
    combined[, c("session_id", "code", "order_in_session")],
    node_groups = codebook,
    method = method
  )

  list(list_form        = net_list,
       actor_type_form  = net_at,
       node_groups_list = net_ng_list,
       node_groups_df   = net_ng_df)
}

# --- Tests -------------------------------------------------------------

test_that("all four input forms produce identical $weights, $initial, $node_groups, $nodes (diverse data)", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("Nestimate")

  # Per-slot alignment + comparison rules. `$data` is large enough
  # that we test it separately in the next block.
  cmp_slot <- function(ref_v, other_v, info) {
    if (is.data.frame(ref_v)) {
      # Identify the row key column: `node_groups` keys on `node`,
      # `nodes` keys on `label`; fall back to first column otherwise.
      key <- intersect(c("node", "label"), names(ref_v))[1L]
      if (is.na(key)) key <- names(ref_v)[1L]
      aligned <- other_v[match(ref_v[[key]], other_v[[key]]), ,
                         drop = FALSE]
      rownames(aligned) <- NULL
      ref_clean <- ref_v
      rownames(ref_clean) <- NULL
      expect_identical(aligned, ref_clean, info = info)
    } else if (!is.null(dim(ref_v))) {
      # Verify natural row/col order matches first; if it doesn't,
      # `[rownames(ref_v), colnames(ref_v), ]` would silently re-align
      # and mask the divergence.
      if (!is.null(rownames(ref_v)) && !is.null(rownames(other_v))) {
        expect_identical(rownames(other_v), rownames(ref_v),
                         info = paste0(info, " (rownames)"))
        expect_identical(colnames(other_v), colnames(ref_v),
                         info = paste0(info, " (colnames)"))
      }
      expect_equal(other_v, ref_v, info = info)
    } else {
      aligned <- if (!is.null(names(other_v)) && !is.null(names(ref_v))) {
        other_v[names(ref_v)]
      } else other_v
      expect_equal(aligned, ref_v, info = info)
    }
  }

  for (ds in equivalence_datasets()) {
    info  <- ds$name
    forms <- build_all_input_forms(ds)
    ref   <- forms$list_form
    for (slot in c("weights", "initial", "node_groups", "nodes",
                   "actor_levels")) {
      for (form_name in setdiff(names(forms), "list_form")) {
        cmp_slot(ref[[slot]], forms[[form_name]][[slot]],
                 info = paste0(info, " | ", slot, " | ", form_name))
      }
    }
  }
})

test_that("all four input forms agree for every `method =` (relative, frequency, attention)", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("Nestimate")
  for (method in c("relative", "frequency", "attention")) {
    for (ds in equivalence_datasets()) {
      info  <- paste0(ds$name, " | method=", method)
      forms <- build_all_input_forms(ds, method = method)
      ref   <- forms$list_form
      # Verify the chosen method propagates onto every form.
      for (form_name in names(forms)) {
        expect_identical(forms[[form_name]]$method, method,
                         info = paste0(info, " | ", form_name, " method"))
      }
      # And the resulting weight matrices are bit-identical across forms.
      for (form_name in setdiff(names(forms), "list_form")) {
        a <- ref$weights
        b <- forms[[form_name]]$weights[rownames(a), colnames(a),
                                        drop = FALSE]
        expect_equal(a, b, info = paste0(info, " | ", form_name,
                                          " weights"))
      }
    }
  }
})

test_that("all four input forms preserve the declared actor order in BOTH directions (diverse data)", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("Nestimate")
  for (ds in equivalence_datasets()) {
    info     <- ds$name
    natural  <- names(ds$actors)
    reversed <- rev(natural)
    for (declared in list(natural, reversed)) {
      forms <- build_all_input_forms(ds, actor_order = declared)
      for (form_name in names(forms)) {
        expect_identical(
          forms[[form_name]]$actor_levels,
          declared,
          info = paste0(info,
                        " | declared=", paste(declared, collapse=","),
                        " | ", form_name)
        )
      }
    }
  }
})

test_that("all four input forms produce a bit-identical wide session matrix (diverse data)", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("Nestimate")

  # `as.matrix(<data.frame>)` doesn't always preserve dimnames across
  # platforms, so strip them before comparing cells + NA pattern.
  to_plain_mat <- function(d) {
    m <- as.matrix(d)
    dimnames(m) <- NULL
    m
  }

  for (ds in equivalence_datasets()) {
    info  <- ds$name
    forms <- build_all_input_forms(ds)
    ref   <- forms$list_form$data
    rn    <- rownames(ref)
    cn    <- colnames(ref)
    ref_mat <- to_plain_mat(ref)
    ref_na  <- is.na(ref_mat)
    for (form_name in setdiff(names(forms), "list_form")) {
      raw <- forms[[form_name]]$data
      # Natural row + column order must match the reference *before*
      # alignment. Subsetting via `[rn, cn]` would silently re-order
      # any divergent natural ordering and mask the bug.
      expect_identical(rownames(raw), rn,
                       info = paste0(info, " | ", form_name,
                                     " natural row order"))
      expect_identical(colnames(raw), cn,
                       info = paste0(info, " | ", form_name,
                                     " natural col order"))
      m <- to_plain_mat(raw)
      expect_identical(m, ref_mat,
                       info = paste0(info, " | ", form_name, " cells"))
      expect_identical(is.na(m), ref_na,
                       info = paste0(info, " | ", form_name, " NA pattern"))
    }
  }
})

test_that("model weights match across htna, tna, Nestimate (diverse data)", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    # The meaningful equivalence is htna (Nestimate engine) vs tna
    # (independent engine, same wide matrix). htna vs nest is a
    # wrapper-integrity check (htna delegates to Nestimate).
    expect_mat_equiv(trip$htna$weights, trip$tna$weights,
                     info = paste0(info, " | weights htna vs tna"))
    expect_mat_equiv(trip$htna$weights, trip$nest$weights,
                     info = paste0(info, " | weights htna vs nest"))
  }
})

test_that("underlying session matrix is identical (htna vs Nestimate)", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    # Both should be a non-empty character matrix; content must match.
    expect_gt(nrow(trip$htna$data), 0L,
              label = paste0("rows in `", info, "$data`"))
    expect_equal(trip$htna$data, trip$nest$data, info = info)
  }
})

test_that("initial distribution matches across htna, tna, Nestimate", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    expect_vec_equiv(trip$htna$initial, trip$tna$inits,
                     info = paste0(info, " | initial htna vs tna"))
    expect_vec_equiv(trip$htna$initial, trip$nest$initial,
                     info = paste0(info, " | initial htna vs nest"))
  }
})

test_that("shared centrality measures match between htna and tna", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    htna_ct <- htna::centralities(trip$htna,
                                  measures = shared_centrality_measures)
    tna_ct  <- tna::centralities(trip$tna,
                                 measures = shared_centrality_measures)
    expect_setequal(htna_ct$node, tna_ct$state)
    htna_aligned <- htna_ct[match(tna_ct$state, htna_ct$node), ]
    for (m in shared_centrality_measures) {
      a <- setNames(htna_aligned[[m]], htna_aligned$node)
      b <- setNames(tna_ct[[m]],       tna_ct$state)
      expect_vec_equiv(a, b, info = paste0(info, " | ", m))
    }
  }
})

test_that("edge betweenness matches between htna and tna", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    htna_eb <- edge_betweenness_htna(trip$htna)
    tna_eb  <- tna::betweenness_network(trip$tna)
    expect_mat_equiv(htna_eb$weights, tna_eb$weights, info = info)
  }
})

test_that("bootstrap weights and CIs match across the three engines", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    set.seed(123); h_boot <- bootstrap_htna(trip$htna, iter = 30,
                                            consistency_range = c(0.75, 1.25))
    set.seed(123); t_boot <- tna::bootstrap(trip$tna, iter = 30,
                                            consistency_range = c(0.75, 1.25))
    set.seed(123); n_boot <- Nestimate::bootstrap_network(trip$nest,
                                                          iter = 30)
    # `boot$model$weights` is the significance-filtered network: it can
    # legitimately be all-zero on small samples where no edge clears the
    # consistency-band test, so we don't enforce nontriviality here.
    # The point-estimate equivalence is already covered by the model
    # weights test above.
    expect_mat_equiv(h_boot$model$weights, t_boot$model$weights,
                     info = paste0(info, " | boot weights htna vs tna"),
                     require_signal = FALSE)
    expect_mat_equiv(h_boot$model$weights, n_boot$model$weights,
                     info = paste0(info, " | boot weights htna vs nest"),
                     require_signal = FALSE)
    # CI bounds typically span a wider range than the point estimate;
    # require_signal guards against an all-zero CI accidentally matching.
    expect_mat_equiv(h_boot$ci_lower, t_boot$ci_lower,
                     info = paste0(info, " | ci_lower htna vs tna"),
                     require_signal = FALSE)
    expect_mat_equiv(h_boot$ci_lower, n_boot$ci_lower,
                     info = paste0(info, " | ci_lower htna vs nest"),
                     require_signal = FALSE)
    expect_mat_equiv(h_boot$ci_upper, t_boot$ci_upper,
                     info = paste0(info, " | ci_upper htna vs tna"))
    expect_mat_equiv(h_boot$ci_upper, n_boot$ci_upper,
                     info = paste0(info, " | ci_upper htna vs nest"))
    # htna is a thin wrapper over Nestimate, so p_values must match
    # (they are *not* expected to match tna's stability-band p_values).
    expect_mat_equiv(h_boot$p_values, n_boot$p_values,
                     info = paste0(info, " | p_values htna vs nest"))

    # Per-edge summary table fields the vignette also asserts on.
    # Sort each summary by the (from, to) edge key so positional equality
    # is meaningful, and verify all three engines emit the same edge set.
    sort_by_edge <- function(d) {
      d[order(as.character(d$from), as.character(d$to)), ]
    }
    h_sum <- sort_by_edge(h_boot$summary)
    t_sum <- sort_by_edge(t_boot$summary)
    n_sum <- sort_by_edge(n_boot$summary)
    edge_key <- function(d) paste(d$from, d$to, sep = "->")
    expect_setequal(edge_key(h_sum), edge_key(t_sum))
    expect_setequal(edge_key(h_sum), edge_key(n_sum))
    expect_gt(sum(h_sum$weight != 0, na.rm = TRUE), 0L,
              label = paste0("nonzero rows in `", info, "$summary$weight`"))
    for (col in c("weight", "cr_lower", "cr_upper",
                  "ci_lower", "ci_upper")) {
      expect_equal(h_sum[[col]], t_sum[[col]],
                   info = paste0(info, " | summary$", col, " htna vs tna"))
      expect_equal(h_sum[[col]], n_sum[[col]],
                   info = paste0(info, " | summary$", col, " htna vs nest"))
    }
    # `sig` matches htna vs nest (same engine); tna's `sig` derives from
    # the consistency-band p_values which differ definitionally.
    expect_equal(h_sum$sig, n_sum$sig,
                 info = paste0(info, " | summary$sig htna vs nest"))
  }
})

test_that("permutation diff/p_values/effect_size/diff_sig match (htna vs Nestimate)", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    actors <- ds$actors
    halve <- function(df) {
      n <- nrow(df)
      list(a = df[seq_len(n %/% 2L), , drop = FALSE],
           b = df[(n %/% 2L + 1L):n, , drop = FALSE])
    }
    halves <- lapply(actors, halve)
    actors_x <- lapply(halves, `[[`, "a")
    actors_y <- lapply(halves, `[[`, "b")
    htna_x <- build_htna(actors_x)
    htna_y <- build_htna(actors_y)

    combined_x <- do.call(rbind, actors_x)
    combined_x <- combined_x[order(combined_x$session_id,
                                   combined_x$order_in_session), ]
    combined_y <- do.call(rbind, actors_y)
    combined_y <- combined_y[order(combined_y$session_id,
                                   combined_y$order_in_session), ]
    nest_x <- Nestimate::build_network(combined_x, method = "relative",
                                       action = "code",
                                       session = "session_id",
                                       order = "order_in_session",
                                       format = "long")
    nest_y <- Nestimate::build_network(combined_y, method = "relative",
                                       action = "code",
                                       session = "session_id",
                                       order = "order_in_session",
                                       format = "long")

    set.seed(42); h_perm <- htna::permutation(htna_x, htna_y, iter = 50)
    set.seed(42); n_perm <- Nestimate::permutation(nest_x, nest_y,
                                                   iter = 50)

    expect_mat_equiv(h_perm$diff, n_perm$diff,
                     info = paste0(info, " | perm diff htna vs nest"))
    expect_mat_equiv(h_perm$p_values, n_perm$p_values,
                     info = paste0(info, " | perm p_values htna vs nest"))
    expect_mat_equiv(h_perm$effect_size, n_perm$effect_size,
                     info = paste0(info, " | perm effect_size htna vs nest"))
    # diff_sig can be all-zero on small samples (no edges crossing the
    # alpha threshold), so don't require nonzero signal here.
    expect_mat_equiv(h_perm$diff_sig, n_perm$diff_sig,
                     info = paste0(info, " | perm diff_sig htna vs nest"),
                     require_signal = FALSE)
  }
})

test_that("permutation diff matches between htna and tna (padded matrices)", {
  skip_if_missing_eq_deps()
  pad_to <- function(d, n) {
    if (ncol(d) >= n) return(d)
    extra <- as.data.frame(matrix(NA_character_, nrow = nrow(d),
                                  ncol = n - ncol(d)))
    names(extra) <- paste0("T", (ncol(d) + 1L):n)
    cbind(d, extra)
  }
  for (ds in equivalence_datasets()) {
    info <- ds$name
    actors <- ds$actors
    halve <- function(df) {
      n <- nrow(df)
      list(a = df[seq_len(n %/% 2L), , drop = FALSE],
           b = df[(n %/% 2L + 1L):n, , drop = FALSE])
    }
    halves <- lapply(actors, halve)
    htna_x <- build_htna(lapply(halves, `[[`, "a"))
    htna_y <- build_htna(lapply(halves, `[[`, "b"))

    set.seed(42); h_perm <- htna::permutation(htna_x, htna_y, iter = 50)

    n_cols <- max(ncol(htna_x$data), ncol(htna_y$data))
    tna_x  <- tna::tna(pad_to(htna_x$data, n_cols))
    tna_y  <- tna::tna(pad_to(htna_y$data, n_cols))
    set.seed(42); t_perm <- tna::permutation_test(tna_x, tna_y,
                                                  iter = 50)
    # tna stores the diff matrix at $edges$diffs_true (same construction
    # as Nestimate's $diff, so it should be cell-identical to htna$diff
    # on the shared node set). tna's p_values are *not* compared here --
    # they use a different test statistic (vignette §4a).
    h_diff <- h_perm$diff
    t_diff <- t_perm$edges$diffs_true
    shared <- intersect(rownames(h_diff), rownames(t_diff))
    expect_gt(length(shared), 1L,
              label = paste0("shared nodes in `", info, "`"))
    expect_mat_equiv(h_diff[shared, shared, drop = FALSE],
                     t_diff[shared, shared, drop = FALSE],
                     info = paste0(info, " | perm diff htna vs tna"))
  }
})

# ---- Centrality stability equivalence (htna ↔ tna ↔ Nestimate) -------

test_that("centrality_stability cs values match across htna, tna, Nestimate", {
  skip_if_missing_eq_deps()
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    n_sessions <- nrow(trip$htna$data)
    set.seed(7)
    h_cs <- centrality_stability_htna(trip$htna, iter = 10L, seed = 7L)
    set.seed(7)
    n_cs <- Nestimate::centrality_stability(trip$nest, iter = 10L,
                                            seed = 7L)

    # htna vs Nestimate: bit-identical for every dataset (htna
    # delegates to Nestimate's engine), every measure, every cell.
    expect_equal(h_cs$cs, n_cs$cs,
                 info = paste0(info, " | cs htna vs nest"))
    for (m in names(h_cs$correlations)) {
      expect_identical(unname(h_cs$correlations[[m]]),
                       unname(n_cs$correlations[[m]]),
                       info = paste0(info,
                                     " | correlations htna vs nest | ",
                                     m))
    }

    # htna vs tna: tna skips drop proportions where rounding leaves
    # zero cases dropped (warning: "No cases dropped..."), which
    # shifts the CS coefficient by one step on very small datasets.
    # We only assert agreement when every drop step actually drops
    # at least one session (n_sessions * 0.1 >= 1 → n_sessions ≥ 10).
    if (n_sessions >= 10L) {
      set.seed(7)
      t_cs <- suppressWarnings(tna::estimate_cs(trip$tna, iter = 10))
      measures <- intersect(names(h_cs$cs), names(t_cs))
      expect_gt(length(measures), 0L)
      for (m in measures) {
        expect_equal(unname(h_cs$cs[[m]]),
                     unname(t_cs[[m]]$cs_coefficient),
                     info = paste0(info, " | cs htna vs tna | ", m))
      }
      # InStrength/OutStrength: per-cell bit-identical against tna
      # (their underlying centralities use the same code path). We
      # assert this only for the strength measures; Betweenness can
      # diverge by O(1e-2) on small/odd graphs because of differences
      # in tna's vs Nestimate's betweenness implementation when
      # graph structure changes after dropping cases.
      for (m in intersect(measures, c("InStrength", "OutStrength"))) {
        h_mat <- unname(h_cs$correlations[[m]])
        t_mat <- unname(t_cs[[m]]$correlations)
        # NaN positions can appear when a resample has zero
        # variance in the centrality vector; require they agree on
        # which positions are NaN, and bit-identical elsewhere.
        expect_identical(is.nan(h_mat) | is.na(h_mat),
                         is.nan(t_mat) | is.na(t_mat),
                         info = paste0(info, " | NaN pattern | ", m))
        ok <- !is.nan(h_mat) & !is.na(h_mat)
        expect_identical(h_mat[ok], t_mat[ok],
                         info = paste0(info,
                                       " | strength correlations | ",
                                       m))
      }
    }
  }
})

# ---- Reliability equivalence (htna ↔ Nestimate only) ------------------

test_that("network_reliability is bit-identical between htna and Nestimate", {
  skip_if_missing_eq_deps()
  # tna's reliability uses a different metric family (Bray-Curtis,
  # Cosine, Dice, ...), so we only verify htna ↔ Nestimate here.
  for (ds in equivalence_datasets()) {
    info <- ds$name
    trip <- build_triple(ds)
    set.seed(11)
    h_rel <- reliability_htna(trip$htna, iter = 10L, seed = 11L)
    set.seed(11)
    n_rel <- Nestimate::network_reliability(trip$nest, iter = 10L,
                                            seed = 11L)
    expect_equal(h_rel$iterations, n_rel$iterations,
                 info = paste0(info, " | reliability iterations"))
    expect_equal(h_rel$summary, n_rel$summary,
                 info = paste0(info, " | reliability summary"))

    # Cell-level bit-identity for every numeric metric column.
    metric_cols <- setdiff(names(h_rel$iterations), "model")
    for (col in metric_cols) {
      expect_identical(h_rel$iterations[[col]],
                       n_rel$iterations[[col]],
                       info = paste0(info,
                                     " | iterations bit-identical | ",
                                     col))
    }
  }
})
