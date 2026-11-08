# The four input forms of build_htna() (a named list of actor logs, one
# combined log, a wide matrix and an actor_type column) give one network.
# These compare htna with itself; comparisons with tna and Nestimate are in
# equivalence/test-equiv-tna.R (local only).

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

test_that("all four input forms agree for every transition/co-occurrence method", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("Nestimate")
  for (method in c("relative", "frequency", "co_occurrence", "attention")) {
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

