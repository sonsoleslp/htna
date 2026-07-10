# Shared fixtures across the test suite.
# `make_htna()` returns a small two-actor htna network built from the
# Nestimate example data; `make_htna_group()` builds a small grouped
# version with two cohorts that share the same alphabet.

make_htna <- function(seed = 1L) {
  set.seed(seed)
  # Use actual Nestimate codes for test compatibility
  human_codes <- c("Command", "Correct", "Frustrate", "Inquire", "Interrupt",
                   "Refine", "Request", "Specify", "Verify")
  ai_codes <- c("Ask", "Delegate", "Execute", "Explain", "Investigate",
                "Plan", "Repair", "Report")

  # Create sessions with cross-actor transitions using Nestimate codes
  sim_session <- function(sid, prefix) {
    n <- sample(5:8, 1)
    s <- character(n)
    actor_seq <- character(n)

    # Start with random actor
    current_actor <- sample(c("Human", "AI"), 1)
    s[1] <- if (current_actor == "Human") sample(human_codes, 1) else sample(ai_codes, 1)
    actor_seq[1] <- current_actor

    for (i in seq_len(n - 1L)) {
      # 25% chance to switch actors (realistic cross-actor transitions)
      if (runif(1) < 0.25) {
        current_actor <- if (current_actor == "Human") "AI" else "Human"
      }
      s[i + 1L] <- if (current_actor == "Human") sample(human_codes, 1) else sample(ai_codes, 1)
      actor_seq[i + 1L] <- current_actor
    }

    data.frame(session_id = paste0(prefix, sid),
               code = s,
               order_in_session = seq_len(n),
               actor = actor_seq,
               stringsAsFactors = FALSE)
  }

  # Create combined data with cross-actor transitions
  combined_data <- do.call(rbind, lapply(1:20, sim_session, "S"))

  # Add a deterministic session with specific sequences that tests expect
  fixed_session <- data.frame(
    session_id = "FIXED1",
    code = c("Command", "Ask", "Correct", "Execute", "Verify"),
    order_in_session = 1:5,
    actor = c("Human", "AI", "Human", "AI", "Human"),
    stringsAsFactors = FALSE
  )

  combined_data <- rbind(combined_data, fixed_session)

  # Ensure correct actor level ordering for test compatibility
  combined_data$actor <- factor(combined_data$actor, levels = c("Human", "AI"))

  # Suppress warnings for cleaner test output
  suppressWarnings(build_htna(combined_data, actor_type = "actor"))
}

# Synthetic Control vs Experimental data with shared alphabet, suitable
# for grouped tests AND for diff/permutation tests (same node set).
make_grouped_data <- function(seed = 1L) {
  set.seed(seed)
  human_codes <- c("H1", "H2", "H3")
  ai_codes    <- c("A1", "A2", "A3")
  alphabet    <- c(human_codes, ai_codes)

  trans_ctrl <- matrix(rep(1 / length(alphabet), length(alphabet) ^ 2),
                       nrow = length(alphabet),
                       dimnames = list(alphabet, alphabet))
  trans_exp  <- trans_ctrl
  # bias Experimental: H1 -> A1 strongly preferred
  trans_exp["H1", ] <- c(0.05, 0.05, 0.05, 0.70, 0.10, 0.05)

  sim <- function(sid, condition, trans) {
    n <- sample(6:10, 1)  # Moderate session length for stability
    s <- character(n)
    s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L)) {
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    }
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n),
               condition = condition,
               stringsAsFactors = FALSE)
  }

  rbind(
    do.call(rbind, lapply(paste0("C", 1:15), sim, "Control", trans_ctrl)),
    do.call(rbind, lapply(paste0("E", 1:15), sim, "Experimental", trans_exp))
  )
}

make_htna_group <- function(seed = 1L) {
  d <- make_grouped_data(seed)
  # Suppress Nestimate warnings about long sequences in test data
  suppressWarnings(build_htna(d,
             node_groups = list(Human = c("H1", "H2", "H3"),
                                AI    = c("A1", "A2", "A3")),
             group = "condition"))
}

# Two single htna networks built from the same alphabet (so diff/permutation
# work without dimension mismatches).
make_htna_pair <- function(seed = 1L) {
  d <- make_grouped_data(seed)
  ctrl <- d[d$condition == "Control", ]
  exp  <- d[d$condition == "Experimental", ]
  # Suppress Nestimate warnings about long sequences in test data
  list(
    ctrl = suppressWarnings(build_htna(ctrl,
                      node_groups = list(Human = c("H1", "H2", "H3"),
                                         AI    = c("A1", "A2", "A3")))),
    exp  = suppressWarnings(build_htna(exp,
                      node_groups = list(Human = c("H1", "H2", "H3"),
                                         AI    = c("A1", "A2", "A3"))))
  )
}

# Capture a base-graphics plot to a null device so plot tests don't pollute.
with_null_device <- function(expr) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

# Skip a test if the loaded Nestimate is missing internal helpers that
# `Nestimate:::.reliability_association()` calls. Nestimate 0.5.4 ships a
# NAMESPACE that references `.param_get` without exporting/defining it,
# which causes a hard error at htna's call sites — out of htna's control.
skip_if_nestimate_param_get_missing <- function() {
  testthat::skip_if_not_installed("Nestimate")
  if (!exists(".param_get", envir = asNamespace("Nestimate"),
              inherits = FALSE)) {
    testthat::skip("Nestimate is missing internal `.param_get` (upstream bug).")
  }
}

# The measure set `centrality_stability_htna()` requests, read off its own
# formals so the equivalence tests cannot drift from the default.
htna_cs_measures <- function() {
  eval(formals(centrality_stability_htna)$measures)
}
