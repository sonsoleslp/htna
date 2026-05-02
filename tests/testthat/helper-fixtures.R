# Shared fixtures across the test suite.
# `make_htna()` returns a small two-actor htna network built from the
# Nestimate example data; `make_htna_group()` builds a small grouped
# version with two cohorts that share the same alphabet.

make_htna <- function() {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  build_htna(list(Human = human_long, AI = ai_long))
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
    n <- sample(8:14, 1)
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
    do.call(rbind, lapply(paste0("C", 1:10), sim, "Control", trans_ctrl)),
    do.call(rbind, lapply(paste0("E", 1:10), sim, "Experimental", trans_exp))
  )
}

make_htna_group <- function(seed = 1L) {
  d <- make_grouped_data(seed)
  build_htna(d,
             node_groups = list(Human = c("H1", "H2", "H3"),
                                AI    = c("A1", "A2", "A3")),
             group = "condition")
}

# Two single htna networks built from the same alphabet (so diff/permutation
# work without dimension mismatches).
make_htna_pair <- function(seed = 1L) {
  d <- make_grouped_data(seed)
  ctrl <- d[d$condition == "Control", ]
  exp  <- d[d$condition == "Experimental", ]
  list(
    ctrl = build_htna(ctrl,
                      node_groups = list(Human = c("H1", "H2", "H3"),
                                         AI    = c("A1", "A2", "A3"))),
    exp  = build_htna(exp,
                      node_groups = list(Human = c("H1", "H2", "H3"),
                                         AI    = c("A1", "A2", "A3")))
  )
}

# Capture a base-graphics plot to a null device so plot tests don't pollute.
with_null_device <- function(expr) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}
