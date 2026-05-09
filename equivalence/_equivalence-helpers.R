# Shared helpers and dataset providers for the per-dataset
# equivalence vignettes. Sourced from each vignette's setup chunk.
# This file is intentionally prefixed with `_` so pkgdown's article
# auto-discovery skips it.

# Pairwise cell-level equivalence summary used throughout the
# equivalence vignettes.
eq_check <- function(a, b, label = "") {
  if (is.null(a) || is.null(b)) {
    return(data.frame(check = label, equal = NA, max_abs_diff = NA_real_,
                      n_disagree = NA_integer_, stringsAsFactors = FALSE))
  }
  diff <- as.numeric(a - b)
  data.frame(
    check         = label,
    equal         = isTRUE(all.equal(a, b, check.attributes = FALSE)),
    max_abs_diff  = max(abs(diff), na.rm = TRUE),
    n_disagree    = sum(diff != 0, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Three-way comparison: returns one row labelling each pairwise check.
eq3 <- function(htna, tna, nest, label = "") {
  rbind(
    transform(eq_check(htna, tna,  paste0(label, " | htna vs tna")),  pair = "htna vs tna"),
    transform(eq_check(htna, nest, paste0(label, " | htna vs nest")), pair = "htna vs nest"),
    transform(eq_check(tna,  nest, paste0(label, " | tna vs nest")),  pair = "tna vs nest")
  )
}

# --- Dataset providers ------------------------------------------------
#
# Each provider returns a list with $name (label) and $actors (named
# list of long frames, one per actor; each frame has session_id, code,
# order_in_session columns).

ds_nestimate <- function() {
  e <- new.env()
  utils::data(human_long, ai_long, package = "Nestimate", envir = e)
  list(name   = "Nestimate human_long + ai_long",
       actors = list(Human = e$human_long, AI = e$ai_long))
}

ds_synthetic_balanced <- function(seed = 1L) {
  set.seed(seed)
  human_codes <- c("H1", "H2", "H3"); ai_codes <- c("A1", "A2", "A3")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(rep(1 / length(alphabet), length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans["H1", ] <- c(0.05, 0.05, 0.05, 0.70, 0.10, 0.05)
  trans["A2", ] <- c(0.30, 0.10, 0.10, 0.10, 0.10, 0.30)
  sim <- function(sid) {
    n <- sample(8:14, 1); s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("S", 1:20), sim))
  list(name   = "synthetic balanced 3+3 codes",
       actors = list(Human = d[d$code %in% human_codes, ],
                     AI    = d[d$code %in% ai_codes, ]))
}

ds_synthetic_wide <- function(seed = 2L) {
  set.seed(seed)
  human_codes <- c("Hask", "Hexplain", "Hcorrect", "Hverify", "Hreflect")
  ai_codes    <- c("Aplan", "Aexec", "Adebug", "Aexplain")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(stats::runif(length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans <- trans / rowSums(trans)
  sim <- function(sid) {
    n <- sample(15:25, 1); s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("W", 1:30), sim))
  list(name   = "synthetic wide 5+4 codes",
       actors = list(Human = d[d$code %in% human_codes, ],
                     AI    = d[d$code %in% ai_codes, ]))
}

ds_tiny <- function(seed = 11L) {
  set.seed(seed)
  human_codes <- c("h1", "h2"); ai_codes <- c("a1", "a2")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(0.25, 4, 4, dimnames = list(alphabet, alphabet))
  sim <- function(sid) {
    n <- sample(5:7, 1); s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("T", 1:6), sim))
  list(name   = "tiny 6 sessions, 2+2 codes",
       actors = list(Human = d[d$code %in% human_codes, ],
                     AI    = d[d$code %in% ai_codes, ]))
}

ds_imbalanced <- function(seed = 12L) {
  set.seed(seed)
  human_codes <- "h_only"
  ai_codes    <- c("a1", "a2", "a3", "a4", "a5")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(stats::runif(length(alphabet) ^ 2),
                  nrow = length(alphabet),
                  dimnames = list(alphabet, alphabet))
  trans <- trans / rowSums(trans)
  sim <- function(sid) {
    n <- sample(10:18, 1); s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("I", 1:20), sim))
  list(name   = "imbalanced 1 vs 5 codes",
       actors = list(Human = d[d$code %in% human_codes, ],
                     AI    = d[d$code %in% ai_codes, ]))
}

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
    n <- sample(10:14, 1); s <- character(n); s[1] <- sample(codes, 1)
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(codes, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  human <- do.call(rbind, lapply(paste0("Hd", 1:12), sim,
                                 human_codes, trans_h))
  ai    <- do.call(rbind, lapply(paste0("Ad", 1:12), sim,
                                 ai_codes,    trans_a))
  list(name   = "disconnected actors (block-diagonal)",
       actors = list(Human = human, AI = ai))
}

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
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  lengths <- c(rep(4, 8), rep(80, 4))
  d <- do.call(rbind, Map(sim, paste0("V", seq_along(lengths)), lengths))
  list(name   = "variable session lengths (4 vs 80)",
       actors = list(Human = d[d$code %in% human_codes, ],
                     AI    = d[d$code %in% ai_codes, ]))
}

ds_near_deterministic <- function(seed = 15L) {
  set.seed(seed)
  human_codes <- c("h1", "h2", "h3"); ai_codes <- c("a1", "a2", "a3")
  alphabet <- c(human_codes, ai_codes)
  trans <- matrix(0.02, length(alphabet), length(alphabet),
                  dimnames = list(alphabet, alphabet))
  preferred <- c(h1 = "a1", a1 = "h2", h2 = "a2", a2 = "h3",
                 h3 = "a3", a3 = "h1")
  for (from in names(preferred)) trans[from, preferred[[from]]] <- 0.9
  trans <- trans / rowSums(trans)
  sim <- function(sid) {
    n <- sample(12:18, 1); s <- character(n); s[1] <- sample(alphabet, 1)
    for (i in seq_len(n - 1L))
      s[i + 1L] <- sample(alphabet, 1, prob = trans[s[i], ])
    data.frame(session_id = sid, code = s,
               order_in_session = seq_len(n), stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, lapply(paste0("D", 1:25), sim))
  list(name   = "near-deterministic 3+3 codes",
       actors = list(Human = d[d$code %in% human_codes, ],
                     AI    = d[d$code %in% ai_codes, ]))
}

ds_tna_group_regulation <- function() {
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
  list(name   = "tna::group_regulation_long (Cognitive/Social)",
       actors = list(
         Cognitive = long[long$code %in% cognitive, ],
         Social    = long[long$code %in% social, ]))
}
