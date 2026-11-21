# frequencies_htna(), state_distribution_htna(), state_frequencies_htna(): how often each code occurs, overall and
# within its actor. Counted from the data in base R. The three forward to Nestimate's state_distribution() and
# state_frequencies(). Re-expressed from htna's equivalence/test-equiv-delegation.R, which compared the wrappers
# with the Nestimate calls they make.

data(human_ai)
by_hand <- "independent: table of the codes, base R"
actor_of <- ref_actor_of(lapply(split(human_ai$code, human_ai$actor_type), unique))

# Count and share of each code, the share taken within each code's actor.
ref_actor_counts <- function(events) {
  counts <- table(events$code)
  out <- data.frame(group = unname(actor_of[names(counts)]), state = names(counts), count = as.vector(counts),
                    stringsAsFactors = FALSE)
  out$proportion <- out$count / stats::ave(out$count, out$group, FUN = sum)
  out
}

test_that("frequencies_htna() and state_distribution_htna(): counts and shares within each actor", {
  net <- build_htna(human_ai, actor_type = "actor_type")
  expected <- ref_actor_counts(human_ai)
  expect_reference(frequencies_htna(net), expected, reference = by_hand, covers = "frequencies_htna()",
                   quantity = "human_ai, codes per actor", tolerance = 1e-12, key = c("group", "state"))
  expect_reference(state_distribution_htna(net), expected, reference = by_hand, covers = "state_distribution_htna()",
                   quantity = "human_ai, codes per actor", tolerance = 1e-12, key = c("group", "state"))
})

test_that("frequencies_htna() of a grouped network: counts and shares within each phase and actor", {
  phase_of <- tapply(as.character(human_ai$phase), human_ai$session_id, \(phase) phase[[1L]])
  expected <- do.call(rbind, lapply(c("Early", "Late"), \(phase) {
    sessions <- names(phase_of)[phase_of == phase]
    counts <- ref_actor_counts(subset(human_ai, session_id %in% sessions))
    names(counts)[names(counts) == "group"] <- "actor"
    cbind(group = phase, counts)
  }))
  expect_reference(frequencies_htna(build_htna(human_ai, actor_type = "actor_type", group = "phase")), expected,
                   reference = by_hand, covers = "frequencies_htna()", quantity = "human_ai by phase, codes per actor",
                   tolerance = 1e-12, key = c("group", "actor", "state"))
})

test_that("state_frequencies_htna(): counts and shares over all events", {
  counts <- table(human_ai$code)
  found <- state_frequencies_htna(ref_wide(human_ai))
  expect_reference(found, data.frame(state = names(counts), count = as.vector(counts)), reference = by_hand,
                   covers = "state_frequencies_htna()", quantity = "human_ai, count of each code", tolerance = 0,
                   key = "state")
  # The share is returned rounded to four decimals (Nestimate::state_frequencies()): half a unit of the fourth.
  expect_reference(found, data.frame(state = names(counts), proportion = as.vector(counts) / sum(counts)),
                   reference = by_hand, covers = "state_frequencies_htna()", quantity = "human_ai, share of each code",
                   tolerance = 5e-5, key = "state")
})
