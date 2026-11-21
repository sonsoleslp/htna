# extract_meta_paths(): the paths of k codes in the sessions (contiguous, or gap + 1 positions apart) and their
# meta-paths (the actors they pass through), with count, number of sessions containing them, support, share among
# the paths of the same length and gap, and lift. No other package returns this table; every column is written
# from the definition in base R (helper-reference.R, ref_paths()): the windows of each session read in the
# sessions themselves, not in htna's data. The number of sessions containing a contiguous path is also the support
# count (COBJ) of TraMineR's event subsequences with max.gap = 1, one event per position.

data(human_ai)
by_hand <- "independent: windows of each session, base R"
sequences <- ref_sequences(ref_wide(human_ai))
actor_of <- ref_actor_of(lapply(split(human_ai$code, human_ai$actor_type), unique))
net <- build_htna(human_ai, actor_type = "actor_type")

# The rows of a selection of paths, with their share recomputed among the selected rows (as extract_meta_paths()
# reports the share of a schema search: among the paths that match the schema).
ref_selected <- function(paths, keep) {
  selected <- paths[keep, , drop = FALSE]
  selected$frequency <- selected$count / sum(selected$count)
  selected
}

test_that("contiguous paths of length 2 to 4: every column from the definition", {
  expected <- do.call(rbind, lapply(2:4, \(k) ref_paths(sequences, k = k)))
  expected$meta_schema <- vapply(strsplit(expected$schema, "->", fixed = TRUE),
                                 \(parts) paste(actor_of[parts], collapse = "->"), character(1))
  expect_reference(extract_meta_paths(net), expected, reference = by_hand, covers = "extract_meta_paths()",
                   quantity = "human_ai, paths of length 2 to 4", tolerance = 1e-10,
                   key = c("schema", "length", "gap"))
})

test_that("meta-paths (level = 'type') of length 3 are the paths of the sessions read as actors", {
  as_actors <- lapply(sequences, \(codes) unname(actor_of[codes]))
  expect_reference(extract_meta_paths(net, level = "type", length = 3), ref_paths(as_actors, k = 3),
                   reference = by_hand, covers = "extract_meta_paths()", quantity = "human_ai, meta-paths of length 3",
                   tolerance = 1e-10, key = c("schema", "length", "gap"))
})

test_that("gapped paths (gap 1 and 2) of length 3", {
  expected <- rbind(ref_paths(sequences, k = 3, gap = 1L), ref_paths(sequences, k = 3, gap = 2L))
  expect_reference(extract_meta_paths(net, length = 3, type = "gapped", gap = 1:2), expected, reference = by_hand,
                   covers = "extract_meta_paths()", quantity = "human_ai, gapped paths of length 3", tolerance = 1e-10,
                   key = c("schema", "length", "gap"))
})

test_that("a schema of actors, and a schema of an actor, a code and a wildcard", {
  paths <- ref_paths(sequences, k = 3)
  parts <- strsplit(paths$schema, "->", fixed = TRUE)
  meta <- vapply(parts, \(p) paste(actor_of[p], collapse = "->"), character(1))
  expect_reference(extract_meta_paths(net, schema = "Human->AI->Human"),
                   ref_selected(paths, meta == "Human->AI->Human"), reference = by_hand,
                   covers = "extract_meta_paths()", quantity = "human_ai, schema Human->AI->Human", tolerance = 1e-10,
                   key = c("schema", "length", "gap"))
  human_ask <- vapply(parts, \(p) identical(unname(actor_of[p[[1L]]]), "Human") && identical(p[[2L]], "Ask"),
                      logical(1))
  expected <- ref_selected(paths, human_ask)
  expect_reference(extract_meta_paths(net, schema = "Human->Ask->*", min_lift = 1),
                   subset(expected, lift >= 1), reference = by_hand, covers = "extract_meta_paths()",
                   quantity = "human_ai, schema Human->Ask->*, lift at least 1", tolerance = 1e-10,
                   key = c("schema", "length", "gap"))
})

test_that("sessions containing a contiguous path equal TraMineR's subsequence support with max.gap = 1", {
  skip_without_reference("TraMineR")
  chosen <- utils::head(sort(unique(human_ai$session_id)), 60L)
  events <- subset(human_ai, session_id %in% chosen)
  events <- events[order(events$session_id, events$order_in_session), , drop = FALSE]
  event_sequences <- TraMineR::seqecreate(id = match(events$session_id, chosen), timestamp = events$order_in_session,
                                          event = events$code)
  mined <- TraMineR::seqefsub(event_sequences, pmin.support = 1 / 1000, max.k = 3,
                              constraint = TraMineR::seqeconstraint(max.gap = 1))
  subsequences <- gsub("^\\(|\\)$", "", as.character(mined$subseq))
  expected <- data.frame(schema = gsub(")-(", "->", subsequences, fixed = TRUE), n_sequences = mined$data$Count,
                         stringsAsFactors = FALSE)
  expected <- subset(expected, grepl("->", schema, fixed = TRUE))
  expected$length <- lengths(strsplit(expected$schema, "->", fixed = TRUE))
  expected$gap <- 0L
  found <- extract_meta_paths(build_htna(events, actor_type = "actor_type"), length = 2:3)
  expect_reference(found, expected, reference = "TraMineR", covers = "extract_meta_paths()",
                   quantity = "60 human_ai sessions, sessions containing the path", tolerance = 0,
                   key = c("schema", "length", "gap"))
})
