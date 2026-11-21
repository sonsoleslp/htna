# Reference-side helpers of the htna equivalence tests: the input data sets and the reshaping of the references'
# results into tables keyed like htna's. Base R only (and the data sets of Nestimate and tna); nothing here calls
# htna, so each function is one side of a comparison and htna is the other.

#' Random two-actor sessions in long format: one row per event, codes drawn uniformly
#'
#' @param codes The codes to draw from.
#' @param n_sessions Number of sessions.
#' @param min_length,max_length Range of the session lengths.
#' @param seed The seed of the draw (the input only; no package randomness is replayed).
#' @param prefix Prefix of the session ids.
#' @return A data frame `session_id`, `code`, `order_in_session`.
ref_random_long <- function(codes, n_sessions, min_length, max_length, seed, prefix = "S") {
  stopifnot("`codes` must be character" = is.character(codes))
  set.seed(seed)
  lengths <- sample(seq.int(min_length, max_length), n_sessions, replace = TRUE)
  sessions <- sprintf("%s%02d", prefix, seq_len(n_sessions))
  data.frame(session_id = rep(sessions, lengths), code = sample(codes, sum(lengths), replace = TRUE),
             order_in_session = sequence(lengths), stringsAsFactors = FALSE)
}

#' The events of one long table split by actor: one long table per actor, the form build_htna() takes as a list
#'
#' @param long A long table with a `code` column.
#' @param codes_by_actor A named list of the codes of each actor.
#' @return A named list of data frames.
ref_split_actors <- function(long, codes_by_actor) {
  lapply(codes_by_actor, \(codes) long[long$code %in% codes, , drop = FALSE])
}

#' The data sets of the comparisons: each a list with `name`, `actors` (one long table per actor, list form of
#' build_htna()) and `codes` (the codes of each actor)
#'
#' Re-expressed from the providers of htna's equivalence/ folder (seeded draws, without their loops).
#' @return A list of data sets.
ref_datasets <- function() {
  balanced <- list(Human = c("H1", "H2", "H3"), AI = c("A1", "A2", "A3"))
  imbalanced <- list(Human = "h_only", AI = c("a1", "a2", "a3", "a4", "a5"))
  separate <- list(Human = c("h1", "h2", "h3"), AI = c("a1", "a2", "a3"))
  nestimate <- new.env()
  utils::data(human_long, ai_long, package = "Nestimate", envir = nestimate)
  variable <- rbind(ref_random_long(unlist(balanced), 8L, 4L, 4L, seed = 14L, prefix = "V"),
                    ref_random_long(unlist(balanced), 4L, 80L, 80L, seed = 15L, prefix = "W"))
  list(
    list(name = "Nestimate human_long + ai_long",
         actors = list(Human = nestimate$human_long, AI = nestimate$ai_long),
         codes = list(Human = unique(nestimate$human_long$code), AI = unique(nestimate$ai_long$code))),
    list(name = "random 3+3 codes, 20 sessions",
         actors = ref_split_actors(ref_random_long(unlist(balanced), 20L, 8L, 14L, seed = 1L), balanced),
         codes = balanced),
    list(name = "imbalanced 1+5 codes, 20 sessions",
         actors = ref_split_actors(ref_random_long(unlist(imbalanced), 20L, 10L, 18L, seed = 12L), imbalanced),
         codes = imbalanced),
    list(name = "actors in separate sessions (block diagonal)",
         actors = list(Human = ref_random_long(separate$Human, 12L, 10L, 14L, seed = 13L, prefix = "H"),
                       AI = ref_random_long(separate$AI, 12L, 10L, 14L, seed = 23L, prefix = "A")),
         codes = separate),
    list(name = "variable session lengths (4 and 80)", actors = ref_split_actors(variable, balanced),
         codes = balanced)
  )
}

#' tna's group_regulation_long as two pseudo-actors (cognitive and social actions), in long format
#'
#' @return A data set as ref_datasets() returns them.
ref_group_regulation <- function() {
  source_env <- new.env()
  utils::data(group_regulation_long, package = "tna", envir = source_env)
  cognitive <- c("plan", "monitor", "discuss", "synthesis", "adapt")
  social <- c("cohesion", "consensus", "emotion", "coregulate")
  events <- as.data.frame(source_env$group_regulation_long)
  events <- events[events$Action %in% c(cognitive, social), , drop = FALSE]
  events$session_id <- paste(events$Group, events$Course, events$Actor, sep = "_")
  events <- events[order(events$session_id, events$Time), , drop = FALSE]
  long <- data.frame(session_id = events$session_id, code = events$Action,
                     order_in_session = stats::ave(seq_len(nrow(events)), events$session_id, FUN = seq_along),
                     stringsAsFactors = FALSE)
  codes <- list(Cognitive = cognitive, Social = social)
  list(name = "tna group_regulation_long (Cognitive / Social)", actors = ref_split_actors(long, codes),
       codes = codes)
}

#' The sessions of long tables as a wide table: one row per session (sessions sorted), columns T1..Tn
#'
#' @param ... Long tables with `session_id`, `code`, `order_in_session` (one per actor, or one).
#' @param width Number of columns; NULL = the longest session. tna::permutation_test() needs two tables of one width.
#' @return A data frame of character columns, padded with NA.
ref_wide <- function(..., width = NULL) {
  long <- do.call(rbind, lapply(list(...), \(part) part[, c("session_id", "code", "order_in_session")]))
  long <- long[order(long$session_id, long$order_in_session), , drop = FALSE]
  sessions <- split(as.character(long$code), long$session_id)
  width <- width %||% max(lengths(sessions))
  rows <- lapply(sessions, \(codes) c(codes, rep(NA_character_, width - length(codes))))
  wide <- as.data.frame(do.call(rbind, unname(rows)), stringsAsFactors = FALSE)
  names(wide) <- paste0("T", seq_len(width))
  wide
}

#' The wide table of a data set's sessions
#'
#' @param dataset A data set from ref_datasets().
#' @return A data frame (ref_wide()).
ref_dataset_wide <- function(dataset) do.call(ref_wide, unname(dataset$actors))

#' The actor of each code
#'
#' @param codes_by_actor A named list of codes per actor.
#' @return A named character vector, code -> actor.
ref_actor_of <- function(codes_by_actor) {
  stats::setNames(rep(names(codes_by_actor), lengths(codes_by_actor)), unlist(codes_by_actor, use.names = FALSE))
}

#' Matrices with the same dimnames as one long table: one row per cell, from = row, to = column
#'
#' @param ... Named matrices; each becomes a column.
#' @param drop_diagonal Leave the self-loops out.
#' @return A data frame with `from`, `to` and one column per matrix.
ref_edge_table <- function(..., drop_diagonal = FALSE) {
  matrices <- list(...)
  first <- matrices[[1L]]
  cells <- expand.grid(from = rownames(first), to = colnames(first), stringsAsFactors = FALSE)
  values <- lapply(matrices, \(m) unname(m[cbind(cells$from, cells$to)]))
  table <- cbind(cells, as.data.frame(values))
  if (drop_diagonal) table <- table[table$from != table$to, , drop = FALSE]
  rownames(table) <- NULL
  table
}

#' A named vector as a table: one row per name
#'
#' @param values A named numeric vector.
#' @param key,value Column names.
#' @return A data frame.
ref_named <- function(values, key = "state", value = "value") {
  out <- data.frame(names(values), unname(values), stringsAsFactors = FALSE)
  names(out) <- c(key, value)
  out
}

#' tna::centralities() as a plain data frame keyed by `node`
#'
#' @param centralities The result of tna::centralities().
#' @param measures The measure columns to keep.
#' @return A data frame `node` and one column per measure.
ref_tna_centralities <- function(centralities, measures) {
  table <- as.data.frame(centralities)
  out <- data.frame(node = as.character(table$state), stringsAsFactors = FALSE)
  out[measures] <- table[measures]
  out
}

#' Range normalisation to 0-1: (x - min) / (max - min)
#'
#' @param values A numeric vector.
#' @return The rescaled vector.
ref_range_normalise <- function(values) (values - min(values)) / (max(values) - min(values))

#' tna's bootstrap summary with the bootstrap means and SDs of its matrices added, one row per edge of its summary
#'
#' @param boot The result of tna::bootstrap().
#' @return A data frame keyed by `from`, `to`.
ref_tna_bootstrap <- function(boot) {
  moments <- ref_edge_table(mean = boot$weights_mean, sd = boot$weights_sd)
  summary_table <- as.data.frame(boot$summary)
  summary_table$from <- as.character(summary_table$from)
  summary_table$to <- as.character(summary_table$to)
  merge(summary_table, moments, by = c("from", "to"))
}

#' tna's "from -> to" edge names split into two key columns
#'
#' @param stats A data frame with a column `edge_name`.
#' @return The data frame with `from` and `to` and without `edge_name`.
ref_split_edge_names <- function(stats) {
  stats <- as.data.frame(stats)
  parts <- strsplit(stats$edge_name, " -> ", fixed = TRUE)
  stats$from <- vapply(parts, `[`, character(1), 1L)
  stats$to <- vapply(parts, `[`, character(1), 2L)
  stats$edge_name <- NULL
  stats
}

#' tna's permutation edge statistics restricted to the edges present in either network
#'
#' @param test The result of tna::permutation_test().
#' @param model_x,model_y The two tna models.
#' @return A data frame keyed by `from`, `to`: weight_x, weight_y, diff (observed), p_value.
ref_tna_edge_stats <- function(test, model_x, model_y) {
  stats <- ref_split_edge_names(test$edges$stats)
  present <- ref_edge_table(weight_x = model_x$weights, weight_y = model_y$weights)
  present <- present[present$weight_x != 0 | present$weight_y != 0, , drop = FALSE]
  merged <- merge(present, stats, by = c("from", "to"))
  data.frame(from = merged$from, to = merged$to, weight_x = merged$weight_x, weight_y = merged$weight_y,
             diff = merged$diff_true, p_value = merged$p_value, stringsAsFactors = FALSE)
}

#' The summary per drop proportion of a case-dropping stability analysis, written from tna's correlations
#'
#' @param estimate The result of tna::estimate_cs().
#' @param threshold The correlation threshold.
#' @return A data frame `measure`, `drop_prop`, `mean_cor`, `sd_cor`, `prop_above`.
ref_stability_summary <- function(estimate, threshold = 0.7) {
  drop_prop <- attr(estimate, "drop_prop")
  do.call(rbind, lapply(names(estimate), \(m) {
    correlations <- estimate[[m]]$correlations
    data.frame(measure = m, drop_prop = drop_prop,
               mean_cor = colMeans(correlations, na.rm = TRUE),
               sd_cor = apply(correlations, 2L, stats::sd, na.rm = TRUE),
               prop_above = colMeans(!is.na(correlations) & correlations >= threshold))
  }))
}

#' The CS coefficients of tna::estimate_cs() as a table
#'
#' @param estimate The result of tna::estimate_cs().
#' @return A data frame `measure`, `cs`.
ref_cs_coefficients <- function(estimate) {
  data.frame(measure = names(estimate), cs = vapply(estimate, \(m) m$cs_coefficient, numeric(1)),
             stringsAsFactors = FALSE)
}

#' tna's split-half reliability rows of the four metrics Nestimate (and so htna) reports, under their names
#'
#' @param reliability The result of tna::reliability().
#' @param model The model label.
#' @return A data frame `model`, `metric`, `mean`, `sd`.
ref_reliability_summary <- function(reliability, model = "relative") {
  names_tna <- c(mean_dev = "Mean Abs. Diff.", median_dev = "Median Abs. Diff.", cor = "Pearson",
                 max_dev = "Max Abs. Diff.")
  table <- as.data.frame(reliability$summary)
  rows <- match(names_tna, table$metric)
  data.frame(model = model, metric = names(names_tna), mean = table$mean[rows], sd = table$sd[rows],
             stringsAsFactors = FALSE)
}

#' The sequences of a wide table: one character vector per row, padding dropped
#'
#' @param wide A wide table.
#' @return A list of character vectors.
ref_sequences <- function(wide) {
  lapply(seq_len(nrow(wide)), \(row) {
    codes <- as.character(unlist(wide[row, ], use.names = FALSE))
    codes[!is.na(codes)]
  })
}

#' Paths of `k` elements, `gap + 1` positions apart, counted from the definition
#'
#' For each sequence every window of positions i, i + (gap + 1), ..., i + (k - 1)(gap + 1) is one occurrence of
#' the path read there. count = occurrences; n_sequences = sequences with at least one; support = n_sequences /
#' sequences; frequency = count / all occurrences of the same k and gap; lift = count / (total windows x product
#' of the marginal shares of its elements among all events).
#'
#' @param sequences A list of character vectors.
#' @param k Path length.
#' @param gap Positions skipped between elements (0 = adjacent).
#' @return A data frame `schema`, `length`, `gap`, `count`, `n_sequences`, `support`, `frequency`, `lift`.
ref_paths <- function(sequences, k, gap = 0L) {
  step <- gap + 1L
  span <- (k - 1L) * step
  windows <- lapply(sequences, \(codes) {
    starts <- seq_len(max(0L, length(codes) - span))
    vapply(starts, \(i) paste(codes[i + (seq_len(k) - 1L) * step], collapse = "->"), character(1))
  })
  occurrences <- unlist(windows)
  count <- table(occurrences)
  n_sequences <- table(unlist(lapply(windows, unique)))
  events <- unlist(sequences)
  share <- table(events) / length(events)
  schema <- names(count)
  expected <- vapply(strsplit(schema, "->", fixed = TRUE), \(parts) prod(share[parts]), numeric(1))
  data.frame(schema = schema, length = k, gap = gap, count = as.vector(count),
             n_sequences = as.vector(n_sequences[schema]), support = as.vector(n_sequences[schema]) / length(sequences),
             frequency = as.vector(count) / length(occurrences),
             lift = as.vector(count) / (expected * length(occurrences)), stringsAsFactors = FALSE)
}
