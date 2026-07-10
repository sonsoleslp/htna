test_that("markov_order_test_htna() is a stable ...-forwarding wrapper", {
  # Durable-wrapper design: documented formals stay `data, ...` (see NEWS 0.1.2).
  expect_identical(names(formals(htna::markov_order_test_htna)), c("data", "..."))

  data(human_long, package = "Nestimate", envir = environment())
  seqs <- split(human_long$code, human_long$session_id)
  seqs <- seqs[lengths(seqs) >= 3L]

  via_htna <- markov_order_test_htna(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  direct   <- Nestimate::markov_order_test(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  expect_equal(via_htna$optimal_order, direct$optimal_order)
})

test_that("markov_order_test_htna() runs on Human-only sequences", {
  data(human_long, package = "Nestimate", envir = environment())
  seqs <- split(human_long$code, human_long$session_id)
  seqs <- seqs[lengths(seqs) >= 3L]

  mo <- markov_order_test_htna(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  expect_s3_class(mo$test_table, "data.frame")
  expect_true(is.numeric(mo$optimal_order))
})

test_that("markov_order_test_htna() runs on AI-only sequences", {
  data(ai_long, package = "Nestimate", envir = environment())
  seqs <- split(ai_long$code, ai_long$session_id)
  seqs <- seqs[lengths(seqs) >= 3L]

  mo <- markov_order_test_htna(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  expect_s3_class(mo$test_table, "data.frame")
  expect_true(is.numeric(mo$optimal_order))
})

test_that("markov_order_test_htna() runs on combined Human+AI sequences", {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  combined <- rbind(human_long, ai_long)
  combined <- combined[order(combined$session_id, combined$order_in_session), ]
  seqs <- split(combined$code, combined$session_id)
  seqs <- seqs[lengths(seqs) >= 3L]

  mo <- markov_order_test_htna(seqs, max_order = 2L, n_perm = 20L, seed = 1L)
  expect_s3_class(mo$test_table, "data.frame")
  expect_true(is.numeric(mo$optimal_order))
})
