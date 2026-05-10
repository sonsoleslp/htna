test_that("markov_order_test_htna() is the same function as Nestimate::markov_order_test()", {
  expect_identical(htna::markov_order_test_htna, Nestimate::markov_order_test)
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
