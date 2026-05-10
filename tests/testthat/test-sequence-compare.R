test_that("sequence_compare_htna() is the same function as Nestimate::sequence_compare()", {
  expect_identical(htna::sequence_compare_htna, Nestimate::sequence_compare)
})

test_that("sequence_compare_htna() runs on a grouped htna network", {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  human_long$actor_type <- "Human"
  ai_long$actor_type    <- "AI"
  combined <- rbind(human_long, ai_long)
  combined <- combined[order(combined$session_id, combined$order_in_session), ]
  n_per <- ave(combined$order_in_session, combined$session_id, FUN = length)
  pos   <- ave(combined$order_in_session, combined$session_id, FUN = seq_along)
  combined$half <- ifelse(pos <= n_per %/% 2, "early", "late")

  net_g <- build_htna(combined,
                      actor_type = "actor_type",
                      action     = "code",
                      group      = "half")

  res <- sequence_compare_htna(
    net_g,
    sub      = 3:4,
    min_freq = 25L,
    test     = "chisq",
    adjust   = "fdr"
  )

  expect_s3_class(res, "net_sequence_comparison")
  expect_true(all(c("patterns", "groups", "n_patterns", "params") %in% names(res)))
  expect_s3_class(res$patterns, "data.frame")
  expect_setequal(res$groups, c("early", "late"))
})
