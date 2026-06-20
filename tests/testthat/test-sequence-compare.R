test_that("sequence_compare_htna() default (level='state') matches Nestimate::sequence_compare()", {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  net <- suppressWarnings(build_htna(list(Human = human_long, AI = ai_long)))
  n   <- nrow(net$data)
  grp <- rep(c("early", "late"), length.out = n)

  set.seed(1L)
  res_htna <- suppressWarnings(
    sequence_compare_htna(net, group = grp, iter = 10, min_freq = 5)
  )
  set.seed(1L)
  res_nest <- suppressWarnings(
    Nestimate::sequence_compare(net, group = grp, iter = 10, min_freq = 5)
  )
  expect_identical(res_htna$patterns, res_nest$patterns)
  expect_identical(res_htna$groups,   res_nest$groups)
})

test_that("sequence_compare_htna() level='type' folds states into actor types", {
  data(human_long, ai_long, package = "Nestimate", envir = environment())
  net <- suppressWarnings(build_htna(list(Human = human_long, AI = ai_long)))
  n   <- nrow(net$data)
  grp <- rep(c("early", "late"), length.out = n)

  res <- suppressWarnings(
    sequence_compare_htna(net, group = grp, level = "type",
                          iter = 10, min_freq = 5)
  )
  # Meta-path patterns should contain only actor type names (Human, AI),
  # never concrete state codes.
  patterns <- res$patterns$pattern
  elements <- unique(unlist(strsplit(patterns, "->", fixed = TRUE)))
  expect_setequal(elements, c("Human", "AI"))
})

test_that("sequence_compare_htna() level='type' requires an actor partition", {
  # Plain sequence data without an htna actor partition is rejected.
  m <- matrix(sample(c("A", "B", "C"), 50, replace = TRUE), nrow = 10)
  grp <- rep(c("g1", "g2"), 5)
  expect_error(
    sequence_compare_htna(m, group = grp, level = "type"),
    "actor partition"
  )
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
