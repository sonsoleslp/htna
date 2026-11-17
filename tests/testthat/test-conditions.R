# Every user-facing error is classed `htna_error` plus a specific class.

test_that(".htna_stop() raises a classed condition with no call", {
  err <- tryCatch(.htna_stop("a", "b", class = "htna_test"),
                  error = function(e) e)
  expect_s3_class(err, c("htna_test", "htna_error", "error", "condition"))
  expect_identical(conditionMessage(err), "ab")
  expect_null(conditionCall(err))
})

test_that("input errors carry their documented classes", {
  d <- make_grouped_data()
  ng <- list(Human = c("H1", "H2", "H3"), AI = c("A1", "A2", "A3"))
  expect_error(build_htna(d, actor_type = "condition", node_groups = ng),
               class = "htna_bad_input")
  d_missing <- d
  d_missing$order_in_session <- NULL
  expect_error(build_htna(d_missing, node_groups = ng),
               class = "htna_missing_column")
  expect_error(build_htna(d, node_groups = list(Human = c("H1", "H2"),
                                                AI = c("A1", "A2", "A3"))),
               class = "htna_unassigned_node")
  expect_error(build_htna(d, node_groups = list(Human = c("H1", "H2", "H3"),
                                                AI = c("H1", "A1", "A2",
                                                       "A3"))),
               class = "htna_actor_conflict")
  expect_error(compare_htna(make_htna()), class = "htna_bad_input")
  one <- make_htna_group()
  one <- structure(unclass(one)[1], class = class(one))
  expect_error(compare_htna(one), class = "htna_empty_input")
})

test_that("every stop() in R/ goes through the classed helper", {
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources not available")
  code <- unlist(lapply(list.files(r_dir, full.names = TRUE), readLines))
  code <- code[!grepl("^\\s*#", code)]
  bare <- grep("(^|[^._a-zA-Z])stop\\(", code, value = TRUE)
  expect_identical(bare, "  stop(errorCondition(paste0(...), class = c(class, \"htna_error\"),")
})
