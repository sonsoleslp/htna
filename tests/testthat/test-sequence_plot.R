test_that("sequence_plot_htna() runs for by = 'state' and 'group'", {
  net <- make_htna()
  with_null_device({
    expect_no_error(sequence_plot_htna(net, by = "state", type = "index"))
    expect_no_error(sequence_plot_htna(net, by = "state", type = "heatmap"))
    expect_no_error(sequence_plot_htna(net, by = "group", type = "distribution"))
  })
})

test_that("sequence_plot_htna() iterates over htna_group", {
  grp <- make_htna_group()
  with_null_device(
    expect_no_error(sequence_plot_htna(grp, by = "group", type = "index"))
  )
})

test_that("plot_sequences() generic dispatches to .htna", {
  net <- make_htna()
  with_null_device(
    expect_no_error(plot_sequences(net))
  )
})

test_that("sequence_plot.htna alias is callable", {
  net <- make_htna()
  with_null_device(
    expect_no_error(sequence_plot.htna(net))
  )
})

test_that("sequence_plot_htna() errors when net$data is missing", {
  net <- make_htna()
  net$data <- NULL
  expect_error(
    sequence_plot_htna(net),
    regexp = "data"
  )
})
