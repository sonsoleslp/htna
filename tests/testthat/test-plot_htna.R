test_that("plot_htna() runs on a single htna network", {
  net <- make_htna()
  with_null_device(
    expect_no_error(plot_htna(net))
  )
})

test_that("plot_htna() iterates over htna_group", {
  grp <- make_htna_group()
  with_null_device(
    expect_no_error(plot_htna(grp))
  )
})

test_that("plot_htna() honours user `minimum` override", {
  net <- make_htna()
  with_null_device(
    expect_no_error(plot_htna(net, minimum = 0))
  )
  with_null_device(
    expect_no_error(plot_htna(net, minimum = 0.2))
  )
})

test_that("plot.htna and plot.htna_group dispatch to plot_htna()", {
  net <- make_htna()
  grp <- make_htna_group()
  with_null_device({
    expect_no_error(plot(net))
    expect_no_error(plot(grp))
  })
})
