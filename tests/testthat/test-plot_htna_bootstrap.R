test_that("plot_htna_bootstrap() runs", {
  net  <- make_htna()
  boot <- bootstrap_htna(net, iter = 10)
  with_null_device(
    expect_no_error(plot_htna_bootstrap(boot))
  )
})

test_that("plot_htna_bootstrap() iterates over htna_bootstrap_group", {
  grp   <- make_htna_group()
  boots <- bootstrap_htna(grp, iter = 10)
  with_null_device(
    expect_no_error(plot_htna_bootstrap(boots))
  )
})

test_that("plot.htna_bootstrap dispatches to plot_htna_bootstrap()", {
  net  <- make_htna()
  boot <- bootstrap_htna(net, iter = 10)
  with_null_device(
    expect_no_error(plot(boot))
  )
})

test_that("plot_htna_bootstrap() rejects non-bootstrap input", {
  expect_error(
    plot_htna_bootstrap(make_htna()),
    regexp = "bootstrap_htna"
  )
})
