test_that("plot_htna_diff() runs on two htna networks", {
  pair <- make_htna_pair()
  with_null_device(
    expect_no_error(plot_htna_diff(pair$ctrl, pair$exp))
  )
})

test_that("plot_htna_diff() runs on a net_permutation result", {
  pair <- make_htna_pair()
  perm <- permutation(pair$ctrl, pair$exp, iter = 20)
  with_null_device(
    expect_no_error(plot_htna_diff(perm))
  )
  with_null_device(
    expect_no_error(plot_htna_diff(perm, show_nonsig = TRUE))
  )
})

test_that("plot_htna_diff() rejects mismatched alphabets", {
  net1 <- make_htna()                           # Human / AI from Nestimate
  net2 <- make_htna_pair()$ctrl                 # synthetic alphabet
  expect_error(
    suppressWarnings(plot_htna_diff(net1, net2)),
    regexp = "actor partition|conformable|same"
  )
})

test_that("plot_htna_diff() rejects non-htna inputs", {
  expect_error(
    plot_htna_diff(list(), list()),
    regexp = "htna"
  )
})
