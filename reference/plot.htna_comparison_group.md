# Plot All Comparisons in an HTNA Comparison Group

Applies Nestimate's `net_comparison` plot method to every cohort pair.

## Usage

``` r
# S3 method for class 'htna_comparison_group'
plot(x, ...)
```

## Arguments

- x:

  An `htna_comparison_group` from
  [`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md).

- ...:

  Passed to the `net_comparison` plot method, including `type` and
  `combined`.

## Value

Invisibly, a named list containing the plot for every comparison.
