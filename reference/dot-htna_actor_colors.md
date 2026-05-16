# Pick actor colours, recycling the palette for \>6 groups

Returns one colour per actor group. When the number of groups exceeds
the palette length, recycles with a one-time warning so plots remain
readable instead of silently producing `NA` fills.

## Usage

``` r
.htna_actor_colors(n_or_levels, palette = htna_palette)
```

## Arguments

- n_or_levels:

  Either an integer count of groups or a character vector of group
  labels (used for [`names()`](https://rdrr.io/r/base/names.html) on the
  result).

- palette:

  Colour palette to draw from. Defaults to
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

## Value

Character vector of colours, length `n` (named when levels given).
