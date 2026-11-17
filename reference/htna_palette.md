# Colours of the actor types in htna plots

`htna_palette` gives the default fill colours of the actor types in the
network plots of htna: the first actor type takes the first colour, the
second the second, and so on. With more than six actor types the colours
are recycled with a warning, so pass a longer vector to `group_colors`
in that case. Each plot also gives the actor types different node
shapes, so the actor types are not distinguished by colour alone.

## Usage

``` r
htna_palette
```

## Format

A character vector of six hexadecimal colours.

## Value

A character vector of six hexadecimal colours.

## See also

[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
[`plot_htna_diff()`](https://sonsoles.me/htna/reference/plot_htna_diff.md),
[`plot_htna_bootstrap()`](https://sonsoles.me/htna/reference/plot_htna_bootstrap.md)

## Examples

``` r
htna_palette
#> [1] "#FFD966" "#B4A7D6" "#93C47D" "#E06666" "#6FA8DC" "#F6B26B"
```
