# Plot sequences

`plot_sequences()` plots the sequences behind a model; it is a generic.
For an htna network it draws the sessions with
[`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md),
so `plot_sequences(net)` gives the same plot as
`sequence_plot_htna(net)`. The 'tna' package exports a generic of the
same name; whichever package is attached last provides
`plot_sequences()`.

## Usage

``` r
plot_sequences(x, ...)
```

## Arguments

- x:

  An object to plot; for an htna network, see
  [`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md).

- ...:

  Arguments for the method.

## Value

Called for the plot; the value is defined by the method (for an htna
network, see
[`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)).

## See also

[`sequence_plot_htna()`](https://sonsoles.me/htna/reference/sequence_plot_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(subset(human_ai, project == "Project_9"),
                  actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (2 sessions)
plot_sequences(net)
```
