# Plot Method for htna Centrality Stability Objects

S3 method for plotting htna centrality stability results. Draws, with
'ggplot2', the mean correlation between the original and the
case-dropped centralities against the proportion of sessions dropped:
one line per measure, a +/- 1 SD ribbon, and a dashed line at the
stability `threshold`. A measure is stable up to the drop proportion at
which its line falls below that threshold. Requires the 'ggplot2'
package (Suggests).

## Usage

``` r
# S3 method for class 'htna_stability'
plot(x, ...)
```

## Arguments

- x:

  An object of class `htna_stability` from
  [`centrality_stability_htna()`](https://sonsoles.me/htna/reference/centrality_stability_htna.md).

- ...:

  Unused; present for consistency with the generic.

## Value

Invisibly, the `ggplot` object; the plot is printed as a side effect.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
cs  <- centrality_stability_htna(net, iter = 20, seed = 1)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot(cs)
}
```
