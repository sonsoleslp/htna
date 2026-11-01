# Plot the Difference Between Two htna Networks

Renders the elementwise edge-weight difference `x - y` as a
heterogeneous transition network. Accepts:

## Usage

``` r
plot_htna_diff(
  x,
  y = NULL,
  pos_color = "#009900",
  neg_color = "#C62828",
  group_colors = htna_palette,
  ...
)
```

## Arguments

- x:

  One of: an htna network, a `net_permutation` result, or a
  `net_permutation_group`.

- y:

  htna network (only used when `x` is also an htna network).

- pos_color:

  Edge colour for positive differences. Default `"#009900"`.

- neg_color:

  Edge colour for negative differences. Default `"#C62828"`.

- group_colors:

  Character vector of colours, one per actor group. Defaults to the
  built-in
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- ...:

  Forwarded to the underlying splot dispatcher
  ([`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  or
  [`cograph::splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.html)).

## Value

The value of the underlying splot call (invisibly).

## Details

- Two htna networks (`x`, `y`): plots `x - y` via
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
  with positive/negative edge coloring.

- A `net_permutation` result: routes through
  [`cograph::splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.html)
  so the significance overlay (significant pos/neg edges, dashed
  non-significant edges, stars) is drawn upstream.

- A `net_permutation_group` (the result of
  [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  on an `htna_group`): iterates over all pairwise comparisons and plots
  each, titled with the pair name (no `par(mfrow=...)` management - user
  controls layout).

- A
  [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  result for two networks, which also inherits `net_permutation`.

In all cases the layout, node colours and donut match
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md); the
drawing itself is done by cograph.

## See also

[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md),
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md),
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md).

## Examples

``` r
data(human_ai)
# Two networks: early vs late sessions
early <- build_htna(subset(human_ai, phase == "Early"),
                    actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
late  <- build_htna(subset(human_ai, phase == "Late"),
                    actor_type = "actor_type")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
plot_htna_diff(early, late)


# Permutation test on a grouped network: one plot per cohort pair
grp  <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
perm <- permutation_htna(grp, iter = 50, seed = 1)
plot_htna_diff(perm)

plot_htna_diff(perm, show_nonsig = TRUE)

```
