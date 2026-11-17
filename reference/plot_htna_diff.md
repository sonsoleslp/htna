# Plot the difference between two htna networks

`plot_htna_diff()` plots the difference between two htna networks: for
every transition, how much more (or less) likely it is in one network
than in another, for example in the late sessions compared with the
early ones. Drawn in the layout of the two heterogeneous networks, the
difference network shows whether the change lies within an actor type or
in the hand-overs between actors. When the difference comes from a
permutation test or a Bayesian comparison, only the differences that the
test supports are emphasized.

`plot_htna_diff()` accepts two htna networks and draws `x - y`, with
positive differences in `pos_color` and negative differences in
`neg_color`; networks whose node sets differ are aligned on the union of
their nodes. It also accepts the result of
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
or
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
for two networks, which adds the significance overlay (significant edges
solid, the others dashed or hidden with `show_nonsig`), and the grouped
results of either test, drawn as one plot per cohort pair titled with
the pair name. It is called for the plot and returns, invisibly, the
value of the drawing call, or `x` for a grouped test result.

For a table of the differences by actor pair use
[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md).

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

  An htna network, or a result of
  [`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
  or
  [`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md)
  (for two networks or for a grouped network).

- y:

  A second htna network, when `x` is an htna network. Default `NULL`.

- pos_color:

  Colour of positive differences. Default `"#009900"`.

- neg_color:

  Colour of negative differences. Default `"#C62828"`.

- group_colors:

  Colours of the actor types, in actor order. Default
  [htna_palette](https://sonsoles.me/htna/reference/htna_palette.md).

- ...:

  Further drawing arguments of
  [`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html),
  such as `title`, `minimum` or, for a test result, `show_nonsig`.

## Value

Called for the plot. Invisibly, the value of the drawing call, or `x`
for a grouped test result.

## Conditions

`htna_bad_input` when `x` and `y` are not two htna networks with the
same actor types, or a test result was not computed on htna networks;
`htna_empty_input` for an empty grouped result.

## See also

[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md),
[`bayes_compare_htna()`](https://sonsoles.me/htna/reference/bayes_compare_htna.md),
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)

## Examples

``` r
data(human_ai)
early <- build_htna(subset(human_ai, phase == "Early"),
                    actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
late  <- build_htna(subset(human_ai, phase == "Late"),
                    actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
plot_htna_diff(late, early)


# a permutation test on the grouped network: one plot per pair of phases
grp  <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
perm <- permutation_htna(grp, iter = 50, seed = 1)
plot_htna_diff(perm)

```
