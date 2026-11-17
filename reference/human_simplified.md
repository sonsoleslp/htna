# Human turns of the vibe-coding sessions

`human_simplified` is the human part of
[human_ai](https://sonsoles.me/htna/reference/human_ai.md): its human
rows, as one data frame for one actor type. Together with
[ai_simplified](https://sonsoles.me/htna/reference/ai_simplified.md) it
is the input for the named-list form of
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md), in
which each actor type comes in its own data frame.

## Usage

``` r
human_simplified
```

## Format

A data frame with 10,796 rows and 10 columns: the columns of
[human_ai](https://sonsoles.me/htna/reference/human_ai.md) without
`phase`; `actor_type` is always `"Human"`.

## Source

Derived from the `human_long` data of the 'Nestimate' package (see
[human_ai](https://sonsoles.me/htna/reference/human_ai.md)).

## See also

[human_ai](https://sonsoles.me/htna/reference/human_ai.md),
[ai_simplified](https://sonsoles.me/htna/reference/ai_simplified.md),
[human_ai_codebook](https://sonsoles.me/htna/reference/human_ai_codebook.md)

## Examples

``` r
data(human_simplified, ai_simplified)
net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_htna(net)
```
