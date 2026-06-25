# Simplified Human Interaction Sequences (per-actor frame)

The Human-only slice of
[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md), in a
long-format data frame ready to feed the named-list form of
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).
Codes have already been collapsed into the simplified Human alphabet;
see [`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) for the
remapping rules.

## Usage

``` r
human_simplified
```

## Format

A data frame with 10796 rows and 10 columns. Schema matches
[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) minus the
`phase` column; every value in `actor_type` is `"Human"`.

## Source

Derived from
[`Nestimate::human_long`](https://saqr.me/Nestimate/reference/long-data.html);
see `data-raw/human_ai.R` for the build script.

## See also

[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md),
[`ai_simplified`](https://sonsoles.me/htna/reference/ai_simplified.md),
[`human_ai_codebook`](https://sonsoles.me/htna/reference/human_ai_codebook.md).

## Examples

``` r
# \donttest{
data(human_simplified, ai_simplified)
net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_htna(net)

# }
```
