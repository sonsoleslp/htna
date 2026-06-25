# Code → Actor-Type Codebook for `human_ai`

A tidy two-column lookup tagging every simplified code in
[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md) with its
actor type. Ready to pass as `node_groups` to
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) (Form
3b in the
[`vignette("input-formats", package = "htna")`](https://sonsoles.me/htna/articles/input-formats.md)).

## Usage

``` r
human_ai_codebook
```

## Format

A data frame with 12 rows and 2 columns:

- code:

  Character. Simplified action code (one of the 12 Human + AI codes in
  [`human_ai`](https://sonsoles.me/htna/reference/human_ai.md)).

- actor_type:

  Character. `"Human"` or `"AI"`.

## Source

Derived from
[`human_simplified`](https://sonsoles.me/htna/reference/human_simplified.md)
and
[`ai_simplified`](https://sonsoles.me/htna/reference/ai_simplified.md);
see `data-raw/human_ai.R` for the build script.

## See also

[`human_ai`](https://sonsoles.me/htna/reference/human_ai.md),
[`human_simplified`](https://sonsoles.me/htna/reference/human_simplified.md),
[`ai_simplified`](https://sonsoles.me/htna/reference/ai_simplified.md).

## Examples

``` r
# \donttest{
data(human_ai, human_ai_codebook)
net <- build_htna(human_ai, node_groups = human_ai_codebook)
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_htna(net)

# }
```
