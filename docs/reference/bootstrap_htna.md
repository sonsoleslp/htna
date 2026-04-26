# Bootstrap an HTNA Network

Thin wrapper around
[`Nestimate::bootstrap_network()`](https://rdrr.io/pkg/Nestimate/man/bootstrap_network.html)
that runs the standard state-level edge bootstrap on an htna network and
tags the result so it is identifiable as an htna bootstrap. The
bootstrap inference itself is Nestimate's: per-edge mean, sd, p-value,
significance, and confidence / credibility intervals across `iter`
resamples. State-level granularity is the correct level for
edge-stability analysis - aggregating up to actor pairs would smear over
real edge-by-edge differences.

## Usage

``` r
bootstrap_htna(x, ...)
```

## Arguments

- x:

  \[`htna`\]  
  A network built with
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- ...:

  Arguments forwarded to
  [`Nestimate::bootstrap_network()`](https://rdrr.io/pkg/Nestimate/man/bootstrap_network.html)
  (e.g. `iter`, `ci_level`, `consistency_range`, `seed`).

## Value

An object of class `c("htna_bootstrap", "net_bootstrap")`. All slots
produced by
[`Nestimate::bootstrap_network()`](https://rdrr.io/pkg/Nestimate/man/bootstrap_network.html)
are preserved verbatim (`$summary`, `$mean`, `$sd`, `$p_values`,
`$significant`, `$ci_lower`/`$ci_upper`, `$cr_lower`/`$cr_upper`,
`$model`, `$original`, etc.).

## Details

What this wrapper adds is identity and downstream class continuity:

- The returned object has class `"htna_bootstrap"` ahead of
  `"net_bootstrap"`, so `inherits(boot, "htna_bootstrap")` works.

- The actor partition (`$nodes$groups`, `$node_groups`) is restored on
  `boot$model` from the input network so cograph's auto-detect still
  recognizes the heterogeneous schema downstream.

- `boot$model` is re-tagged with `"htna"` at the front of its class
  chain so the chain is `c("htna", "netobject", "cograph_network")` -
  identical to what
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
  returns.

## See also

[`Nestimate::bootstrap_network()`](https://rdrr.io/pkg/Nestimate/man/bootstrap_network.html)
for the underlying algorithm and slot documentation.
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) for
constructing the input.

## Examples

``` r
if (FALSE) { # \dontrun{
data(human_long, ai_long, package = "Nestimate")
net  <- build_htna(list(Human = human_long, AI = ai_long))
boot <- bootstrap_htna(net, iter = 200)
inherits(boot, "htna_bootstrap")            # TRUE
inherits(boot$model, "htna")                # TRUE
head(boot$summary)                          # state-level edge stability
} # }
```
