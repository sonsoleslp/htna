# Bootstrap an HTNA Network

Thin wrapper around
[`Nestimate::bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html)
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

# S3 method for class 'htna'
bootstrap(x, ...)
```

## Arguments

- x:

  \[`htna`\]  
  A network built with
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md).

- ...:

  Arguments forwarded to
  [`Nestimate::bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html)
  (e.g. `iter`, `ci_level`, `consistency_range`, `seed`).

## Value

An object of class `c("htna_bootstrap", "net_bootstrap")`. All slots
produced by
[`Nestimate::bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html)
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

[`Nestimate::bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html)
for the underlying algorithm and slot documentation.
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) for
constructing the input.

## Examples

``` r
# \donttest{
data(human_ai)
net  <- build_htna(human_ai, actor_type = "actor_type")
#> Warning: A network with one long sequence is not recommended and can't be validated using bootstrap and other confirmatory testings.
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
boot <- bootstrap_htna(net, iter = 50)
inherits(boot, "htna_bootstrap")            # TRUE
#> [1] TRUE
inherits(boot$model, "htna")                # TRUE
#> [1] TRUE
head(boot$summary)                          # state-level edge stability
#>   from        to     weight       mean          sd    p_value   sig   ci_lower
#> 1  Ask       Ask 0.01823579 0.01816094 0.003196253 0.17647059 FALSE 0.01264274
#> 2  Ask     Check 0.06318914 0.06295842 0.005074601 0.01960784  TRUE 0.05493032
#> 3  Ask   Execute 0.02205259 0.02151466 0.003435516 0.15686275 FALSE 0.01512313
#> 4  Ask Frustrate 0.11577608 0.11618293 0.007063159 0.01960784  TRUE 0.10475565
#> 5  Ask   Inquire 0.06318914 0.06305100 0.005471929 0.01960784  TRUE 0.05356894
#> 6  Ask      Plan 0.40882103 0.41042171 0.014962235 0.01960784  TRUE 0.38207243
#>     ci_upper   cr_lower   cr_upper
#> 1 0.02458365 0.01367684 0.02279474
#> 2 0.07155427 0.04739186 0.07898643
#> 3 0.02742455 0.01653944 0.02756573
#> 4 0.12988671 0.08683206 0.14472010
#> 5 0.07531484 0.04739186 0.07898643
#> 6 0.44045715 0.30661578 0.51102629
# }
```
