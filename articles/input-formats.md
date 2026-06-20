# Input formats for build_htna()

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
accepts data in **three interchangeable shapes**: a named list of
per-actor frames, a single combined frame with an actor-type column, or
a single combined frame with a node-to-actor lookup. This vignette walks
through each form using `htna`’s integrated `human_ai` dataset. All
three input forms produce the same network, then covers two more
parameters that compose with any input form: `actor` (forwarded to
[`Nestimate::build_network()`](https://saqr.me/Nestimate/reference/build_network.html))
and `disambiguate` (for code-label collisions).

## Setup

``` r

library(htna)
data(human_ai)
```

Both frames share the canonical schema htna expects: `session_id`,
`code`, `order_in_session`. Within each session, `order_in_session` is a
**shared** time index across both actors — Human’s events occupy the odd
positions, AI’s the even ones — so concatenating and sorting recovers
the temporal interleaving.

## Form 1 — Named list of per-actor frames

The most direct form. Each entry is one actor’s long frame; the list
name becomes the actor-type label on the network.

``` r

human_long <- human_ai[human_ai$actor_type == "Human",]
ai_long <- human_ai[human_ai$actor_type == "AI",]
net_list <- build_htna(list(Human = human_long, AI = ai_long))
summary(net_list)
#> <htna network>
#>   Method:    relative
#>   Sessions:  429   (max 287 timesteps)
#>   Nodes:     12
#>   Edges:     135 / 144 (non-zero)
#> 
#> Actor types (2):
#>   Human (6 nodes):  Check, Frustrate, Inquire, Refine, Request, Specify
#>   AI    (6 nodes):  Ask, Delegate, Execute, Plan, Repair, Report
#> 
#> Edge counts by actor (rows = source, cols = target):
#>           Human    AI
#>   Human      35    36
#>   AI         36    28
```

``` r

plot_htna(net_list)
```

![](input-formats_files/figure-html/unnamed-chunk-3-1.png)

Use this form when each actor’s data lives in a separate frame (the
typical case when the two streams were prepared independently).

## Form 2 — Single combined frame with `actor_type`

When the data already arrives as one long table, tag each row with the
actor-type and pass the column name to `actor_type =`.

``` r

net_actor_type <- build_htna(human_ai, actor_type = "actor_type", session = "session_id", order =  "order_in_session")
```

The actor-type order on the resulting network follows the factor levels
(or, for character vectors, the order of first appearance), which is how
the actor groups will be ordered in legends and plots.

## Form 3 — Single combined frame with `node_groups`

When the combined frame has **no** actor-type column, declare the
node-to-actor partition directly via `node_groups`. `node_groups`
accepts two interchangeable shapes:

### 3a. Named list of code vectors

``` r

human_codes <- unique(human_long$code)
ai_codes    <- unique(ai_long$code)

net_node_groups <- build_htna(
  human_ai,
  node_groups = list(Human = human_codes, AI = ai_codes)
)
net_node_groups
#> Transition Network (relative probabilities) [directed]
#>   Weights: [0.002, 0.611]  |  mean: 0.089
#> 
#>   Weight matrix:
#>               Ask Check Delegate Execute Frustrate Inquire  Plan Refine Repair
#>   Ask       0.018 0.063    0.000   0.022     0.116   0.063 0.409  0.051  0.004
#>   Check     0.117 0.050    0.015   0.411     0.058   0.005 0.008  0.042  0.033
#>   Delegate  0.000 0.039    0.000   0.011     0.110   0.032 0.611  0.014  0.000
#>   Execute   0.061 0.087    0.000   0.074     0.143   0.088 0.107  0.069  0.002
#>   Frustrate 0.194 0.104    0.039   0.119     0.114   0.060 0.004  0.098  0.029
#>   Inquire   0.251 0.069    0.027   0.285     0.039   0.033 0.009  0.016  0.042
#>   Plan      0.000 0.146    0.000   0.015     0.215   0.083 0.003  0.086  0.005
#>   Refine    0.143 0.045    0.006   0.206     0.008   0.014 0.013  0.000  0.025
#>   Repair    0.241 0.012    0.028   0.391     0.043   0.047 0.016  0.024  0.004
#>   Report    0.102 0.114    0.000   0.009     0.124   0.102 0.050  0.058  0.039
#>   Request   0.148 0.055    0.018   0.292     0.016   0.010 0.008  0.009  0.007
#>   Specify   0.269 0.017    0.040   0.273     0.096   0.011 0.015  0.002  0.013
#>             Report Request Specify
#>   Ask        0.028   0.173   0.052
#>   Check      0.055   0.049   0.156
#>   Delegate   0.018   0.131   0.035
#>   Execute    0.010   0.293   0.065
#>   Frustrate  0.021   0.135   0.085
#>   Inquire    0.170   0.028   0.029
#>   Plan       0.026   0.325   0.095
#>   Refine     0.023   0.105   0.413
#>   Repair     0.103   0.071   0.020
#>   Report     0.077   0.257   0.066
#>   Request    0.034   0.067   0.336
#>   Specify    0.037   0.126   0.101 
#> 
#>   Initial probabilities:
#>   Specify       0.818  ████████████████████████████████████████
#>   Request       0.156  ████████
#>   Frustrate     0.023  █
#>   Refine        0.002  
#>   Ask           0.000  
#>   Check         0.000  
#>   Delegate      0.000  
#>   Execute       0.000  
#>   Inquire       0.000  
#>   Plan          0.000  
#>   Repair        0.000  
#>   Report        0.000
```

### 3b. Two-column data frame (codebook)

The same partition expressed as a tidy codebook — one row per code, one
column for the action and one for the actor type. The action column must
match the `action` parameter (default `"code"`); the other column is
treated as the actor-type tag. Column order is irrelevant.

``` r

codebook <- data.frame(
  code       = c(human_codes, ai_codes),
  actor_type = c(rep("Human", length(human_codes)),
                 rep("AI",    length(ai_codes))),
  stringsAsFactors = FALSE
)
head(codebook)
#>        code actor_type
#> 1   Request      Human
#> 2   Specify      Human
#> 3     Check      Human
#> 4 Frustrate      Human
#> 5    Refine      Human
#> 6   Inquire      Human

net_node_groups_df <- build_htna(
  human_ai,
  node_groups = codebook
)
```

The actor-type ordering follows the column’s factor levels (if a factor)
or the order of first appearance (if a character vector). Use this form
when your code/actor mapping already lives in a tidy lookup table.

## All forms produce the same network

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
resolves every input shape to the same combined sequence and forwards to
[`Nestimate::build_network()`](https://saqr.me/Nestimate/reference/build_network.html).
The four networks below are drawn side by side — the layout, colours,
and edges are indistinguishable:

``` r

forms <- list(
  list_form        = net_list,
  actor_type_form  = net_actor_type,
  node_groups_list = net_node_groups,
  node_groups_df   = net_node_groups_df
)

op <- par(mfrow = c(2, 2), mar = c(1, 1, 3, 1))
for (nm in names(forms)) plot_htna(forms[[nm]], title = nm)
```

![](input-formats_files/figure-html/unnamed-chunk-7-1.png)

``` r

par(op)
```

A one-line sanity check confirms that the edge-weight matrices match the
named-list reference cell-for-cell:

``` r

ref <- forms$list_form
vapply(forms[-1L], function(n)
       isTRUE(all.equal(unname(n$weights),
                        unname(ref$weights[rownames(n$weights),
                                           colnames(n$weights)]))),
       logical(1L))
#>  actor_type_form node_groups_list   node_groups_df 
#>             TRUE             TRUE             TRUE
```

## Handling code-label collisions with `disambiguate =`

If both actor types share a code label (e.g. both Human and AI have a
code called `Ask`),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
errors by default — keeping such states distinct is usually what you
want. Pass `disambiguate = TRUE` to prefix every code with its
actor-type label so the two `Ask` nodes become `Human:Ask` and `AI:Ask`.

``` r

overlap <- data.frame(
  session_id        = rep(paste0("s", 1:3), each = 6),
  code              = rep(c("Ask", "Reply", "Ask", "Reply", "Ask", "Reply"), 3),
  actor_type        = rep(c("Human", "Human", "AI", "AI", "Human", "AI"), 3),
  order_in_session  = rep(1:6, 3),
  stringsAsFactors  = FALSE
)

# Without disambiguate=, this errors:
try(build_htna(overlap, actor_type = "actor_type"))
#> Error : Code(s) appear in more than one actor group: Ask, Reply. Pass `disambiguate = TRUE` to prefix codes with the actor label.

# With disambiguate=, the two `Ask`s become distinct nodes:
net_dis <- build_htna(overlap, actor_type = "actor_type",
                      disambiguate = TRUE)
net_dis$nodes$label
#> [1] "AI:Ask"      "AI:Reply"    "Human:Ask"   "Human:Reply"
```

## Splitting into cohorts with `group =`

Any of the three input forms can additionally be split into per-cohort
networks by passing `group =` — the result is an `htna_group`, one
`htna` network per level of the grouping column. Below we synthesise a
“phase” cohort variable on the combined frame (chronological split:
first-half of sessions = early, rest = late) and let
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) build
one network per phase.

``` r

sess_start <- aggregate(session_date ~ session_id, data = human_ai,
                        FUN = min)
sess_start <- sess_start[order(sess_start$session_date,
                               sess_start$session_id), ]
half       <- nrow(sess_start) %/% 2L
early_sess <- sess_start$session_id[seq_len(half)]
human_ai$phase <- ifelse(human_ai$session_id %in% early_sess,
                         "early", "late")

grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
class(grp)
#> [1] "htna_group"      "netobject_group" "list"
names(grp)
#> [1] "late"  "early"
```

``` r

plot_htna(grp)
```

![](input-formats_files/figure-html/unnamed-chunk-11-1.png)![](input-formats_files/figure-html/unnamed-chunk-11-2.png)

## Cheat sheet

| Form | When to use |
|----|----|
| `list(Actor1 = a1, Actor2 = a2)` | Each actor’s data is in its own frame |
| `df, actor_type = "actor_type"` | One combined frame, actor identity in a row-level column |
| `df, node_groups = list(Actor1 = ..., Actor2 = ...)` | One combined frame; partition declared as a named list of codes |
| `df, node_groups = data.frame(code = ..., actor_type = ...)` | One combined frame; partition declared as a tidy codebook |
| `+ actor = "user_id"` | Identify the individual actor that performed each event (forwarded to [`Nestimate::build_network()`](https://saqr.me/Nestimate/reference/build_network.html)) |
| `+ disambiguate = TRUE` | Same code label appears in more than one actor type |
| `+ group = "phase"` | Build one network per cohort/phase |

All three input shapes are interchangeable; pick whichever matches the
shape your data already has.
