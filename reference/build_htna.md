# Build a heterogeneous transition network

`build_htna()` builds a heterogeneous transition network: one transition
network over interaction sequences in which the events are produced by
two or more types of actor, such as a learner and an AI assistant. In
heterogeneous transition network analysis (HTNA) each node is a code (an
action or event) and belongs to exactly one actor type, and each
directed edge is the transition from one code to the next within a
session (Lopez-Pernas, Misiejuk and Saqr, 2026). Because the actor
partition is kept on the network, the within-actor transitions (Human to
Human) and the hand-overs between actors (Human to AI, AI to Human) can
be estimated, plotted and tested as distinct parts of one model.

`build_htna()` orders the events of every session in time, estimates the
transition network over the combined sequence with the chosen `method`
(relative transition probabilities by default, or frequencies,
co-occurrences or attention weights), and attaches the actor type of
every node. The actor partition can be given in three ways: a named list
of one long data frame per actor type, a single data frame with a column
naming the actor type of each row (`actor_type`), or a single data frame
together with a code-to-actor lookup (`node_groups`). With `group`, one
network is estimated per level of a session-level variable (for example
an early and a late phase), in the order of the factor levels. A fitted
sequence clustering (from
[`Nestimate::build_clusters()`](https://pak.dynasite.org/Nestimate/reference/build_clusters.html),
[`Nestimate::cluster_mmm()`](https://pak.dynasite.org/Nestimate/reference/cluster_mmm.html)
or
[`tna::cluster_sequences()`](https://sonsoles.me/tna/reference/cluster_data.html))
is turned into one network per cluster without refitting, and a fitted
[`Nestimate::build_mcml()`](https://pak.dynasite.org/Nestimate/reference/build_mcml.html)
model into one network whose actor types are its node clusters.

It returns an `htna` network, or an `htna_group` with one network per
cohort or cluster. The grouped result is passed as it is to the
comparison, assessment and plotting functions, which work through every
cohort. Use
[`summary.htna()`](https://sonsoles.me/htna/reference/summary.htna.md)
to describe the result and
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
for its edges and nodes.

## Usage

``` r
build_htna(
  data,
  actor_type = NULL,
  actor = NULL,
  node_groups = NULL,
  action = "code",
  session = "session_id",
  order = "order_in_session",
  method = "relative",
  group = NULL,
  disambiguate = FALSE,
  source_data = NULL,
  time = NULL,
  ...
)
```

## Arguments

- data:

  The event data, in one of these forms:

  - a named list of long data frames, one per actor type, with the same
    columns, e.g. `list(Human = human_simplified, AI = ai_simplified)`;

  - one long data frame, with `actor_type` or `node_groups`;

  - a fitted clustering (`net_clustering`, `net_mmm`, `tna_clustering`)
    or a `netobject_group`, giving one network per cluster;

  - a fitted `mcml` model, whose node clusters become the actor types.

- actor_type:

  Name of the column giving the actor type of each row, for a single
  data frame. A factor column sets the order of the actor types;
  otherwise they are ordered by first appearance.

- actor:

  Name of an optional column identifying the individual who produced
  each event (for example a learner id). Independent of `actor_type`.
  Default `NULL`.

- node_groups:

  Code-to-actor lookup for a single data frame without an actor-type
  column: a named list of code vectors, e.g.
  `list(Human = c("Specify", "Check"), AI = c("Plan", "Execute"))`, or a
  two-column data frame with the code column (named as `action`) and the
  actor-type column, such as
  [human_ai_codebook](https://sonsoles.me/htna/reference/human_ai_codebook.md).
  Every code must belong to exactly one actor type. For a clustering or
  `mcml` input it optionally supplies or overrides the partition. Use
  either `actor_type` or `node_groups`, not both.

- action:

  Name of the code column. Default `"code"`.

- session:

  Name of the session column. Default `"session_id"`.

- order:

  Name of the within-session order column. Default `"order_in_session"`.

- method:

  Network estimator: `"relative"` (default, transition probabilities),
  `"frequency"`, `"co_occurrence"` (undirected), `"attention"`,
  `"reverse"`, `"ngram"`, `"gap"` or `"latency"`. `"latency"` weights
  each reply by its speed and needs `time` (Nestimate 1.0.20 or later),
  with times that differ within a session; the bundled `human_ai` has
  one time per session and cannot be used.

- group:

  Name of a session-level column defining cohorts; one network is built
  per cohort. Default `NULL` (one network).

- disambiguate:

  If `TRUE`, a code used by more than one actor type is split into one
  node per actor type, prefixed with the actor label (`"Human:Ask"`,
  `"AI:Ask"`). Default `FALSE` (a shared code is an error).

- source_data:

  The original event data for an `mcml` input whose fitted object did
  not keep it. Default `NULL`.

- time:

  Name of an optional column holding the time of each event. With it the
  events of a session are ordered by time, the `order` column is not
  required, and a session is split where two events are further apart
  than `time_threshold` seconds (900 by default; pass
  `time_threshold = Inf` to keep sessions whole). Required for
  `method = "latency"`. Default `NULL`.

- ...:

  Further estimation arguments, such as `threshold`, `scaling`,
  `time_threshold`, or `latency_floor`, `latency_cap`,
  `latency_aggregate` and `latency_scope` for `method = "latency"`. With
  `start` or `end` the network gains a start or an end state; these
  belong to no actor and form the actor type `"Boundary"`, placed last.
  The plots draw them above and below the actor types.

## Value

An object of class `c("htna", "netobject", "cograph_network")`: a
transition network with its weight matrix, nodes, sequence data and the
actor partition (the actor type of every node and the ordered actor
types). With `group` or a clustering input, an `htna_group`: a named
list with one `htna` network per cohort or cluster, which keeps the
clustering assignments and diagnostics as attributes. An `mcml` input
returns one `htna`.
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
returns the edges (one row per non-zero transition, with the actor types
of its source and target) or the nodes.

## Conditions

Errors inherit from `htna_error`. Specific classes:
`htna_missing_column` when the session, order, time or code column is
absent; `htna_actor_conflict` when a code belongs to more than one actor
type and `disambiguate = FALSE`; `htna_unassigned_node` when a code has
no actor type in `node_groups`; `htna_tied_times` when
`method = "latency"` is given times that are tied within sessions;
`htna_bad_input` for other invalid arguments.

## References

Lopez-Pernas, S., Misiejuk, K., & Saqr, M. (2026). How AI-generated
feedback hinders or helps learning: A heterogeneous TNA study of
learning dynamics. *Journal of Computer Assisted Learning*, 42(4),
e70285. [doi:10.1002/jcal.70285](https://doi.org/10.1002/jcal.70285)

## See also

[`summary.htna()`](https://sonsoles.me/htna/reference/summary.htna.md)
and
[`as.data.frame.htna()`](https://sonsoles.me/htna/reference/as.data.frame.htna.md)
to describe the network;
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) to draw
it;
[`compare_htna()`](https://sonsoles.me/htna/reference/compare_htna.md),
[`permutation_htna()`](https://sonsoles.me/htna/reference/permutation_htna.md)
and
[`bootstrap_htna()`](https://sonsoles.me/htna/reference/bootstrap_htna.md)
for inference.

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
net
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

# the same network from one data frame per actor type
build_htna(list(Human = human_simplified, AI = ai_simplified))
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
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

# or from a code-to-actor codebook
build_htna(human_ai, node_groups = human_ai_codebook)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
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

# one network per phase, in the order of the factor levels
grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase")
#> Metadata aggregated per session: ties resolved by first occurrence in 'cluster' (24 sessions), 'actor_type' (9 sessions)
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (18 sessions), 'actor_type' (15 sessions)
summary(grp)
#> <htna_group: 2 cohorts (Early, Late)>
#> 
#> === Early ===
#> <htna network>
#>   Method:    relative
#>   Sessions:  214   (max 287 timesteps)
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
#> 
#> Network metrics:
#>                       metric        value
#>                   Node Count  12.00000000
#>                   Edge Count 135.00000000
#>              Network Density   1.00000000
#>                Mean Distance   0.02704448
#>            Mean Out-Strength   1.00000000
#>              SD Out-Strength   0.00000000
#>             Mean In-Strength   1.00000000
#>               SD In-Strength   0.65969479
#>              Mean Out-Degree  11.25000000
#>                SD Out-Degree   0.96530730
#>  Centralization (Out-Degree)   0.05785124
#>   Centralization (In-Degree)   0.05785124
#>                  Reciprocity   0.96000000
#> 
#> === Late ===
#> <htna network>
#>   Method:    relative
#>   Sessions:  215   (max 208 timesteps)
#>   Nodes:     12
#>   Edges:     134 / 144 (non-zero)
#> 
#> Actor types (2):
#>   Human (6 nodes):  Check, Frustrate, Inquire, Refine, Request, Specify
#>   AI    (6 nodes):  Ask, Delegate, Execute, Plan, Repair, Report
#> 
#> Edge counts by actor (rows = source, cols = target):
#>           Human    AI
#>   Human      35    36
#>   AI         36    27
#> 
#> Network metrics:
#>                       metric        value
#>                   Node Count 1.200000e+01
#>                   Edge Count 1.340000e+02
#>              Network Density 1.000000e+00
#>                Mean Distance 2.438201e-02
#>            Mean Out-Strength 1.000000e+00
#>              SD Out-Strength 3.347448e-17
#>             Mean In-Strength 1.000000e+00
#>               SD In-Strength 6.109845e-01
#>              Mean Out-Degree 1.116667e+01
#>                SD Out-Degree 9.374369e-01
#>  Centralization (Out-Degree) 5.785124e-02
#>   Centralization (In-Degree) 5.785124e-02
#>                  Reciprocity 9.600000e-01
```
