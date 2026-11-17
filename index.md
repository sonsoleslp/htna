# htna: Heterogeneous Transition Network Analysis

## Vignettes and guides

**Vignettes**

- [Heterogeneous transition network
  analysis](https://sonsoles.me/htna/articles/htna.html)
- [Grouped heterogeneous transition
  networks](https://sonsoles.me/htna/articles/grouped.html)

**Guides**

- [Input formats](https://sonsoles.me/htna/articles/input-formats.html)
- [Comparing
  networks](https://sonsoles.me/htna/articles/comparing-networks.html)
- [Model
  assessment](https://sonsoles.me/htna/articles/model-assessment.html)
- [Markov-order
  adequacy](https://sonsoles.me/htna/articles/markov-order.html)
- [Sequences and
  patterns](https://sonsoles.me/htna/articles/sequences-and-patterns.html)
- [Frequencies and mosaic
  plots](https://sonsoles.me/htna/articles/frequencies-and-mosaic.html)
- [Clustering inputs](https://sonsoles.me/htna/articles/clustering.html)
- [Three or more actor
  groups](https://sonsoles.me/htna/articles/multigroup.html)

**Case studies**

- [Early and late sessions of vibe
  coding](https://sonsoles.me/htna/articles/case-early-late.html)
- [Help-seeking in vibe
  coding](https://sonsoles.me/htna/articles/case-vibe-coding.html)

## Overview

`htna` analyses sequences of events produced by actors of different
kinds that take turns: a learner and an AI assistant, a programmer and a
coding agent, a student and a tutor. Heterogeneous transition network
analysis (HTNA) represents such sequences as one transition network in
which each code belongs to one actor type, so the network separates what
each actor does after its own moves from how the actors hand over to
each other (López-Pernas, Misiejuk & Saqr, 2026). It shows, for example,
what the AI does after the learner asks for help, and how the learner
responds. Networks can be built for one group or for several cohorts or
clusters, compared by permutation or Bayesian tests, and assessed by
bootstrap, split-half reliability, case-dropping stability and
Markov-order tests; paths and meta-paths of turn-taking can be counted.

The networks are estimated from long event data or clustering results by
the [Nestimate](https://CRAN.R-project.org/package=Nestimate) package
and drawn by [cograph](https://CRAN.R-project.org/package=cograph), with
each actor type on its own arc of the layout and in its own colour and
shape. The actor partition is kept on the results of every later
analysis.

## The model

A session is a sequence of codes $`c_1, \ldots, c_T`$, and every code
$`i`$ belongs to one actor type $`a(i)`$. With $`n_{ij}`$ the number of
times code $`i`$ is directly followed by code $`j`$ over all sessions,
the relative transition network has the weights
$`w_{ij} = n_{ij} / \sum_k n_{ik}`$. The actor partition divides the
weight matrix into blocks $`W_{AB}`$ of transitions from codes of actor
$`A`$ to codes of actor $`B`$: the diagonal blocks describe how an actor
continues its own turn and the off-diagonal blocks how the actors hand
over to each other. A meta-path is the sequence of actor types that a
path of codes passes through, such as Human $`\to`$ AI $`\to`$ Human.

## Installation

``` r

install.packages("htna")

# development version
install.packages("remotes")
remotes::install_github("sonsoleslp/htna")
```

## Example

The bundled `human_ai` data hold 19,347 coded events from 429 sessions
in which programmers worked with an AI coding agent, with six human and
six AI codes.
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
estimates the network; `actor_type` names the column that assigns each
event to its actor.

``` r

library(htna)
net <- build_htna(human_ai, actor_type = "actor_type")
as.data.frame(summary(net), what = "actor_pairs")
#>   source_actor target_actor n_edges
#> 1        Human        Human      35
#> 2        Human           AI      36
#> 3           AI        Human      36
#> 4           AI           AI      28
```

All 36 possible transitions between the human and the AI codes occur in
both directions.
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) draws
each actor’s codes on their own arc.

``` r

plot_htna(net)
```

![plot of chunk htna-plot](reference/figures/README-htna-plot-1.png)

plot of chunk htna-plot

Meta-paths count the order in which the actors take turns; a lift above
one means a pattern occurs more often than the frequencies of the actors
would predict.

``` r

extract_meta_paths(net, level = "type", length = 3)
#> Meta-paths (type-level) over 429 sequences
#> Rows: 8 | Lengths: 3 | Gaps: 0
#>               schema length gap count n_seq support frequency lift
#>     Human->AI->Human      3   0  3593   402   0.937     0.194 1.41
#>     Human->Human->AI      3   0  3172   422   0.984     0.172 1.25
#>     AI->Human->Human      3   0  2828   403   0.939     0.153 1.11
#>        AI->Human->AI      3   0  2744   383   0.893     0.148 1.36
#>        Human->AI->AI      3   0  2189   403   0.939     0.118 1.09
#>        AI->AI->Human      3   0  2100   397   0.925     0.114 1.04
#>  Human->Human->Human      3   0  1472   396   0.923     0.080 0.46
#>           AI->AI->AI      3   0   392   165   0.385     0.021 0.25
```

The bootstrap shows which transitions the resampled sessions reproduce.

``` r

bootstrap_htna(net, iter = 100, seed = 1)
#>   Edge                   Mean     95% CI          p
#>   -----------------------------------------------
#>   Delegate → Plan      0.609  [0.541, 0.670]  ** 
#>   Refine → Specify     0.412  [0.360, 0.468]  ** 
#>   Ask → Plan           0.411  [0.384, 0.437]  ** 
#>   Check → Execute      0.411  [0.382, 0.444]  ** 
#>   Repair → Execute     0.389  [0.333, 0.454]  ** 
#>   ... and 55 more significant edges
#> 
#> Bootstrap Network  [Transition Network (relative) | directed]
#>   Iterations : 100  |  Nodes : 12
#>   Edges      : 60 significant / 135 total
#>   CI         : 95%  |  Inference: stability  |  CR [0.75, 1.25]
```

## Authors and citation

`htna` is written by [Sonsoles López-Pernas](https://sonsoles.me),
Kamila Misiejuk and [Mohammed Saqr](https://saqr.me). Sonsoles
López-Pernas maintains the package. Questions and bug reports can be
submitted through the [repository issue
tracker](https://github.com/sonsoleslp/htna/issues). The package
citation is available through `citation("htna")`.

The method is described in:

- López-Pernas, S., Misiejuk, K., & Saqr, M. (2026). How AI-generated
  feedback hinders or helps learning: A heterogeneous TNA study of
  learning dynamics. *Journal of Computer Assisted Learning*, 42(4),
  e70285. <https://doi.org/10.1002/jcal.70285>

Related research:

- López-Pernas, S., Misiejuk, K., Tikka, S., & Saqr, M. (2026). Role
  dynamics in student-AI collaboration: A heterogeneous transition
  network analysis approach. *Innovations in Analytics of Learning
  Dynamics: Proceedings of the 1st Workshop on Transition Network
  Analysis (TNA)*. <https://dx.doi.org/10.2139/ssrn.6082190>
- Rinja, D., Oliveira, E. A., López-Pernas, S., Saqr, M., Specht, M., &
  Misiejuk, K. (2027). Unpacking vibe coding: Help-seeking processes in
  student-AI interactions while programming. *Proceedings of Artificial
  Intelligence in Education: 27th International Conference, AIED 2026*.
  Springer Nature Switzerland.
  <https://doi.org/10.1007/978-3-032-29763-1_35>

Related packages: [tna](https://CRAN.R-project.org/package=tna) for
transition network analysis of single-actor sequences,
[Nestimate](https://CRAN.R-project.org/package=Nestimate) for network
estimation and inference, and
[cograph](https://CRAN.R-project.org/package=cograph) for network plots.
