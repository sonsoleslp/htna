# Agent Note for htna — Architecture, Styling, and Plot Wrappers

**Audience:** the next agent (or developer) extending `htna`.
**Status:** Draft, derived from a 2026-04-26 design review against
`cograph` 2.1.1. **Scope:** how htna should layer on top of `cograph`
and `Nestimate` — what belongs here, what stays upstream, and the
templates for building it.

------------------------------------------------------------------------

## 1. Architectural principle (non-negotiable)

`cograph` is a **generic** plotting engine. It can render single
networks and multi-group networks (`plot_htna`), but it has no opinion
about what a “group” *means*. `htna` is the package that knows actor
partitions are real and that they need consistent visual identity across
plots.

**Therefore:** - `cograph` exposes raw params and (eventually) layout
helpers. No htna knowledge. - `htna` owns: actor-partition styling,
default layout choices for actor partitions, multi-plot layout
consistency (so a bootstrap plot and the base plot look like *the same
network*), and the `htna` class chain. - htna wrappers compose: read
partition from `x$node_groups` → merge htna styling defaults with user
`...` → call `cograph::*` with explicit args.

**Anti-pattern to avoid:** writing htna-specific styling (e.g.
`htna_styling = TRUE`) into cograph. It couples cograph’s release
cadence to htna’s visual decisions, and it sets the same precedent that
produced the `tna_styling`/`psych_styling` historical drift in cograph.

------------------------------------------------------------------------

## 2. The active partition contract

[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)
writes the partition in two places on the returned `netobject`:

- `x$nodes$groups` — column on the nodes data frame; cograph’s
  `plot_htna` *auto-detects* this name. Kept for backward-compat with
  users who call `cograph::plot_htna(net)` directly.
- `x$node_groups` — canonical schema: a data frame with columns `node`
  and `group`. **htna’s wrappers MUST read from here**, not from
  `$nodes$groups`, not from cograph’s auto-detect.

Class chain: `c("htna", "netobject", "cograph_network")`.
`inherits(x, "htna")` is the only safe guard.

**Why two slots?** `$nodes$groups` keeps cograph happy when called
directly. `$node_groups` is the contract htna enforces. They must agree;
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md) is
responsible for keeping them in sync.

**Don’t rely on cograph’s auto-detect inside htna.** That auto-detect (a
priority list of column names: `groups`, `group`, `clusters`, `cluster`,
`community`, `module`, `layer`) is a generic UX shortcut, not a
contract. htna’s wrappers should always pass `node_list` explicitly so
the call site is readable and the partition source is unambiguous.

------------------------------------------------------------------------

## 3. Styling: `.htna_style_defaults()` (starter implementation)

A single internal helper that returns a list of
[`cograph::plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html)
parameters. Wrappers merge it with user overrides via
[`modifyList()`](https://rdrr.io/r/utils/modifyList.html).

``` r

#' htna visual defaults
#'
#' Returns the standard htna visual defaults as a named list of cograph
#' `plot_htna()` parameters. Used by every htna plot wrapper.
#'
#' @param net An htna network (built by `build_htna()`).
#' @param directed Logical; if NULL, inferred from the network.
#' @return Named list of `cograph::plot_htna()` parameters.
#' @keywords internal
.htna_style_defaults <- function(net, directed = NULL) {
  n_groups <- if (!is.null(net$node_groups)) {
    length(unique(net$node_groups$group))
  } else NA_integer_

  if (is.null(directed)) {
    directed <- isTRUE(attr(net, "directed")) ||
                isTRUE(net$directed) ||
                !.is_symmetric_matrix(.weights_of(net))
  }

  defaults <- list(
    layout                  = "circular",     # cograph 2.1.1+: circular default
    angle_spacing           = 0.35,           # clear inter-group gap
    legend                  = TRUE,
    legend_position         = "bottom",
    legend_horiz            = TRUE,
    edge_color              = "#003355",
    edge_label_style        = "estimate",
    edge_label_leading_zero = FALSE,
    edge_label_size         = 0.4,
    edge_label_position     = 0.7,
    minimum                 = 0.01,
    node_size               = 7,
    layout_margin           = 0.15,
    curvature               = 0.4
  )

  if (isTRUE(directed)) {
    defaults$arrow_size        <- 0.61
    defaults$edge_start_length <- 0.2
    defaults$edge_start_style  <- "dotted"
  }

  if (!is.na(n_groups) && n_groups >= 2) {
    # Curated palette: starts with the 2-actor anchor (Human=#4FC3F7 /
    # AI=#fbb550 from the cograph plot_htna default), then perceptually
    # distinct extensions. Override per-call via group_colors.
    palette <- c("#4FC3F7", "#fbb550", "#7eb5d6", "#98d4a2",
                 "#f4a582", "#92c5de", "#d6c1de", "#b8e186")
    shapes  <- c("circle", "square", "diamond", "triangle",
                 "pentagon", "hexagon", "star", "cross")
    defaults$group_colors <- rep_len(palette, n_groups)
    defaults$group_shapes <- rep_len(shapes,  n_groups)
  }

  defaults
}

# Tiny helpers to keep .htna_style_defaults pure -----------------------------
.weights_of <- function(net) {
  net$weights %||% as.matrix(net)
}
.is_symmetric_matrix <- function(m) {
  is.matrix(m) && nrow(m) == ncol(m) && isTRUE(all.equal(m, t(m)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
```

**Override discipline:** `modifyList(defaults, user_args)` — user always
wins. Don’t conditionally apply (`if not in user_args`) for parameter
shape.

------------------------------------------------------------------------

## 4. Wrapper template (the canonical pattern)

Every plot wrapper follows the same shape:

``` r
plot_<thing> <- function(x, ..., .style = .htna_style_defaults) {
  # 1. Validate: must be htna
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network produced by build_htna().",
         call. = FALSE)
  }

  # 2. Read partition from canonical schema
  node_list <- split(x$node_groups$node, x$node_groups$group)

  # 3. Merge defaults with user overrides
  defaults <- .style(x)
  args     <- modifyList(defaults, list(...))

  # 4. Always pass node_list explicitly; never rely on auto-detect
  do.call(cograph::plot_htna,
          c(list(x = x, node_list = node_list), args))
}
```

This is the entire
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md)
wrapper. Register it as an export and let it override the current
re-export of
[`cograph::plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html).

------------------------------------------------------------------------

## 5. Layout helper (htna-side port until cograph exports one)

For wrappers that compose with `cograph` plotters that do **not**
auto-detect groups (`splot.net_bootstrap`, `splot.net_permutation`,
`plot_compare`), htna needs to compute the multi-group layout itself and
inject it via the existing `layout = matrix` path that
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
already supports.

cograph 2.1.1 has the geometry inside
[`plot_htna()`](https://sonsoles.me/htna/reference/plot_htna.md) but
does **not** yet export it as a public helper. Until that lands, htna
ports a minimal version:

``` r

#' Compute a circular multi-group layout (htna port)
#'
#' Mirrors cograph::compute_circular_layout (private). Drop this when cograph
#' exports a public `compute_htna_layout()` and switch all callers to that.
#'
#' @param node_list Named list of character vectors, one per group.
#' @param all_labels Character vector of every node label, in network order.
#' @param angle_spacing Inter-group gap as fraction of the per-group arc.
#' @param scale Radius multiplier.
#' @return Named list with x, y numeric vectors aligned to all_labels.
#' @keywords internal
.htna_circular_layout <- function(node_list, all_labels,
                                  angle_spacing = 0.35, scale = 1) {
  n_groups <- length(node_list)
  group_indices <- lapply(node_list, function(g) match(g, all_labels))

  n <- length(all_labels)
  x_pos <- rep(0, n); y_pos <- rep(0, n)
  radius <- 2 * scale

  angle_per_group <- 2 * pi / n_groups
  gap_angle       <- angle_per_group * angle_spacing
  arc_angle       <- angle_per_group - gap_angle

  for (i in seq_len(n_groups)) {
    g_idx <- group_indices[[i]]
    n_in  <- length(g_idx)
    start <- pi/2 - (i - 1) * angle_per_group - gap_angle/2
    end   <- start - arc_angle
    if (n_in > 1) {
      ang <- seq(start, end, length.out = n_in)
    } else if (n_in == 1) {
      ang <- (start + end) / 2
    } else next
    x_pos[g_idx] <- radius * cos(ang)
    y_pos[g_idx] <- radius * sin(ang)
  }
  list(x = x_pos, y = y_pos)
}
```

**The matrix-injection pattern**, used wherever an htna wrapper has to
drive a cograph plotter that doesn’t read `$node_groups`:

``` r
node_list  <- split(net$node_groups$node, net$node_groups$group)
labels     <- get_nodes(net)$label %||% rownames(weights)
xy         <- .htna_circular_layout(node_list, labels, angle_spacing = 0.35)
layout_mat <- cbind(x = xy$x, y = xy$y)

# Critical: cograph::splot rescales layouts by default. We pre-rescaled, so
# disable both rescaling and layout-scale to preserve our coordinates.
args <- modifyList(.htna_style_defaults(net), list(...))
args$layout       <- layout_mat
args$rescale      <- FALSE
args$layout_scale <- 1

do.call(cograph::<some_plotter>, c(list(x = thing), args))
```

Three args together — `layout = matrix`, `rescale = FALSE`,
`layout_scale = 1` — are the contract that lets the htna geometry
survive into cograph’s renderer unchanged. Drop any one and cograph
re-normalizes.

------------------------------------------------------------------------

## 6. Bootstrap wrapper template

``` r

#' Plot an htna bootstrap result with the htna multi-group layout.
#'
#' Composes htna styling + the htna multi-group layout with cograph's
#' bootstrap renderer. The CI / significance overlay is whatever
#' `cograph::splot.net_bootstrap` produces; htna only owns geometry + style.
#'
#' @param boot Output of `bootstrap_htna()`.
#' @param ... Forwarded to `cograph::splot.net_bootstrap`. User args win.
#' @export
plot_htna_bootstrap <- function(boot, ...) {
  if (!inherits(boot, "htna_bootstrap")) {
    stop("`boot` must come from bootstrap_htna().", call. = FALSE)
  }
  net <- boot$model
  if (is.null(net$node_groups)) {
    stop("boot$model has no $node_groups; was it built by build_htna()?",
         call. = FALSE)
  }

  node_list  <- split(net$node_groups$node, net$node_groups$group)
  labels     <- net$nodes$label %||% rownames(net$weights)
  xy         <- .htna_circular_layout(node_list, labels, angle_spacing = 0.35)
  layout_mat <- cbind(x = xy$x, y = xy$y)

  defaults <- .htna_style_defaults(net)
  args     <- modifyList(
    defaults,
    list(layout = layout_mat, rescale = FALSE, layout_scale = 1, ...)
  )

  do.call(cograph::splot.net_bootstrap, c(list(x = boot), args))
}
```

The same template applies for `plot_htna_permutation` (just call
[`cograph::splot.net_permutation`](https://sonsoles.me/cograph/reference/splot.html)
and the underlying object is a `net_permutation`).

------------------------------------------------------------------------

## 7. plot_compare wrapper (TBD until cograph supports it)

cograph’s `plot_compare(x, y, ...)` does not currently accept a
`node_list` or a pre-computed layout matrix. Until either lands
upstream, the cleanest htna-side implementation is to **avoid** wrapping
`plot_compare` directly and instead expose a difference-network plot via
`plot_htna`:

``` r

plot_htna_diff <- function(x, y, ...) {
  if (!inherits(x, "htna") || !inherits(y, "htna")) {
    stop("Both `x` and `y` must be htna networks.", call. = FALSE)
  }
  if (!identical(sort(unique(x$node_groups$group)),
                 sort(unique(y$node_groups$group)))) {
    stop("x and y must share the same actor partition.", call. = FALSE)
  }
  diff_mat <- cograph::to_matrix(x) - cograph::to_matrix(y)
  rownames(diff_mat) <- colnames(diff_mat) <- x$nodes$label
  node_list <- split(x$node_groups$node, x$node_groups$group)
  defaults <- .htna_style_defaults(x)
  args     <- modifyList(defaults, list(...))
  # plot_htna can take a matrix as x and node_list explicitly
  do.call(cograph::plot_htna,
          c(list(x = diff_mat, node_list = node_list), args))
}
```

This loses cograph’s signed (positive/negative) edge coloring. When
cograph adds `node_list` (or a layout-matrix arg) to `plot_compare`,
swap to a real wrapper that preserves diff semantics.

------------------------------------------------------------------------

## 8. Testing convention (visual checks are mandatory)

This package’s value is visual. Numerical correctness is necessary but
not sufficient. **No styling or layout change ships without an HTML
visual check.**

Convention: - Place visual checks in `tmp/` (gitignore-friendly) or
`inst/visual_checks/`. - Each check renders both the *new* output and a
deliberately-old reference side-by-side at multiple group counts (2, 3,
4, 5, 6, 8) and at multiple device aspect ratios (square, wide, tall). -
Use
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
Do not assume the agent can see the image; ask the user to review the
HTML and confirm before committing.

------------------------------------------------------------------------

## 9. Dependency posture

- `cograph (>= 2.1.1)` — required for the matrix-edge-color fix in
  `resolve_edge_colors()` (commit `72b7fbc0`), the MTNA self-loop fixes
  (commit `7b711e63`), `legend_position = "bottom"` default, and
  `angle_spacing = 0.35` for circular.
- When cograph exports `compute_htna_layout()`, drop
  `.htna_circular_layout` in this package and import the public helper.
  That is the single planned cograph-side change for Phase 2.
- `Nestimate (>= 0.4.3)` — required by `build_htna` and
  `bootstrap_htna`. Verify this version is on r-universe before tagging
  an htna release.

------------------------------------------------------------------------

## 10. Roadmap (for the next agent)

| Phase | Scope | Owner |
|----|----|----|
| 1 | Replace re-export [`cograph::plot_htna`](https://sonsoles.me/cograph/reference/plot_htna.html) with the wrapper template (Section 4). Adopt `.htna_style_defaults()` (Section 3). Add `plot_htna_bootstrap` (Section 6). | htna |
| 2 | Wait for cograph to export `compute_htna_layout()`. Replace `.htna_circular_layout` port. Add `plot_htna_permutation`. | htna + cograph |
| 3 | Once cograph adds `node_list` (or layout-matrix) to `plot_compare`, swap `plot_htna_diff` for a real `plot_compare` wrapper that preserves signed-difference rendering. | htna + cograph |
| 4 | `find_patterns()` is referenced in roxygen but unimplemented. Either ship a state-level pattern enumerator that mirrors `extract_meta_paths` (use `.discover_paths` on node-label alphabet rather than type alphabet) or remove the dangling references. | htna |
| 5 | Tests + README. The package currently has neither. Minimum: `testthat` for `build_htna`, `bootstrap_htna`, `extract_meta_paths`; visual HTML for each plot wrapper; README with one Human/AI example end-to-end. | htna |

------------------------------------------------------------------------

## 11. Cograph quirks to know about

- **R list NULL trap:** `args$x <- NULL` *deletes* the element. To store
  NULL, use `args["x"] <- list(NULL)`. Affects
  [`modifyList()`](https://rdrr.io/r/utils/modifyList.html) results when
  a default is non-NULL but the user wants NULL.
- **Auto-detect priority list:** see Section 2. htna does not depend on
  it, but be aware that calls like `cograph::plot_htna(net)` (no
  `node_list`) silently pick whichever group column they find first.
  Pass `node_list` explicitly to be unambiguous.
- **`splot()` does not dispatch via `UseMethod`:** all `splot.foo`
  methods are found via an
  [`inherits()`](https://rdrr.io/r/base/class.html) cascade in
  `splot()`. Adding a new class for cograph to recognize means editing
  that cascade. htna does not need to.
- **`tna_styling = TRUE` and `psych_styling = TRUE` exist in cograph**
  for legacy reasons. Don’t add `htna_styling = TRUE` upstream — it’s
  anti-pattern for the reasons in Section 1.

------------------------------------------------------------------------

## 12. Decision log

- **Auto-detect by column name was a design issue from moment 0.** It is
  a generic shortcut that became a de-facto contract by accident. htna
  does not participate in it; it always reads `$node_groups` and always
  passes `node_list` explicitly.
- **Circular layout is the htna default.** Set in
  `.htna_style_defaults()`. Bipartite remains available via explicit
  `layout = "bipartite"`. The cograph default also flipped to circular
  in 2.1.1.
- **Styling lives in htna, not cograph.** The `.tna_style_defaults` /
  `.psych_style_defaults` precedent inside cograph is historical drift,
  not a pattern to follow.
- **plot_htna’s broken “per-axis + dev-aspect stretch” normalization
  (commit `7b711e63` upstream) was rejected.** PR \#1 against
  `sonsoleslp/cograph` reverts it. htna should pin `cograph (>= 2.1.1)`
  once that PR merges.
