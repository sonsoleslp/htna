# HTNA Full Function Review Report (No Code Changes)

## Scope
Reviewed all exported and internal R functions for correctness, stability, API consistency, and edge-case robustness.

## Findings

### 1) High — Undefined helper functions in centrality calculations
- Where
  - `R/centralities.R:122`
  - `R/centralities.R:132`
- Why
  - `centralities_htna()` relies on `.weighted_clustering()` and `.rsp_betweenness()` using `.colSums` and `.rowSums`, but these helpers are not defined/imported in this package.
  - Calling measures `Clustering` and/or `BetweennessRSP` can fail at runtime.
- How to fix
  - Replace with base `colSums()` / `rowSums()` calls.
  - Or define/import compatible helpers explicitly and add regression tests for `BetweennessRSP` and `Clustering` outputs.

---

### 2) High — Restrictive legend-position matching in sequence overlay helpers
- Where
  - `R/sequence_plot_htna.R:250-252` (`.htna_overlay_legend`)
  - `R/sequence_plot_htna.R:279-281` (`.htna_grouped_legend`)
- Why
  - `match.arg(position, c("right", "bottom"))` only accepts two positions.
  - Upstream plotting workflows can pass other valid base-graphics legend specifiers (e.g., `left`, `topleft`, `none`), causing hard errors.
- How to fix
  - Expand accepted positions to match upstream behavior, or gracefully no-op for unsupported positions with a clear warning/default fallback.

---

### 3) Medium — Palette indexing is not safe for >6 actor groups
- Where
  - `R/plot_frequencies_htna.R:52`
  - `R/plot_htna.R:60`
  - `R/plot_htna_bootstrap.R:94`
  - `R/centralities.R:319`
  - `R/sequence_plot_htna.R:84`
- Why
  - Palettes are finite and colors are extracted by short indexing (`palette[seq_along(x$actor_levels)]`) with no guard.
  - If actor groups exceed palette size, mapped colors become `NA`, causing broken legends/plot layers.
- How to fix
  - Decide a package policy: either enforce an explicit max actor cap (with clear error) or safely extend/recycle palette with documented behavior.
  - Apply same policy consistently across all plotters.

---

### 4) Medium — Silent coercion of `gap = 0L` in gapped sequence discovery
- Where
  - `R/discover_paths.R:31`
- Why
  - In `type = "gapped"`, `gap_vals == 0` is silently rewritten to `1L`.
  - This hides user intent and conflates contiguous and gapped semantics without transparency.
- How to fix
  - Reject `gap = 0` with a targeted error directing users to `type = "contiguous"`, or
  - explicitly normalize with warning and documented equivalence.

---

### 5) Medium — Inconsistent optional dependency handling in ggplot-dependent paths
- Where
  - `R/centralities.R:211` (explicit `ggplot2` requirement check)
  - `R/plot_frequencies_htna.R:35-68` (no explicit requirement check)
  - `R/sequence_plot_htna.R:211-213` (no explicit requirement check)
- Why
  - Different plotting functions exhibit inconsistent failure modes when `ggplot2` is unavailable.
  - This is a user-facing inconsistency that complicates debugging and support.
- How to fix
  - Standardize behavior: add explicit `ggplot2` checks in all ggplot-reliant wrappers or declare/use it consistently as required.

---

## Notes
- I did not modify any package code.
- I only created this review report file.
