### 2026-05-10 — CRAN check + posit-dev/skills CRAN-extrachecks pass

Two follow-ups in one session: ran `R CMD check --as-cran` and applied the
posit-dev/skills/r-lib/cran-extrachecks SKILL.md checklist (saved as a
reference memory entry).

**R CMD check fixes:**
- `vignettes/grouped.Rmd`, `vignettes/htna.Rmd`, `vignettes/multigroup.Rmd`:
  renamed leftover `permutation()` calls → `permutation_htna()` (cleanup of
  last session's rename). Vignettes now build cleanly.
- `tests/testthat/test-tna-equivalence.R`: same rename
  `htna::permutation` → `htna::permutation_htna` (lines 716, 755).
- `tests/testthat/helper-fixtures.R`: added
  `skip_if_nestimate_param_get_missing()` helper that probes
  `asNamespace("Nestimate")` for `.param_get`. Used in 3 reliability
  tests so they skip cleanly under the broken Nestimate 0.5.4 instead of
  erroring during R CMD check.
- `R/htna-package.R`: added `utils::globalVariables(".param_get")` so
  the "no visible global function definition" NOTE (caused by
  `casedrop_reliability_htna <- Nestimate::casedrop_reliability` inheriting
  Nestimate's body that references the missing helper) is silenced.
- `.Rbuildignore`: added `AGENT-NOTE-HTNA-REVIEW.md`, `HANDOFF.md`,
  `LEARNINGS.md`, `CHANGES.md`, `cran-comments.md`.

**CRAN extra-checks (posit-dev SKILL.md):**
- 8 alias files (`association_rules_htna`, `casedrop_reliability_htna`,
  `markov_order_test_htna`, `mosaic_plot_htna`, `permutation_htna`,
  `sequence_compare_htna`, `state_distribution_htna`,
  `state_frequencies_htna`): added `@inheritParams Nestimate::<fn>` so
  the `Undocumented arguments` warning clears.
- 12 exports (8 aliases plus `frequencies_htna`,
  `plot_frequencies_htna`, `bootstrap`, `plot_sequences`): added
  `@examples` blocks, using real example data (`Nestimate::human_long`,
  `Nestimate::ai_long`) wrapped in `\donttest{}` for slower runs.
- `DESCRIPTION`: `URL:` updated to `https://sonsoles.me/htna/` (trailing
  slash; the previous form redirected and would have been rejected).
- `NEWS.md`: created (initial release notes).
- `cran-comments.md`: created — documents the upstream
  Nestimate `wtna_perm_mixed` warning for CRAN reviewers and notes
  the absence of method references.
- `inst/WORDLIST`: created. Whitelists British-English prose
  (colour/behaviour/visualise/standardise/...) and package-specific
  technical terms (Nestimate, cograph, htna, RSP, Markovian,
  treemap, splot, ...). `spelling::spell_check_package(".")` now
  returns 0 hits.

**Final R CMD check status:** 0 ERRORs, 3 WARNINGs, 4 NOTEs.
All 7 remaining warnings/notes collapse to a single upstream
`Nestimate` bug: `print.wtna_perm_mixed` and `summary.wtna_perm_mixed`
declared in `Nestimate`'s NAMESPACE without implementations. The
warning is repeated by every R CMD check stage that touches NAMESPACE
or S3 dispatch. Documented for CRAN reviewers in `cran-comments.md`.

**`urlchecker::url_check(".")`**: ✓ all URLs correct.

### 2026-05-10 — Address AGENT-NOTE-HTNA-REVIEW.md findings (#2–#5; #1 was a false positive)

- `R/sequence_plot_htna.R`:
  - Added `.htna_normalize_overlay_pos()` — translates any base-graphics legend position into `"right"` / `"bottom"` (the only two the gutter overlay system supports), `NULL` for `"none"`/`FALSE`, with a warning + sensible fallback otherwise.
  - Both `by = "group"` and `by = "state"` paths now route their `legend` argument through the normalizer instead of crashing on `match.arg`.
  - `htna_palette[seq_along(actor_chr)]` replaced with `.htna_actor_colors(actor_chr)` for safe recycling beyond 6 actors.
- `R/plot_htna.R`: added internal `.htna_actor_colors(n_or_levels, palette)` — warns and recycles when actor count exceeds palette length. `plot_htna()` now uses it.
- `R/plot_htna_bootstrap.R`: `.htna_node_styling()` switched to `.htna_actor_colors()`.
- `R/plot_frequencies_htna.R`:
  - Added `requireNamespace("ggplot2")` guard for `view %in% c("bars","facet")` (treemap path is already ggplot-free).
  - Switched palette indexing to `.htna_actor_colors(x$actor_levels)`.
- `R/centralities.R`: `plot_centralities()` switched palette indexing to `.htna_actor_colors()` for the `by = "group"` path.
- `R/discover_paths.R`: `gap = 0` with `type = "gapped"` now raises a targeted error directing users to `type = "contiguous"`, instead of silently rewriting to `gap = 1L`.
- Tests:
  - `tests/testthat/test-paths.R`: new test asserting both `extract_meta_paths()` and `extract_paths()` reject `gap = 0` in gapped mode.
  - `tests/testthat/test-coverage-extras.R`: new tests for `.htna_actor_colors` (recycle warning, names) and `.htna_normalize_overlay_pos` (canonical pass-through, none/FALSE/NULL → `NULL`, fallback warnings).
- Tests pass: 2126 (up from 2107). 5 pre-existing failures (Nestimate `.param_get`; old `htna::permutation` references) are unchanged and unrelated.
