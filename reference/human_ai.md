# Human and AI turns in vibe-coding sessions

`human_ai` is a coded dataset of the turns in 429 human-AI
pair-programming ("vibe coding") sessions in 34 projects, held between 5
February and 8 March 2026. The data are the standard example for a
heterogeneous transition network with two actor types: six human codes
(Check, Frustrate, Inquire, Refine, Request, Specify) and six AI codes
(Ask, Delegate, Execute, Plan, Repair, Report). In each session a person
directs an AI coding assistant; every turn is coded with one or more
interaction codes, and the codes of both actors are interleaved in the
order in which they occurred.

The codes are a simplified version of the original coding, with
near-synonyms merged. AI codes: Investigate, Ask and Inquire became Ask;
Explain and Report became Report. Human codes: Command and Request
became Request; Correct and Verify became Check; Interrupt and Frustrate
became Frustrate. The `phase` column splits the sessions into an early
and a late half for grouped examples. The same rows are available split
by actor type in
[human_simplified](https://sonsoles.me/htna/reference/human_simplified.md)
and
[ai_simplified](https://sonsoles.me/htna/reference/ai_simplified.md),
and the code-to-actor lookup in
[human_ai_codebook](https://sonsoles.me/htna/reference/human_ai_codebook.md).

## Usage

``` r
human_ai
```

## Format

A data frame with 19,347 rows (10,796 human and 8,551 AI codes) and 11
columns, one row per code:

- message_id:

  Integer identifier of the turn; the codes of one turn share it.

- project:

  Project identifier, `"Project_1"` to `"Project_34"`.

- session_id:

  Session identifier (the session key for
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)).

- timestamp:

  Integer Unix time of the session date (day resolution).

- session_date:

  Date of the row, `"YYYY-MM-DD"`.

- code:

  Simplified interaction code.

- cluster:

  High-level cluster of the original code, kept for reference; because
  merged codes may come from different clusters, one simplified code can
  carry more than one cluster label.

- code_order:

  Order of the code within its turn.

- order_in_session:

  Position of the row in its session, counting both actors (the order
  key for
  [`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)).

- actor_type:

  `"Human"` or `"AI"`.

- phase:

  Factor with levels `"Early"` and `"Late"`: sessions ordered by their
  first date (session id breaking ties) and split in half, 214 early and
  215 late sessions.

## Source

Saqr, M. (2026). Human-AI vibe coding interaction study.
<https://saqr.me/blog/2026/human-ai-interaction-cograph/>. Derived from
the `human_long` and `ai_long` data of the 'Nestimate' package; the
build script is `data-raw/human_ai.R` in the package sources.

## See also

[human_simplified](https://sonsoles.me/htna/reference/human_simplified.md),
[ai_simplified](https://sonsoles.me/htna/reference/ai_simplified.md),
[human_ai_codebook](https://sonsoles.me/htna/reference/human_ai_codebook.md),
[`build_htna()`](https://sonsoles.me/htna/reference/build_htna.md)

## Examples

``` r
data(human_ai)
net <- build_htna(human_ai, actor_type = "actor_type")
#> Metadata aggregated per session: ties resolved by first occurrence in 'session_date' (1 sessions), 'cluster' (42 sessions), 'actor_type' (24 sessions)
plot_htna(net)
```
