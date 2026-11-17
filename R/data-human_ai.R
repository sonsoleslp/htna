#' Human and AI turns in vibe-coding sessions
#'
#' @description
#' `human_ai` is a coded dataset of the turns in 429 human-AI pair-programming
#' ("vibe coding") sessions in 34 projects, held between 5 February and 8 March
#' 2026. The data are the standard example for a heterogeneous transition
#' network with two actor types: six human codes (Check, Frustrate, Inquire,
#' Refine, Request, Specify) and six AI codes (Ask, Delegate, Execute, Plan,
#' Repair, Report). In each session a person directs an AI coding assistant;
#' every turn is coded with one or more interaction codes, and the codes of both
#' actors are interleaved in the order in which they occurred.
#'
#' The codes are a simplified version of the original coding, with
#' near-synonyms merged. AI codes: Investigate, Ask and Inquire became Ask;
#' Explain and Report became Report. Human codes: Command and Request became
#' Request; Correct and Verify became Check; Interrupt and Frustrate became
#' Frustrate. The `phase` column splits the sessions into an early and a late
#' half for grouped examples. The same rows are available split by actor
#' type in [human_simplified] and [ai_simplified], and the code-to-actor
#' lookup in [human_ai_codebook].
#'
#' @format A data frame with 19,347 rows (10,796 human and 8,551 AI codes)
#'   and 11 columns, one row per code:
#' \describe{
#'   \item{message_id}{Integer identifier of the turn; the codes of one turn
#'     share it.}
#'   \item{project}{Project identifier, `"Project_1"` to `"Project_34"`.}
#'   \item{session_id}{Session identifier (the session key for
#'     [build_htna()]).}
#'   \item{timestamp}{Integer Unix time of the session date (day
#'     resolution).}
#'   \item{session_date}{Date of the row, `"YYYY-MM-DD"`.}
#'   \item{code}{Simplified interaction code.}
#'   \item{cluster}{High-level cluster of the original code, kept for
#'     reference; because merged codes may come from different clusters, one
#'     simplified code can carry more than one cluster label.}
#'   \item{code_order}{Order of the code within its turn.}
#'   \item{order_in_session}{Position of the row in its session, counting
#'     both actors (the order key for [build_htna()]).}
#'   \item{actor_type}{`"Human"` or `"AI"`.}
#'   \item{phase}{Factor with levels `"Early"` and `"Late"`: sessions ordered
#'     by their first date (session id breaking ties) and split in half, 214
#'     early and 215 late sessions.}
#' }
#'
#' @source Saqr, M. (2026). Human-AI vibe coding interaction study.
#'   \url{https://saqr.me/blog/2026/human-ai-interaction-cograph/}. Derived
#'   from the `human_long` and `ai_long` data of the 'Nestimate' package; the
#'   build script is `data-raw/human_ai.R` in the package sources.
#'
#' @seealso [human_simplified], [ai_simplified], [human_ai_codebook],
#'   [build_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' plot_htna(net)
"human_ai"

#' Human turns of the vibe-coding sessions
#'
#' @description
#' `human_simplified` is the human part of [human_ai]: its human rows, as one
#' data frame for one actor type. Together with [ai_simplified] it is the input
#' for the named-list form of [build_htna()], in which each actor type comes in
#' its own data frame.
#'
#' @format A data frame with 10,796 rows and 10 columns: the columns of
#'   [human_ai] without `phase`; `actor_type` is always `"Human"`.
#'
#' @source Derived from the `human_long` data of the 'Nestimate' package (see
#'   [human_ai]).
#'
#' @seealso [human_ai], [ai_simplified], [human_ai_codebook]
#'
#' @examples
#' data(human_simplified, ai_simplified)
#' net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#' plot_htna(net)
"human_simplified"

#' AI turns of the vibe-coding sessions
#'
#' @description
#' `ai_simplified` is the AI part of [human_ai]: its AI rows, as one data frame
#' for one actor type. Together with [human_simplified] it is the input for the
#' named-list form of [build_htna()], in which each actor type comes in its own
#' data frame.
#'
#' @format A data frame with 8,551 rows and 10 columns: the columns of
#'   [human_ai] without `phase`; `actor_type` is always `"AI"`.
#'
#' @source Derived from the `ai_long` data of the 'Nestimate' package (see
#'   [human_ai]).
#'
#' @seealso [human_ai], [human_simplified], [human_ai_codebook]
#'
#' @examples
#' data(human_simplified, ai_simplified)
#' net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#' summary(net)
"ai_simplified"

#' Code-to-actor codebook for the vibe-coding data
#'
#' @description
#' `human_ai_codebook` is the codebook of [human_ai]: the actor type of each of
#' its twelve codes. A codebook of this form declares the actor partition when
#' the event data have no actor-type column: it is passed to [build_htna()] as
#' `node_groups`.
#'
#' @format A data frame with 12 rows, one per code, and 2 columns:
#' \describe{
#'   \item{code}{Simplified interaction code.}
#'   \item{actor_type}{`"Human"` or `"AI"`.}
#' }
#'
#' @source Derived from [human_simplified] and [ai_simplified].
#'
#' @seealso [human_ai], [build_htna()]
#'
#' @examples
#' data(human_ai, human_ai_codebook)
#' human_ai_codebook
#' net <- build_htna(human_ai, node_groups = human_ai_codebook)
#' summary(net)
"human_ai_codebook"
