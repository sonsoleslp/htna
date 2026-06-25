#' Simplified Human + AI Interaction Sequences
#'
#' A long-format heterogeneous sequence dataset built from
#' \code{Nestimate::human_long} and \code{Nestimate::ai_long} by
#' collapsing several near-synonym codes into a smaller, more
#' interpretable alphabet. Suitable as a teaching example for
#' \code{\link{build_htna}()}.
#'
#' Code remapping (all other codes pass through unchanged):
#' \itemize{
#'   \item AI: \code{Investigate}, \code{Ask}, \code{Inquire} \eqn{\to}
#'     \code{Ask}; \code{Explain}, \code{Report} \eqn{\to} \code{Report}.
#'   \item Human: \code{Command}, \code{Request} \eqn{\to} \code{Request};
#'     \code{Correct}, \code{Verify} \eqn{\to} \code{Check};
#'     \code{Interrupt}, \code{Frustrate} \eqn{\to} \code{Frustrate}.
#' }
#'
#' Resulting alphabets:
#' \itemize{
#'   \item AI (6): \code{Ask}, \code{Delegate}, \code{Execute},
#'     \code{Plan}, \code{Repair}, \code{Report}.
#'   \item Human (6): \code{Check}, \code{Frustrate}, \code{Inquire},
#'     \code{Refine}, \code{Request}, \code{Specify}.
#' }
#'
#' Because two source codes can map to the same simplified code while
#' originally belonging to different \code{cluster} values, a single
#' simplified \code{code} may appear with more than one \code{cluster}
#' label across rows. The \code{cluster} column is preserved verbatim
#' from the source data and should be treated as informational only.
#'
#' @format A data frame with 19347 rows and 11 columns:
#' \describe{
#'   \item{message_id}{Integer. Source message identifier.}
#'   \item{project}{Character. Project label (e.g. \code{"Project_1"}).}
#'   \item{session_id}{Character. Session identifier — pass to
#'     \code{\link{build_htna}()} as the session key.}
#'   \item{timestamp}{Integer. Unix timestamp of the message.}
#'   \item{session_date}{Character. Date of the session
#'     (\code{YYYY-MM-DD}).}
#'   \item{code}{Character. Simplified action code; the
#'     \code{code}-column value passed to \code{\link{build_htna}()}.}
#'   \item{cluster}{Character. Original cluster label from the source
#'     data, retained for reference (see Details).}
#'   \item{code_order}{Integer. Order of the code within the message.}
#'   \item{order_in_session}{Integer. Order of the row within the
#'     session — pass as the order key to \code{\link{build_htna}()}.}
#'   \item{actor_type}{Character. \code{"AI"} or \code{"Human"} — the
#'     actor partition for \code{\link{build_htna}(actor_type = "actor_type")}.}
#'   \item{phase}{Factor with levels \code{"Early"} and \code{"Late"}.
#'     Session-level cohort tag from a chronological split: sessions
#'     ordered by their first \code{session_date} (with \code{session_id}
#'     as a deterministic tiebreak), then split in half. Suitable for
#'     \code{\link{build_htna}(group = "phase")}.}
#' }
#'
#' @source Derived from \code{Nestimate::human_long} and
#'   \code{Nestimate::ai_long}; see \code{data-raw/human_ai.R} for the
#'   build script.
#'
#' @seealso \code{\link{human_simplified}}, \code{\link{ai_simplified}},
#'   \code{\link{human_ai_codebook}}.
#'
#' @examples
#' \donttest{
#' data(human_ai)
#' net <- build_htna(human_ai, actor_type = "actor_type")
#' plot_htna(net)
#' }
"human_ai"

#' Simplified Human Interaction Sequences (per-actor frame)
#'
#' The Human-only slice of \code{\link{human_ai}}, in a long-format
#' data frame ready to feed the named-list form of
#' \code{\link{build_htna}()}. Codes have already been collapsed into
#' the simplified Human alphabet; see \code{\link{human_ai}} for the
#' remapping rules.
#'
#' @format A data frame with 10796 rows and 10 columns. Schema matches
#'   \code{\link{human_ai}} minus the \code{phase} column; every value
#'   in \code{actor_type} is \code{"Human"}.
#'
#' @source Derived from \code{Nestimate::human_long}; see
#'   \code{data-raw/human_ai.R} for the build script.
#'
#' @seealso \code{\link{human_ai}}, \code{\link{ai_simplified}},
#'   \code{\link{human_ai_codebook}}.
#'
#' @examples
#' \donttest{
#' data(human_simplified, ai_simplified)
#' net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#' plot_htna(net)
#' }
"human_simplified"

#' Simplified AI Interaction Sequences (per-actor frame)
#'
#' The AI-only slice of \code{\link{human_ai}}, in a long-format data
#' frame ready to feed the named-list form of \code{\link{build_htna}()}.
#' Codes have already been collapsed into the simplified AI alphabet;
#' see \code{\link{human_ai}} for the remapping rules.
#'
#' @format A data frame with 8551 rows and 10 columns. Schema matches
#'   \code{\link{human_ai}} minus the \code{phase} column; every value
#'   in \code{actor_type} is \code{"AI"}.
#'
#' @source Derived from \code{Nestimate::ai_long}; see
#'   \code{data-raw/human_ai.R} for the build script.
#'
#' @seealso \code{\link{human_ai}}, \code{\link{human_simplified}},
#'   \code{\link{human_ai_codebook}}.
#'
#' @examples
#' \donttest{
#' data(human_simplified, ai_simplified)
#' net <- build_htna(list(Human = human_simplified, AI = ai_simplified))
#' plot_htna(net)
#' }
"ai_simplified"

#' Code → Actor-Type Codebook for \code{human_ai}
#'
#' A tidy two-column lookup tagging every simplified code in
#' \code{\link{human_ai}} with its actor type. Ready to pass as
#' \code{node_groups} to \code{\link{build_htna}()} (Form 3b in the
#' \code{vignette("input-formats", package = "htna")}).
#'
#' @format A data frame with 12 rows and 2 columns:
#' \describe{
#'   \item{code}{Character. Simplified action code (one of the 12
#'     Human + AI codes in \code{\link{human_ai}}).}
#'   \item{actor_type}{Character. \code{"Human"} or \code{"AI"}.}
#' }
#'
#' @source Derived from \code{\link{human_simplified}} and
#'   \code{\link{ai_simplified}}; see \code{data-raw/human_ai.R} for
#'   the build script.
#'
#' @seealso \code{\link{human_ai}}, \code{\link{human_simplified}},
#'   \code{\link{ai_simplified}}.
#'
#' @examples
#' \donttest{
#' data(human_ai, human_ai_codebook)
#' net <- build_htna(human_ai, node_groups = human_ai_codebook)
#' plot_htna(net)
#' }
"human_ai_codebook"
