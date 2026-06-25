# Builds `human_ai`: a Human + AI long-format sequence frame derived
# from `Nestimate::human_long` and `Nestimate::ai_long` by collapsing
# several near-synonym codes into a smaller, easier-to-read alphabet
# (6 Human codes, 6 AI codes). Suitable as a teaching example for
# `build_htna()`.
#
# Run:  source("data-raw/human_ai.R")
#
# Writes to data/human_ai.rda.

utils::data(human_long, ai_long, package = "Nestimate")

ai_simplified <- ai_long |>
  dplyr::mutate(
    actor_type = "AI",
    code  = dplyr::case_when(
      code %in% c("Investigate", "Ask", "Inquire") ~ "Ask",
      code %in% c("Explain", "Report")             ~ "Report",
      TRUE                                         ~ code
    )
  )

human_simplified <- human_long |>
  dplyr::mutate(
    actor_type = "Human",
    code  = dplyr::case_when(
      code %in% c("Command", "Request")     ~ "Request",
      code %in% c("Correct", "Verify")      ~ "Check",
      code %in% c("Interrupt", "Frustrate") ~ "Frustrate",
      TRUE                                  ~ code
    )
  )

human_ai <- dplyr::bind_rows(ai_simplified, human_simplified) |>
  dplyr::arrange(project, order_in_session)

# Chronological session order; session_id is a deterministic
# tiebreak among sessions that started on the same date.
sess_start <- aggregate(session_date ~ session_id, data = human_ai,
                        FUN = min)
sess_start <- sess_start[order(sess_start$session_date,
                               sess_start$session_id), ]
half       <- nrow(sess_start) %/% 2L
early_sess <- sess_start$session_id[seq_len(half)]

human_ai$phase <- ifelse(human_ai$session_id %in% early_sess,
                         "Early", "Late")
human_ai$phase <- factor(human_ai$phase, levels = c("Early", "Late"))


human_codes <- unique(human_simplified$code)
ai_codes    <- unique(ai_simplified$code)

human_ai_codebook <- data.frame(
  code       = c(human_codes, ai_codes),
  actor_type = c(rep("Human", length(human_codes)),
                 rep("AI",    length(ai_codes))),
  stringsAsFactors = FALSE
)

usethis::use_data(human_ai, overwrite = TRUE, compress = "xz")
usethis::use_data(ai_simplified, overwrite = TRUE, compress = "xz")
usethis::use_data(human_simplified, overwrite = TRUE, compress = "xz")
usethis::use_data(human_ai_codebook, overwrite = TRUE, compress = "xz")
