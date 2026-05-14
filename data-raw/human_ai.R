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

usethis::use_data(human_ai, overwrite = TRUE, compress = "xz")
