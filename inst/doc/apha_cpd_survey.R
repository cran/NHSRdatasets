## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NHSRdatasets)
library(tibble)
library(purrr)

apha_cpd_survey <- NHSRdatasets::apha_cpd_survey

## -----------------------------------------------------------------------------
attributes(apha_cpd_survey$q00a_age_bracket_cat)

## -----------------------------------------------------------------------------
df <- tibble(
  variable = names(apha_cpd_survey),
  variable_label = map_chr(
    apha_cpd_survey,
    ~ attr(.x, "label") %||% NA_character_
  ),
  value_labels = map(
    apha_cpd_survey,
    ~ attr(.x, "labels")
  )
)

## -----------------------------------------------------------------------------
tibble::glimpse(apha_cpd_survey)

## ----eval=FALSE---------------------------------------------------------------
# # Simple function that uses base R unique() to return the unique data from a column
# unique_data <- function(data, column) {
#   unique(data$column)
# }
# 
# # maps across all the columns and gives unique data
# purrr::imap(
#   apha_cpd_survey,
#   ~ unique(.x)
# )

## -----------------------------------------------------------------------------
data <- apha_cpd_survey |>
  # unselect the columns with dttm in the name
  dplyr::select(!dplyr::ends_with("dttm")) |>
  # unselect respondent_id as that is a unique number
  dplyr::select(-respondent_id) |>
  # freetext columns
  dplyr::select(-c(
    q04a_role_description_cat,
    q13b_cpd_outside_work_days_txt,
    q17b_org_cpd_budget_txt,
    q18b_org_study_leave_days_txt,
    q20h_cpd_opps_other_txt,
    q21a_cpd_opps_sources_txt
  ))

# Rerun the purrr loop to see the data
purrr::imap(
  data,
  ~ unique(.x)
)

## -----------------------------------------------------------------------------
apha_cpd_survey |>
  dplyr::select(q22a_org_nhs_type_cat) |>
  dplyr::distinct() |>
  dplyr::filter(q22a_org_nhs_type_cat %in% c("Nhse", "NHS England", "NHS England ", "NHSE"))

