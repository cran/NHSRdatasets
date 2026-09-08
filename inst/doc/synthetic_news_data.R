## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(NHSRdatasets)

NEWS_var <- NHSRdatasets::synthetic_news_data

## -----------------------------------------------------------------------------
library(NHSRdatasets)
library(dplyr)

sbp_news <- NEWS_var |>
  dplyr::mutate(sbp = as.numeric(syst)) |>
  dplyr::mutate(news = dplyr::case_when(
    sbp <= 90 | sbp >= 220 ~ 3,
    sbp %in% c(91:100) ~ 2,
    sbp %in% c(101:110) ~ 1,
    !is.numeric(pulse) ~ NA_real_,
    TRUE ~ 0
  ))

## -----------------------------------------------------------------------------
hr_news <- NEWS_var |>
  dplyr::mutate(pulse = as.numeric(pulse)) |>
  dplyr::mutate(news = dplyr::case_when(
    pulse <= 40 | pulse >= 131 ~ 3,
    pulse %in% c(111:130) ~ 2,
    pulse %in% c(41:50, 91:110) ~ 1,
    !is.numeric(pulse) ~ NA_real_,
    TRUE ~ 0
  ))

## -----------------------------------------------------------------------------
rr_news <- NEWS_var |>
  dplyr::mutate(resp_rate = as.numeric(resp)) |>
  dplyr::mutate(news = dplyr::case_when(
    resp_rate <= 8 | resp_rate >= 25 ~ 3,
    resp_rate %in% c(21:24) ~ 2,
    resp_rate %in% c(9:11) ~ 1,
    !is.numeric(resp_rate) ~ NA_real_,
    TRUE ~ 0
  ))

## -----------------------------------------------------------------------------
NEWS_var |>
  dplyr::mutate(news = dplyr::case_when(
    sat <= 91 ~ 3,
    sat %in% c(92:93) ~ 2,
    sat %in% c(94:95) ~ 1,
    !is.numeric(sat) ~ NA_real_,
    TRUE ~ 0
  ))

## -----------------------------------------------------------------------------
NEWS_var |>
  dplyr::mutate(news = dplyr::case_when(
    temp <= 35 ~ 3,
    temp >= 39.1 ~ 2,
    temp %in% c(38.1:39, 35.1:36) ~ 1,
    !is.numeric(temp) ~ NA_real_,
    TRUE ~ 0
  ))

