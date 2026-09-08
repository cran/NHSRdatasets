## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NHSRdatasets)

covid19 <- NHSRdatasets::covid19

## -----------------------------------------------------------------------------
covid19 |>
  dplyr::filter(stringr::str_detect(countries_and_territories, "and")) |>
  dplyr::distinct(countries_and_territories) |>
  head(5)

## -----------------------------------------------------------------------------
covid19 |>
  # rather than having two separate filters which is "AND" and returns nothing the code uses | for "OR"
  dplyr::filter(stringr::str_detect(countries_and_territories, "_and_") |
    stringr::str_detect(countries_and_territories, " and ")) |>
  dplyr::distinct(countries_and_territories)

## -----------------------------------------------------------------------------
covid19 |>
  dplyr::mutate(countries_and_territories = tolower(countries_and_territories)) |>
  dplyr::filter(stringr::str_detect(countries_and_territories, "_and_") |
    stringr::str_detect(countries_and_territories, " and ")) |>
  dplyr::distinct(countries_and_territories)

## -----------------------------------------------------------------------------
covid19 |>
  dplyr::filter(stringr::str_detect(countries_and_territories, "_and_") |
    stringr::str_detect(countries_and_territories, " and ")) |>
  dplyr::distinct(countries_and_territories) |>
  dplyr::mutate(countries_and_territories = stringr::str_replace(countries_and_territories, "_", ""))

## -----------------------------------------------------------------------------
covid19 |>
  dplyr::filter(stringr::str_detect(countries_and_territories, "_and_") |
    stringr::str_detect(countries_and_territories, " and ")) |>
  dplyr::distinct(countries_and_territories) |>
  dplyr::mutate(countries_and_territories = stringr::str_replace_all(countries_and_territories, "_", " "))

