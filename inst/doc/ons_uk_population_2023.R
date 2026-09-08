## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NHSRdatasets)

ons_uk_population_2023 <- NHSRdatasets::ons_uk_population_2023

## -----------------------------------------------------------------------------
ons_uk_population_2023 |>
  dplyr::filter(name == "UNITED KINGDOM") |>
  ggplot2::ggplot(ggplot2::aes(age, count, colour = sex)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(~sex)

## -----------------------------------------------------------------------------
ons_uk_population_2023 |>
  dplyr::filter(name == "UNITED KINGDOM") |>
  dplyr::slice_tail(n = 5, by = sex)

