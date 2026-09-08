## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NHSRdatasets)

ons_mortality <- NHSRdatasets::ons_mortality

## -----------------------------------------------------------------------------
unique(ons_mortality$category_1)

## ----eval = FALSE-------------------------------------------------------------
# ons_mortality |>
#   dplyr::distinct(category_1)

## -----------------------------------------------------------------------------
ons_mortality |>
  dplyr::filter(is.na(category_1))

## ----eval=FALSE---------------------------------------------------------------
# ons_mortality |>
#   dplyr::filter(!is.na(category_1))

## ----eval=FALSE---------------------------------------------------------------
# ons_mortality |>
#   dplyr::filter_out(is.na(category_1))

## -----------------------------------------------------------------------------
library(magrittr)

ons_mortality |>
  dplyr::filter(is.na(category_1)) %>%
  print(n = nrow(.))

## -----------------------------------------------------------------------------
by_region <- ons_mortality |>
  dplyr::filter(category_1 == "Region") |>
  ggplot2::ggplot(ggplot2::aes(date, counts, colour = category_2)) +
  ggplot2::geom_line() +
  ggplot2::geom_point()

by_region

## -----------------------------------------------------------------------------
by_region + ggplot2::facet_wrap(~category_2)

