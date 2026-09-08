## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load data, message=TRUE, warning=TRUE------------------------------------
library(scales)
library(ggrepel)
library(lubridate)
library(dplyr)
library(forcats)
library(tidyr)
library(kableExtra)
library(NHSRdatasets)

ae_attendances <- NHSRdatasets::ae_attendances

ae_attendances |>
  dplyr::mutate(
    # set the period column to show in Month-Year as a character format
    period = format(period, "%b-%y"),
    # set the numeric columns to have a comma at the 1000's place
    dplyr::across(
      c(attendances, breaches, admissions),
      scales::comma
    )
  ) |>
  # show the first 10 rows
  head(10) |>
  # format for display
  kableExtra::kable()

## ----england performance------------------------------------------------------
england_performance <- ae_attendances |>
  dplyr::group_by(period) |>
  dplyr::summarise(
    dplyr::across(c(attendances, breaches), sum),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    performance = 1 - breaches / attendances
  )

# format for display
england_performance |>
  dplyr::mutate(
    # same format options as above
    period = format(period, "%b-%y"),
    dplyr::across(c(attendances, breaches), scales::comma),
    # this time show the performance column as a percentage
    performance = scales::percent(performance)
  ) |>
  # show the first 10 rows and format as a table
  head(10) |>
  kableExtra::kable()

## ----england performance plot-------------------------------------------------
ggplot2::ggplot(england_performance, ggplot2::aes(period, performance)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(
    x = "Month of attendance",
    y = "% of attendances that met the 4 hour standard",
    title = "NHS England A&E 4 Hour Performance",
    caption = "Source: NHS England Statistical Work Areas (OGL v3.0)"
  )

## ----england performance by type----------------------------------------------
ae_attendances |>
  dplyr::group_by(period, type) |>
  dplyr::summarise_if(is.numeric, sum) |>
  dplyr::mutate(performance = 1 - breaches / attendances) |>
  ggplot2::ggplot(ggplot2::aes(period, performance, colour = type)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  # facet_wrap(vars(type), nrow = 1) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(
    x = "Month of attendance",
    y = "% of attendances that met the 4 hour standard",
    title = "NHS England A&E 4 Hour Performance",
    subtitle = "By Department Type",
    caption = "Source: NHS England Statistical Work Areas (OGL v3.0)"
  )

## ----performance_by_trust-----------------------------------------------------
performance_by_trust <- ae_attendances |>
  dplyr::group_by(org_code, period) |>
  # make sure that this trust has a type 1 department
  dplyr::filter(any(type == 1)) |>
  dplyr::summarise(
    dplyr::across(
      c(attendances, breaches),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    performance = 1 - breaches / attendances
  )

# format for display
performance_by_trust |>
  dplyr::mutate(
    period = format(period, "%b-%y"),
    dplyr::across(c(attendances, breaches), scales::comma),
    performance = scales::percent(performance)
  ) |>
  head(10) |>
  kableExtra::kable()

## ----performance_by_trust_ranking---------------------------------------------
performance_by_trust_ranking <- performance_by_trust |>
  dplyr::summarise(performance = 1 - sum(breaches) / sum(attendances), .by = org_code) |>
  dplyr::arrange(performance) |>
  dplyr::pull(org_code) |>
  as.character()

print("Bottom 5")
head(performance_by_trust_ranking, 5)

print("Top 5")
tail(performance_by_trust_ranking, 5)

## ----performance_by_trust top 5 bottom 5 plot---------------------------------
performance_by_trust |>
  dplyr::mutate(
    org_code = forcats::fct_relevel(
      org_code,
      performance_by_trust_ranking
    )
  ) |>
  dplyr::filter(org_code %in% c(
    head(performance_by_trust_ranking, 5),
    tail(performance_by_trust_ranking, 5)
  )) |>
  ggplot2::ggplot(ggplot2::aes(period, performance)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::facet_wrap(ggplot2::vars(org_code), nrow = 2) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(
    x = "Month of attendance",
    y = "% of attendances that met the 4 hour standard",
    title = "NHS England A&E 4 Hour Performance",
    subtitle = "Bottom 5/Top 5 over the whole 3 years",
    caption = "Source: NHS England Statistical Work Areas (OGL v3.0)"
  )

## ----bencmarking plot---------------------------------------------------------
ae_attendances |>
  dplyr::filter(period == last(period)) |>
  dplyr::group_by(org_code) |>
  dplyr::filter(any(type == 1)) |>
  dplyr::summarise_at(vars(attendances, breaches), sum) |>
  dplyr::mutate(
    performance = 1 - breaches / attendances,
    overall_performance = 1 - sum(breaches) / sum(attendances),
    org_code = forcats::fct_reorder(org_code, -performance)
  ) |>
  #
  dplyr::arrange(performance) |>
  # lets highlight the organsiations that are at the lower and upper quartile
  # and at the median. First "tile" the data into 4 groups, then we use the
  # lag function to check to see if the value changes between rows. We will get
  # NA for the first row, so replace this with FALSE
  dplyr::mutate(
    highlight = ntile(n = 4),
    highlight = tidyr::replace_na(highlight != lag(highlight), FALSE)
  ) |>
  ggplot2::ggplot(ggplot2::aes(org_code, performance)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = overall_performance)) +
  ggplot2::geom_point(ggplot2::aes(fill = highlight), show.legend = FALSE, pch = 21) +
  ggrepel::geom_text_repel(ggplot2::aes(label = ifelse(highlight, as.character(org_code), NA)),
    na.rm = TRUE
  ) +
  ggplot2::scale_fill_manual(values = c(
    "TRUE" = "black",
    "FALSE" = NA
  )) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(),
    axis.ticks.y = ggplot2::element_line()
  )

