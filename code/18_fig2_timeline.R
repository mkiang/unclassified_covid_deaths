## Imports ----
library(tidyverse)
library(here)
library(patchwork)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
results_df <- readRDS(here::here("data", "out_of_hosp_predictions.RDS"))

## Constants ----
GROUPING_VARS <- c("month_into_pandemic")

## Cleaning ----
time_df <- results_df |>
    dplyr::filter(
        model == "xgboost_bayes_tune_nocountynoucr",
        misclassification_adjustment == "unadjusted",
        age_group == "all"
    ) |>
    dplyr::filter(grouping_var %in% GROUPING_VARS) |>
    calculate_arr() |>
    dplyr::mutate(date = as.Date("2020-03-01") + months(as.integer(grouping_value)))

## Timeline ----
p1a <- ggplot2::ggplot(
    time_df,
    ggplot2::aes(
        x = date,
        y = reporting_ratio,
        ymin = reporting_ratio_lower,
        ymax = reporting_ratio_upper
    )
) +
    ggplot2::geom_hline(
        yintercept = 1,
        color = "black",
        alpha = .25,
        size = 1
    ) +
    ggplot2::geom_vline(
        xintercept = c(
            as.Date("2020-06-01"),
            as.Date("2020-09-01"),
            as.Date("2021-06-01"),
            as.Date("2021-11-01")
        ),
        linetype = "dotted",
        alpha = .75
    ) +
    ggplot2::geom_ribbon(color = NA, alpha = .25) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous("Adjusted Reporting Ratio (95% CI)") +
    ggplot2::scale_x_date(
        NULL,
        labels = scales::label_date_short(),
        expand = c(0, 0),
        limits = c(as.Date("2020-02-15"), as.Date("2021-12-14")),
        breaks = c(
            as.Date("2020-03-01"),
            as.Date("2020-07-01"),
            as.Date("2021-01-01"),
            as.Date("2021-07-01"),
            as.Date("2021-12-01")
        )
    ) +
    mk_nytimes(axis.text.x = ggplot2::element_text(hjust = c(0, .5, .5, .5, 1)))
## numbers ----
p1b <- ggplot2::ggplot(
    time_df,
    ggplot2::aes(
        x = date,
        y = official_covid
    )
) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(
        yintercept = seq(0, 105000, 20000),
        color = "white",
        alpha = 1,
        size = .5
    ) +
    ggplot2::geom_vline(
        xintercept = c(
            as.Date("2020-06-01"),
            as.Date("2020-09-01"),
            as.Date("2021-06-01"),
            as.Date("2021-11-01")
        ),
        linetype = "dotted",
        alpha = .75
    ) +
    ggplot2::scale_y_continuous(
        "Official COVID-19\nDeaths (thousands)",
        expand = c(0, 0),
        limits = c(0, 105000),
        breaks = seq(0, 100000, 20000),
        labels = function(x) {
            x / 1000
        }
    ) +
    ggplot2::scale_x_date(
        NULL,
        labels = scales::label_date_short(),
        limits = c(as.Date("2020-02-15"), as.Date("2021-12-14")),
        expand = c(0, 0),
        breaks = c(
            as.Date("2020-03-01"),
            as.Date("2020-07-01"),
            as.Date("2021-01-01"),
            as.Date("2021-07-01"),
            as.Date("2021-12-01")
        )
    ) +
    mk_nytimes(axis.text.x = ggplot2::element_text(hjust = c(0, .5, .5, .5, 1)))

p1 <- p1a + p1b + patchwork::plot_layout(ncol = 1, heights = c(4, 1))

## Save ----
ggplot2::ggsave(
    here::here("plots", "fig2_timeline.pdf"),
    p1,
    width = 9,
    height = 6.5,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig2_timeline.jpg"),
    p1,
    width = 9,
    height = 6.5,
    scale = 1,
    dpi = 1200
)
readr::write_csv(
    time_df |>
        dplyr::select(
            date,
            official_covid,
            predicted_covid,
            predicted_covid_lower,
            predicted_covid_upper,
            reporting_ratio,
            reporting_ratio_lower,
            reporting_ratio_upper
        ),
    here::here("output", "figure_data", "fig2_data_timeline.csv")
)
