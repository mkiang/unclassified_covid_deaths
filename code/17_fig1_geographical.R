## Imports ----
library(tidyverse)
library(here)
library(patchwork)
library(usmap)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## Load fonts (Arial Narrow) ----
## You may need to also run `extrafont::font_import()` to get exact fonts.
extrafont::loadfonts(device = "all")

## Data ----
results_df <- readRDS(here::here("data", "out_of_hosp_predictions.RDS"))

## Constants ----
GROUPING_VARS <- c("county_fips_cat", "state_cat")

## Cleaning ----
sub_df <- results_df |>
    dplyr::filter(
        model == "xgboost_bayes_tune_nocountynoucr",
        misclassification_adjustment == "unadjusted",
        age_group == "all"
    ) |>
    dplyr::filter(grouping_var %in% GROUPING_VARS) |>
    calculate_arr()

## State figure ----
state_df <- sub_df |>
    dplyr::filter(grouping_var %in% c("state_cat")) |>
    dplyr::left_join(return_st_info(),
        by = c("grouping_value" = "abbrev")
    ) |>
    dplyr::mutate(state_rank = dplyr::row_number(reporting_ratio)) |>
    dplyr::arrange(state_rank)
state_df$rank_cat <- factor(state_df$grouping_value,
    levels = state_df$grouping_value,
    ordered = TRUE
)

state_fig <- ggplot2::ggplot(
    state_df,
    ggplot2::aes(
        x     = rank_cat,
        y     = reporting_ratio,
        ymin  = reporting_ratio_lower,
        ymax  = reporting_ratio_upper,
        color = division
    )
) +
    ggplot2::geom_hline(
        yintercept = 1,
        color = "black",
        alpha = .2
    ) +
    ggplot2::geom_point(ggplot2::aes(size = official_covid),
        alpha = .5
    ) +
    ggplot2::geom_errorbar(width = 0) +
    mk_nytimes() +
    ggplot2::scale_y_continuous(
        "Adjusted Reporting\nRatio (95% UI, log)",
        trans = "log",
        breaks = c(1 / 1.5, 1, 1.5),
        labels = c(".67", "1", "1.5")
    ) +
    ggplot2::scale_x_discrete(NULL) +
    ggplot2::scale_size_binned_area(
        "Official COVID-19\nDeaths (thousands)",
        max_size = 2,
        breaks = c(10000, 25000, 50000),
        labels = function(x) {
            x / 1000
        }
    ) +
    ggsci::scale_colour_aaas(name = "Census\nDivision") +
    mk_nytimes(legend.position = "bottom")

## County figure ----
county_df <- sub_df |>
    dplyr::filter(grouping_var %in% c("county_fips_cat")) |>
    dplyr::rename(fips = grouping_value) |>
    dplyr::right_join(usmap::us_map(regions = "county"))

county_df <- county_df |>
    dplyr::mutate(
        color = dplyr::case_when(
            dplyr::between(reporting_ratio, 2.5, Inf) ~ "#A50026",
            dplyr::between(reporting_ratio, 1.75, 2.5) ~ "#D73027",
            dplyr::between(reporting_ratio, 1.25, 1.75) ~ "#F46D43",
            dplyr::between(reporting_ratio, 1, 1.25) ~ "#FDAE61",
            dplyr::between(reporting_ratio, .75, 1) ~ "#66BD63",
            dplyr::between(reporting_ratio, 0, .75) ~ "#006837",
            is.na(reporting_ratio) ~ "grey"
        )
    )

county_fig <- ggplot2::ggplot() +
    ggplot2::geom_sf(
        data = county_df,
        ggplot2::aes(
            geometry = geom,
            fill = color
        ),
        color = NA
    ) +
    ggplot2::geom_sf(
        data = usmap::us_map("states"),
        ggplot2::aes(geometry = geom),
        color = "white",
        fill = NA,
        size = .025
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
        legend.position = "right",
        plot.margin = ggplot2::unit(c(0, 50, 0, 0), "pt")
    ) +
    ggplot2::scale_fill_identity(
        name = "Adjusted\nReporting\nRatio",
        breaks = c("#006837", "#66BD63", "#FDAE61", "#F46D43", "#D73027", "#A50026", "grey"),
        labels = c("<.75", "(.75, 1]", "(1, 1.25]", "(1.25,1.75]", "(1.75, 2.5]", ">2.5", "Missing"),
        guide = "legend"
    ) +
    ggplot2::coord_sf()

## Combine figures ----
p1 <- county_fig + patchwork::free(state_fig) + patchwork::plot_layout(ncol = 1, heights = c(4, 2))

## Save ----
ggplot2::ggsave(
    here::here("plots", "fig1_geographical_results.pdf"),
    p1,
    width = 9,
    height = 8,
    scale = 1.3,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig1_geographical_results.jpg"),
    p1, 
    width = 9,
    height = 8,
    scale = 1.3,
    dpi = 1200
)
readr::write_csv(
    county_df |>
        dplyr::select(
            fips,
            reporting_ratio,
            reporting_ratio_lower,
            reporting_ratio_upper,
            color
        ),
    here::here("output", "figure_data", "fig1a_data_county.csv")
)
readr::write_csv(
    state_df |>
        dplyr::select(
            state = grouping_value,
            official_covid,
            predicted_covid,
            predicted_covid_lower,
            predicted_covid_upper,
            reporting_ratio,
            reporting_ratio_lower,
            reporting_ratio_upper
        ),
    here::here("output", "figure_data", "fig1b_data_state.csv")
)

# ggplot2::ggsave(
#     here::here("plots", "fig99_github_header.jpg"),
#     county_fig,
#     width = 9,
#     height = 5,
#     scale = 1.3,
#     dpi = 300
# )
