## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
results_df <- readRDS(here::here("data", "out_of_hosp_predictions.RDS"))

## Constants
GROUPING_VARS <- c(
    "rucc_2013_cat",
    "home_ownership",
    "household_income",
    "some_college",
    "poor_or_fair_health",
    "diabetes"
)

## Cleaning ----
county_df <- results_df |>
    dplyr::filter(
        model == "xgboost_bayes_tune_nocountynoucr",
        misclassification_adjustment == "unadjusted",
        age_group == "all"
    ) |>
    dplyr::filter(
        grouping_var %in% GROUPING_VARS,
        !is.na(grouping_value)
    ) |>
    calculate_arr()

county_df <- county_df |>
    dplyr::mutate(
        y_pos = dplyr::case_when(
            ## Rurality
            axis_lab == "rucc_2013_cat 1" ~ 39,
            axis_lab == "rucc_2013_cat 2" ~ 38,
            axis_lab == "rucc_2013_cat 3" ~ 37,
            axis_lab == "rucc_2013_cat 4" ~ 36,
            axis_lab == "rucc_2013_cat 5" ~ 35,
            axis_lab == "rucc_2013_cat 6" ~ 34,
            axis_lab == "rucc_2013_cat 7" ~ 33,
            axis_lab == "rucc_2013_cat 8" ~ 32,
            axis_lab == "rucc_2013_cat 9" ~ 31,

            ## Percent homeownership
            axis_lab == "home_ownership [0,0.659]" ~ 29,
            axis_lab == "home_ownership (0.659,0.71]" ~ 28,
            axis_lab == "home_ownership (0.71,0.745]" ~ 27,
            axis_lab == "home_ownership (0.745,0.781]" ~ 26,
            axis_lab == "home_ownership (0.781,0.931]" ~ 25,

            ## Household median income
            axis_lab == "household_income [2.47e+04,4.44e+04]" ~ 23,
            axis_lab == "household_income (4.44e+04,5.07e+04]" ~ 22,
            axis_lab == "household_income (5.07e+04,5.64e+04]" ~ 21,
            axis_lab == "household_income (5.64e+04,6.44e+04]" ~ 20,
            axis_lab == "household_income (6.44e+04,1.52e+05]" ~ 19,

            ## Percent some college
            axis_lab == "some_college [0.00833,0.48]" ~ 17,
            axis_lab == "some_college (0.48,0.551]" ~ 16,
            axis_lab == "some_college (0.551,0.613]" ~ 15,
            axis_lab == "some_college (0.613,0.686]" ~ 14,
            axis_lab == "some_college (0.686,1]" ~ 13,

            ## Percent reporting poor or fair health
            axis_lab == "poor_or_fair_health [0.0859,0.154]" ~ 11,
            axis_lab == "poor_or_fair_health (0.154,0.182]" ~ 10,
            axis_lab == "poor_or_fair_health (0.182,0.21]" ~ 9,
            axis_lab == "poor_or_fair_health (0.21,0.245]" ~ 8,
            axis_lab == "poor_or_fair_health (0.245,0.41]" ~ 7,

            ## Percent diabetes
            axis_lab == "diabetes [0.024,0.091]" ~ 5,
            axis_lab == "diabetes (0.091,0.11]" ~ 4,
            axis_lab == "diabetes (0.11,0.128]" ~ 3,
            axis_lab == "diabetes (0.128,0.152]" ~ 2,
            axis_lab == "diabetes (0.152,0.295]" ~ 1
        )
    )

y_labs <- rev(
    c(
        "Rural / Urban",
        "   1. Metro 1M+ pop",
        "   2. Metro 250k-1M pop",
        "   3. Metroc 250k pop",
        "   4. Urban 20k metro-adjacent",
        "   5. Urban 20k not metro-adjacent 5",
        "   6. Urban 2.5-20k metro-adjacent",
        "   7. Urban 2.5-20k not metro-adjacent",
        "   8. Rural metro-adjacent",
        "   9. Rural not metro-adjacent",
        "Proportion homeowners",
        "   Q1 [0, 0.66]",
        "   Q2 (0.66, 0.71]",
        "   Q3 (0.71, 0.75]",
        "   Q4 (0.75, 0.78]",
        "   Q5 (0.78, 0.93]",
        "Household income (thousands)",
        "   Q1 [24.7, 44.4]",
        "   Q2 (44.4, 50.7]",
        "   Q3 (50.7, 56.4]",
        "   Q4 (56.4, 64.4]",
        "   Q5 (64.4, 152]",
        "Proportion some college",
        "   Q1 [0.01, 0.48]",
        "   Q2 (0.48, 0.55]",
        "   Q3 (0.55, 0.61]",
        "   Q4 (0.61, 0.69]",
        "   Q5 (0.69, 1.00]",
        "Proportion poor or fair health",
        "   Q1 [0.09, 0.15]",
        "   Q2 (0.15, 0.18]",
        "   Q3 (0.18, 0.21]",
        "   Q4 (0.21, 0.24]",
        "   Q5 (0.24, 0.41]",
        "Proportion diabetes",
        "   Q1 [0.02, 0.09]",
        "   Q2 (0.09, 0.11]",
        "   Q3 (0.11, 0.13]",
        "   Q4 (0.13, 0.15]",
        "   Q5 (0.15, 0.30]"
    )
)

y_face <- ifelse(substr(y_labs, 1, 3) == "   ", "plain", "bold")


## Plot ----
p1 <- ggplot2::ggplot(county_df, ggplot2::aes(y = y_pos)) +
    ggplot2::geom_segment(
        ggplot2::aes(
            x = official_covid,
            xend = predicted_covid,
            yend = y_pos
        ),
        alpha = .5
    ) +
    ggplot2::geom_point(
        ggplot2::aes(x = official_covid),
        alpha = .9,
        size = 1.5,
        color = ggsci::pal_aaas()(2)[1]
    ) +
    ggplot2::geom_point(
        ggplot2::aes(x = predicted_covid),
        alpha = .9,
        size = 1.5,
        color = ggsci::pal_aaas()(2)[2]
    ) +
    ggplot2::annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = c(-Inf, 12.5, 24.5),
        ymax = c(6.5, 18.5, 30.5),
        color = NA,
        fill = "black",
        alpha = .1
    ) +
    ggplot2::scale_y_continuous(
        "County-level characteristics",
        labels = y_labs,
        breaks = 1:NROW(y_labs),
        expand = c(0, .5),
        limits = c(.5, NROW(y_labs))
    ) +
    ggplot2::scale_x_continuous(
        "COVID-19 Deaths (thousands)",
        expand = c(0, .1),
        limits = c(0, NA),
        labels = function(x) {
            x / 1000
        }
    ) +
    mk_nytimes(
        axis.text.y = ggplot2::element_text(hjust = 0, face = y_face),
        panel.grid.major.y = ggplot2::element_blank()
    )

## Save ----
ggplot2::ggsave(here::here("plots", "figS2_county_chacteristics_absolute.pdf"),
    p1,
    width = 6,
    height = 7,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(here::here("plots", "figS2_county_chacteristics_absolute.jpg"),
    p1,
    width = 6,
    height = 7,
    scale = 1,
    dpi = 1200
)
