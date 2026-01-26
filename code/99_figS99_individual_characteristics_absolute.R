## Imports ----
library(tidyverse)
library(here)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
results_df <- readRDS(here("data", "out_of_hosp_predictions.RDS"))

## Constants
GROUPING_VARS <- c(
    "total",
    "age_bin",
    "educ_cat",
    "marital_cat",
    "sex_cat",
    "racerbig_recode"
)

## Cleaning ----
individual_df <- results_df |>
    filter(
        model == "xgboost_bayes_tune_nocountynoucr",
        misclassification_adjustment == "unadjusted",
        age_group == "all"
    ) |>
    filter(grouping_var %in% GROUPING_VARS) |> 
    calculate_arr()

individual_df <- individual_df |> 
    mutate(
        y_pos = case_when(
            ## Age
            axis_lab == "age_bin 25to34" ~ 37,
            axis_lab == "age_bin 35to44" ~ 36,
            axis_lab == "age_bin 45to54" ~ 35,
            axis_lab == "age_bin 55to64" ~ 34,
            axis_lab == "age_bin 65to74" ~ 33,
            axis_lab == "age_bin 75to84" ~ 32,
            axis_lab == "age_bin 85andup" ~ 31,
            
            ## Sex
            axis_lab == "sex_cat F" ~ 29,
            axis_lab == "sex_cat M" ~ 28,
            
            ## Education
            axis_lab == "educ_cat < hs" ~ 26,
            axis_lab == "educ_cat hs_no_diploma" ~ 25,
            axis_lab == "educ_cat hs_ged" ~ 24,
            axis_lab == "educ_cat some_college" ~ 23,
            axis_lab == "educ_cat associate" ~ 22,
            axis_lab == "educ_cat bachelor" ~ 21,
            axis_lab == "educ_cat master" ~ 20,
            axis_lab == "educ_cat doctorate" ~ 19,
            axis_lab == "educ_cat unknown" ~ 18,
            
            ## Marital status
            axis_lab == "marital_cat S" ~ 16,
            axis_lab == "marital_cat M" ~ 15,
            axis_lab == "marital_cat D" ~ 14,
            axis_lab == "marital_cat W" ~ 13,
            axis_lab == "marital_cat U" ~ 12,
            
            ## Race / ethnicity
            axis_lab == "racerbig_recode hispanic" ~ 10,
            axis_lab == "racerbig_recode nhaian" ~ 9,
            axis_lab == "racerbig_recode nhasian" ~ 8,
            axis_lab == "racerbig_recode nhasian_indian" ~ 7,
            axis_lab == "racerbig_recode nhblack" ~ 6,
            axis_lab == "racerbig_recode nhmultiracial" ~ 5,
            axis_lab == "racerbig_recode nhpacific_islander" ~ 4,
            axis_lab == "racerbig_recode nhwhite" ~ 3,
            axis_lab == "racerbig_recode unknown" ~ 2,
            
            ## Total
            axis_lab == "total total" ~ 1,
        )
    ) |> 
    arrange(y_pos)

## Make y-axis labels ----
y_labs <- c(
    "Total",
    "   Unknown",
    "   Non-Hispanic White",
    "   Non-Hispanic Pacific Islander",
    "   Non-Hispanic Multiracial",
    "   Non-Hispanic Black",
    "   Non-Hispanic Asian Indian",
    "   Non-Hispanic Asian",
    "   Non-Hispanic American Indian/Alaska Native",
    "   Hispanic",
    "Race/ethnicity",
    "   Unknown",
    "   Widowed",
    "   Divorced",
    "   Married",
    "   Single, never married",
    "Marital status",
    "   Unknown",
    "   Doctorate",
    "   Masters",
    "   Bachelors",
    "   Associate",
    "   Some college",
    "   High School Diploma / GED",
    "   Some high school",
    "   Less than high school",
    "Education",
    "   Male",
    "   Female",
    "Sex",
    "   85 and up",
    "   75 to 84",
    "   65 to 74",
    "   55 to 64",
    "   45 to 54",
    "   35 to 44",
    "   25 to 34",
    "Age in years"
)

y_labs[individual_df |> pull(y_pos)] <- sprintf(
    "%s (%s / %s)",
    y_labs[individual_df |> pull(y_pos)],
    individual_df |> pull(predicted_covid) |> prettyNum(big.mark = ","),
    individual_df |> pull(official_covid) |> prettyNum(big.mark = ",")
)

y_face <- ifelse(substr(y_labs, 1, 3) == "   ", "plain", "bold")


## Plot ----
p1 <- ggplot(individual_df, aes(y = y_pos)) +
    geom_segment(aes(x = official_covid, xend = predicted_covid, yend = y_pos),
                 alpha = .5) +
    geom_point(
        aes(x = official_covid),
        alpha = .9,
        size = 1.5,
        color = ggsci::pal_aaas()(2)[1]
    ) +
    geom_point(
        aes(x = predicted_covid),
        alpha = .9,
        size = 1.5,
        color = ggsci::pal_aaas()(2)[2]
    ) +
    annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = c(1.5, 17.5, 30.5),
        ymax = c(11.5, 27.5, Inf),
        color = NA,
        fill = "black",
        alpha = .08
    ) +
    scale_y_continuous(
        "Individual-level characteristics\n(Predicted COVID-19 deaths / Official COVID-19 deaths)",
        labels = y_labs[2:NROW(y_labs)],
        breaks = 2:NROW(y_labs),
        expand = c(0, .25),
        limits = c(1.5, NROW(y_labs))
    ) +
    scale_x_continuous(
        "COVID-19 Deaths (thousands)",
        expand = c(0, .05),
        limits = c(0, 625000),
        labels = function(x)
            x / 1000
    ) +
    mk_nytimes(axis.text.y = element_text(hjust = 0, 
                                          face = y_face[2:NROW(y_labs)]),
               panel.grid.major.y = element_blank())

## Save ----
ggsave(here("plots", "figS99_individual_characteristics_absolute.pdf"),
       p1,
       width = 7.5,
       height = 8,
       scale = 1,
       device = cairo_pdf)
ggsave(here("plots", "figS99_individual_characteristics_absolute.jpg"),
       p1,
       width = 7.5,
       height = 8,
       scale = 1,
       dpi = 1200)
