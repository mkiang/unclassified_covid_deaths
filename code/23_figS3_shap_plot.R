## Imports ----
library(shapviz)
library(here)
library(tidyverse)
library(patchwork)
source(here::here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
shap_explainer <- readRDS(here::here(
    "data",
    paste0(
        "shap_values_xgboost_bayes_tune_",
        "nocountynoucr_nsims1000_nobs100k.RDS"
    )
))$shap_vals

## Helpers ----
### Reshape the dummy variables back into original columns ----
pivot_subset <- function(df, col_string, new_col_name, multi_col = FALSE) {
    if (multi_col == FALSE) {
        sub_df <- df |>
            dplyr::select(rowid, dplyr::starts_with(col_string)) |>
            tidyr::pivot_longer(
                cols = dplyr::starts_with(col_string),
                names_to = "name",
                values_to = "value"
            ) |>
            dplyr::filter(value == 1) |>
            dplyr::rename({{ new_col_name }} := name) |>
            dplyr::select(-value) |>
            dplyr::mutate({{ new_col_name }} := gsub(col_string, "", {{ new_col_name }}))
    } else if (multi_col) {
        sub_df <- df |>
            dplyr::select(rowid, dplyr::starts_with(col_string)) |>
            tidyr::pivot_longer(
                cols = dplyr::starts_with(col_string),
                names_to = "name",
                values_to = "value"
            ) |>
            dplyr::filter(grepl("_yes", name, fixed = TRUE)) |>
            dplyr::group_by(rowid) |>
            dplyr::summarize(name = sum(value == 1)) |>
            dplyr::ungroup() |>
            dplyr::rename({{ new_col_name }} := name)
    }

    dplyr::left_join(feat_values |>
        dplyr::select(-dplyr::starts_with(col_string)), sub_df, by = "rowid")
}

### List to collapse shap values ----
collapse_list <- list(
    month = c(
        "month_cat_Jan",
        "month_cat_Feb",
        "month_cat_Mar",
        "month_cat_Apr",
        "month_cat_May",
        "month_cat_Jun",
        "month_cat_Jul",
        "month_cat_Aug",
        "month_cat_Sep",
        "month_cat_Oct",
        "month_cat_Nov",
        "month_cat_Dec",
        "month_cat_Unknown"
    ),
    weekday = c(
        "weekday_cat_Sun",
        "weekday_cat_Mon",
        "weekday_cat_Tue",
        "weekday_cat_Wed",
        "weekday_cat_Thu",
        "weekday_cat_Fri",
        "weekday_cat_Sat",
        "weekday_cat_Unknown"
    ),
    state = c(
        "state_cat_AK",
        "state_cat_HI",
        "state_cat_OR",
        "state_cat_CA",
        "state_cat_WA",
        "state_cat_NV",
        "state_cat_ID",
        "state_cat_AZ",
        "state_cat_UT",
        "state_cat_MT",
        "state_cat_WY",
        "state_cat_NM",
        "state_cat_CO",
        "state_cat_ND",
        "state_cat_SD",
        "state_cat_NE",
        "state_cat_TX",
        "state_cat_KS",
        "state_cat_OK",
        "state_cat_MN",
        "state_cat_IA",
        "state_cat_MO",
        "state_cat_AR",
        "state_cat_LA",
        "state_cat_WI",
        "state_cat_MS",
        "state_cat_IL",
        "state_cat_AL",
        "state_cat_TN",
        "state_cat_IN",
        "state_cat_KY",
        "state_cat_MI",
        "state_cat_GA",
        "state_cat_OH",
        "state_cat_FL",
        "state_cat_WV",
        "state_cat_SC",
        "state_cat_NC",
        "state_cat_VA",
        "state_cat_PA",
        "state_cat_DC",
        "state_cat_MD",
        "state_cat_NY",
        "state_cat_DE",
        "state_cat_NJ",
        "state_cat_VT",
        "state_cat_CT",
        "state_cat_MA",
        "state_cat_NH",
        "state_cat_RI",
        "state_cat_ME"
    ),
    education = c(
        "educ_cat_1",
        "educ_cat_2",
        "educ_cat_3",
        "educ_cat_4",
        "educ_cat_5",
        "educ_cat_6",
        "educ_cat_7",
        "educ_cat_8",
        "educ_cat_9"
    ),
    marital_status = c(
        "marital_cat_D",
        "marital_cat_M",
        "marital_cat_S",
        "marital_cat_U",
        "marital_cat_W"
    ),
    sex = c("sex_cat_F", "sex_cat_M"),
    nonhispanic = c(
        "is_nonhispanic_cat_1",
        "is_nonhispanic_cat_2",
        "is_nonhispanic_cat_3"
    ),
    race = c(
        "racerbig_cat_aian",
        "racerbig_cat_asian",
        "racerbig_cat_asian_indian",
        "racerbig_cat_black",
        "racerbig_cat_multiracial",
        "racerbig_cat_pacific_islander",
        "racerbig_cat_white"
    ),
    birth_state = c(
        "birth_state_cat_AK",
        "birth_state_cat_AL",
        "birth_state_cat_AR",
        "birth_state_cat_AS",
        "birth_state_cat_AZ",
        "birth_state_cat_CA",
        "birth_state_cat_CO",
        "birth_state_cat_CT",
        "birth_state_cat_DC",
        "birth_state_cat_DE",
        "birth_state_cat_FL",
        "birth_state_cat_GA",
        "birth_state_cat_GU",
        "birth_state_cat_HI",
        "birth_state_cat_IA",
        "birth_state_cat_ID",
        "birth_state_cat_IL",
        "birth_state_cat_IN",
        "birth_state_cat_KS",
        "birth_state_cat_KY",
        "birth_state_cat_LA",
        "birth_state_cat_MA",
        "birth_state_cat_MD",
        "birth_state_cat_ME",
        "birth_state_cat_MI",
        "birth_state_cat_MN",
        "birth_state_cat_MO",
        "birth_state_cat_MP",
        "birth_state_cat_MS",
        "birth_state_cat_MT",
        "birth_state_cat_NC",
        "birth_state_cat_ND",
        "birth_state_cat_NE",
        "birth_state_cat_NH",
        "birth_state_cat_NJ",
        "birth_state_cat_NM",
        "birth_state_cat_NV",
        "birth_state_cat_NY",
        "birth_state_cat_OH",
        "birth_state_cat_OK",
        "birth_state_cat_OR",
        "birth_state_cat_PA",
        "birth_state_cat_PR",
        "birth_state_cat_RI",
        "birth_state_cat_SC",
        "birth_state_cat_SD",
        "birth_state_cat_TN",
        "birth_state_cat_TX",
        "birth_state_cat_UT",
        "birth_state_cat_VA",
        "birth_state_cat_VI",
        "birth_state_cat_VT",
        "birth_state_cat_WA",
        "birth_state_cat_WI",
        "birth_state_cat_WV",
        "birth_state_cat_WY",
        "birth_state_cat_ZZ"
    ),
    division = c(
        "division_East.North.Central",
        "division_East.South.Central",
        "division_Middle.Atlantic",
        "division_Mountain",
        "division_New.England",
        "division_Pacific",
        "division_South.Atlantic",
        "division_West.North.Central",
        "division_West.South.Central"
    ),
    resident_status = c(
        "restatus_cat_residents",
        "restatus_cat_intrastate_nonresidents",
        "restatus_cat_interstate_nonresidents",
        "restatus_cat_foreign_residents"
    ),
    smoker = c(
        "tobacco_use_cat_N",
        "tobacco_use_cat_p",
        "tobacco_use_cat_P",
        "tobacco_use_cat_U",
        "tobacco_use_cat_Y"
    ),
    copds = c(
        "copds_all_cat_no",
        "copds_all_cat_yes",
        "copds_j440_cat_no",
        "copds_j440_cat_yes",
        "copds_j441_cat_no",
        "copds_j441_cat_yes",
        "copds_j448_cat_no",
        "copds_j448_cat_yes",
        "copds_j449_cat_no",
        "copds_j449_cat_yes"
    ),
    hypertensive = c(
        "hypertensive_all_cat_no",
        "hypertensive_all_cat_yes",
        "hypertensive_i10_cat_no",
        "hypertensive_i10_cat_yes",
        "hypertensive_i11_cat_no",
        "hypertensive_i11_cat_yes",
        "hypertensive_i12_cat_no",
        "hypertensive_i12_cat_yes",
        "hypertensive_i13_cat_no",
        "hypertensive_i13_cat_yes",
        "hypertensive_i15_cat_no",
        "hypertensive_i15_cat_yes",
        "hypertensive_i16_cat_no",
        "hypertensive_i16_cat_yes"
    ),
    hyperlipidemia = c("hyperlipidemia_e785_cat_no", "hyperlipidemia_e785_cat_yes"),
    ischemic = c("ischemic_cat_no", "ischemic_cat_yes"),
    kidney = c("kidney_cat_no", "kidney_cat_yes"),
    other_resp = c("other_resp_cat_no", "other_resp_cat_yes"),
    pneumonia = c("pneumonia_cat_no", "pneumonia_cat_yes"),
    t2diabetes = c("t2d_cat_no", "t2d_cat_yes"),
    community_transmission = c(
        "comm_trans_level_cat_unknown",
        "comm_trans_level_cat_high",
        "comm_trans_level_cat_low",
        "comm_trans_level_cat_moderate",
        "comm_trans_level_cat_substantial"
    ),
    rural_urban = c(
        "rucc_2013_cat_X1",
        "rucc_2013_cat_X2",
        "rucc_2013_cat_X3",
        "rucc_2013_cat_X4",
        "rucc_2013_cat_X5",
        "rucc_2013_cat_X6",
        "rucc_2013_cat_X7",
        "rucc_2013_cat_X8",
        "rucc_2013_cat_X9",
        "rucc_2013_cat_unknown"
    )
)

## Reshape the shap_explainer featuer values ----
feat_values <- shap_explainer$feature_values |>
    tibble::as_tibble() |>
    tibble::rowid_to_column()

feat_values <- pivot_subset(feat_values, "month_cat_", month)
feat_values <- pivot_subset(feat_values, "weekday_cat_", weekday)
feat_values <- pivot_subset(feat_values, "state_cat_", state)
feat_values <- pivot_subset(feat_values, "educ_cat_", education)
feat_values <- pivot_subset(feat_values, "marital_cat_", marital_status)
feat_values <- pivot_subset(feat_values, "sex_cat_", sex)
feat_values <- pivot_subset(feat_values, "is_nonhispanic_cat_", nonhispanic)
feat_values <- pivot_subset(feat_values, "racerbig_cat_", race)
feat_values <- pivot_subset(feat_values, "birth_state_cat_", birth_state)
feat_values <- pivot_subset(feat_values, "division_", division)
feat_values <- pivot_subset(feat_values, "restatus_cat_", resident_status)
feat_values <- pivot_subset(feat_values, "tobacco_use_", smoker)
feat_values <- pivot_subset(feat_values, "copds_", copds, multi_col = TRUE)
feat_values <- pivot_subset(feat_values, "hypertensive_", hypertensive, multi_col = TRUE)
feat_values <- pivot_subset(feat_values, "hyperlipidemia_e785_cat_", hyperlipidemia)
feat_values <- pivot_subset(feat_values, "ischemic_cat_", ischemic)
feat_values <- pivot_subset(feat_values, "kidney_cat_", kidney)
feat_values <- pivot_subset(feat_values, "other_resp_cat_", other_resp)
feat_values <- pivot_subset(feat_values, "pneumonia_cat_", pneumonia)
feat_values <- pivot_subset(feat_values, "t2d_cat_", t2diabetes)
feat_values <- pivot_subset(feat_values, "comm_trans_level_cat_", community_transmission)
feat_values <- pivot_subset(feat_values, "rucc_2013_cat_", rural_urban)

## Create ordered categorical values to get the right color ----
feat_values <- feat_values |>
    dplyr::mutate(
        month = factor(
            month,
            levels = c("Unknown", month.abb),
            ordered = TRUE
        ),
        weekday = factor(
            weekday,
            levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
            ordered = TRUE
        ),
        community_transmission = factor(
            community_transmission,
            levels = c("unknown", "low", "moderate", "high", "substantial"),
            ordered = TRUE
        ),
        smoker = factor(
            smoker,
            levels = c("cat_N", "cat_P", "cat_Y", "cat_U"),
            ordered = TRUE
        )
    )

## Now we can make a collapsed shapviz object ----
shv <- shapviz::shapviz(
    object = shap_explainer$shapley_values,
    X = feat_values,
    baseline = shap_explainer$baseline,
    collapse = collapse_list
)

mean_abs_shap <- shv$S |>
    tibble::as_tibble() |>
    dplyr::summarize_all(function(x) {
        mean(abs(x))
    }) |>
    tidyr::pivot_longer(
        cols = dplyr::everything(),
        values_to = "mean_abs_shap",
        names_to = "column_name"
    ) |>
    dplyr::slice_max(mean_abs_shap, n = 15) |>
    dplyr::mutate(
        label = dplyr::case_when(
            column_name == "pneumonia" ~ "Pneuomnia contributing",
            column_name == "other_resp" ~ "ORI contributing",
            column_name == "month_into_pandemic" ~ "Time",
            column_name == "month" ~ "Calendar month",
            column_name == "community_transmission" ~ "County transmission level",
            column_name == "smoker" ~ "Tobacco use",
            column_name == "nonhispanic" ~ "Non-Hispanic ethnicity",
            column_name == "age_years" ~ "Age (years)",
            column_name == "copds" ~ "COPD contributing",
            column_name == "state" ~ "State of residence",
            column_name == "sex" ~ "Male Sex",
            column_name == "perc_total_pop_vaccinated" ~ "County vaccination rate",
            column_name == "race" ~ "Race",
            column_name == "birth_state" ~ "State of birth",
            column_name == "division" ~ "Census division"
        )
    ) |>
    dplyr::mutate(label = sprintf("%s (%0.3f)", label, round(mean_abs_shap, 3))) |>
    dplyr::arrange(mean_abs_shap)

mean_abs_shap$label <- factor(mean_abs_shap$label,
    levels = mean_abs_shap$label,
    ordered = TRUE
)

## General feature importance ----
set.seed(1)
shv_sub <- shv[sample(1:NROW(feat_values), 1000), ]

p1 <- shapviz::sv_importance(
    shv_sub,
    kind = "beeswarm",
    size = .5,
    alpha = .8,
    max_display = 15
) +
    mk_nytimes(legend.position = "bottom") +
    ggplot2::scale_color_viridis_c(
        "Feature value",
        option = "A",
        begin = .1,
        end = .8,
        breaks = c(0, 1),
        labels = c("Low (No)", "High (Yes)"),
        guide = ggplot2::guide_colorbar(
            barwidth = ggplot2::unit(10, "cm"),
            barheight = ggplot2::unit(.25, "cm"),
            title.position = "top"
        )
    ) +
    ggplot2::scale_y_discrete("Feature\n(Mean absolute SHAP value)",
        labels = mean_abs_shap$label
    )

## Save ----
ggplot2::ggsave(
    here::here("plots", "figS3_shap_plot.pdf"),
    p1,
    width = 8,
    height = 5,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS3_shap_plot.jpg"),
    p1,
    width = 8,
    height = 5,
    scale = 1,
    dpi = 1200
)
