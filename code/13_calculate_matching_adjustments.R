## 13_calculate_matching_adjustments.R ----
##
## When applying our classifier to the out-of-hospital deaths, we can take
## into account the misclassification characteristics of our classifier by
## accounting for the false discovery and false omission rates. Under the very
## strong assumption that the in-hospital FOR and FDR are identical to the
## out-of-hospital FOR/FDR, we can adjust our estimates by randomly drawing
## from binomial distributions for each and flipping the classification. Again,
## note that this fundamentally assumes the model performance is identical in
## both both populations and is likely not an assumption that would hold.
##
## To test this assumption, we do it naively, then we do it after subsetting
## the in-hospital death to just those most representative of out-of-hospital
## deaths (using a variety of methods).

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(MatchIt)
source(here::here("code", "utils.R"))

## Data ----
preds_df <- readRDS(here::here("data_private", "split_inhosp_predictions.RDS"))
combined_df <- dplyr::bind_rows(
    readRDS(here::here("data_private", "analytic_data.RDS")) |>
        dplyr::mutate(out_of_hospital = 0),
    readRDS(here::here("data_private", "analytic_prediction_data.RDS")) |>
        dplyr::mutate(out_of_hospital = 1)
)
propensity_scores <- readRDS(here::here("data_private", "propensity_scores.RDS"))

combined_df <- combined_df |>
    dplyr::left_join(propensity_scores)

## Match on individual-level data ----
## We want to find the subset of in-hospital deaths that are most closely
## representative of the set of out-of-hospital deaths so we can calculate
## the FDR and FOR in this subset of in-hospital deaths while accounting for
## distributional differences by individual characteristics for who dies in
## the hospital. Below we try a few different matching methods. Note that in
## this context, out-of-hospital deaths are the "treated" group and in-hospital
## deaths are the "control" groups so that (nearly?) all out-of-hospital deaths
## should be matched to an in-hospital death but not necessarily true all
## in-hospital deaths will be matched. We want to focus on the subset of in-
## hospital deaths that are matched.

### No caliper but constrained to exact match ----
## With constraints for exact match on sex, state of occurrence, calendar
## month, race, ethnicity, and day of week.
if (!fs::file_exists(here::here("data_private", "matched_object_exact.RDS"))) {
    matched_exact <- MatchIt::matchit(
        out_of_hospital ~ .pred_out_of_hospital,
        distance = combined_df$.pred_out_of_hospital,
        method = "nearest",
        exact = ~ sex_cat + state_cat + month_cat + weekday_cat + year +
            is_nonhispanic_cat + racerbig_cat,
        data = combined_df,
        verbose = TRUE,
        replace = TRUE
    )

    ## Save the (very large) MatchIt object
    saveRDS_xz(
        matched_exact,
        here::here("data_private", "matched_object_exact.RDS")
    )

    ## Save just the summary (take a while to process)
    s_exact <- MatchIt:::summary.matchit(matched_exact)
    saveRDS_xz(
        s_exact,
        here::here("data", "matched_summary_exact.RDS")
    )

    ## Save just the row_ids of matched in-hospital deaths
    d_exact <- MatchIt::match.data(matched_exact) |>
        tibble::as_tibble() |>
        dplyr::filter(out_of_hospital == 0) |>
        dplyr::select(1:19, 165:170)
    saveRDS_xz(
        d_exact,
        here::here("data_private", "matched_data_exact.RDS")
    )
} else {
    s_exact <- readRDS(here::here("data", "matched_summary_exact.RDS"))
    d_exact <- readRDS(here::here("data_private", "matched_data_exact.RDS"))
}

### Standard caliper and constrained to exact match ----
if (!fs::file_exists(here::here("data_private", "matched_object_caliper_exact.RDS"))) {
    matched_caliper_exact <- MatchIt::matchit(
        out_of_hospital ~ .pred_out_of_hospital,
        distance = combined_df$.pred_out_of_hospital,
        method = "nearest",
        data = combined_df,
        exact = ~ sex_cat + state_cat + month_cat + weekday_cat + year +
            is_nonhispanic_cat + racerbig_cat,
        caliper = .15,
        std.caliper = TRUE,
        verbose = TRUE,
        replace = TRUE
    )

    ## Save the whole MatchIt object
    saveRDS_xz(
        matched_caliper_exact,
        here::here("data_private", "matched_object_caliper_exact.RDS")
    )

    ## Save just the summary (take a while to process)
    s_caliper_exact <- MatchIt:::summary.matchit(matched_caliper_exact)
    saveRDS_xz(
        s_caliper_exact,
        here::here("data", "matched_summary_caliper_exact.RDS")
    )

    ## Save just the row_ids of matched in-hospital deaths
    d_caliper_exact <- MatchIt::match.data(matched_caliper_exact) |>
        tibble::as_tibble() |>
        dplyr::filter(out_of_hospital == 0) |>
        dplyr::select(1:19, 165:170)
    saveRDS_xz(
        d_caliper_exact,
        here::here("data_private", "matched_data_caliper_exact.RDS")
    )
} else {
    s_caliper_exact <- readRDS(here::here("data", "matched_summary_caliper_exact.RDS"))
    d_caliper_exact <- readRDS(here::here("data_private", "matched_data_caliper_exact.RDS"))
}

## Now get misclassification metrics ----
if (!fs::file_exists(here::here("data", "misclassification_metrics_df.RDS"))) {
    ## Calculate inverse-odds weights
    propensity_scores <- propensity_scores |>
        dplyr::mutate(invodds_weight = (1 - .pred_in_hospital) / .pred_in_hospital)

    ## Join up all the weights
    preds_df <- preds_df |>
        dplyr::left_join(
            d_caliper_exact |>
                dplyr::select(row_id, caliper_weights = weights)
        ) |>
        dplyr::left_join(
            d_exact |>
                dplyr::select(row_id, exact_weights = weights)
        ) |>
        dplyr::left_join(
            propensity_scores |>
                dplyr::select(row_id, invodds_weight)
        )

    ## Clean up
    rm(combined_df, propensity_scores)

    ## Calculate the FDR and FOR under different adjustments
    misclass_df <- dplyr::bind_rows(
        get_misclassification_metrics(preds_df) |>
            dplyr::mutate(adjustment = "no_pruning"),
        get_misclassification_metrics(preds_df, case_weights = invodds_weight) |>
            dplyr::mutate(adjustment = "inverse_odds_weighted"),
        get_misclassification_metrics(preds_df |>
            dplyr::filter(!is.na(exact_weights))) |>
            dplyr::mutate(adjustment = "exact_matching"),
        get_misclassification_metrics(preds_df |>
            dplyr::filter(!is.na(caliper_weights))) |>
            dplyr::mutate(adjustment = "exact_with_caliper")
    )

    misclass_df <- misclass_df |>
        dplyr::left_join(
            dplyr::bind_rows(
                tibble::tibble(
                    n_matched_control = s_caliper_exact$nn[4, 1],
                    n_matched_treated = s_caliper_exact$nn[4, 2],
                    n_unmatched_control = s_caliper_exact$nn[5, 1],
                    n_unmatched_treated = s_caliper_exact$nn[5, 2],
                    n_discarded_control = s_caliper_exact$nn[6, 1],
                    n_discarded_treated = s_caliper_exact$nn[6, 2]
                ) |>
                    dplyr::mutate(adjustment = "exact_with_caliper"),
                tibble::tibble(
                    n_matched_control = s_exact$nn[4, 1],
                    n_matched_treated = s_exact$nn[4, 2],
                    n_unmatched_control = s_exact$nn[5, 1],
                    n_unmatched_treated = s_exact$nn[5, 2],
                    n_discarded_control = s_exact$nn[6, 1],
                    n_discarded_treated = s_exact$nn[6, 2]
                ) |>
                    dplyr::mutate(adjustment = "exact_matching")
            )
        ) |>
        dplyr::arrange(dplyr::desc(adjustment), metric)

    ### Save ----
    saveRDS(
        misclass_df,
        here::here("data", "misclassification_metrics_df.RDS"),
        compress = "xz"
    )
}
