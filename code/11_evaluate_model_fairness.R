## 11_evaluate_model_fairness.R ----
##
## Based on the fair predictions we created before, we'll assess model fairness
## using a variety of metrics and across subgroups of interest.

## Imports ----
library(tidyverse)
library(here)
library(fairness)
source(here::here("code", "utils.R"))

## Data ----
analytic_df <- readRDS(here::here("data_private", "analytic_data.RDS"))
preds_df <- readRDS(here::here("data_private", "split_inhosp_predictions.RDS"))

## Fairness helpers ----
reshape_fairness_object <- function(fairness_obj) {
    tibble::tibble(
        group_name = colnames(fairness_obj$Metric),
        metric_name = rownames(fairness_obj$Metric)[1],
        parity_name = rownames(fairness_obj$Metric)[2],
        metric_value = fairness_obj$Metric[1, ],
        parity_value = fairness_obj$Metric[2, ],
        group_size = fairness_obj$Metric[3, ]
    )
}

evaluate_fairness <- function(prediction_df,
                              analytic_df,
                              grouping_column,
                              grouping_base,
                              return_original = FALSE) {
    sub_df <- prediction_df |>
        dplyr::left_join(
            analytic_df |>
                dplyr::select(tidyr::all_of(c("row_id", grouping_column))),
            by = "row_id"
        ) |>
        as.data.frame()

    ## Factors need to be unordered
    sub_df[[grouping_column]] <- factor(sub_df[[grouping_column]],
        ordered = FALSE
    )

    ### Predictive rate parity ----
    ### Compare model precision across groups using "White" as a reference. Closer
    ### to 1 is more fair and over 1 indicates the model performs better, while
    ### under 1 indicates the model performs worse.

    pred_rate_par <- fairness::pred_rate_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### Demographic parity ----
    ### Just calculate this because it's so common but this makes no sense in
    ### our context with different group sizes and different prevalences.
    dem_par <- fairness::dem_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### Proportional parity ----
    ### Proportion of positive predictions in each subgroup should be close to
    ### each other.
    prop_par <- fairness::prop_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### Equalized odds ----
    ### How close are the sensitivities to each other?
    eq_odds_par <- fairness::equal_odds(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### Accuracy parity ----
    ### How close are the accuracies to each other?
    acc_par <- fairness::acc_parity(
        data = sub_df,
        outcome = "has_covid",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### False negative rate parity ----
    ### How close are the false negative rates to each other?
    fnr_par <- fairness::fnr_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### False positive rate parity ----
    ### How close are the false positive rates to each other?
    fpr_par <- fairness::fpr_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### NPV parity ----
    ### How close are the negative predictive values to each other?
    npv_par <- fairness::npv_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### Specificity parity ----
    ### How close are the specificities to each other?
    spec_par <- fairness::spec_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    ### ROC AUC parity ----
    ### How close are the ROC AUCs to each other?
    roc_par <- fairness::roc_parity(
        data = sub_df,
        outcome = "has_covid",
        probs = ".pred_no_covid",
        group = grouping_column,
        base = grouping_base
    )

    ### MCC parity ----
    ### How close are the Matthews correlation coefficients to each other?
    mcc_par <- fairness::mcc_parity(
        data = sub_df,
        outcome = "has_covid",
        outcome_base = "0",
        probs = ".pred_no_covid",
        cutoff = 0.5,
        group = grouping_column,
        base = grouping_base
    )

    if (return_original) {
        list(
            mcc_par = mcc_par,
            roc_par = roc_par,
            spec_par = spec_par,
            npv_par = npv_par,
            fpr_par = fpr_par,
            fnr_par = fnr_par,
            acc_par = acc_par,
            eq_odds_par = eq_odds_par,
            prop_par = prop_par,
            dem_par = dem_par,
            pred_rate_par = pred_rate_par
        )
    } else {
        dplyr::bind_rows(
            reshape_fairness_object(mcc_par),
            reshape_fairness_object(roc_par),
            reshape_fairness_object(spec_par),
            reshape_fairness_object(npv_par),
            reshape_fairness_object(fpr_par),
            reshape_fairness_object(fnr_par),
            reshape_fairness_object(acc_par),
            reshape_fairness_object(eq_odds_par),
            reshape_fairness_object(prop_par),
            reshape_fairness_object(dem_par),
            reshape_fairness_object(pred_rate_par)
        ) |>
            dplyr::mutate(
                grouping = grouping_column,
                .before = 1
            ) |>
            tibble::as_tibble()
    }
}

grouped_pr_curve <- function(prediction_df,
                             analytic_df,
                             grouping_column) {
    sub_df <- prediction_df |>
        dplyr::left_join(
            analytic_df |>
                dplyr::select(tidyr::all_of(c("row_id", grouping_column))),
            by = "row_id"
        ) |>
        as.data.frame()
    
    groups <- sort(unique(sub_df[, grouping_column]))
    
    holder <- vector("list", NROW(groups))
    pr_auc <- vector("list", NROW(groups))
    
    for (g in groups) {
        temp_x <- sub_df[sub_df[, grouping_column] == g, ]
        holder[[g]] <- pr_curve(temp_x, truth = has_covid_cat, .pred_has_covid) |> 
            mutate(grouping = grouping_column, 
                   group = g)
        
        pr_auc[[g]] <- tibble(
            grouping = grouping_column,
            group = g,
            group_pr_auc = pr_auc(temp_x, truth = has_covid_cat, .pred_has_covid)$.estimate
        )
    }
    
    list(holder = bind_rows(holder),
         pr_auc = bind_rows(pr_auc))
}

## Calculate fairness metrics ----
## Category / reference group (largest group)
##      By race: racerbig_cat / white
##      By ethnicity: is_nonhispanic_cat / nonhispanic
##      By state: state_cat / CA
##      By urbanicity: rucc_2013_cat / 1
##      By sex: sex_cat / "F"
##      By education: educ_cat / "doctorate"
fairness_df <- dplyr::bind_rows(
    evaluate_fairness(preds_df,
        analytic_df,
        grouping_column = "racerbig_cat",
        grouping_base = "white"
    ),
    evaluate_fairness(preds_df,
        analytic_df,
        grouping_column = "is_nonhispanic_cat",
        grouping_base = "nonhispanic"
    ),
    evaluate_fairness(preds_df,
        analytic_df,
        grouping_column = "state_cat",
        grouping_base = "CA"
    ),
    evaluate_fairness(preds_df,
        analytic_df,
        grouping_column = "rucc_2013_cat",
        grouping_base = "1"
    ),
    evaluate_fairness(preds_df,
        analytic_df,
        grouping_column = "sex_cat",
        grouping_base = "M"
    ),
    evaluate_fairness(preds_df,
        analytic_df,
        grouping_column = "educ_cat",
        grouping_base = "hs_ged"
    )
)

## Save ----
saveRDS(fairness_df,
    here::here("data", "fairness_metrics.RDS"),
    compress = "xz"
)

## Reviewer asked for precision-recall plots by race/ethnicity
pr_df <- grouped_pr_curve(preds_df, analytic_df, "racerbig_cat")
saveRDS(pr_df,
        here::here("data", "pr_curves_grouped.RDS"),
        compress = "xz")
