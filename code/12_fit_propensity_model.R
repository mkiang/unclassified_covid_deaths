## 12_fit_propensity_model.R ----

## We want some sense of how our models would perform on out-of-hospital data
## under the scenario of covariate shift. One way to do that is to fit a
## propensity score model on *all* data where the outcome of interest is
## whether or not that observation was an in- or out-of-hospital death. Once
## you fit the propensity score model, you get the predicted probability of
## an in-hospital death for all observations (i.e., a propensity score for
## the death being an in-hospital death). With this propensity score, we can
## now calculate the ROC AUC, weighted by the inverse-odds of a death being
## an in-hospital death. The intuition here is the same as inverse probability
## of treatment weighting where we are upweighing observations that look more
## like out-of-hospital deaths and downweighing observations that look more
## like in-hospital deaths.
##
## See https://pmc.ncbi.nlm.nih.gov/articles/PMC10188769/ for details.

## Imports ----
library(tidyverse)
library(tidymodels)
library(embed)
library(foreach)
library(doParallel)
library(parallel)
library(future)
library(here)
library(fs)
library(bonsai)
library(lightgbm)
source(here::here("code", "utils.R"))

## Settings ----
lightgbm::setLGBMthreads(future::availableCores())

## CONSTANTS ----
N_CORES <- 4
BAYES_ITER <- config::get("tune_bayes_iter")
BAYES_STOP <- config::get("tune_bayes_stop")
M_SET <- yardstick::metric_set(
    yardstick::roc_auc,
    yardstick::accuracy,
    yardstick::sens,
    yardstick::spec,
    yardstick::f_meas,
    yardstick::j_index,
    yardstick::mcc,
    yardstick::npv,
    yardstick::ppv,
    yardstick::kap,
    yardstick::average_precision,
    yardstick::bal_accuracy,
    yardstick::mn_log_loss,
    yardstick::brier_class,
    yardstick::gain_capture,
    yardstick::pr_auc,
    yardstick::detection_prevalence
)

## Data ----
combined_df <- dplyr::bind_rows(
    readRDS(here::here("data_private", "analytic_data.RDS")) |>
        dplyr::mutate(in_hospital_num = 1),
    readRDS(here::here("data_private", "analytic_prediction_data.RDS")) |>
        dplyr::mutate(in_hospital_num = 0)
) |>
    dplyr::mutate(in_hospital = factor(in_hospital_num,
        levels = c(1, 0),
        labels = c("in_hospital", "out_of_hospital"),
        ordered = TRUE
    )) |>
    dplyr::select(-in_hospital_num)

## Fit a propensity score model using lightgbm ----
if (!fs::file_exists(here::here("data_private", "propensity_score_model_objects.RDS"))) {
    ### Recipe ----
    ## Given just data from the death certificate, how well can we distinguish
    ## between in-hospital and out-of-hospital deaths?
    propensity_recipe <- recipes::recipe(in_hospital ~ .,
        data = combined_df
    ) |>
        recipes::step_rm(tidyr::any_of(c("row_id"))) |>
        recipes::step_rm(tidyr::any_of(c("county_fips_cat"))) |>
        recipes::step_rm(tidyr::starts_with(c("has_covid"))) |>
        recipes::step_rm(tidyr::starts_with(c("ucr113_"))) |>
        recipes::step_rm(tidyr::starts_with(c("copds_"))) |>
        recipes::step_rm(tidyr::starts_with(c("hypertensive_"))) |>
        recipes::step_rm(tidyr::any_of(c("hyperlipidemia_e785_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("ischemic_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("kidney_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("other_resp_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("pneumonia_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("t2d_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("tobacco_use_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("poor_or_fair_health"))) |>
        recipes::step_rm(tidyr::any_of(c("smoking"))) |>
        recipes::step_rm(tidyr::any_of(c("obesity"))) |>
        recipes::step_rm(tidyr::any_of(c("some_college"))) |>
        recipes::step_rm(tidyr::any_of(c("diabetes"))) |>
        recipes::step_rm(tidyr::any_of(c("household_income"))) |>
        recipes::step_rm(tidyr::any_of(c("income_inequality"))) |>
        recipes::step_rm(tidyr::any_of(c("home_ownership"))) |>
        recipes::step_rm(tidyr::any_of(c("percent_65_over"))) |>
        recipes::step_rm(tidyr::any_of(c("percent_black"))) |>
        recipes::step_rm(tidyr::any_of(c("percent_hispanic"))) |>
        recipes::step_rm(tidyr::any_of(c("percent_white"))) |>
        recipes::step_rm(tidyr::any_of(c("rucc_2013_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("comm_trans_level_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("month_into_pandemic"))) |>
        recipes::step_rm(tidyr::any_of(c("perc_total_pop_vaccinated"))) |>
        recipes::step_rm(tidyr::any_of(c("perc_65up_pop_vaccinated"))) |>
        recipes::step_impute_mean(recipes::all_numeric_predictors()) |>
        recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)

    ### Divide up the data ----
    ## 60-20-20 train-validate-test split
    initial_split <- rsample::initial_validation_split(combined_df, prop = c(.6, .2))
    test_df <- rsample::testing(initial_split)
    train_df <- rsample::training(initial_split)
    valid_rset <- rsample::validation_set(initial_split)

    ### Make modeling spec ----
    mod_engine <- parsnip::boost_tree(
        mtry = tune::tune(),
        min_n = tune::tune(),
        trees = 1000,
        tree_depth = tune::tune(),
        learn_rate = tune::tune(),
        loss_reduction = tune::tune()
    ) %>%
        parsnip::set_engine("lightgbm",
            num_threads = future::availableCores()
        ) %>%
        parsnip::set_mode("classification")

    ### Create new workflow ----
    empty_workflow <- workflows::workflow() %>%
        workflows::add_recipe(propensity_recipe) %>%
        workflows::add_model(mod_engine)

    ### Need to finalize because mtry() depends on number of columns
    ### (which are not known until we get to this point)
    updated_params <- empty_workflow |>
        workflows::extract_parameter_set_dials() |>
        dials::finalize(train_df)

    tuned_workflow <- empty_workflow |>
        tune::tune_bayes(
            resamples = valid_rset,
            initial = 10,
            param_info = updated_params,
            iter = BAYES_ITER,
            metrics = M_SET,
            control = tune::control_bayes(
                no_improve = BAYES_STOP,
                uncertain = 7,
                verbose = TRUE,
                verbose_iter = TRUE,
                save_pred = FALSE,
                allow_par = TRUE,
                parallel_over = NULL,
                save_gp_scoring = FALSE,
                save_workflow = FALSE
            )
        )

    ### Select best model
    best_auc_params <- tuned_workflow |>
        tune::select_best(metric = "roc_auc")

    ### Finalize the workflow with hyperparameters from best model
    finalized_workflow <- empty_workflow |>
        tune::finalize_workflow(best_auc_params)

    ### Refit one more time
    finalized_fit <- finalized_workflow |>
        tune::last_fit(
            split = initial_split,
            metrics = M_SET,
            control = tune::control_last_fit(
                verbose = TRUE,
                allow_par = TRUE
            )
        )

    ### Get finalized metrics
    final_metrics <- finalized_fit |>
        tune::collect_metrics()

    ### Save ----
    saveRDS_xz(
        list(
            finalized_workflow = finalized_workflow,
            finalized_fit = finalized_fit,
            final_metrics = final_metrics
        ),
        here::here("data_private", "propensity_score_model_objects.RDS")
    )
}

## Extract propensity scores from final model using 5-fold CV ----
if (!fs::file_exists(here::here("data_private", "propensity_scores.RDS"))) {
    ### Get in the fitted model objects ----
    finalized_objects <- readRDS(here::here("data_private", "propensity_score_model_objects.RDS"))
    finalized_workflow <- finalized_objects$finalized_workflow
    rm(finalized_objects)

    ### Create new k-folds rset and get predictions for each fold
    fold5 <- rsample::vfold_cv(combined_df, v = 5)
    refit_predictions <- finalized_workflow |>
        tune::fit_resamples(
            resamples = fold5,
            metrics = M_SET,
            control = tune::control_resamples(
                verbose = TRUE,
                allow_par = TRUE,
                save_pred = TRUE,
            )
        ) |>
        workflowsets::collect_predictions()

    ### Combine propensity scores with their row_id so we can merge later
    propensity_scores <- vector("list", 5)
    for (k in 1:5) {
        propensity_scores[[k]] <- dplyr::bind_cols(
            rsample::assessment(rsample::get_rsplit(fold5, k)) |>
                dplyr::select(row_id),
            refit_predictions |>
                dplyr::filter(id == sprintf("Fold%i", k)) |>
                dplyr::arrange(.row) |>
                dplyr::select(-id, -.config, -.row)
        )
    }

    ### Save
    saveRDS_xz(
        dplyr::bind_rows(propensity_scores) |>
            dplyr::arrange(row_id),
        here::here("data_private", "propensity_scores.RDS")
    )
}
