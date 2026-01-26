## 10_generate_fair_predictions.R ----
##
## We want to be able to use predictions for each observation based on a model
## that was fit *without* that observation (ie fair predictions). Here, we
## just use k-folds to fit the top models on the data and generate the fair
## predictions, which we can then use for fairness metrics later.

## Set up environment ----
# Sys.setenv(R_CONFIG_ACTIVE = "prototyping")
# Sys.setenv(R_CONFIG_ACTIVE = "debug")
source(here::here("code", "04_setup_environment.R"))
library(furrr)
library(future)
library(probably)
library(fs)

## Options ----
options("future.globals.maxSize" = 8 * 1024^3)

## Data ----
analytic_df <- readRDS(here::here("data_private", "analytic_data.RDS"))

## CONSTANTS -----
MODEL_DIR <- here::here("output", "models")
K_FOLDS <- config::get("k_fold_fair")
TOP_MODELS <- config::get("top_models")[1]
N_CORES <- 10
BEST_FOLD <- as.numeric(substr(TOP_MODELS, nchar(TOP_MODELS), nchar(TOP_MODELS)))
TOP_MODELS <- substr(TOP_MODELS, 1, nchar(TOP_MODELS) - 2)

## K-fold splits of in-hospital deaths ----
## To evaluate in-hospital fairness metrics we need a prediction for each
## observation but we also want this prediction to come from a model that
## did not use that observation as part of its training. Below we just do
## a standard K-folds split of the data and then predict on each fold,
## retaining the final predicted probabilities for each observation.
if (!fs::file_exists(here::here("data_private", "split_inhosp_predictions.RDS"))) {
    ## Get the best model ----
    ### Get paths ----
    wf_path <- fs::dir_ls(
        MODEL_DIR,
        recurse = TRUE,
        regexp = paste0(TOP_MODELS, "_full-data_final_workflow.RDS")
    )

    ## Pick best model across the 5 folds
    wf_path <- wf_path[grepl(sprintf("fold%i", BEST_FOLD), wf_path)]

    parsed <- parse_file_path(wf_path)
    all_f_paths <- return_file_paths(
        here::here(MODEL_DIR, parsed$save_dir, parsed$fold_dir),
        parsed$regression_stub,
        parsed$tuning_method,
        parsed$recipe_stub,
        parsed$data_prop
    )

    ### Read in the finalized workflow ----
    final_wf <- readRDS(all_f_paths$final_workflow)

    ### Spilt *all* in-hospital deaths for final calibration model ----
    fair_splits <- rsample::vfold_cv(analytic_df, v = K_FOLDS)

    ### Map the predict_splits() function (in utils.R) across folds ----
    future::plan(future::multisession, workers = N_CORES)
    fair_splits$results <- furrr::future_map(
        fair_splits$splits,
        predict_splits,
        final_wf,
        .options = furrr::furrr_options(packages = REQ_PACKAGES)
    )

    ### Close out ----
    doParallel::stopImplicitCluster()
    closeAllConnections()
    unregister_dopar()
    future::plan(future::sequential)

    ### Combine the predicted probabilities ----
    preds_df <- fair_splits$results |>
        purrr::pluck() |>
        dplyr::bind_rows()

    ### Save ----
    saveRDS_xz(
        preds_df,
        here::here("data_private", "split_inhosp_predictions.RDS")
    )
}
