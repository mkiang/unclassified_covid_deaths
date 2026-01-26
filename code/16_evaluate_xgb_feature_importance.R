## 16_evaluate_xgb_feature_importance.R ----
##
## To understand how our model is making these predictions, we use SHAP values.
## Here, we calculate the SHAP values using an approximation (given the
## sample size, exact calculation would be intractable) on a random subset of
## the data.

## Note about number of simulations:
## When doing the shap approximation, we need to balance how many simulations
## we run (therefore getting more accurate estimates for each column)
## against the size of the data (which has higher mem and time requirements).
## We want something that is computationally feasible while still being
## accurate. After a bit of testing, we settled on running 1000 simulations
## on a subsample of 100,000 observations (randomly selected without
## replacement).
##
## WARNING: This takes ~1200 cpu-hours to complete in our compute environment.

## Set up environment ----
library(here)
source(here::here("code", "04_setup_environment.R"))
library(fastshap)
library(tictoc)

## Data ----
train_df <- readRDS(here::here("data_private", "analytic_data.RDS"))

## CONSTANTS -----
N_CORES <- 16 # number of cores to use for SHAP explainer
N_SIMS <- 1000 # number of monte carlo simulations for SHAP explainer
N_SAMPLE <- 100000 # number of observations to sample for SHAP explainer
FORCE_REFRESH <- FALSE
MODEL_DIR <- here::here("output", "models")
TOP_MODELS <- "xgboost_bayes_tune_nocountynoucr-2" # code is specific to XGB
K_FOLDS <- as.numeric(stringr::str_extract(TOP_MODELS, "([a-z]{1})\\-([1-5]{1})", 2))
TOP_MODELS <- substr(TOP_MODELS, 1, nchar(TOP_MODELS) - 2)

save_path <- here::here(
    "data",
    sprintf(
        "shap_values_%s_nsims%s_nobs%sk.RDS",
        TOP_MODELS,
        N_SIMS,
        round(N_SAMPLE / 1000)
    )
)

f_path <- fs::dir_ls(MODEL_DIR,
    recurse = TRUE,
    regexp = sprintf(
        "fold%s\\/%s\\_%s",
        K_FOLDS,
        TOP_MODELS,
        "full-data_final_model.RDS"
    )
)

parsed_f_path <- parse_file_path(f_path)
all_f_paths <- return_file_paths(
    parsed_f_path$orig_dirname,
    parsed_f_path$regression_stub,
    parsed_f_path$tuning_method,
    parsed_f_path$recipe_stub,
    parsed_f_path$data_prop
)

## Helper function to calculate approximate shapley values ----
### Need a prediction wrapper that returns class probabilities (of COVID) ----
pred_func <- function(object, newdata) {
    predict(object, newdata = newdata, type = "prob")
}

## Run shap value estimates only if we don't already have them ----
if (!fs::file_exists(save_path) | FORCE_REFRESH) {
    ## Read in the finalized workflow and model
    final_fit <- readRDS(all_f_paths$final_fitted_model)
    fit_engine <- parsnip::extract_fit_engine(final_fit)

    ### Need processed data ----
    ### Needs to be a matrix for xgboost to refit. Note that we take a subsample
    ### because monte carlo'ing 1.58M observations is computationally prohibitive.
    proc_df <- final_fit |>
        workflowsets::extract_recipe() |>
        recipes::bake(new_data = train_df) |>
        dplyr::select(-has_covid_cat)

    set.seed(112233)
    sub_proc_df <- proc_df |>
        dplyr::slice_sample(n = N_SAMPLE)
    set.seed(NULL)

    ### Run the explainer on the random subsample ----
    tictoc::tic()
    doParallel::registerDoParallel(N_CORES)
    shap_explainer_subsample <- fastshap::explain(
        object = fit_engine,
        X = as.matrix(proc_df),
        pred_wrapper = pred_func,
        nsim = N_SIMS,
        adjust = TRUE,
        shap_only = FALSE,
        parallel = TRUE,
        newdata = as.matrix(sub_proc_df),
        exact = FALSE
    )
    shap_timer_subsample <- tictoc::toc(quiet = TRUE)

    ### Close out ----
    doParallel::stopImplicitCluster()
    closeAllConnections()
    unregister_dopar()

    ## Save ----
    ## We set a seed so in theory we can reproduce the subsample if needed
    ## later, but I don't think seeds are guaranteed to be be consistent
    ## across platforms. We save the subsample just in case.
    saveRDS_xz(
        list(
            timer = shap_timer_subsample,
            shap_vals = shap_explainer_subsample,
            sub_df = sub_proc_df
        ),
        save_path
    )
}
