## Imports ----
library(here)
library(tidyverse)
library(tidymodels)
library(fs)
library(future)
library(furrr)
source(here("code", "utils.R"))

## Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" &&
    !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Constants ----
N_CORES <- 8

if (!file_exists(here("data", "performance_curves.RDS"))) {
    ## Get all predictions across all models ----
    model_paths <- dir_ls(here("output", "models"),
        recurse = TRUE,
        regexp = "_full-data_final_model.RDS\\>")
    model_paths <- model_paths[grepl("fold2", model_paths)]

    future::plan(future::multisession(workers = N_CORES))
    pred_df <- furrr::future_map_dfr(
        .x = model_paths,
        .f = ~ {
            readRDS(.x) |>
                collect_predictions() |>
                mutate(model = parse_file_path(.x)$orig_basename)
        }
    )

    ### Close out ----
    closeAllConnections()
    doParallel::stopImplicitCluster()
    unregister_dopar()

    ### Indicator column for different models ----
    pred_df <- pred_df |>
        mutate(pref_model = case_when(
            grepl("xgboost_bayes_tune_nocountynoucr", model) ~ "main model",
            grepl("logistic_elasticnet", model) ~ "reference model",
            TRUE ~ "other model"))

    ### Calculate different performance curves ----
    performance_curvs <- pred_df |>
        nest(.by = c(model, pref_model)) |>
        mutate(
            roc_curv = lapply(data, function(x)
                roc_curve(x, .pred_has_covid, truth = has_covid_cat)),
            roc_auc_val = lapply(data, function(x)
                roc_auc(x, .pred_has_covid, truth = has_covid_cat)),
            lift_curv = lapply(data, function(x)
                lift_curve(x, .pred_has_covid, truth = has_covid_cat)),
            gain_curv = lapply(data, function(x)
                gain_curve(x, .pred_has_covid, truth = has_covid_cat)),
            gain_auc_val = lapply(data, function(x)
                gain_capture(x, .pred_has_covid, truth = has_covid_cat)),
            pr_curv = lapply(data, function(x)
                pr_curve(x, .pred_has_covid, truth = has_covid_cat)),
            pr_auc_val = lapply(data, function(x)
                pr_auc(x, .pred_has_covid, truth = has_covid_cat)),
        ) |>
        select(-data)

    ### Save ----
    saveRDS_xz(performance_curvs,
               here("data", "performance_curves.RDS"))
} 
