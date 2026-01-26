## 14_bootstrap_out_of_hospital_predictions.R ----
##
## To get uncertainty intervals, we'll bootstrap the out-of-hospital
## predictions. Because this takes a while and storage space is cheap, we'll
## do it in parallel and save intermediate files for each bootstrap and then
## summarize them later.

## Set up environment ----
# Sys.setenv(R_CONFIG_ACTIVE = "prototyping")
# Sys.setenv(R_CONFIG_ACTIVE = "debug")
source(here::here("code", "04_setup_environment.R"))
library(probably)
library(tictoc)
N_CORES <- 16
MODEL_DIR <- here::here("output", "models")

### Extract model specifications
### In the config.yml file, best models are specified as [model]-[fold] so here
### we parse out the model and folds into two vectors.
TOP_MODELS <- config::get("top_models")
K_FOLDS <- as.numeric(stringr::str_extract(TOP_MODELS, "([a-z]{1})\\-([1-5]{1})", 2))
TOP_MODELS <- substr(TOP_MODELS, 1, nchar(TOP_MODELS) - 2)

## Get data ----
pred_df <- readRDS(here::here("data_private", "analytic_prediction_data.RDS"))
analytic_df <- readRDS(here::here("data_private", "analytic_data.RDS"))

## Clear old things ----
rm(analytic_split)
gc()

## Predictions based on each of the "best models" specified in config.yml ----
for (model_x in TOP_MODELS) {
    BEST_FOLD <- K_FOLDS[which(TOP_MODELS == model_x)]

    ### Create a grid to iterate over ----
    ## Each bootstrap takes 1.5-2.5h so we want to save each one individually.
    ## NOTE: batch == 0 and boot == 0 is the full-training-data prediction (i.e.,
    ## using all in-hospital deaths with no resampling).
    boot_grid <- expand.grid(
        batch = 1:50,
        boot = 1:100,
        stringsAsFactors = FALSE
    ) |>
        tibble::add_case(batch = 0, boot = 0) |>
        dplyr::arrange(batch, boot)

    # For the sensitivity analyses, just run 2000 bootstraps
    if (model_x != TOP_MODELS[1]) {
        boot_grid <- boot_grid |>
            dplyr::filter(
                batch <= 20,
                boot <= 100
            )
    }

    ### Read in files ----
    wf_path <- fs::dir_ls(
        MODEL_DIR,
        recurse = TRUE,
        regexp = sprintf("fold%s\\/%s\\_%s", BEST_FOLD, model_x, "full-data_final_workflow.RDS")
    )

    parsed <- parse_file_path(wf_path)
    final_wf <- readRDS(wf_path)

    ### Remove files we've already done ----
    boot_grid$f_path <- here::here(
        gsub("models", "out_of_hosp", parsed$orig_dirname),
        parsed$recipe_stub,
        sprintf("batch%03d", boot_grid$batch),
        sprintf(
            "out_of_hospital_%s_%s_%s_batch%03d_boot%03d.RDS",
            parsed$regression_stub,
            parsed$tuning_method,
            parsed$recipe_stub,
            boot_grid$batch,
            boot_grid$boot
        )
    )
    boot_grid <- boot_grid |>
        dplyr::filter(!fs::file_exists(f_path))

    if (NROW(boot_grid) == 0) {
        next
    }

    ### Make out of hospital predictions ----
    ### Recall boot==0 and batch==0 is the full training data with
    ### no resampling.
    doParallel::registerDoParallel(N_CORES)
    foreach::foreach(i = sample(1:NROW(boot_grid)), .inorder = FALSE) %dopar% {
        batch_x <- boot_grid$batch[i]
        boot_x <- boot_grid$boot[i]
        f_path <- boot_grid$f_path[i]

        ## Seems like in rare instances (or maybe on job restarts), a file
        ## will be rerun/overwritten. This should prevent that.
        if (fs::file_exists(f_path)) {
            return(NULL)
        }

        fs::dir_create(dirname(f_path))

        ## Use full in-hospital data for batch==0, boot==0, else use a
        ## random bootstrap sample
        if (batch_x == 0 & boot_x == 0) {
            train_df <- analytic_df
        } else {
            train_df <- analytic_df |>
                dplyr::slice_sample(prop = 1, replace = TRUE)
        }

        ### Refit ----
        tictoc::tic(msg = "starting refit")
        boot_fit <- parsnip::fit(final_wf, train_df)
        refit_timer <- tictoc::toc(quiet = TRUE)
        refit_time <- as.numeric(refit_timer$toc - refit_timer$tic)

        ### Out of hospital predictions ----
        tictoc::tic(msg = "starting prediction")
        boot_test <- predict(boot_fit,
            pred_df,
            type = "prob"
        )
        predict_timer <- tictoc::toc(quiet = TRUE)
        predict_time <- as.numeric(predict_timer$toc - predict_timer$tic)

        ### Store predictions ----
        prediction_holder <- pred_df |>
            dplyr::select(row_id, has_covid) |>
            dplyr::bind_cols(boot_test |>
                dplyr::select(.pred_has_covid))

        ### Store timers ----
        timer_holder <- tibble::tibble(
            model = model_x,
            batch_id = batch_x,
            sim_id = boot_x,
            refit_time = refit_time,
            predict_time = predict_time
        )

        ### Save ----
        saveRDS_xz(
            list(
                meta = tibble::tibble(
                    sim_id = boot_x,
                    batch_id = batch_x,
                    model = model_x
                ),
                predictions = prediction_holder,
                times = timer_holder
            ),
            f_path
        )

        ### Clean up ----
        ### Looks like some cores aren't cleaning up automatically (?)
        rm(boot_fit, boot_test, train_df, prediction_holder)
        gc()
    }

    ## Close out ----
    doParallel::stopImplicitCluster()
    closeAllConnections()
    unregister_dopar()
    future::plan(future::sequential)
}
