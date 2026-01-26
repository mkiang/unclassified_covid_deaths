## 06_train_random_forest.R ----
##
## Fits a standard random forest model.
##
## All train_*.R files follow the same basic structure:
##      (1) call in 04_setup_environment.R to get packages and data,
##      (2) set up the folder infrastructure,
##      (3) create the model specification,
##      (4) add in the appropriate preprocessing steps (i.e., recipes),
##      (5) tune the model,
##      (6) select the best model based on the train-validate set,
##      (7) refit the final (i.e., tuned) model on the whole train-validate set,
##      (8) evaluate the model against the holdout (i.e., test) set.
##
## Note if you need to debug, switch the config environment (Sys.setenv()) and
## because each model may have different compute/memory requirements, you can
## override the number of cores in the CONSTANTS section.

## Set up environment ----
# Sys.setenv(R_CONFIG_ACTIVE = "prototyping")
# Sys.setenv(R_CONFIG_ACTIVE = "debug")
source(here::here("code", "04_setup_environment.R"))

## Imports ----
library(ranger)

## CONSTANTS ----
## WARNING: Each core specified in N_CORES requires (up to) 120GB of memory.
## Adjust this value according to your compute environment. N_CORES *
## N_CORES_TREE should be less than the number of cores in your compute
## environment.
# N_CORES <- 1
# N_CORES_TREE <- 4
N_CORES <- 1
N_CORES_TREE <- future::availableCores()
options("future.globals.maxSize" = 10 * 1024^3) # 10GB
options("ranger.num.threads" = N_CORES_TREE)

## Loop through each of the k-folds ----
param_grid <- tidyr::expand_grid(
    k = 1:5,
    r = recipe_names()
)

## Loop through each of the k-folds ----
for (i in 1:NROW(param_grid)) {
    k <- param_grid$k[i]
    r <- param_grid$r[i]

    ### Infrastructure ----
    SAVE_FOLDER <- here::here("output", "models", "random_forest", sprintf("fold%s", k))
    fs::dir_create(SAVE_FOLDER)

    ### Test-validate-train splits ----
    current_rsplit <- rsample::get_rsplit(analytic_split, k)
    test_df <- rsample::testing(current_rsplit)
    train_df <- rsample::training(current_rsplit)
    valid_rset <- analytic_split$inner_resamples[[k]]

    ### Get recipes for this training set ----
    recipe_list <- return_tree_recipes(train_df)

    ### Set up sparse matrix infrastructure ----
    ### As of parsnip 1.1.1, ranger will support sparse matrices.
    ### https://www.tidyverse.org/blog/2020/11/tidymodels-sparse-support/
    sparse_bp <- hardhat::default_recipe_blueprint(composition = "dgCMatrix")

    ## Random forests using ranger (sparse matrix version doesn't work) ----
    f_paths <- return_file_paths(
        SAVE_FOLDER,
        regression_stub = "random_forest",
        tuning_method = "bayes_tune",
        recipe_stub = r,
        data_prop = DATA_PROP,
        file_type = "RDS"
    )

    if (!fs::file_exists(f_paths$final_fitted_model)) {
        future::plan(future::multisession, workers = N_CORES)

        ## Open logging ----
        sink(f_paths$log_file,
            split = TRUE,
            type = c("output", "message")
        )
        cat("-----------------------\n")
        print(sprintf("FILE: %s", basename(f_paths$final_fitted_model)))
        print(sprintf("Created: %s", Sys.Date()))
        print(sprintf("Current memory used: %s", lobstr::mem_used()))
        cat("-----------------------\n")

        ### Make modeling spec ----
        mod_engine <- parsnip::rand_forest(
            mtry = tune::tune(),
            min_n = tune::tune(),
            trees = 1000
        ) %>%
            parsnip::set_engine("ranger",
                importance = "impurity",
                num.threads = N_CORES_TREE
                # num.threads = 7
            ) %>%
            parsnip::set_mode("classification")

        ### Create new workflow ----
        empty_workflow <- workflows::workflow() %>%
            workflows::add_recipe(recipe_list[[r]],
                blueprint = sparse_bp
            ) %>%
            workflows::add_model(mod_engine)

        ### Need to finalize because mtry() depends on number of columns
        ### (which are not known until we get to this point)
        updated_params <- empty_workflow |>
            workflows::extract_parameter_set_dials() |>
            dials::finalize(train_df)

        ### Tune the models ----
        print(sprintf("Started tuning: %s", Sys.time()))

        tuned_workflow <- empty_workflow |>
            tune::tune_bayes(
                resamples = valid_rset,
                initial = 5,
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

        print(sprintf("Finished tuning: %s", Sys.time()))
        print(sprintf(
            "Size of tuned object: %s",
            lobstr::obj_size(tuned_workflow)
        ))

        ## For debugging purposes, one could save the tuned workflow but
        ## note that it takes a long time and is a large object so it should
        ## only be done when necessary.
        # saveRDS(tuned_workflow, f_paths$tuned_workflow, compress = "xz")
        # print(sprintf("Finished saving tuning: %s", Sys.time()))
        cat("-----------------------\n")

        ## Get the metrics out ----
        tuning_metrics <- tuned_workflow |>
            tune::collect_metrics()
        saveRDS_xz(tuning_metrics, f_paths$tuning_metrics)

        ## Select a best one ----
        ## NOTE: Does not need to be ROC AUC
        best_auc_params <-
            tuned_workflow |>
            tune::select_best(metric = "roc_auc")

        finalized_workflow <- empty_workflow |>
            tune::finalize_workflow(best_auc_params)
        saveRDS_xz(finalized_workflow, f_paths$final_workflow)

        ## Now fit tuned model ----
        print(sprintf("Started final fit: %s", Sys.time()))
        finalized_fit <- finalized_workflow |>
            tune::last_fit(
                split = current_rsplit,
                metrics = M_SET,
                control = tune::control_last_fit(
                    verbose = TRUE,
                    allow_par = TRUE
                )
            )

        print(sprintf("Finished final fit: %s", Sys.time()))
        saveRDS_xz(finalized_fit, f_paths$final_fitted_model)
        print(sprintf("Finished saving final fit: %s", Sys.time()))
        print(sprintf(
            "Size of final fit object: %s",
            lobstr::obj_size(finalized_fit)
        ))
        cat("-----------------------\n")

        ## Get the metrics out ----
        final_metrics <- finalized_fit |>
            tune::collect_metrics() |>
            dplyr::mutate(kfold = k)
        saveRDS_xz(final_metrics, f_paths$final_metrics)

        ## Close logging ----
        print(sprintf("Closing out: %s", Sys.time()))
        cat("-----------------------\n")
        print(tune::show_notes(.Last.tune.result))
        sink()

        ## Close up ----
        doParallel::stopImplicitCluster()
        closeAllConnections()
        unregister_dopar()
        future::plan(future::sequential)

        rm(
            finalized_fit,
            finalized_workflow,
            tuned_workflow,
            empty_workflow,
            mod_engine,
            recipe_list,
            f_paths
        )
    }

    rm(current_rsplit, test_df, train_df, valid_rset)
    gc()
}
