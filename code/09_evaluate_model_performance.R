## 09_evaluate_model_performance.R ----
##
## During the training of each model, we collected a variety of performance
## metrics (e.g., ROC AUC, sensitivity, specificity, etc.). This file just
## collects all the metrics and saves them in a single dataframe.

## Import ----
library(tidyverse)
library(here)
library(fs)
library(tidymodels)
source(here::here("code", "utils.R"))

## Get metrics paths ----
metric_paths <- fs::dir_ls(here::here("output", "models"),
    recurse = TRUE,
    glob = "*final_metrics.RDS"
)

## Collect metrics ----
### Final model metrics (on holdout data) ----
final_metrics <- purrr::map_dfr(
    .x = metric_paths,
    .f = ~ {
        parsed <- parse_file_path(.x)

        temp_x <- readRDS(.x)

        if (!is.null(temp_x)) {
            temp_x |>
                dplyr::mutate(
                    f_path = basename(.x),
                    model_type = parsed$regression_stub,
                    kfold = as.numeric(substr(parsed$fold_dir, 5, 5)),
                    preproc_type = parsed$recipe_stub,
                    tuning_type = parsed$tuning_method
                )
        }
    }
)

## Clean and munge ----
all_metrics <- final_metrics |>
    dplyr::transmute(
        model_type,
        preproc_type,
        tuning_type,
        kfold,
        metric = .metric,
        value = .estimate,
        estimator = .estimator
    )

## Summarize across folds ----
summarized_metrics <- all_metrics |>
    dplyr::group_by(
        model_type,
        preproc_type,
        tuning_type,
        metric,
        estimator
    ) |>
    dplyr::summarize(
        n_folds = dplyr::n(),
        best_fold = kfold[value == max(value)],
        mean = mean(value),
        min = min(value),
        max = max(value),
        sd = sd(value),
        range = max(value) - min(value),
        .groups = "drop"
    )

## Better column names ----
summarized_metrics <- summarized_metrics |>
    categorize_model_type() |>
    categorize_preproc_type() |>
    categorize_metric() |>
    categorize_model_preprocessing_type() |>
    dplyr::arrange(tuning_type, metric_cat, model_cat, preproc_cat, dplyr::desc(mean))

## Save ----
saveRDS(summarized_metrics,
    here::here("data", "model_metrics.RDS"),
    compress = "xz"
)
