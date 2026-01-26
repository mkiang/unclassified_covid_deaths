## 04_setup_environment.R ----
##
## This file just sets up our work environment before we run all the train_*.R
## files. Mostly, it loads appropriate packages, unpacks our config.yml file,
## and makes sure we create a consistent test-validate-train split.
##
## The first time you run this file, it will create the nested k-fold object
## so we can ensure it is consistent across all models. After that, it just
## loads the object from disk.

## Imports ----
library(tidyverse)
library(tidymodels)
library(themis)
library(embed)
library(foreach)
library(doParallel)
library(parallel)
library(future)
library(discrim)
library(here)
library(fs)
library(bonsai)
library(lobstr)
source(here::here("code", "utils.R"))
source(here::here("code", "utils_regression_recipes.R"))
source(here::here("code", "utils_tree_recipes.R"))

## Constants ----
N_CORES <- config::get("n_cores")
N_CORES_TUNE <- config::get("n_cores_tune")
R_SEED <- config::get("random_seed")
K_FOLDS <- config::get("k_folds")
V_REPEATS <- config::get("v_repeats")
TRAIN_PROP <- config::get("train_prop")
BAYES_ITER <- config::get("tune_bayes_iter")
BAYES_STOP <- config::get("tune_bayes_stop")
DATA_PROP <- config::get("data_prop")

### Metrics we want to evaluate the models on ----
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

### Packages to pass on when parallelizing ----
REQ_PACKAGES <- c(
    "here",
    "fs",
    "tidyverse",
    "tidymodels",
    "bonsai",
    "embed",
    "themis",
    "discrim",
    "lobstr"
)

### Nested test-validate-train splits ----
## We want a nested CV with 5-fold CV on the outside and simple test-validate
## on the inside. Specifically, we want a 60-20-20 train-validate-test split
## that rotates five times so we use five different (mutually exclusive) test
## splits.
if (!fs::file_exists(here::here("data_private", "analytic_data_rsplit.RDS")) |
    DATA_PROP < 1) {
    ### Read in analytic data, subsetting if necessary ----
    analytic_df <- readRDS(here::here("data_private", "analytic_data.RDS"))

    if (DATA_PROP < 1) {
        analytic_df <- analytic_df |>
            dplyr::slice_sample(prop = DATA_PROP)
    }

    set.seed(R_SEED)
    analytic_split <- rsample::nested_cv(
        analytic_df,
        outside = rsample::vfold_cv(v = K_FOLDS, repeats = V_REPEATS),
        inside = rsample::validation_split(prop = TRAIN_PROP)
    )
    set.seed(NULL)

    if (DATA_PROP == 1) {
        saveRDS_xz(analytic_split, here::here("data_private", "analytic_data_rsplit.RDS"))
    }
} else {
    analytic_split <- readRDS(here::here("data_private", "analytic_data_rsplit.RDS"))
}
