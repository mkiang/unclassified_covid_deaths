## 01_ingest_raw_data.R ----
##
## This file takes the raw (restricted-access) multiple cause of death files
## as input, subsets them to the time period of interest, and then saves them
## as in-hospital training/validating/testing data and out-of-hospital
## prediction data (separately).

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(future)
library(furrr)
library(narcan)
library(doParallel)
source(here::here("code", "utils.R"))

## Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" &&
    !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Constants ----
YEARS <- config::get("start_year"):config::get("end_year")
N_CORES <- NROW(YEARS)

## Read in the data files ----
future::plan(future::multisession(workers = N_CORES))
all_deaths <- furrr::future_map_dfr(
    .x = YEARS,
    .f = ~ {
        fwf_col_pos <- narcan::mcod_fwf_dicts |>
            dplyr::filter(year == 2020) |>
            dplyr::select("start", "end", col_names = "name")
        c_types <-
            narcan::mcod_fwf_dicts |>
            dplyr::filter(year == 2020) |>
            dplyr::pull("type") |>
            paste(collapse = "")

        readr::read_fwf(
            file = fs::dir_ls(here::here("data_private"),
                regexp = sprintf(".*%s.*\\.zip", .x)
            ),
            col_positions = readr::fwf_positions(
                start = fwf_col_pos$start,
                end = fwf_col_pos$end,
                col_names = fwf_col_pos$col_names
            ),
            col_types = c_types,
            na = c("", "NA", " ")
        ) |>
            dplyr::mutate(date = lubridate::ymd(sprintf("%i-%i-01", year, monthdth)))
    }
)

### Close out ----
closeAllConnections()
doParallel::stopImplicitCluster()
unregister_dopar()

## Munge data ----
### Add row numbers for subsetting and debugging later ----
all_deaths <- all_deaths |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::relocate(row_id)

### Keep tabs of sample size for flow plot ----
N0_INITIAL <- NROW(all_deaths)
N0_2020 <- all_deaths |>
    dplyr::filter(year == 2020) |>
    NROW()
N0_2021 <- all_deaths |>
    dplyr::filter(year == 2021) |>
    NROW()

### Subset to time period of interest ----
all_deaths <- all_deaths |>
    dplyr::filter(date >= as.Date("2020-03-01"))
N1_SUBSETTIME <- NROW(all_deaths)

### Categorize certifier and place of death ----
## Need to do this now so we can get our holdout and prediction sets before
## we do any feature engineering.
all_deaths <- all_deaths |>
    recode_certifier() |>
    recode_placdth()

## Flag external causes of death ----
## We assume external causes of death are correctly coded
all_deaths <- all_deaths |>
    flag_autopsy_death() |>
    flag_any_external_death()

## Subset training/validation/testing data sets ----
## We consider in-hospital in-patient deaths as gold standard
test_train <- all_deaths |>
    dplyr::filter(placdth_str %in% c("hospital_inpatient"))

## Subset out prediction data ----
### All data that was not in the training or holdout data
prediction_only <- all_deaths |>
    dplyr::filter(!(row_id %in% test_train$row_id))

## Save raw data files ----
saveRDS_xz(test_train, here::here("data_private", "raw_data_test_training.RDS"))
saveRDS_xz(prediction_only, here::here("data_private", "raw_data_prediction_only.RDS"))
saveRDS_xz(
    all_deaths |>
        recode_records() |>
        flag_covid_death(ucod_only = FALSE) |>
        rename(has_covid_contributing = has_covid) |>
        flag_covid_death(ucod_only = TRUE) |>
        rename(has_covid_underlying = has_covid) |>
        mutate(has_covid_any = has_covid_contributing + has_covid_underlying) |>
        dplyr::select(
            row_id,
            placdth_str,
            certifier_str,
            has_covid_any,
            has_covid_underlying,
            has_covid_contributing,
            autopsy_performed
        ),
    here::here("data_private", "place_certifier_index.RDS")
)

## Take some notes about sample size ----
fs::dir_create(here::here("output"))
sink(here::here("output", "sample_size_notes.txt"))
cat("-----------------   ")
cat(sprintf("Processing of raw death data"))
cat("   -----------------\n")
cat(sprintf("Run on: %s\n", Sys.time()))
cat(sprintf("All deaths at import: %i\n", N0_INITIAL))
cat(sprintf("    Deaths in 2020: %i\n", N0_2020))
cat(sprintf("    Deaths in 2021: %i\n", N0_2021))
cat(sprintf(
    "Deaths after subsetting to on or after March 2020: %i\n",
    N1_SUBSETTIME
))
cat(sprintf(
    "Deaths in the gold standard (train + holdout) data: %i\n",
    NROW(test_train)
))
cat(sprintf("Deaths used for prediction: %i\n", NROW(prediction_only)))
sink()
