## 02_create_analytic_data.R ----
##
## This file takes the training/validation/testing data set and incorporates
## (1) county-level time-invariant data (before 2020) and (2) county-level
## time-varying data to the death certificates, producing an analytic data set.

## Imports ----
library(tidyverse)
library(tidymodels)
library(readxl)
library(here)
library(janitor)
source(here::here("code", "utils.R"))

## Data ----
### Read in raw data ----
raw_data <- readRDS(here::here("data_private", "raw_data_test_training.RDS"))
N_IMPORT <- NROW(raw_data)

### Read in aux data ----
county_df <- readr::read_csv(
    here::here("data_raw", "County_Covariates_Sociodemographics_2021.csv")
)
rucc_df <- readxl::read_excel(here::here("data_raw", "ruralurbancodes2013.xls"))
vacc_df <- readr::read_csv(here::here(
    "data_raw",
    "COVID-19_Vaccinations_in_the_United_States_County.csv.zip"
))
trans_df <- dplyr::bind_rows(
    readr::read_csv(here::here(
        "data_raw",
        paste0(
            "Weekly_COVID-19_County_Level_of_",
            "Community_Transmission_as_Originally_",
            "Posted_-_ARCHIVED.csv"
        )
    )) |>
        dplyr::rename(date = report_date),
    readr::read_csv(here::here(
        "data_raw",
        paste0(
            "United_States_COVID-19_County_Level_of",
            "_Community_Transmission_Historical_",
            "Changes_-_ARCHIVED.csv"
        )
    )) |>
        dplyr::mutate(
            cases_per_100K_7_day_count_change = as.numeric(cases_per_100K_7_day_count_change)
        )
)

## Make an extended data frame with all potential covariates ----
working_df <- raw_data |>
    dplyr::filter(statersr %in% c("DC", datasets::state.abb)) |>
    create_county_fips() |>
    recode_county_fips() |>
    recode_age() |>
    recode_records() |>
    recode_nonhispanic() |>
    recode_racer40() |>
    recode_racer40_bigger() |>
    flag_covid_death(ucod_only = FALSE) |>
    rename(has_covid_contributing = has_covid) |>
    flag_covid_death(ucod_only = TRUE) |>
    rename(has_covid_underlying = has_covid) |>
    mutate(has_covid = has_covid_contributing + has_covid_underlying) |>
    select(-has_covid_contributing, -has_covid_underlying) |>
    flag_copds() |>
    flag_hyperlipidemia() |>
    flag_hypertensives() |>
    flag_ischemic() |>
    flag_kidney() |>
    flag_other_resp() |>
    flag_pneumonia() |>
    flag_t2d() |>
    flag_ucr113() |>
    categorize_restatus() |>
    categorize_weekday() |>
    categorize_month() |>
    categorize_educ() |>
    categorize_outcome()

## Remove deaths under 25 ----
working_df <- working_df |>
    dplyr::filter(age_years >= 25)
N_AGE <- NROW(working_df)

## Remove external causes of death ----
working_df <- working_df |>
    dplyr::filter(external_death_ucod == 0)
N_EXTERNAL <- NROW(working_df)

## Join with county-level dataframe ----
county_df <- county_df |>
    dplyr::mutate(county_fips = sprintf("%05d", county_code)) |>
    dplyr::left_join(rucc_df |>
        dplyr::select(
            county_fips = FIPS,
            rucc_2013 = RUCC_2013
        ))
working_df <- working_df |>
    dplyr::left_join(county_df)

## Join with county-level time-varying covariates ----
### Cumulative vaccination rates over time ----
sub_vac <- vacc_df |>
    janitor::clean_names() |>
    dplyr::mutate(
        date = lubridate::mdy(date),
        year = lubridate::year(date),
        month = lubridate::month(date)
    ) |>
    dplyr::select(date,
        year,
        month,
        county_fips = fips,
        dplyr::matches("\\<series_complete_.+pct\\>")
    ) |>
    dplyr::arrange(county_fips, date) |>
    dplyr::group_by(county_fips, year, month) |>
    dplyr::slice_max(date, na_rm = TRUE) |>
    dplyr::select(
        county_fips,
        year,
        month,
        series_complete_pop_pct,
        series_complete_65plus_pop_pct
    ) |>
    dplyr::ungroup()

### Community transmission levels over time ----
sub_trans <- trans_df |>
    dplyr::mutate(
        date = lubridate::mdy(date),
        month = lubridate::month(date),
        year = lubridate::year(date),
        county_fips = fips_code,
        comm_trans_level_num = dplyr::case_when(
            community_transmission_level == "high" ~ 4,
            community_transmission_level == "substantial" ~ 3,
            community_transmission_level == "moderate" ~ 2,
            community_transmission_level == "low" ~ 1,
            TRUE ~ NA_integer_
        )
    ) |>
    dplyr::group_by(county_fips, year, month) |>
    dplyr::slice_max(comm_trans_level_num, na_rm = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(county_fips, year, month) |>
    dplyr::select(
        county_fips,
        year,
        month,
        comm_trans_level_num,
        community_transmission_level
    ) |>
    dplyr::mutate(community_transmission_level = ifelse(
        is.na(community_transmission_level),
        "unknown",
        community_transmission_level
    )) |>
    dplyr::mutate(comm_trans_level_cat = factor(
        community_transmission_level,
        levels = c("unknown", "low", "moderate", "substantial", "high"),
        ordered = TRUE
    ))

### Join them ----
county_time_varying <- sub_trans |>
    dplyr::left_join(sub_vac) |>
    dplyr::mutate(
        series_complete_pop_pct = ifelse(is.na(series_complete_pop_pct), 0, series_complete_pop_pct),
        series_complete_65plus_pop_pct = ifelse(
            is.na(series_complete_65plus_pop_pct),
            0,
            series_complete_65plus_pop_pct
        )
    )

working_df <- working_df |>
    dplyr::left_join(county_time_varying |>
        dplyr::rename(monthdth = month))

## Make an analytic data frame containing just what we need for training ----
## Note that if something is an indicator or will end up being dummy variables,
## we should convert them to (ordered or unordered) factors here so that
## the recipes know later not to normalize them. (For example, we want
## county fixed effects to be binary and we don't want to drop them if there
## is zero variance in the training data.)
analytic_df <- working_df |>
    dplyr::mutate(
        month_into_pandemic = (year - 2020) * 12 + (monthdth - 3),
        restatus_str = as.character(restatus_cat),
        tobacco_use = ifelse(is.na(tobacco_use), "U", tobacco_use),
        rucc_2013_str = ifelse(is.na(rucc_2013), "unknown", as.character(rucc_2013)),
        rucc_2013_cat = factor(rucc_2013_str, ordered = FALSE),
        county_fips_cat = factor(county_fips_known, ordered = FALSE),
        tobacco_use_cat = factor(tobacco_use, ordered = FALSE),
        tobacco_use = dplyr::case_when(
            tobacco_use == "u" ~ "U",
            tobacco_use == "n" ~ "N",
            TRUE ~ tobacco_use
        ),
        is_nonhispanic_cat = factor(is_nonhispanic, ordered = TRUE),
        birth_state_cat = factor(statbth, ordered = FALSE),
        sex = ifelse(is.na(sex), "U", sex),
        sex_cat = factor(sex, ordered = FALSE),
        comm_trans_level_str =
            ifelse(
                is.na(community_transmission_level),
                "unknown",
                community_transmission_level
            ),
        comm_trans_level_cat = factor(comm_trans_level_str, ordered = FALSE),
        state_cat = factor(st_cat, ordered = FALSE),
        racerbig_cat = factor(racerbig_str, ordered = FALSE),
        marital_cat = factor(marstat, ordered = FALSE)
    ) |>
    dplyr::select(
        row_id,
        has_covid_cat,
        has_covid,
        year,
        month_cat,
        month_into_pandemic,
        weekday_cat,
        state_cat,
        age_years,
        educ_cat,
        marital_cat,
        sex_cat,
        is_nonhispanic_cat,
        racerbig_cat,
        birth_state_cat,
        county_fips_cat,
        division,
        restatus_cat,
        tobacco_use_cat,
        tidyselect::matches("\\<copds.+cat\\>"),
        tidyselect::matches("\\<hypertensive.+cat\\>"),
        hyperlipidemia_e785_cat,
        ischemic_cat,
        kidney_cat,
        other_resp_cat,
        pneumonia_cat,
        t2d_cat,
        tidyselect::matches("\\<ucr113.+cat\\>"),
        comm_trans_level_cat,
        perc_total_pop_vaccinated = series_complete_pop_pct,
        perc_65up_pop_vaccinated = series_complete_65plus_pop_pct,
        poor_or_fair_health,
        smoking,
        obesity,
        some_college,
        diabetes,
        household_income,
        income_inequality,
        home_ownership,
        percent_65_over,
        percent_black,
        percent_hispanic,
        percent_white,
        rucc_2013_cat
    )

## Save ----
saveRDS_xz(analytic_df, here::here("data_private", "analytic_data.RDS"))

## Take some notes about sample size ----
sink(here::here("output", "sample_size_notes.txt"), append = TRUE)
cat("-----------------   ")
cat(sprintf("Creation of gold standard analytic data"))
cat("   -----------------\n")
cat(sprintf("Run on: %s\n", Sys.time()))
cat(sprintf(
    "Deaths in gold standard data at import: %i\n",
    N_IMPORT
))
cat(sprintf(
    "Deaths in gold standard data after removing under 25 y/o: %i\n",
    N_AGE
))
cat(sprintf(
    "Deaths in gold standard data after removing external causes of death: %i\n\n",
    N_EXTERNAL
))
sink()
