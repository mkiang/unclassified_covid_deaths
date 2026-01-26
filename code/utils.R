## Imports ----
library(tidyverse)
library(tidymodels)
library(here)

## Data ----
ucr113_df <- readRDS(here("data_raw", "ucr113_to_ucods.RDS"))

## Recode variables ----
recode_age <- function(df) {
    df %>%
        dplyr::mutate(
            age_years = dplyr::case_when(
                age %in% c(9999, 1999, 2999, 4999, 5999, 6999) ~ NA_real_,
                age > 1999 ~ 0,
                age < 1999 ~ age - 1000,
                TRUE ~ NA_real_
            )
        )
}

recode_certifier <- function(df) {
    df %>%
        dplyr::mutate(
            certifier_str = dplyr::case_when(
                certifier == "D" ~ "certifying_physician",
                certifier == "P" ~ "pronouncing_certifying_physician",
                certifier == "M" ~ "me_coroner",
                certifier == "O" ~ "other",
                TRUE ~ "unknown"
            )
        )
}

recode_records <- function(df) {
    df %>%
        dplyr::select(-record_1) |>
        tidyr::unite(f_records_all,
            dplyr::starts_with("record_"),
            sep = " "
        ) %>%
        dplyr::mutate(f_records_all = gsub(
            f_records_all,
            pattern = "\\s{0,1}NA",
            replacement = ""
        ))
}

recode_placdth <- function(df) {
    df %>%
        dplyr::mutate(
            placdth_str = dplyr::case_when(
                placdth == 1 ~ "hospital_inpatient",
                placdth == 2 ~ "hospital_outpatient_or_er",
                placdth == 3 ~ "hospital_doa",
                placdth == 4 ~ "home",
                placdth == 5 ~ "hospice",
                placdth == 6 ~ "nursing_home_ltc",
                placdth == 7 ~ "other",
                TRUE ~ "unknown"
            )
        )
}

recode_nonhispanic <- function(df) {
    df %>%
        dplyr::mutate(
            is_nonhispanic = dplyr::case_when(
                hispanic == 100 ~ "nonhispanic",
                hispanic > 900 ~ "unknown",
                is.na(hispanic) ~ "unknown",
                TRUE ~ "hispanic"
            )
        )
}

recode_racer40 <- function(df) {
    df %>%
        dplyr::mutate(
            racer40_str =
                dplyr::case_when(
                    racer40 == 1 ~ "white",
                    racer40 == 2 ~ "black",
                    racer40 == 3 ~ "aian",
                    racer40 == 4 ~ "asian_indian",
                    racer40 == 5 ~ "chinese",
                    racer40 == 6 ~ "filipino",
                    racer40 == 7 ~ "japanese",
                    racer40 == 8 ~ "korean",
                    racer40 == 9 ~ "vietnamese",
                    racer40 == 10 ~ "other_multi_asian",
                    racer40 == 11 ~ "hawaiian",
                    racer40 == 12 ~ "guamanian",
                    racer40 == 13 ~ "samoan",
                    racer40 == 14 ~ "other_pac_islander",
                    racer40 == 15 ~ "black_white",
                    racer40 == 16 ~ "black_aian",
                    racer40 == 17 ~ "black_asian",
                    racer40 == 18 ~ "black_nhopi",
                    racer40 == 19 ~ "aian_white",
                    racer40 == 20 ~ "aian_asian",
                    racer40 == 21 ~ "aian_nhopi",
                    racer40 == 22 ~ "asian_white",
                    racer40 == 23 ~ "asian_nhopi",
                    racer40 == 24 ~ "nhopi_white",
                    racer40 == 25 ~ "black_aian_white",
                    racer40 == 26 ~ "black_aian_asian",
                    racer40 == 27 ~ "black_aian_nhopi",
                    racer40 == 28 ~ "black_asian_white",
                    racer40 == 29 ~ "black_asian_nhopi",
                    racer40 == 30 ~ "black_nhopi_white",
                    racer40 == 31 ~ "aian_asian_white",
                    racer40 == 32 ~ "aian_nhopi_white",
                    racer40 == 33 ~ "aian_asian_nhopi",
                    racer40 == 34 ~ "asian_nhopi_white",
                    racer40 == 35 ~ "black_aian_asian_white",
                    racer40 == 36 ~ "black_aian_asian_nhopi",
                    racer40 == 37 ~ "black_aian_nhopi_white",
                    racer40 == 38 ~ "black_asian_nhopi_white",
                    racer40 == 39 ~ "aian_asian_nhopi_white",
                    racer40 == 40 ~ "black_aian_asian_nhopi_white",
                    TRUE ~ NA_character_
                )
        )
}

create_county_fips <- function(df) {
    df %>%
        dplyr::left_join(return_st_info() %>%
            dplyr::rename(staters = abbrev)) %>%
        dplyr::mutate(county_fips = paste0(st_fips, substr(countyoc, 3, 5)))
}

recode_county_fips <- function(df) {
    county_df <- tidycensus::fips_codes %>%
        dplyr::transmute(
            county_fips = paste0(state_code, county_code),
            county_name = county
        )

    df %>%
        dplyr::mutate(
            county_fips_known = dplyr::case_when(
                county_fips %in% county_df$county_fips ~ county_fips,
                !(county_fips %in% county_df$county_fips) ~ "unknown"
            )
        )
}

recode_racer40_bigger <- function(df) {
    df %>%
        dplyr::mutate(
            racerbig_str =
                dplyr::case_when(
                    racer40 == 1 ~ "white",
                    racer40 == 2 ~ "black",
                    racer40 == 3 ~ "aian",
                    racer40 == 4 ~ "asian_indian",
                    racer40 == 5 ~ "asian",
                    racer40 == 6 ~ "asian",
                    racer40 == 7 ~ "asian",
                    racer40 == 8 ~ "asian",
                    racer40 == 9 ~ "asian",
                    racer40 == 10 ~ "asian",
                    racer40 == 11 ~ "pacific_islander",
                    racer40 == 12 ~ "pacific_islander",
                    racer40 == 13 ~ "pacific_islander",
                    racer40 == 14 ~ "pacific_islander",
                    racer40 == 15 ~ "multiracial",
                    racer40 == 16 ~ "multiracial",
                    racer40 == 17 ~ "multiracial",
                    racer40 == 18 ~ "multiracial",
                    racer40 == 19 ~ "multiracial",
                    racer40 == 20 ~ "multiracial",
                    racer40 == 21 ~ "multiracial",
                    racer40 == 22 ~ "multiracial",
                    racer40 == 23 ~ "multiracial",
                    racer40 == 24 ~ "multiracial",
                    racer40 == 25 ~ "multiracial",
                    racer40 == 26 ~ "multiracial",
                    racer40 == 27 ~ "multiracial",
                    racer40 == 28 ~ "multiracial",
                    racer40 == 29 ~ "multiracial",
                    racer40 == 30 ~ "multiracial",
                    racer40 == 31 ~ "multiracial",
                    racer40 == 32 ~ "multiracial",
                    racer40 == 33 ~ "multiracial",
                    racer40 == 34 ~ "multiracial",
                    racer40 == 35 ~ "multiracial",
                    racer40 == 36 ~ "multiracial",
                    racer40 == 37 ~ "multiracial",
                    racer40 == 38 ~ "multiracial",
                    racer40 == 39 ~ "multiracial",
                    racer40 == 40 ~ "multiracial",
                    TRUE ~ NA_character_
                )
        )
}

recode_racer_big <- function(df) {
    df |>
        dplyr::mutate(
            racerbig_recode = dplyr::case_when(
                is_nonhispanic_cat == "hispanic" ~ "hispanic",
                is_nonhispanic_cat == "unknown" ~ "unknown",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "aian" ~ "nhaian",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "asian" ~ "nhasian",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "asian_indian" ~ "nhasian_indian",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "black" ~ "nhblack",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "multiracial" ~ "nhmultiracial",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "pacific_islander" ~ "nhpacific_islander",
                is_nonhispanic_cat == "nonhispanic" &
                    racerbig_cat == "white" ~ "nhwhite",
                TRUE ~ "unknown"
            )
        )
}

create_age_bins <- function(df) {
    df |>
        dplyr::mutate(
            age_bin =
                dplyr::case_when(
                    dplyr::between(age_years, 25, 34) ~ "25to34",
                    dplyr::between(age_years, 35, 44) ~ "35to44",
                    dplyr::between(age_years, 45, 54) ~ "45to54",
                    dplyr::between(age_years, 55, 64) ~ "55to64",
                    dplyr::between(age_years, 65, 74) ~ "65to74",
                    dplyr::between(age_years, 75, 84) ~ "75to84",
                    dplyr::between(age_years, 85, Inf) ~ "85andup",
                    TRUE ~ "unknown"
                )
        )
}

## Categorize variables ----
categorize_placdth <- function(df) {
    df %>%
        dplyr::mutate(placdth_cat = factor(
            placdth_str,
            levels = c(
                "hospital_inpatient",
                "hospital_outpatient_or_er",
                "hospital_doa",
                "home",
                "hospice",
                "nursing_home_ltc",
                "other",
                "unknown"
            ),
            labels = c(
                "Hospital in-patient",
                "Hospital out-patient or ER",
                "Hospital DOA",
                "Home",
                "Hospice",
                "Nursing home / LTCF",
                "Other",
                "Unknown"
            ),
            ordered = TRUE
        ))
}

categorize_certifier <- function(df) {
    df %>%
        dplyr::mutate(certifier_cat = factor(
            certifier_str,
            levels = c(
                "certifying_physician",
                "pronouncing_certifying_physician",
                "me_coroner",
                "other",
                "unknown"
            ),
            labels = c(
                "Certifying physician",
                "Pronouncing and certifying physician",
                "Medical examiner / coroner",
                "Other",
                "Unknown"
            ),
            ordered = TRUE
        ))
}

categorize_restatus <- function(df) {
    df %>%
        dplyr::mutate(restatus_cat = factor(
            restatus,
            levels = 1:4,
            labels = c(
                "residents",
                "intrastate_nonresidents",
                "interstate_nonresidents",
                "foreign_residents"
            ),
            ordered = FALSE
        ))
}

categorize_weekday <- function(df) {
    df %>%
        dplyr::mutate(weekday = ifelse(is.na(weekday), 9, weekday)) |>
        dplyr::mutate(weekday_cat = factor(
            weekday,
            levels = c(1:7, 9),
            labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Unknown"),
            ordered = FALSE
        ))
}

categorize_month <- function(df) {
    df %>%
        dplyr::mutate(month_cat = factor(
            monthdth,
            levels = c(1:12, 99),
            labels = c(
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "May",
                "Jun",
                "Jul",
                "Aug",
                "Sep",
                "Oct",
                "Nov",
                "Dec",
                "Unknown"
            ),
            ordered = FALSE
        ))
}

categorize_educ <- function(df) {
    df %>%
        dplyr::mutate(educ_cat = factor(
            educ,
            levels = c(1:9),
            labels = c(
                "< hs",
                "hs_no_diploma",
                "hs_ged",
                "some_college",
                "associate",
                "bachelor",
                "master",
                "doctorate",
                "unknown"
            ),
            ordered = TRUE
        ))
}

categorize_outcome <- function(df) {
    df %>%
        dplyr::mutate(has_covid_cat = factor(
            has_covid,
            levels = 1:0,
            labels = c("has_covid", "no_covid"),
            ordered = TRUE
        ))
}

categorize_model_type <- function(df) {
    df |>
        dplyr::mutate(model_cat = factor(
            model_type,
            levels = c(
                "xgboost",
                "lightgbm",
                "random_forest",
                "bart",
                "logistic_elasticnet",
                "logistic_lasso",
                "null_model"
            ),
            labels = c(
                "XGBoost",
                "LightGBM",
                "Random Forest",
                "BART",
                "Logistic (ElasticNet)",
                "Logistic (LASSO)",
                "Null Model"
            ),
            ordered = TRUE
        ))
}

categorize_adjustment_type <- function(df) {
    df |>
        dplyr::mutate(
            misclassification_cat = factor(
                misclassification_adjustment,
                levels = c(
                    "unadjusted",
                    "exact_with_caliper",
                    "exact_matching",
                    "no_matching",
                    "exact_with_caliper_uniform",
                    "exact_matching_uniform",
                    "no_matching_uniform"
                ),
                labels = c(
                    "Unadjusted",
                    "PSM, exact with caliper",
                    "PSM, exact",
                    "No matching",
                    "PSM, exact with caliper (uniform)",
                    "PSM, exact (uniform)",
                    "No matching (uniform)"
                ),
                ordered = TRUE
            )
        )
}

categorize_preproc_type <- function(df) {
    df |>
        dplyr::mutate(preproc_cat = factor(
            preproc_type,
            levels = c(
                "nocountynoucr",
                "nocounty",
                "noucr",
                "naive",
                "none"
            ),
            labels = c(
                "No county, no contributing cause",
                "No county",
                "No contributing causes",
                "All covariates",
                "No covariates"
            ),
            ordered = TRUE
        ))
}

categorize_metric <- function(df) {
    df |>
        dplyr::mutate(metric_cat = factor(
            metric,
            levels = c(
                "roc_auc",
                "brier_class",
                "accuracy",
                "sens",
                "spec",
                "f_meas",
                "j_index",
                "precision",
                "recall",
                "mcc",
                "npv",
                "ppv",
                "kap",
                "mn_log_loss",
                "bal_accuracy",
                "average_precision",
                "gain_capture",
                "pr_auc"
            ),
            labels = c(
                "AUC ROC",
                "Brier score",
                "Accuracy",
                "Sensitivity",
                "Specificity",
                "F1 measure",
                "Youden's J",
                "Precision",
                "Recall",
                "Matthews correlation",
                "Negative predictive value",
                "Positive predictive value",
                "Kappa",
                "Log loss",
                "Avg(sensitivity + specificity)",
                "Average precision",
                "Accuracy ratio",
                "AUC Precision-Recall"
            ),
            ordered = TRUE
        )) |>
        dplyr::mutate(metric_cat_short = factor(
            metric,
            levels = c(
                "roc_auc",
                "brier_class",
                "accuracy",
                "sens",
                "spec",
                "f_meas",
                "j_index",
                "precision",
                "recall",
                "mcc",
                "npv",
                "ppv",
                "kap",
                "mn_log_loss",
                "bal_accuracy",
                "average_precision",
                "gain_capture",
                "pr_auc"
            ),
            labels = c(
                "AUC ROC",
                "Brier",
                "Accu.",
                "Sens.",
                "Spec.",
                "F1",
                "J",
                "Prec.",
                "Recall",
                "MCC",
                "NPV",
                "PPV",
                "Kappa",
                "Log loss",
                "Bal. Accu.",
                "Avg. Prec.",
                "Accu. ratio",
                "AUC PR"
            ),
            ordered = TRUE
        ))
}

categorize_model_preprocessing_type <- function(df) {
    labels_df <- df |>
        dplyr::select(model_cat, preproc_cat) |>
        dplyr::distinct() |>
        dplyr::arrange(model_cat, preproc_cat) |>
        dplyr::mutate(
            model_pre_label = sprintf("%s, %s", model_cat, preproc_cat),
            model_pre_order = dplyr::row_number()
        ) |>
        dplyr::arrange(model_pre_order)

    labels_df <- labels_df |>
        dplyr::mutate(
            model_pre_cat = factor(
                model_pre_label,
                levels = labels_df$model_pre_label,
                ordered = TRUE
            )
        ) |>
        dplyr::arrange(dplyr::desc(model_pre_order))

    labels_df <- labels_df |>
        dplyr::mutate(
            model_pre_cat_rev = factor(
                model_pre_label,
                levels = labels_df$model_pre_label,
                ordered = TRUE
            )
        )

    dplyr::left_join(df, labels_df)
}

## Flag death types ----
flag_covid_death <- function(cleaned_df, ucod_only = FALSE) {
    ## cleaned_df is a dataframe with ucod and record_all columns
    if (ucod_only) {
        cleaned_df %>%
            dplyr::mutate(has_covid = grepl("\\<U071", ucod) + 0)
    } else {
        cleaned_df %>%
            dplyr::mutate(has_covid = grepl("\\<U071", f_records_all) + 0)
    }
}

flag_autopsy_death <- function(cleaned_df) {
    cleaned_df |>
        dplyr::mutate(autopsy_performed = (autopsy == "Y" | autopsy == "y") + 0)
}

flag_any_external_death <- function(cleaned_df) {
    # https://www.cdc.gov/nchs/nvss/manuals/2022/2b-sectionv-2022.htm
    cleaned_df %>%
        dplyr::mutate(
            external_death_ucod = grepl(
                paste0(
                    "\\<[VWX]{1}[0123456789]{2,3}|",
                    "\\<Y[012345678]{1}[0123456789]{1,2}"
                ),
                ucod
            ) + 0
        )
}

flag_hypertensives <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(hypertensive_all = grepl("\\<I1[012356]{1}", f_records_all) + 0) %>%
        dplyr::mutate(hypertensive_i10 = grepl("\\<I10", f_records_all) + 0) %>%
        dplyr::mutate(hypertensive_i11 = grepl("\\<I11", f_records_all) + 0) %>%
        dplyr::mutate(hypertensive_i12 = grepl("\\<I12", f_records_all) + 0) %>%
        dplyr::mutate(hypertensive_i13 = grepl("\\<I13", f_records_all) + 0) %>%
        dplyr::mutate(hypertensive_i15 = grepl("\\<I15", f_records_all) + 0) %>%
        dplyr::mutate(hypertensive_i16 = grepl("\\<I16", f_records_all) + 0) |>
        dplyr::mutate(hypertensive_all_cat = factor(
            hypertensive_all,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(hypertensive_i10_cat = factor(
            hypertensive_i10,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(hypertensive_i11_cat = factor(
            hypertensive_i11,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(hypertensive_i12_cat = factor(
            hypertensive_i12,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(hypertensive_i13_cat = factor(
            hypertensive_i13,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(hypertensive_i15_cat = factor(
            hypertensive_i15,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(hypertensive_i16_cat = factor(
            hypertensive_i16,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_hyperlipidemia <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(hyperlipidemia_e785 = grepl("\\<E785", f_records_all) + 0) |>
        dplyr::mutate(hyperlipidemia_e785_cat = factor(
            hyperlipidemia_e785,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_copds <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(copds_all = grepl("\\<J44[0189]{1}", f_records_all) + 0) %>%
        dplyr::mutate(copds_j440 = grepl("\\<J440", f_records_all) + 0) %>%
        dplyr::mutate(copds_j441 = grepl("\\<J441", f_records_all) + 0) %>%
        dplyr::mutate(copds_j448 = grepl("\\<J448", f_records_all) + 0) %>%
        dplyr::mutate(copds_j449 = grepl("\\<J449", f_records_all) + 0) %>%
        dplyr::mutate(copds_all_cat = factor(
            copds_all,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(copds_j440_cat = factor(
            copds_j440,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(copds_j441_cat = factor(
            copds_j441,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(copds_j448_cat = factor(
            copds_j448,
            levels = 0:1,
            labels = c("no", "yes")
        )) %>%
        dplyr::mutate(copds_j449_cat = factor(
            copds_j449,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_other_resp <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(other_resp = grepl("\\<J96", f_records_all) + 0) |>
        dplyr::mutate(other_resp_cat = factor(
            other_resp,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_ischemic <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(ischemic = grepl("\\<I251", f_records_all) + 0) |>
        dplyr::mutate(ischemic_cat = factor(
            ischemic,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_kidney <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(kidney = grepl("\\<N179", f_records_all) + 0) |>
        dplyr::mutate(kidney_cat = factor(
            kidney,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_pneumonia <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(pneumonia = grepl("\\<J189", f_records_all) + 0) |>
        dplyr::mutate(pneumonia_cat = factor(
            pneumonia,
            levels = 0:1,
            labels = c("no", "yes")
        ))
}

flag_t2d <- function(cleaned_df) {
    ## Based on this paper: 10.1001/jamanetworkopen.2021.24643
    cleaned_df %>%
        dplyr::mutate(t2d = grepl("\\<E119", f_records_all) + 0) |>
        dplyr::mutate(t2d_cat = factor(t2d, levels = 0:1, labels = c("no", "yes")))
}

flag_ucr113 <- function(cleaned_df) {
    ## Note: this is pretty slow and we could probably use fuzzyjoin to do
    ## a regex join instead but we only need to run this once so keep it as-is
    ## for now since I think it is the most transparent way of doing this.

    ucr113_df_unique <- ucr113_df |>
        dplyr::select(ucr113, regexp_str) |>
        dplyr::distinct()

    for (i in 1:NROW(ucr113_df_unique)) {
        ucr113_ix <- paste0("ucr113_", ucr113_df_unique$ucr113[i])
        ucr113_cat <- paste0(ucr113_ix, "_cat")
        regexp_str <- ucr113_df_unique$regexp_str[i]

        cleaned_df[ucr113_ix] <-
            grepl(regexp_str, cleaned_df$f_records_all, perl = TRUE) + 0
        cleaned_df[ucr113_cat] <-
            factor(cleaned_df[[ucr113_ix]],
                levels = 0:1,
                labels = c("no", "yes")
            )
    }

    cleaned_df
}

## Misc. Helpers ----
calculate_arr <- function(sub_df) {
    sub_df |>
        mutate(n_hosp_covid  = ifelse(is.na(n_hosp_covid), 0, n_hosp_covid),
            n_hosp_deaths = ifelse(is.na(n_hosp_deaths), 0, n_hosp_deaths)) |>
        mutate(
            official_covid        = n_hosp_covid + n_covid,
            predicted_covid       = round(n_hosp_covid + pred_p500),
            predicted_covid_upper = round(n_hosp_covid + pred_p975),
            predicted_covid_lower = round(n_hosp_covid + pred_p025)
        ) |>
        mutate(
            unrecognized_covid = predicted_covid - official_covid,
            unrecognized_upper = predicted_covid_upper - official_covid,
            unrecognized_lower = predicted_covid_lower - official_covid
        ) |>
        mutate(
            reporting_ratio       = predicted_covid / official_covid,
            reporting_ratio_upper = predicted_covid_upper / official_covid,
            reporting_ratio_lower = predicted_covid_lower / official_covid
        )
}

predict_splits <- function(splits, final_wf, ...) {
    # Fit the model
    mod <- parsnip::fit(final_wf, data = rsample::analysis(splits))

    # Save the 10%
    holdout <- rsample::assessment(splits)

    # save predictions
    res <- broom::augment(mod, new_data = holdout)

    # just return what we will need to reassemble
    res |>
        dplyr::select(
            row_id,
            has_covid_cat,
            has_covid,
            dplyr::starts_with(".pred")
        )
}

reassign_covid <- function(df) {
    df |>
        dplyr::mutate(has_covid = ifelse(.pred_has_covid > .5, 1, 0))
}

boot_summarize <- function(temp_x, sub_df, col) {
    if (temp_x$meta$sim_id == 0 & temp_x$meta$batch_id == 0) {
        temp_df <- dplyr::left_join(
            sub_df |>
                dplyr::select(row_id, has_covid, has_covid_underlying, {{ col }}),
            temp_x$predictions |>
                dplyr::rename(pred_has_covid = has_covid),
            by = "row_id"
        ) |>
            dplyr::group_by({{ col }}) |>
            dplyr::summarize(
                n_deaths = dplyr::n(),
                n_covid = sum(has_covid),
                n_covid_ucod = sum(has_covid_underlying),
                n_pred_full = sum(pred_has_covid)
            )
    } else {
        temp_df <- dplyr::left_join(
            sub_df |>
                dplyr::select(row_id, has_covid, {{ col }}),
            temp_x$predictions |>
                dplyr::rename(pred_has_covid = has_covid),
            by = "row_id"
        ) |>
            dplyr::group_by({{ col }}) |>
            dplyr::summarize(n_pred_covid = sum(pred_has_covid))
    }
    temp_df |>
        dplyr::mutate(
            model = temp_x$meta$model,
            batch_id = temp_x$meta$batch_id,
            sim_id = temp_x$meta$sim_id,
            .before = 1
        ) |>
        dplyr::ungroup()
}

boot_summarize_by_age <- function(temp_x, sub_df, age_df, col) {
    holder <- vector("list", NROW(age_df))
    for (i in 1:NROW(age_df)) {
        min_age <- age_df$min_age[i]
        max_age <- age_df$max_age[i]
        age_lab <- age_df$age_label[i]

        holder[[i]] <- boot_summarize(
            temp_x,
            sub_df |>
                dplyr::filter(dplyr::between(age_years, min_age, max_age)),
            {{ col }}
        ) |>
            dplyr::mutate(age_group = age_lab)
    }
    dplyr::bind_rows(holder)
}

weight_summarize <- function(train_df, col) {
    train_df |>
        dplyr::select(has_covid, has_covid_underlying, autopsy_performed, {{ col }}) |>
        dplyr::group_by({{ col }}) |>
        dplyr::summarize(
            n_hosp_deaths = dplyr::n(),
            n_hosp_covid = sum(has_covid),
            n_hosp_covid_ucod = sum(has_covid_underlying),
            n_hosp_autopsy_performed = sum(autopsy_performed)
        )
}

weight_summarize_by_age <- function(train_df, age_df, col) {
    holder <- vector("list", NROW(age_df))
    for (i in 1:NROW(age_df)) {
        min_age <- age_df$min_age[i]
        max_age <- age_df$max_age[i]
        age_lab <- age_df$age_label[i]

        holder[[i]] <- weight_summarize(
            train_df |>
                dplyr::filter(dplyr::between(age_years, min_age, max_age)),
            {{ col }}
        ) |>
            dplyr::mutate(age_group = age_lab)
    }
    dplyr::bind_rows(holder)
}


# gather_calibrated_metrics <- function(calibrated_fit) {
#     dplyr::bind_rows(
#         yardstick::roc_auc(calibrated_fit, truth = has_covid_cat, .pred_has_covid),
#         yardstick::accuracy(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::bal_accuracy(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::sens(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::spec(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::f_meas(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::j_index(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::mcc(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::npv(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::ppv(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::kap(calibrated_fit, truth = has_covid_cat, estimate = .pred_class),
#         yardstick::average_precision(calibrated_fit, truth = has_covid_cat, .pred_has_covid),
#         yardstick::mn_log_loss(calibrated_fit, truth = has_covid_cat, .pred_has_covid),
#         yardstick::brier_class(calibrated_fit, truth = has_covid_cat, .pred_has_covid),
#         yardstick::gain_capture(calibrated_fit, truth = has_covid_cat, .pred_has_covid),
#         yardstick::pr_auc(calibrated_fit, truth = has_covid_cat, .pred_has_covid)
#     )
# }

return_file_paths <- function(save_dir,
                              regression_stub = "logistic_lasso",
                              tuning_method = "bayes-tune",
                              recipe_stub,
                              data_prop,
                              file_type = "RDS") {
    base_path <- here(
        save_dir,
        paste0(
            regression_stub,
            "_",
            tuning_method,
            "_",
            recipe_stub,
            "_",
            ifelse(
                data_prop < 1,
                sprintf("SAMPLED-DATA-%03d", round(data_prop * 100)),
                "full-data"
            ),
            "_",
            "%s.",
            file_type
        )
    )

    paths <- list()
    paths$final_workflow <- sprintf(base_path, "final_workflow")
    paths$tuned_workflow <- sprintf(base_path, "tuned_workflow")
    paths$final_fitted_model <- sprintf(base_path, "final_model")
    paths$tuning_metrics <- sprintf(base_path, "tuning_metrics")
    paths$final_metrics <- sprintf(base_path, "final_metrics")
    # paths$calibrated_metrics <-
    #     sprintf(base_path, "calibrated_metrics")
    # paths$calibration_curves <-
    #     sprintf(base_path, "calibration_curves")
    # paths$cal_model_iso <-
    #     sprintf(base_path, "calibration_model_iso")
    # paths$cal_model_beta <-
    #     sprintf(base_path, "calibration_model_beta")
    # paths$cal_final_iso <-
    #     sprintf(base_path, "calibration_final_iso")
    # paths$cal_final_beta <-
    #     sprintf(base_path, "calibration_final_beta")
    paths$log_file <- sprintf(base_path, "log_file.txt")
    paths$log_file <-
        gsub(paste0(".", file_type), "", paths$log_file)

    paths
}

parse_file_path <- function(f_path) {
    b_name <- unlist(stringr::str_split(basename(f_path), "\\_"))
    d_name <- unlist(stringr::str_split(dirname(f_path), pattern = "\\/"))

    res <- list()
    res$orig_dirname <- dirname(f_path)
    res$orig_basename <- basename(f_path)
    res$save_dir <- d_name[length(d_name) - 1]
    res$fold_dir <- d_name[length(d_name)]

    res$regression_stub <- dplyr::case_when(
        grepl("logistic_elasticnet", basename(f_path)) ~ "logistic_elasticnet",
        grepl("logistic_lasso", basename(f_path)) ~ "logistic_lasso",
        grepl("null_model", basename(f_path)) ~ "null_model",
        grepl("random_forest", basename(f_path)) ~ "random_forest",
        TRUE ~ b_name[1]
    )

    ## In case we decide we want to try grid search for some models
    res$tuning_method <- dplyr::case_when(TRUE ~ "bayes_tune")

    res$recipe_stub <- dplyr::case_when(
        grepl("\\_naive\\_", basename(f_path)) ~ "naive",
        grepl("\\_nocountynoucr\\_", basename(f_path)) ~ "nocountynoucr",
        grepl("\\_nocounty\\_", basename(f_path)) ~ "nocounty",
        grepl("\\_noucr\\_", basename(f_path)) ~ "noucr"
    )

    res$data_prop <- 1

    res
}

get_misclassification_metrics <- function(preds_df, ...) {
    fdrate <- 1 - yardstick::ppv(preds_df,
        truth = has_covid_cat,
        estimate = .pred_class,
        ...
    )$.estimate
    forate <- 1 - yardstick::npv(preds_df,
        truth = has_covid_cat,
        estimate = .pred_class,
        ...
    )$.estimate

    tibble::tibble(
        false_discovery_rate = fdrate,
        false_omission_rate = forate
    ) |>
        pivot_longer(
            everything(),
            names_to = "metric",
            values_to = "switching_probability"
        )
}

# calc_binned_misclassifiction_metrics <- function(df, ...) {
#     holder <- df |>
#         dplyr::select(pred_bin, pred_bin_cat) |>
#         dplyr::distinct() |>
#         dplyr::arrange(pred_bin) |>
#         dplyr::mutate(
#             n_obs = NA,
#             metric = NA,
#             switching_probability = NA
#         )
#
#     for (i in 1:NROW(holder)) {
#         sub_df <- df |>
#             dplyr::filter(pred_bin == holder$pred_bin[i])
#         temp_x <- get_misclassification_metrics(sub_df, ...) |>
#             tidyr::pivot_longer(dplyr::everything()) |>
#             dplyr::filter(!is.na(value))
#
#         holder$n_obs[i] <- NROW(sub_df)
#         holder$switching_probability[i] <- temp_x$value
#         holder$metric[i] <- temp_x$name
#     }
#
#     holder
# }

unregister_dopar <- function() {
    ## Sometimes the parallel backends don't close fully so this
    ## helps to close them.
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
}

## RDS helper functions ----
saveRDS_xz <- function(object, file, threads = parallel::detectCores() - 1) {
    ## https://gist.github.com/ShanSabri/b1bdf0951efa0dfee0edeb5509f87e88
    ## If xz version 5 or higher is installed, parallelize
    if (any(grepl("(XZ Utils) 5.", system("xz -V", intern = TRUE), fixed = TRUE))) {
        con <- pipe(paste0("xz -T", threads, " > ", file), "wb")
        saveRDS(object, file = con)
        close(con)
    } else {
        saveRDS(object, file = file, compress = "xz")
    }
}

recipe_names <- function() {
    c(
        "naive",
        "noucr",
        "nocounty",
        "nocountynoucr"
    )
}

return_st_info <- function() {
    structure(
        list(
            abbrev = c(
                "AK",
                "AL",
                "AR",
                "AZ",
                "CA",
                "CO",
                "CT",
                "DC",
                "DE",
                "FL",
                "GA",
                "HI",
                "IA",
                "ID",
                "IL",
                "IN",
                "KS",
                "KY",
                "LA",
                "MA",
                "MD",
                "ME",
                "MI",
                "MN",
                "MO",
                "MS",
                "MT",
                "NC",
                "ND",
                "NE",
                "NH",
                "NJ",
                "NM",
                "NV",
                "NY",
                "OH",
                "OK",
                "OR",
                "PA",
                "RI",
                "SC",
                "SD",
                "TN",
                "TX",
                "US",
                "UT",
                "VA",
                "VT",
                "WA",
                "WI",
                "WV",
                "WY",
                NA
            ),
            division = c(
                "Pacific",
                "East South Central",
                "West South Central",
                "Mountain",
                "Pacific",
                "Mountain",
                "New England",
                "South Atlantic",
                "South Atlantic",
                "South Atlantic",
                "South Atlantic",
                "Pacific",
                "West North Central",
                "Mountain",
                "East North Central",
                "East North Central",
                "West North Central",
                "East South Central",
                "West South Central",
                "New England",
                "South Atlantic",
                "New England",
                "East North Central",
                "West North Central",
                "West North Central",
                "East South Central",
                "Mountain",
                "South Atlantic",
                "West North Central",
                "West North Central",
                "New England",
                "Middle Atlantic",
                "Mountain",
                "Mountain",
                "Middle Atlantic",
                "East North Central",
                "West South Central",
                "Pacific",
                "Middle Atlantic",
                "New England",
                "South Atlantic",
                "West North Central",
                "East South Central",
                "West South Central",
                "Whole US",
                "Mountain",
                "South Atlantic",
                "New England",
                "Pacific",
                "East North Central",
                "South Atlantic",
                "Mountain",
                "Unknown"
            ),
            st_lat = c(
                49.25,
                32.5901,
                34.7336,
                34.2192,
                36.5341,
                38.6777,
                41.5928,
                38.9072,
                38.6777,
                27.8744,
                32.3329,
                31.75,
                41.9358,
                43.5648,
                40.0495,
                40.0495,
                38.4204,
                37.3915,
                30.6181,
                42.3645,
                39.2778,
                45.6226,
                43.1361,
                46.3943,
                38.3347,
                32.6758,
                46.823,
                35.4195,
                47.2517,
                41.3356,
                43.3934,
                39.9637,
                34.4764,
                39.1063,
                43.1361,
                40.221,
                35.5053,
                43.9078,
                40.9069,
                41.5928,
                33.619,
                44.3365,
                35.6767,
                31.3897,
                0,
                39.1063,
                37.563,
                44.2508,
                47.4231,
                44.5937,
                38.4204,
                43.0504,
                0
            ),
            st_lon = c(
                -127.25,
                -86.7509,
                -92.2992,
                -111.625,
                -119.773,
                -105.513,
                -72.3573,
                -77.0369,
                -74.9841,
                -81.685,
                -83.3736,
                -126.25,
                -93.3714,
                -113.93,
                -89.3776,
                -86.0808,
                -98.1156,
                -84.7674,
                -92.2724,
                -71.58,
                -76.6459,
                -68.9801,
                -84.687,
                -94.6043,
                -92.5137,
                -89.8065,
                -109.32,
                -78.4686,
                -100.099,
                -99.5898,
                -71.3924,
                -74.2336,
                -105.942,
                -116.851,
                -75.1449,
                -82.5963,
                -97.1239,
                -120.068,
                -77.45,
                -71.1244,
                -80.5056,
                -99.7238,
                -86.456,
                -98.7857,
                200,
                -111.33,
                -78.2005,
                -72.545,
                -119.746,
                -89.9941,
                -80.6665,
                -107.256,
                199
            ),
            name = c(
                "Alaska",
                "Alabama",
                "Arkansas",
                "Arizona",
                "California",
                "Colorado",
                "Connecticut",
                "District of Columbia",
                "Delaware",
                "Florida",
                "Georgia",
                "Hawaii",
                "Iowa",
                "Idaho",
                "Illinois",
                "Indiana",
                "Kansas",
                "Kentucky",
                "Louisiana",
                "Massachusetts",
                "Maryland",
                "Maine",
                "Michigan",
                "Minnesota",
                "Missouri",
                "Mississippi",
                "Montana",
                "North Carolina",
                "North Dakota",
                "Nebraska",
                "New Hampshire",
                "New Jersey",
                "New Mexico",
                "Nevada",
                "New York",
                "Ohio",
                "Oklahoma",
                "Oregon",
                "Pennsylvania",
                "Rhode Island",
                "South Carolina",
                "South Dakota",
                "Tennessee",
                "Texas",
                "Whole US",
                "Utah",
                "Virginia",
                "Vermont",
                "Washington",
                "Wisconsin",
                "West Virginia",
                "Wyoming",
                "Unknown State"
            ),
            st_fips = c(
                "02",
                "01",
                "05",
                "04",
                "06",
                "08",
                "09",
                "11",
                "10",
                "12",
                "13",
                "15",
                "19",
                "16",
                "17",
                "18",
                "20",
                "21",
                "22",
                "25",
                "24",
                "23",
                "26",
                "27",
                "29",
                "28",
                "30",
                "37",
                "38",
                "31",
                "33",
                "34",
                "35",
                "32",
                "36",
                "39",
                "40",
                "41",
                "42",
                "44",
                "45",
                "46",
                "47",
                "48",
                "999",
                "49",
                "51",
                "50",
                "53",
                "55",
                "54",
                "56",
                NA
            ),
            lon_rank = c(
                1L,
                28L,
                23L,
                8L,
                4L,
                13L,
                47L,
                41L,
                44L,
                35L,
                33L,
                2L,
                21L,
                7L,
                27L,
                30L,
                18L,
                31L,
                24L,
                48L,
                42L,
                51L,
                32L,
                20L,
                22L,
                26L,
                10L,
                38L,
                14L,
                16L,
                49L,
                45L,
                12L,
                6L,
                43L,
                34L,
                19L,
                3L,
                40L,
                50L,
                37L,
                15L,
                29L,
                17L,
                53L,
                9L,
                39L,
                46L,
                5L,
                25L,
                36L,
                11L,
                52L
            ),
            alpha_rank = c(
                2L,
                1L,
                4L,
                3L,
                5L,
                6L,
                7L,
                9L,
                8L,
                10L,
                11L,
                12L,
                16L,
                13L,
                14L,
                15L,
                17L,
                18L,
                19L,
                22L,
                21L,
                20L,
                23L,
                24L,
                26L,
                25L,
                27L,
                34L,
                35L,
                28L,
                30L,
                31L,
                32L,
                29L,
                33L,
                36L,
                37L,
                38L,
                39L,
                40L,
                41L,
                42L,
                43L,
                44L,
                52L,
                45L,
                47L,
                46L,
                48L,
                50L,
                49L,
                51L,
                53L
            ),
            st_cat = structure(
                c(
                    1L,
                    28L,
                    23L,
                    8L,
                    4L,
                    13L,
                    47L,
                    41L,
                    44L,
                    35L,
                    33L,
                    2L,
                    21L,
                    7L,
                    27L,
                    30L,
                    18L,
                    31L,
                    24L,
                    48L,
                    42L,
                    51L,
                    32L,
                    20L,
                    22L,
                    26L,
                    10L,
                    38L,
                    14L,
                    16L,
                    49L,
                    45L,
                    12L,
                    6L,
                    43L,
                    34L,
                    19L,
                    3L,
                    40L,
                    50L,
                    37L,
                    15L,
                    29L,
                    17L,
                    52L,
                    9L,
                    39L,
                    46L,
                    5L,
                    25L,
                    36L,
                    11L,
                    NA
                ),
                levels = c(
                    "AK",
                    "HI",
                    "OR",
                    "CA",
                    "WA",
                    "NV",
                    "ID",
                    "AZ",
                    "UT",
                    "MT",
                    "WY",
                    "NM",
                    "CO",
                    "ND",
                    "SD",
                    "NE",
                    "TX",
                    "KS",
                    "OK",
                    "MN",
                    "IA",
                    "MO",
                    "AR",
                    "LA",
                    "WI",
                    "MS",
                    "IL",
                    "AL",
                    "TN",
                    "IN",
                    "KY",
                    "MI",
                    "GA",
                    "OH",
                    "FL",
                    "WV",
                    "SC",
                    "NC",
                    "VA",
                    "PA",
                    "DC",
                    "MD",
                    "NY",
                    "DE",
                    "NJ",
                    "VT",
                    "CT",
                    "MA",
                    "NH",
                    "RI",
                    "ME",
                    "US"
                ),
                class = c(
                    "ordered",
                    "factor"
                )
            ),
            name_cat = structure(
                c(
                    2L,
                    1L,
                    4L,
                    3L,
                    5L,
                    6L,
                    7L,
                    9L,
                    8L,
                    10L,
                    11L,
                    12L,
                    16L,
                    13L,
                    14L,
                    15L,
                    17L,
                    18L,
                    19L,
                    22L,
                    21L,
                    20L,
                    23L,
                    24L,
                    26L,
                    25L,
                    27L,
                    34L,
                    35L,
                    28L,
                    30L,
                    31L,
                    32L,
                    29L,
                    33L,
                    36L,
                    37L,
                    38L,
                    39L,
                    40L,
                    41L,
                    42L,
                    43L,
                    44L,
                    51L,
                    46L,
                    48L,
                    47L,
                    49L,
                    52L,
                    50L,
                    53L,
                    45L
                ),
                levels = c(
                    "Alabama",
                    "Alaska",
                    "Arizona",
                    "Arkansas",
                    "California",
                    "Colorado",
                    "Connecticut",
                    "Delaware",
                    "District of Columbia",
                    "Florida",
                    "Georgia",
                    "Hawaii",
                    "Idaho",
                    "Illinois",
                    "Indiana",
                    "Iowa",
                    "Kansas",
                    "Kentucky",
                    "Louisiana",
                    "Maine",
                    "Maryland",
                    "Massachusetts",
                    "Michigan",
                    "Minnesota",
                    "Mississippi",
                    "Missouri",
                    "Montana",
                    "Nebraska",
                    "Nevada",
                    "New Hampshire",
                    "New Jersey",
                    "New Mexico",
                    "New York",
                    "North Carolina",
                    "North Dakota",
                    "Ohio",
                    "Oklahoma",
                    "Oregon",
                    "Pennsylvania",
                    "Rhode Island",
                    "South Carolina",
                    "South Dakota",
                    "Tennessee",
                    "Texas",
                    "Unknown State",
                    "Utah",
                    "Vermont",
                    "Virginia",
                    "Washington",
                    "West Virginia",
                    "Whole US",
                    "Wisconsin",
                    "Wyoming"
                ),
                class = c("ordered", "factor")
            ),
            name_cat_alpha = structure(
                c(
                    2L,
                    1L,
                    4L,
                    3L,
                    5L,
                    6L,
                    7L,
                    9L,
                    8L,
                    10L,
                    11L,
                    12L,
                    16L,
                    13L,
                    14L,
                    15L,
                    17L,
                    18L,
                    19L,
                    22L,
                    21L,
                    20L,
                    23L,
                    24L,
                    26L,
                    25L,
                    27L,
                    34L,
                    35L,
                    28L,
                    30L,
                    31L,
                    32L,
                    29L,
                    33L,
                    36L,
                    37L,
                    38L,
                    39L,
                    40L,
                    41L,
                    42L,
                    43L,
                    44L,
                    52L,
                    45L,
                    47L,
                    46L,
                    48L,
                    50L,
                    49L,
                    51L,
                    53L
                ),
                levels = c(
                    "Alabama",
                    "Alaska",
                    "Arizona",
                    "Arkansas",
                    "California",
                    "Colorado",
                    "Connecticut",
                    "Delaware",
                    "District of Columbia",
                    "Florida",
                    "Georgia",
                    "Hawaii",
                    "Idaho",
                    "Illinois",
                    "Indiana",
                    "Iowa",
                    "Kansas",
                    "Kentucky",
                    "Louisiana",
                    "Maine",
                    "Maryland",
                    "Massachusetts",
                    "Michigan",
                    "Minnesota",
                    "Mississippi",
                    "Missouri",
                    "Montana",
                    "Nebraska",
                    "Nevada",
                    "New Hampshire",
                    "New Jersey",
                    "New Mexico",
                    "New York",
                    "North Carolina",
                    "North Dakota",
                    "Ohio",
                    "Oklahoma",
                    "Oregon",
                    "Pennsylvania",
                    "Rhode Island",
                    "South Carolina",
                    "South Dakota",
                    "Tennessee",
                    "Texas",
                    "Utah",
                    "Vermont",
                    "Virginia",
                    "Washington",
                    "West Virginia",
                    "Wisconsin",
                    "Wyoming",
                    "Whole US",
                    "Unknown State"
                ),
                class = c("ordered", "factor")
            )
        ),
        row.names = c(NA, -53L),
        class = c(
            "tbl_df",
            "tbl", "data.frame"
        )
    )
}
