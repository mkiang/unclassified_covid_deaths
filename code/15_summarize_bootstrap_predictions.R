## 15_summarize_bootstrap_predictions.R ----
##
## Take the intermediate bootstrap files created earlier and summarize them
## over the categories/groups we are interested in.

## Set up environment ----
source(here::here("code", "04_setup_environment.R"))
library(probably)
library(tictoc)

## CONSTANTS ----
N_CORES <- 10
age_df <- tibble::tibble(
    min_age = c(25, 45, 65, -Inf),
    max_age = c(44, 64, Inf, Inf),
    age_label = c("25to44", "45to64", "65andup", "all")
)
COLS_TO_KEEP <- c(
    "row_id",
    "county_fips_cat",
    "has_covid",
    "has_covid_cat",
    "has_covid_underlying",
    "autopsy_performed",
    "year",
    "month_cat",
    "month_into_pandemic",
    "weekday_cat",
    "division",
    "state_cat",
    "age_years",
    "educ_cat",
    "marital_cat",
    "sex_cat",
    "is_nonhispanic_cat",
    "restatus_cat",
    "racerbig_recode",
    "rucc_2013_cat",
    "tobacco_use_cat",
    "comm_trans_level_cat",
    "place_cert",
    "placdth_str",
    "certifier_str"
)

## Data ----
train_df <- readRDS(here::here("data_private", "analytic_data.RDS"))
pred_df <- readRDS(here::here("data_private", "analytic_prediction_data.RDS"))
place_cert <- readRDS(here::here("data_private", "place_certifier_index.RDS"))
misclass_df <- readRDS(here::here("data", "misclassification_metrics_df.RDS"))

## Step 1: Prep the prediction data ----
### Discretize county-covariates ----
county_discrete <- dplyr::bind_rows(train_df, pred_df) |>
    dplyr::select(
        county_fips_cat,
        some_college,
        household_income,
        income_inequality,
        home_ownership,
        percent_black,
        poor_or_fair_health,
        smoking,
        obesity,
        diabetes,
        percent_65_over
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
        some_college = ggplot2::cut_number(some_college, n = 5, ordered_result = TRUE),
        household_income = ggplot2::cut_number(household_income, n = 5, ordered_result = TRUE),
        income_inequality = ggplot2::cut_number(income_inequality, n = 5, ordered_result = TRUE),
        home_ownership = ggplot2::cut_number(home_ownership, n = 5, ordered_result = TRUE),
        percent_black = ggplot2::cut_number(percent_black, n = 5, ordered_result = TRUE),
        poor_or_fair_health = ggplot2::cut_number(poor_or_fair_health, n = 5, ordered_result = TRUE),
        smoking = ggplot2::cut_number(smoking, n = 5, ordered_result = TRUE),
        obesity = ggplot2::cut_number(obesity, n = 5, ordered_result = TRUE),
        diabetes = ggplot2::cut_number(diabetes, n = 5, ordered_result = TRUE),
        percent_65_over = ggplot2::cut_number(percent_65_over, n = 5, ordered_result = TRUE)
    ) |>
    dplyr::arrange(county_fips_cat)

### Add in death reporting data ----
pred_df <- pred_df |>
    dplyr::left_join(place_cert,
        by = "row_id"
    ) |>
    dplyr::mutate(place_cert = paste(placdth_str, certifier_str, sep = "_"))
train_df <- train_df |>
    dplyr::left_join(place_cert,
        by = "row_id"
    ) |>
    dplyr::mutate(place_cert = paste(placdth_str, certifier_str, sep = "_"))

### Regroup race/ethnicity ----
pred_df <- pred_df |>
    recode_racer_big()
train_df <- train_df |>
    recode_racer_big()

### Subset predictions and join with discrete variables ----
sub_df <- pred_df |>
    dplyr::select(tidyr::all_of(COLS_TO_KEEP)) |>
    dplyr::left_join(county_discrete,
        by = "county_fips_cat"
    )
sub_train <- train_df |>
    dplyr::select(tidyr::all_of(COLS_TO_KEEP)) |>
    dplyr::left_join(county_discrete,
        by = "county_fips_cat"
    )

## Step 2: Summarize each bootstrap ----
### Create grid to parallelize over ----
par_grid <- dplyr::bind_rows(
    fs::dir_info(
        here::here("output", "out_of_hosp"),
        recurse = TRUE,
        type = "dir",
        regexp = "lightgbm.*batch"
    ) |>
        dplyr::transmute(path, model = "lightgbm"),
    fs::dir_info(
        here::here("output", "out_of_hosp"),
        recurse = TRUE,
        type = "dir",
        regexp = "xgboost.*batch"
    ) |> dplyr::transmute(path, model = "xgboost")
) |>
    tidyr::expand_grid(
        misclass_df |>
            dplyr::select(adjustment) |>
            dplyr::distinct() |>
            tibble::add_case(adjustment = "unadjusted")
    ) |>
    dplyr::mutate(batch = as.integer(stringr::str_extract(path, "[0-9]{3}"))) |>
    dplyr::arrange(model, adjustment, batch)

### Now we go over each row and summarize bootstraps ----
for (i in sample(1:NROW(par_grid))) {
    BATCH_X <- par_grid$batch[i]
    MODEL_X <- par_grid$model[i]
    MISCLASS_X <- par_grid$adjustment[i]
    PATH_X <- par_grid$path[i]

    f_path <- here::here(
        "output",
        "out_of_hosp",
        "summaries",
        MISCLASS_X,
        MODEL_X,
        sprintf("%s_%s_batch%03d.RDS", MODEL_X, MISCLASS_X, BATCH_X)
    )

    if (!fs::file_exists(f_path)) {
        print(sprintf("Starting: %s (%s)", basename(f_path), round(Sys.time())))
        fs::dir_create(dirname(f_path))

        ### Get list of of hospital predictions ----
        boot_paths <- fs::dir_ls(PATH_X,
            recurse = TRUE,
            glob = "*.RDS"
        )

        tictoc::tic("starting collection")
        doParallel::registerDoParallel(N_CORES)
        boots_df <- foreach::foreach(f = boot_paths, .inorder = FALSE) %dopar% {
            ### Read in a bootstrap ----
            temp_x <- readRDS(f)

            ### Reassign COVID based on predictions and bin ----
            temp_x$predictions <- temp_x$predictions |>
                dplyr::mutate(has_covid = ifelse(.pred_has_covid > .5, 1, 0))

            ### Misclassification switches ----
            ### only if we are doing a misclassification adjustment
            if (MISCLASS_X != "unadjusted") {
                ## Extract the false discovery and false omission rates
                fdr_prob <- misclass_df |>
                    dplyr::filter(adjustment == MISCLASS_X) |>
                    dplyr::filter(metric == "false_discovery_rate") |>
                    dplyr::pull(switching_probability)

                for_prob <- misclass_df |>
                    dplyr::filter(adjustment == MISCLASS_X) |>
                    dplyr::filter(metric == "false_omission_rate") |>
                    dplyr::pull(switching_probability)

                holder <- vector("list", 2)

                ## If predicted to have COVID-19, binomial draw from FDR to
                ## switch it from 1 to 0
                holder[[1]] <- temp_x$predictions |>
                    dplyr::filter(has_covid == 1) |>
                    dplyr::mutate(has_covid = ifelse(
                        stats::rbinom(dplyr::n(), 1, fdr_prob) == 1,
                        1 - has_covid,
                        has_covid
                    ))

                ## If predicted not to have COVID-19, binomial draw from FOR to
                ## switch it from 0 to 1
                holder[[2]] <- temp_x$predictions |>
                    dplyr::filter(has_covid == 0) |>
                    dplyr::mutate(has_covid = ifelse(
                        stats::rbinom(dplyr::n(), 1, for_prob) == 1,
                        1 - has_covid,
                        has_covid
                    ))

                ## Put the randomly split predictions back
                temp_x$predictions <- dplyr::bind_rows(holder)
            }

            ### By age group ----
            age_bins <- boot_summarize(
                temp_x,
                sub_df |>
                    create_age_bins(),
                age_bin
            ) |>
                dplyr::mutate(age_group = "all")

            ### Total population ----
            total <- boot_summarize_by_age(
                temp_x,
                sub_df |>
                    dplyr::mutate(total = "total"),
                age_df,
                total
            )

            ### Geographic columns (across all ages) ----
            county_fips_cat <- boot_summarize(temp_x, sub_df, county_fips_cat) |>
                dplyr::mutate(age_group = "all")
            division <- boot_summarize(temp_x, sub_df, division) |>
                dplyr::mutate(age_group = "all")
            state_cat <- boot_summarize(temp_x, sub_df, state_cat) |>
                dplyr::mutate(age_group = "all")

            ### By individual/county columns (by age groups) ----
            weekday_cat <-
                boot_summarize_by_age(temp_x, sub_df, age_df, weekday_cat)
            month_into_pandemic <-
                boot_summarize_by_age(temp_x, sub_df, age_df, month_into_pandemic)
            educ_cat <-
                boot_summarize_by_age(temp_x, sub_df, age_df, educ_cat)
            marital_cat <-
                boot_summarize_by_age(temp_x, sub_df, age_df, marital_cat)
            sex_cat <-
                boot_summarize_by_age(temp_x, sub_df, age_df, sex_cat)
            racerbig_recode <-
                boot_summarize_by_age(temp_x, sub_df, age_df, racerbig_recode)
            rucc_2013_cat <-
                boot_summarize_by_age(temp_x, sub_df, age_df, rucc_2013_cat)
            percent_black <-
                boot_summarize_by_age(temp_x, sub_df, age_df, percent_black)
            home_ownership <-
                boot_summarize_by_age(temp_x, sub_df, age_df, home_ownership)
            household_income <-
                boot_summarize_by_age(temp_x, sub_df, age_df, household_income)
            income_inequality <-
                boot_summarize_by_age(temp_x, sub_df, age_df, income_inequality)
            some_college <-
                boot_summarize_by_age(temp_x, sub_df, age_df, some_college)
            poor_or_fair_health <-
                boot_summarize_by_age(temp_x, sub_df, age_df, poor_or_fair_health)
            smoking <-
                boot_summarize_by_age(temp_x, sub_df, age_df, smoking)
            obesity <-
                boot_summarize_by_age(temp_x, sub_df, age_df, obesity)
            diabetes <-
                boot_summarize_by_age(temp_x, sub_df, age_df, diabetes)
            percent_65_over <-
                boot_summarize_by_age(temp_x, sub_df, age_df, percent_65_over)
            place_cert <-
                boot_summarize_by_age(temp_x, sub_df, age_df, place_cert)
            placdth_str <-
                boot_summarize_by_age(temp_x, sub_df, age_df, placdth_str)
            certifier_str <-
                boot_summarize_by_age(temp_x, sub_df, age_df, certifier_str)

            ### Gather summaries into a list ----
            list(
                total = total,
                age_bin = age_bins,
                county_fips_cat = county_fips_cat,
                weekday_cat = weekday_cat,
                division = division,
                month_into_pandemic = month_into_pandemic,
                state_cat = state_cat,
                educ_cat = educ_cat,
                marital_cat = marital_cat,
                sex_cat = sex_cat,
                racerbig_recode = racerbig_recode,
                rucc_2013_cat = rucc_2013_cat,
                percent_black = percent_black,
                home_ownership = home_ownership,
                household_income = household_income,
                income_inequality = income_inequality,
                some_college = some_college,
                poor_or_fair_health = poor_or_fair_health,
                smoking = smoking,
                obesity = obesity,
                diabetes = diabetes,
                percent_65_over = percent_65_over,
                place_cert = place_cert,
                placdth_str = placdth_str,
                certifier_str = certifier_str
            )
        }

        gather_timer <- tictoc::toc(quiet = TRUE)
        gather_time <- as.numeric(gather_timer$toc - gather_timer$tic)

        ### Save ----
        saveRDS_xz(
            list(
                boots_df = boots_df,
                timer = gather_time
            ),
            f_path
        )

        rm(boots_df)

        ### Close out ----
        doParallel::stopImplicitCluster()
        closeAllConnections()
        unregister_dopar()
    } else {
        print(sprintf("Skipping :%s (%s)", basename(f_path), Sys.time()))
    }
}

## Step 3: Get counts for in- vs out-of-hospital deaths ----
weights_list <- list(
    ### By age group ----
    age_bin = weight_summarize(
        sub_train |>
            create_age_bins(),
        age_bin
    ) |>
        dplyr::mutate(age_group = "all"),
    ### Total population ----
    total = weight_summarize_by_age(
        sub_train |>
            dplyr::mutate(total = "total"),
        age_df,
        total
    ),
    ### Geographic columns (across all ages) ----
    county_fips_cat = weight_summarize(sub_train, county_fips_cat) |>
        dplyr::mutate(age_group = "all"),
    division = weight_summarize(sub_train, division) |>
        dplyr::mutate(age_group = "all"),
    state_cat = weight_summarize(sub_train, state_cat) |>
        dplyr::mutate(age_group = "all"),

    ### By individual/county columns (by age groups) ----
    weekday_cat = weight_summarize_by_age(sub_train, age_df, weekday_cat),
    month_into_pandemic = weight_summarize_by_age(sub_train, age_df, month_into_pandemic),
    educ_cat = weight_summarize_by_age(sub_train, age_df, educ_cat),
    marital_cat = weight_summarize_by_age(sub_train, age_df, marital_cat),
    sex_cat = weight_summarize_by_age(sub_train, age_df, sex_cat),
    racerbig_recode = weight_summarize_by_age(sub_train, age_df, racerbig_recode),
    rucc_2013_cat = weight_summarize_by_age(sub_train, age_df, rucc_2013_cat),
    percent_black = weight_summarize_by_age(sub_train, age_df, percent_black),
    home_ownership = weight_summarize_by_age(sub_train, age_df, home_ownership),
    household_income = weight_summarize_by_age(sub_train, age_df, household_income),
    income_inequality = weight_summarize_by_age(sub_train, age_df, income_inequality),
    some_college = weight_summarize_by_age(sub_train, age_df, some_college),
    poor_or_fair_health = weight_summarize_by_age(sub_train, age_df, poor_or_fair_health),
    smoking = weight_summarize_by_age(sub_train, age_df, smoking),
    obesity = weight_summarize_by_age(sub_train, age_df, obesity),
    diabetes = weight_summarize_by_age(sub_train, age_df, diabetes),
    percent_65_over = weight_summarize_by_age(sub_train, age_df, percent_65_over),
    place_cert = weight_summarize_by_age(sub_train, age_df, place_cert),
    placdth_str = weight_summarize_by_age(sub_train, age_df, placdth_str),
    certifier_str = weight_summarize_by_age(sub_train, age_df, certifier_str)
)

## Step 4: Reassemble bootstrap summaries ----
reassemble_grid <- par_grid |>
    dplyr::select(model, adjustment) |>
    dplyr::distinct()

for (i in 1:NROW(reassemble_grid)) {
    MODEL_X <- reassemble_grid$model[i]
    MISCLASS_X <- reassemble_grid$adjustment[i]

    save_path <- here::here(
        "output",
        "out_of_hosp",
        "summaries",
        sprintf(
            "out_of_hosp_preds_%s_%s.RDS",
            MODEL_X,
            MISCLASS_X
        )
    )

    if (!fs::file_exists(save_path)) {
        tictoc::tic("starting reassembly")

        ### Get file paths ----
        f_paths <- fs::dir_ls(
            here::here(
                "output",
                "out_of_hosp",
                "summaries",
                MISCLASS_X,
                MODEL_X
            ),
            recurse = TRUE,
            regexp = sprintf("\\_%s\\_batch", MISCLASS_X)
        )

        doParallel::registerDoParallel(N_CORES)
        boots_df <- foreach::foreach(
            f = f_paths,
            .inorder = FALSE,
            .combine = c
        ) %dopar% {
            readRDS(f)$boots_df
        }
        #### Close out ----
        doParallel::stopImplicitCluster()
        closeAllConnections()
        unregister_dopar()

        ### Reassemble bootstrap results from the list ----
        C_NAMES <- names(boots_df[[1]])
        results <- vector("list", NROW(C_NAMES))
        for (j in 1:NROW(C_NAMES)) {
            #### Focus on each stratification ----
            v <- C_NAMES[j]

            #### Remove each bootstrap summary by stratification ----
            temp_x <- boots_df |>
                lapply(function(x) x[[j]]) |>
                dplyr::bind_rows()

            #### Record stratification variable and reorder ----
            temp_x <- temp_x |>
                dplyr::mutate(grouping_var = v)
            temp_x$grouping_value <- temp_x |>
                dplyr::select(tidyr::all_of(v)) |>
                dplyr::pull() |>
                as.character()
            temp_x <- temp_x |>
                dplyr::select(-tidyr::all_of(v))

            weight_x <- weights_list[[v]] |>
                dplyr::mutate(grouping_var = v)
            weight_x$grouping_value <- weight_x |>
                dplyr::select(tidyr::all_of(v)) |>
                dplyr::pull() |>
                as.character()
            weight_x <- weight_x |>
                dplyr::select(-tidyr::all_of(v))

            #### Take out the "full" (i.e., non bootstrap) results ----
            full_data_prediction <- temp_x |>
                dplyr::filter(batch_id == 0) |>
                dplyr::select(
                    -n_pred_covid,
                    -batch_id,
                    -sim_id
                ) |>
                dplyr::left_join(weight_x) |>
                dplyr::select(
                    model,
                    grouping_var,
                    grouping_value,
                    age_group,
                    n_deaths,
                    n_hosp_deaths,
                    n_covid,
                    n_hosp_covid,
                    n_pred_full,
                    tidyr::everything()
                )

            #### Take out the bootstrap results ----
            bootstrap_predictions <- temp_x |>
                dplyr::filter(batch_id != 0) |>
                dplyr::group_by(model, age_group, grouping_var, grouping_value) |>
                dplyr::summarize(
                    n_sim = dplyr::n(),
                    pred_mean = mean(n_pred_covid),
                    pred_sd = stats::sd(n_pred_covid),
                    pred_min = min(n_pred_covid),
                    pred_max = max(n_pred_covid),
                    pred_p025 = stats::quantile(n_pred_covid, .025),
                    pred_p250 = stats::quantile(n_pred_covid, .25),
                    pred_p500 = stats::quantile(n_pred_covid, .5),
                    pred_p750 = stats::quantile(n_pred_covid, .75),
                    pred_p975 = stats::quantile(n_pred_covid, .975)
                ) |>
                dplyr::ungroup()

            #### Reassemble the full and bootstrap predictions ----
            results[[j]] <- dplyr::left_join(
                full_data_prediction,
                bootstrap_predictions
            )
        }

        ## Combine and save ----
        results <- results |>
            dplyr::bind_rows() |>
            dplyr::mutate(axis_lab = paste(grouping_var, grouping_value)) |>
            dplyr::mutate(
                misclassification_adjustment = MISCLASS_X
            )

        gather_timer <- tictoc::toc(quiet = TRUE)
        gather_time <- as.numeric(gather_timer$toc - gather_timer$tic)

        saveRDS(
            list(
                results = results,
                timer = gather_time
            ),
            save_path,
            compress = "xz"
        )
    }
}

## Step 5: Combine and save results ----
if (!fs::file_exists(here::here("data", "out_of_hosp_predictions.RDS"))) {
    all_pred_results <- purrr::map_dfr(
        .x = fs::dir_ls(
            here::here("output", "out_of_hosp", "summaries"),
            regexp = "out_of_hosp_preds",
            type = "file"
        ),
        .f = ~ readRDS(.x)$results
    ) |>
        dplyr::select(
            model,
            misclassification_adjustment,
            grouping_var,
            grouping_value,
            axis_lab,
            age_group,
            tidyr::everything()
        )

    saveRDS_xz(
        all_pred_results,
        here::here("data", "out_of_hosp_predictions.RDS")
    )
}
