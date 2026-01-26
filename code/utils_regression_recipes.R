return_regression_recipes <- function(train_df) {
    ## Create a few preprocessing pipelines ----
    naive_recipe <- recipes::recipe(has_covid_cat ~ .,
        data = train_df
    ) |>
        recipes::step_rm(tidyr::any_of(c("row_id"))) |>
        recipes::step_rm(tidyr::any_of(c("has_covid"))) |>
        recipes::step_impute_mean(recipes::all_numeric_predictors()) |>
        recipes::step_dummy(recipes::all_nominal_predictors()) |>
        recipes::step_poly(tidyr::any_of("age_years"), degree = 3) |>
        recipes::step_normalize(
            recipes::all_numeric_predictors(),
            -tidyr::matches(".+\\_cat")
        ) |>
        themis::step_downsample(has_covid_cat)

    noucr_recipe <- recipes::recipe(has_covid_cat ~ .,
        data = train_df
    ) |>
        recipes::step_rm(tidyr::any_of(c("row_id"))) |>
        recipes::step_rm(tidyr::any_of(c("has_covid"))) |>
        recipes::step_rm(tidyr::starts_with("ucr113_")) |>
        recipes::step_impute_mean(recipes::all_numeric_predictors()) |>
        recipes::step_dummy(recipes::all_nominal_predictors()) |>
        recipes::step_poly(tidyr::any_of("age_years"), degree = 3) |>
        recipes::step_normalize(
            recipes::all_numeric_predictors(),
            -tidyr::matches(".+\\_cat")
        ) |>
        themis::step_downsample(has_covid_cat)

    nocounty_recipe <- recipes::recipe(has_covid_cat ~ .,
        data = train_df
    ) |>
        recipes::step_rm(tidyr::any_of(c("row_id"))) |>
        recipes::step_rm(tidyr::any_of(c("county_fips_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("has_covid"))) |>
        recipes::step_impute_mean(recipes::all_numeric_predictors()) |>
        recipes::step_dummy(recipes::all_nominal_predictors()) |>
        recipes::step_poly(tidyr::any_of("age_years"), degree = 3) |>
        recipes::step_normalize(
            recipes::all_numeric_predictors(),
            -tidyr::matches(".+\\_cat")
        ) |>
        themis::step_downsample(has_covid_cat)

    nocounty_noucr_recipe <- recipes::recipe(has_covid_cat ~ .,
        data = train_df
    ) |>
        recipes::step_rm(tidyr::any_of(c("row_id"))) |>
        recipes::step_rm(tidyr::any_of(c("county_fips_cat"))) |>
        recipes::step_rm(tidyr::any_of(c("has_covid"))) |>
        recipes::step_rm(tidyr::starts_with("ucr113_")) |>
        recipes::step_impute_mean(recipes::all_numeric_predictors()) |>
        recipes::step_dummy(recipes::all_nominal_predictors()) |>
        recipes::step_poly(tidyr::any_of("age_years"), degree = 3) |>
        recipes::step_normalize(
            recipes::all_numeric_predictors(),
            -tidyr::matches(".+\\_cat")
        ) |>
        themis::step_downsample(has_covid_cat)

    ### Create a list of recipes ----
    list(
        "naive" = naive_recipe,
        "noucr" = noucr_recipe,
        "nocounty" = nocounty_recipe,
        "nocountynoucr" = nocounty_noucr_recipe
    )
}
