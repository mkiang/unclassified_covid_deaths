## Imports ----
library(here)
library(tidyverse)
source(here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
performance_curvs <- readRDS(here("data", "performance_curves.RDS"))

## Gain curve ----
## How does the model perform above random chance and compared to
## a perfect classifier?
gain_curv <- performance_curvs |>
    select(pref_model, model, gain_curv, gain_auc_val) |>
    unnest(cols = everything())
gain_cap <- gain_curv |>
    filter(pref_model == "main model") |>
    pull(.estimate) |>
    unique()

m_labels <- c(
    sprintf("Main model\n(Gain AUC: %0.3f)", round(gain_cap, 3)),
    "Reference models",
    "Other models",
    "Perfect model",
    "Random"
)

gain_curv <- gain_curv |>
    mutate(pref_cat = factor(
        pref_model,
        levels = c(
            "main model",
            "reference model",
            "other model",
            "perfect",
            "random"
        ),
        labels = m_labels,
        ordered = TRUE
    ))

poly_df <- gain_curv |>
    filter(pref_model == "main model") |>
    summarize(slope = 1 / (max(.n_events) / last(.n))) |>
    mutate(perfect = 100 / slope) |>
    dplyr::do(dplyr::tibble(x = c(0, .$perfect, 100), y = c(0, 100, 100))) |>
    mutate(pref_model = "perfect") |>
    add_case(pref_model = "random",
             x = c(0, 100),
             y = c(0, 100)) |>
    mutate(pref_cat = factor(
        pref_model,
        levels = c(
            "main model",
            "reference model",
            "other model",
            "perfect",
            "random"
        ),
        labels = m_labels,
        ordered = TRUE
    ))

fig_gain <- ggplot() +
    geom_polygon(
        data = poly_df |>
            filter(pref_model == "perfect"),
        aes(x, y),
        fill = "black",
        alpha = .1,
        color = NA
    ) +
    geom_line(data = poly_df,
              aes(
                  x,
                  y,
                  color = pref_cat,
                  linewidth = pref_cat,
                  group = pref_cat
              )) +
    geom_line(
        data = gain_curv,
        aes(
            x = .percent_tested,
            y = .percent_found,
            color = pref_cat,
            group = model,
            linewidth = pref_cat,
            alpha = pref_cat
        )
    ) +
    geom_line() +
    scale_x_continuous("Observations Tested (%)", expand = c(0, .25)) +
    scale_y_continuous("Cases Found (%)", expand = c(0, .25)) +
    scale_color_manual(
        NULL,
        values = c(
            ggsci::pal_aaas()(2)[2:1],
            "grey10",
            RColorBrewer::brewer.pal(7, "Set1")[4:5]
        ),
        labels = m_labels,
        drop = FALSE
    ) +
    scale_alpha_manual(NULL, values = c(1, .5, .25, .8, .8), labels = m_labels) +
    scale_linewidth_manual(NULL,
                           values = c(2, .75, .25, .75, .75),
                           labels = m_labels) +
    coord_equal() +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0)
    ) +
    guides(alpha = "none")

## Save ----
ggsave(
    here("plots", "figS9_gain_curve.pdf"),
    fig_gain,
    width = 5,
    height = 5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS9_gain_curve.jpg"),
    fig_gain,
    width = 5,
    height = 5,
    scale = 1,
    dpi = 1200
)
