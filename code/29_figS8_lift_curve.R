## Imports ----
library(here)
library(tidyverse)
source(here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
performance_curvs <- readRDS(here("data", "performance_curves.RDS"))

## Lift curve ----
## Lift curves show you how much you gain from using the model. The y-axis is
## the ratio of cases detected over the cases expected to be detected by
## chance alone. The x-axis shows the proportion of data being tested.
lift_curv <- performance_curvs |>
    select(pref_model, model, lift_curv) |>
    unnest(cols = everything())

m_labels <- c("Main model",
              "Reference models\n(logistic regression)",
              "Other models")

lift_curv <- lift_curv |>
    mutate(pref_cat = factor(
        pref_model,
        levels = c("main model", "reference model", "other model"),
        labels = m_labels,
        ordered = TRUE
    ))


fig_lift <- ggplot(
    lift_curv,
    aes(
        x = .percent_tested,
        y = .lift,
        color = pref_cat,
        group = model,
        linewidth = pref_cat,
        alpha = pref_cat
    )
) +
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_line() +
    scale_x_continuous("Observations Tested (%)", expand = c(0, .0125)) +
    scale_y_continuous("Lift", expand = c(0, .0125)) +
    scale_color_manual(NULL,
                       values = c(ggsci::pal_aaas()(2)[2:1], "grey10"),
                       labels = m_labels) +
    scale_alpha_manual(NULL, values = c(1, .75, .25), labels = m_labels) +
    scale_linewidth_manual(NULL, values = c(1.5, .75, .5), labels = m_labels) +
    mk_nytimes(
        legend.position = c(.99, .99),
        legend.justification = c(1, 1),
        axis.text.x = element_text(hjust = c(0, .5, .5, .5, 1))
    )

## Save ----
ggsave(
    here("plots", "figS8_lift_curve.pdf"),
    fig_lift,
    width = 5,
    height = 5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS8_lift_curve.jpg"),
    fig_lift,
    width = 5,
    height = 5,
    scale = 1,
    dpi = 1200
)
