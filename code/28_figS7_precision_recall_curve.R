## Imports ----
library(here)
library(tidyverse)
source(here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
performance_curvs <- readRDS(here("data", "performance_curves.RDS"))

## Precision-recall curve ----
pr_curv <- performance_curvs |>
    select(pref_model, model, pr_curv, pr_auc_val) |>
    unnest(cols = everything())
pr_auc <- pr_curv |>
    filter(pref_model == "main model") |>
    pull(.estimate) |>
    unique()

m_labels <- c(
    sprintf("Main model\n(PR AUC: %0.3f)", round(pr_auc, 3)),
    "Reference models\n(logistic regression)",
    "Other models"
)

pr_curv <- pr_curv |>
    mutate(pref_cat = factor(
        pref_model,
        levels = c("main model", "reference model", "other model"),
        labels = m_labels,
        ordered = TRUE
    ))

fig_pr <- ggplot(
    pr_curv,
    aes(
        x = recall,
        y = precision,
        color = pref_cat,
        group = model,
        linewidth = pref_cat,
        alpha = pref_cat
    )
) +
    geom_line() +
    geom_point(
        data = pr_curv |>
            filter(pref_model == "main model") |>
            filter(is.finite(.threshold)) |>
            mutate(abs_diff = abs(.5 - .threshold)) |>
            slice_min(abs_diff),
        aes(x = recall, y = precision),
        shape = 21,
        color = "black",
        fill = "white",
        size = 3
    ) +
    coord_equal() +
    scale_x_continuous("Recall (Sensitivity)",
                       expand = c(0, .0125),
                       limits = c(0, 1)) +
    scale_y_continuous(
        "Precision (Positive Predictive Value)",
        expand = c(0, .0125),
        limits = c(0, 1)
    ) +
    scale_color_manual(NULL,
                       values = c(ggsci::pal_aaas()(2)[2:1], "grey10"),
                       labels = m_labels) +
    scale_alpha_manual(NULL, values = c(1, .75, .25), labels = m_labels) +
    scale_linewidth_manual(NULL, values = c(1.5, .75, .5), labels = m_labels) +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = c(0, 0),
        # legend.position = "none",
        legend.justification = c(0, 0)
    )

## Save ----
ggsave(
    here("plots", "figS7_pr_curve.pdf"),
    fig_pr,
    width = 5,
    height = 5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS7_pr_curve.jpg"),
    fig_pr,
    width = 5,
    height = 5,
    scale = 1,
    dpi = 1200
)
