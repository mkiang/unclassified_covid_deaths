## Imports ----
library(here)
library(tidyverse)
source(here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
performance_curvs <- readRDS(here("data", "performance_curves.RDS"))

## ROC curve ----
## Basically answers "How well can our model separate classes?". For
## every threshold, calculates the true and false positive rates. The closer
## to the upper left, the better the model is at distinguishing between the
## classes. The worse performance is the identify line. For reference, I put
## a dot at the traditional threshold of .5 on the main model.
roc_curv <- performance_curvs |>
    select(pref_model, model, roc_curv, roc_auc_val) |>
    unnest(cols = everything())
roc_auc_main <- roc_curv |>
    filter(pref_model == "main model") |>
    pull(.estimate) |>
    unique()

m_labels <- c(
    sprintf("Main model\n(ROC AUC: %0.3f)", round(roc_auc_main, 3)),
    "Reference models\n(logistic regression)",
    "Other models"
)

roc_curv <- roc_curv |>
    mutate(pref_cat = factor(
        pref_model,
        levels = c("main model", "reference model", "other model"),
        labels = m_labels,
        ordered = TRUE
    ))

fig_roc <- ggplot(
    roc_curv,
    aes(
        x = 1 - specificity,
        y = sensitivity,
        color = pref_cat,
        group = model,
        linewidth = pref_cat,
        alpha = pref_cat
    )
) +
    geom_line() +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dotted") +
    geom_point(
        data = roc_curv |>
            filter(pref_model == "main model") |>
            filter(is.finite(.threshold)) |>
            mutate(abs_diff = abs(.5 - .threshold)) |>
            slice_min(abs_diff),
        aes(x = 1 - specificity, y = sensitivity),
        shape = 21,
        color = "black",
        fill = "white",
        size = 3
    ) +
    coord_equal() +
    scale_x_continuous("1 - Specificity (False Positive Rate)", expand = c(0, .0125)) +
    scale_y_continuous("Sensitivity (True Positive Rate)", expand = c(0, .0125)) +
    scale_color_manual(NULL,
                       values = c(ggsci::pal_aaas()(2)[2:1], "grey10"),
                       labels = m_labels) +
    scale_alpha_manual(NULL, values = c(1, .75, .25), labels = m_labels) +
    scale_linewidth_manual(NULL, values = c(1.5, .75, .5), labels = m_labels) +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0)
    )

## Save ----
ggsave(
    here("plots", "figS6_roc_curve.pdf"),
    fig_roc,
    width = 5,
    height = 5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS6_roc_curve.jpg"),
    fig_roc,
    width = 5,
    height = 5,
    scale = 1,
    dpi = 1200
)
