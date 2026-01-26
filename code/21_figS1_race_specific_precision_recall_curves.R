## Imports ----
library(here)
library(tidyverse)
library(ggsci)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Data ----
pr_obj <- readRDS(here::here("data", "pr_curves_grouped.RDS"))

## Precision-recall curve ----
pr_curv <- pr_obj$holder
pr_auc <- pr_obj$pr_auc |>
    mutate(race_cat = factor(group,
        levels = c(
            "aian",
            "asian",
            "asian_indian", "black",
            "multiracial",
            "pacific_islander",
            "white"
        ),
        labels = c(
            "American Indian / Alaska Native",
            "Asian",
            "Asian Indian",
            "Black",
            "Multiracial",
            "Pacific Islander",
            "White"
        ),
        ordered = TRUE
    )) |> 
    mutate(labels = sprintf("%s (%0.2f)", race_cat, round(group_pr_auc, 2))) |> 
    mutate(ordered_labels = factor(labels, ordered = TRUE))

pr_curv <- pr_curv |> 
    left_join(pr_auc)

fig_pr <- ggplot(
    pr_curv,
    aes(
        x = recall,
        y = precision,
        color = ordered_labels,
        group = ordered_labels
    )
) +
    geom_path() +
    coord_equal() +
    scale_x_continuous("Recall (Sensitivity)",
                       expand = c(0, .0125),
                       limits = c(0, 1)) +
    scale_y_continuous(
        "Precision (Positive Predictive Value)",
        expand = c(0, .0125),
        limits = c(0, 1)
    ) +
    scale_color_aaas(name = "Racial category (AUC PR)") +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = c(0, 0),
        # legend.position = "none",
        legend.justification = c(0, 0)
    )

## Save ----
ggsave(
    here("plots", "figS1_race_specific_pr_curve.pdf"),
    fig_pr,
    width = 5,
    height = 5,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggsave(
    here("plots", "figS1_race_specific_pr_curve.jpg"),
    fig_pr,
    width = 5,
    height = 5,
    scale = 1,
    dpi = 1200
)
