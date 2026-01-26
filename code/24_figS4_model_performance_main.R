## Imports ----
library(here)
library(tidyverse)
source(here::here("code", "mk_nytimes.R"))

## Load fonts (Arial Narrow) ----
extrafont::loadfonts(device = "all")

## Constants ----
MAIN_METRICS <- c("roc_auc", "sens", "spec", "accuracy")

## Data ----
summarized_metrics <- readRDS(here::here("data", "model_metrics.RDS"))
sub_metrics <- summarized_metrics |>
    dplyr::filter(model_type != "null_model",
                  metric %in% MAIN_METRICS,
                  !is.na(metric_cat))

## Figure ----
p1 <- ggplot2::ggplot(
    sub_metrics,
    ggplot2::aes(
        x = mean,
        xmin = min,
        xmax = max,
        y = model_pre_cat_rev,
        color = model_cat,
    )
) +
    ggplot2::annotate(
        "rect",
        xmin = rep(-Inf, 2),
        xmax = rep(Inf, 2),
        ymin = c(.5, 8.5),
        ymax = c(4.5, 12.5),
        fill = "black",
        color = NA,
        alpha = .1
    ) +
    ggplot2::geom_errorbarh(height = 0, alpha = .9) +
    ggplot2::geom_point(alpha = .9) +
    ggplot2::facet_grid( ~ metric_cat, scales = "free_x") +
    ggplot2::scale_y_discrete("Model, covariate set") +
    ggplot2::scale_x_continuous("Average metric value (min, max)") +
    ggplot2::scale_color_brewer("Model type", palette = "Dark2") +
    mk_nytimes(legend.position = "none")

## Save ----
ggplot2::ggsave(
    here::here("plots", "figS4_model_performance_main.pdf"),
    p1,
    width = 10,
    height = 4,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS4_model_performance_main.jpg"),
    p1,
    width = 10,
    height = 4,
    dpi = 1200
)
write_csv(
    sub_metrics,
    here(
        "output",
        "figure_data",
        "figS4_data_model_performance_main.csv"
    )
)
