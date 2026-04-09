library(tidyverse)
library(ggplot2)
library(ggbreak)
library(gridExtra)
library(grid) # Needed for textGrob and gpar
library(paletteer)

data <- read.csv("research-analysis/surrogate/v3/predictions.csv")

ggplot(data, aes(x = max_cpu, y = max_link_score, color = latency)) +
    geom_point(size = 5) +
    theme_light() +
    labs(x = "Maximum CPU Demand", y = "Maximum Bandwidth Demand", color = "Actual Traffic Latency (ms)") +
    scale_colour_viridis_c(limits = c(0, 1000), oob = scales::squish, guide = guide_colorbar(title.position = "right", barheight = 10)) +
    theme(text = element_text(size = 14, family = "sans"), axis.text = element_text(size = 14), axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
    scale_y_continuous(breaks = seq(0, 600, by = 50)) +
    theme(
        legend.title = element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "#e9e7e7"),
        legend.background = element_rect(fill = "#e9e7e7"),
        legend.key = element_rect(fill = "#e9e7e7"),
        panel.background = element_rect(fill = "#e9e7e7"),
        text = element_text(size = 14, family = "sans", color = "#29454a"),
        axis.text = element_text(size = 12, color = "#29454a"),
        axis.title = element_text(size = 14, color = "#29454a"),
    )

ggsave("research-analysis/surrogate/v3/actual_latency_plot.png", width = 7, height = 4)

ggplot(data, aes(x = max_cpu, y = max_link_score, color = PredictedLatency)) +
    geom_point(size = 5) +
    theme_light() +
    labs(x = "Maximum CPU Demand", y = "Maximum Bandwidth Demand", color = "Predicted Traffic Latency (ms)") +
    scale_colour_viridis_c(limits = c(0, 1000), oob = scales::squish, guide = guide_colorbar(title.position = "right", barheight = 10)) +
    theme(text = element_text(size = 14, family = "sans"), axis.text = element_text(size = 14), axis.text.x = element_text(angle = 90)) +
    scale_x_continuous(breaks = seq(0, 2, by = 0.2)) +
    scale_y_continuous(breaks = seq(0, 600, by = 50)) +
    theme(
        legend.title = element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "#e9e7e7"),
        legend.background = element_rect(fill = "#e9e7e7"),
        legend.key = element_rect(fill = "#e9e7e7"),
        panel.background = element_rect(fill = "#e9e7e7"),
        text = element_text(size = 14, family = "sans", color = "#29454a"),
        axis.text = element_text(size = 12, color= "#29454a"),
        axis.title = element_text(size = 14, color= "#29454a"),
    )

ggsave("research-analysis/surrogate/v3/predicted_latency_plot.png", width = 7, height = 4)
