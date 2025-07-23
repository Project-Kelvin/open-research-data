library(tidyverse)
library(ggplot2)
library(gridExtra)

data  <- read.csv("experiments/experiment_server_20_07_2024/data.csv")
pfs  <- read.csv("experiments/experiment_server_20_07_2024/pfs.csv")

ar_plot <- ggplot(data, aes(x = generation, y = average_ar)) +
    geom_ribbon(aes(ymin = min_ar, ymax = max_ar), fill="gray80", alpha=0.5) +
    geom_line(size=1.5, color="blue") +
    theme_minimal() +
    theme(axis.title = element_text(size = 16), plot.title = element_text(size=20, hjust=1.5), axis.text = element_text(size=16), plot.background = element_rect(fill="white")) +
    labs(
       x = "Generation",
       y = "Acceptance ratio",
       title = "a) Acceptance ratio over generations") +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10))

latency_plot <- ggplot(data, aes(x = generation, y = average_latency)) +
    geom_ribbon(aes(ymin = min_latency, ymax = max_latency), fill = "gray80", alpha = 0.5) +
    geom_line(size = 1.5, color = "blue") +
    theme_minimal() +
    theme(axis.title = element_text(size = 16), plot.title = element_text(size = 20, hjust=1.5), axis.text = element_text(size = 16), plot.background = element_rect(fill = "white")) +
    labs(
        x = "Generation",
        y = "Average latency (ms)",
        title = "b) Average latency over generations") +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10))

plots = grid.arrange(ar_plot, latency_plot, ncol=2)
ggsave("experiments/experiment_server_20_07_2024/ar_lat_plots.png", plots, width=10, height=5, dpi=300)
