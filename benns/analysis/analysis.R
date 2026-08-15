library(tidyverse)
library(ggplot2)
library(ggbreak)
library(gridExtra)
library(grid) # Needed for textGrob and gpar

hybrid_baseline_data <- read.csv("experiments/surrogate_experiments/bega_2000_hybrid/v3/8_0.1_False_10_2/data.csv")
hybrid_bw_5_data <- read.csv("experiments/surrogate_experiments/bega_2000_hybrid/v3/8_0.1_False_5_2/data.csv")
hybrid_cpus_1_data <- read.csv("experiments/surrogate_experiments/bega_2000_hybrid/v3/8_0.1_False_10_1/data.csv")
hybrid_traffic_b_data <- read.csv("experiments/surrogate_experiments/bega_2000_hybrid/v3/8_0.1_True_10_2/data.csv")
hybrid_traffic_scale_2_data <- read.csv("experiments/surrogate_experiments/bega_2000_hybrid/v3/8_0.2_False_10_2/data.csv")
online_baseline_data <- read.csv("experiments/surrogate_experiments/bega_online/v3/8_0.1_False_10_2/data.csv")
online_bw_5_data <- read.csv("experiments/surrogate_experiments/bega_online/v3/8_0.1_False_5_2/data.csv")
online_cpus_1_data <- read.csv("experiments/surrogate_experiments/bega_online/v3/8_0.1_False_10_1/data.csv")
online_traffic_b_data <- read.csv("experiments/surrogate_experiments/bega_online/v3/8_0.1_True_10_2/data.csv")
online_traffic_scale_2_data <- read.csv("experiments/surrogate_experiments/bega_online/v3/8_0.2_False_10_2/data.csv")
gaha_baseline_data <- read.csv("experiments/genesis_experiments/gaha/8_0.1_False_10_2/data.csv")
gaha_bw_5_data <- read.csv("experiments/genesis_experiments/gaha/8_0.1_False_5_2/data.csv")
gaha_cpus_1_data <- read.csv("experiments/genesis_experiments/gaha/8_0.1_False_10_1/data.csv")
gaha_traffic_b_data <- read.csv("experiments/genesis_experiments/gaha/8_0.1_True_10_2/data.csv")
gaha_traffic_scale_2_data <- read.csv("experiments/genesis_experiments/gaha/8_0.2_False_10_2/data.csv")
genesis_baseline_data <- read.csv("experiments/genesis_experiments/genesis/8_0.1_False_10_2/data.csv")
genesis_bw_5_data <- read.csv("experiments/genesis_experiments/genesis/8_0.1_False_5_2/data.csv")
genesis_cpus_1_data <- read.csv("experiments/genesis_experiments/genesis/8_0.1_False_10_1/data.csv")
genesis_traffic_b_data <- read.csv("experiments/genesis_experiments/genesis/8_0.1_True_10_2/data.csv")
genesis_traffic_scale_2_data <- read.csv("experiments/genesis_experiments/genesis/8_0.2_False_10_2/data.csv")
hybrid_100_baseline_data <- read.csv("experiments/genesis_experiments/ga_hybrid_100/8_0.1_False_10_2/data.csv")
hybrid_100_bw_5_data <- read.csv("experiments/genesis_experiments/ga_hybrid_100/8_0.1_False_5_2/data.csv")
hybrid_100_cpus_1_data <- read.csv("experiments/genesis_experiments/ga_hybrid_100/8_0.1_False_10_1/data.csv")
hybrid_100_traffic_b_data <- read.csv("experiments/genesis_experiments/ga_hybrid_100/8_0.1_True_10_2/data.csv")
hybrid_100_traffic_scale_2_data <- read.csv("experiments/genesis_experiments/ga_hybrid_100/8_0.2_False_10_2/data.csv")

online_data <- bind_rows(
  mutate(online_baseline_data, experiment = "Baseline"),
  mutate(online_bw_5_data, experiment = "Bandwidth"),
  mutate(online_cpus_1_data, experiment = "CPU"),
  mutate(online_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(online_traffic_scale_2_data, experiment = "Traffic Scale"),
)

hybrid_data <- bind_rows(
  mutate(hybrid_baseline_data, experiment = "Baseline"),
  mutate(hybrid_bw_5_data, experiment = "Bandwidth"),
  mutate(hybrid_cpus_1_data, experiment = "CPU"),
  mutate(hybrid_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(hybrid_traffic_scale_2_data, experiment = "Traffic Scale"),
) %>%
  mutate(generation = ifelse(method == "emulator", generation + 1, generation))

hybrid_100_data <- bind_rows(
  mutate(hybrid_100_baseline_data, experiment = "Baseline"),
  mutate(hybrid_100_bw_5_data, experiment = "Bandwidth"),
  mutate(hybrid_100_cpus_1_data, experiment = "CPU"),
  mutate(hybrid_100_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(hybrid_100_traffic_scale_2_data, experiment = "Traffic Scale"),
) %>%
  mutate(generation = ifelse(method == "emulator", generation + 1, generation))

gaha_data <- bind_rows(
  mutate(gaha_baseline_data, experiment = "Baseline"),
  mutate(gaha_bw_5_data, experiment = "Bandwidth"),
  mutate(gaha_cpus_1_data, experiment = "CPU"),
  mutate(gaha_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(gaha_traffic_scale_2_data, experiment = "Traffic Scale"),
) %>%
  mutate(generation = as.integer(round(generation))) %>%
  mutate(average_ar = avg_ar) %>%
  mutate(average_latency = avg_latency)

genesis_data <- bind_rows(
  mutate(genesis_baseline_data, experiment = "Baseline"),
  mutate(genesis_bw_5_data, experiment = "Bandwidth"),
  mutate(genesis_cpus_1_data, experiment = "CPU"),
  mutate(genesis_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(genesis_traffic_scale_2_data, experiment = "Traffic Scale"),
) %>%
  mutate(generation = as.integer(round(generation))) %>%
  mutate(generation = ifelse(method == "emulator", generation + 1, generation))

# Hybrid AR plots
hybrid_baseline_ar_gg <- ggplot(hybrid_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#003f5c") +
  geom_vline(xintercept = 34, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_bandwidth_ar_gg <- ggplot(hybrid_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = 11, linetype = "dashed", color = "#323232") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_cpus_ar_gg <- ggplot(hybrid_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = 16, linetype = "dashed", color = "#323232") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_traffic_b_ar_gg <- ggplot(hybrid_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = 12, linetype = "dashed", color = "#323232") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_traffic_scale_2_ar_gg <- ggplot(hybrid_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = 14, linetype = "dashed", color = "#323232") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

# Hybrid 100 AR plots
hybrid_100_baseline_ar_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_bandwidth_ar_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = 92, linetype = "dashed", color = "#323232") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_cpus_ar_gg <- ggplot(hybrid_100_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_traffic_b_ar_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_traffic_scale_2_ar_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

# Online AR plots
online_baseline_ar_gg <- ggplot(online_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_bandwidth_ar_gg <- ggplot(online_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_cpus_ar_gg <- ggplot(online_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_traffic_b_ar_gg <- ggplot(online_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_traffic_scale_2_ar_gg <- ggplot(online_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

# GAHA AR plots
gaha_baseline_ar_gg <- ggplot(gaha_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_bandwidth_ar_gg <- ggplot(gaha_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_cpus_ar_gg <- ggplot(gaha_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_traffic_b_ar_gg <- ggplot(gaha_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_traffic_scale_2_ar_gg <- ggplot(gaha_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

# genesis AR plots
genesis_baseline_ar_gg <- ggplot(genesis_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#003f5c") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_bandwidth_ar_gg <- ggplot(genesis_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#444e86") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_cpus_ar_gg <- ggplot(genesis_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#955196") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_traffic_b_ar_gg <- ggplot(genesis_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ff6e54") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_traffic_scale_2_ar_gg <- ggplot(genesis_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = c(1,2,3), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#ffa600") +
  geom_vline(xintercept = 2, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )


ars <- grid.arrange(
  arrangeGrob(
    online_baseline_ar_gg,
    hybrid_baseline_ar_gg,
    hybrid_100_baseline_ar_gg,
    gaha_baseline_ar_gg,
    genesis_baseline_ar_gg,
    nrow = 5,
    top = textGrob("Basic", gp = gpar(fontsize = 12, fontfamily = "Times New Roman" ))
  ),
  arrangeGrob(
    online_bandwidth_ar_gg,
    hybrid_bandwidth_ar_gg,
    hybrid_100_bandwidth_ar_gg,
    gaha_bandwidth_ar_gg,
    genesis_bandwidth_ar_gg,
    nrow = 5,
    top = textGrob("Bandwidth", gp = gpar(fontsize = 12, fontfamily = "Times New Roman" ))
  ),
  arrangeGrob(
    online_cpus_ar_gg,
    hybrid_cpus_ar_gg,
    hybrid_100_cpus_ar_gg,
    gaha_cpus_ar_gg,
    genesis_cpus_ar_gg,
    nrow = 5,
    top = textGrob("CPU", gp = gpar(fontsize = 12, fontfamily = "Times New Roman" ))
  ),
  arrangeGrob(
    online_traffic_b_ar_gg,
    hybrid_traffic_b_ar_gg,
    hybrid_100_traffic_b_ar_gg,
    gaha_traffic_b_ar_gg,
    genesis_traffic_b_ar_gg,
    nrow = 5,
    top = textGrob("Traffic Pattern",
      gp = gpar(fontsize = 12, fontfamily = "Times New Roman" )
    )
  ),
  arrangeGrob(
    arrangeGrob(
      online_traffic_scale_2_ar_gg,
      right = textGrob("BEGA\nOnline", rot = 90, gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      hybrid_traffic_scale_2_ar_gg,
      right = textGrob("BEGA 2000\nHybrid", rot = 90, gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      hybrid_100_traffic_scale_2_ar_gg,
      right = textGrob("BEGA 100\nHybrid", rot = 90, gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      gaha_traffic_scale_2_ar_gg,
      right = textGrob("GAHA\nOffline", rot = 90, gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      genesis_traffic_scale_2_ar_gg,
      right = textGrob("GENESIS\nHybrid", rot = 90, gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    nrow = 5,
    top = textGrob("Traffic Scale", gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
  ),
  ncol = 5,
  widths = c(1.2, 1, 1, 1, 1.4),
  bottom = textGrob("Generation", gp = gpar(fontsize = 12, fontfamily = "Times New Roman")),
  left = textGrob("Acceptance Ratio", rot = 90, gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
)

ggsave("experiments/surrogate_experiments/analysis/v3/acceptance_ratios.png", width = 8, height = 5, plot = ars)

# Hybrid Latency plots
hybrid_baseline_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#003f5c") +
  geom_vline(xintercept = 34, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_bandwidth_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#444e86") +
  geom_vline(xintercept = 11, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_cpus_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#955196") +
  geom_vline(xintercept = 16, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_traffic_b_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#ff6e54") +
  geom_vline(xintercept = 12, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_traffic_scale_2_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#ffa600") +
  geom_vline(xintercept = 14, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )


# Hybrid 100 Latency plots
hybrid_100_baseline_latency_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_bandwidth_latency_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#444e86") +
  geom_vline(xintercept = 92, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_cpus_latency_gg <- ggplot(hybrid_100_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_traffic_b_latency_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_100_traffic_scale_2_latency_gg <- ggplot(hybrid_100_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )


# Online Latency plots
online_baseline_latency_gg <- ggplot(online_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_bandwidth_latency_gg <- ggplot(online_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_cpus_latency_gg <- ggplot(online_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_traffic_b_latency_gg <- ggplot(online_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_traffic_scale_2_latency_gg <- ggplot(online_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

# gaha Latency plots
gaha_baseline_latency_gg <- ggplot(gaha_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 2600)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_bandwidth_latency_gg <- ggplot(gaha_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 2600)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_cpus_latency_gg <- ggplot(gaha_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 2600)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_traffic_b_latency_gg <- ggplot(gaha_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 2600)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

gaha_traffic_scale_2_latency_gg <- ggplot(gaha_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 2600)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

# genesis Latency plots
genesis_baseline_latency_gg <- ggplot(genesis_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50000)) +
  geom_line(color = "#003f5c") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_bandwidth_latency_gg <- ggplot(genesis_data %>% filter(experiment == "Bandwidth"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#444e86", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50000)) +
  geom_line(color = "#444e86") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_cpus_latency_gg <- ggplot(genesis_data %>% filter(experiment == "CPU"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#955196", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50000)) +
  geom_line(color = "#955196") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_traffic_b_latency_gg <- ggplot(genesis_data %>% filter(experiment == "Traffic Pattern"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ff6e54", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50000)) +
  geom_line(color = "#ff6e54") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

genesis_traffic_scale_2_latency_gg <- ggplot(genesis_data %>% filter(experiment == "Traffic Scale"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#ffa600", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(limits = c(1, 50000)) +
  geom_line(color = "#ffa600") +
  geom_vline(xintercept = 2, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

latencies <- grid.arrange(
  arrangeGrob(
    online_baseline_latency_gg,
    hybrid_baseline_latency_gg,
    hybrid_100_baseline_latency_gg,
    gaha_baseline_latency_gg,
    genesis_baseline_latency_gg,
    nrow = 5,
    top = textGrob("Basic",
      gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
  ),
  arrangeGrob(
    online_bandwidth_latency_gg,
    hybrid_bandwidth_latency_gg,
    hybrid_100_bandwidth_latency_gg,
    gaha_bandwidth_latency_gg,
    genesis_bandwidth_latency_gg,
    nrow = 5,
    top = textGrob("Bandwidth",
      gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
  ),
  arrangeGrob(
    online_cpus_latency_gg,
    hybrid_cpus_latency_gg,
    hybrid_100_cpus_latency_gg,
    gaha_cpus_latency_gg,
    genesis_cpus_latency_gg,
    nrow = 5,
    top = textGrob("CPU",
      gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
  ),
  arrangeGrob(
    online_traffic_b_latency_gg,
    hybrid_traffic_b_latency_gg,
    hybrid_100_traffic_b_latency_gg,
    gaha_traffic_b_latency_gg,
    genesis_traffic_b_latency_gg,
    nrow = 5,
    top = textGrob("Traffic Pattern",
      gp = gpar(fontsize = 12, fontfamily = "Times New Roman")
    )
  ),
  arrangeGrob(
    arrangeGrob(
      online_traffic_scale_2_latency_gg,
      right = textGrob("BEGA\nOnline", rot = 90,
        gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      hybrid_traffic_scale_2_latency_gg,
      right = textGrob("BEGA 2000\nHybrid", rot = 90,
        gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      hybrid_100_traffic_scale_2_latency_gg,
      right = textGrob("BEGA 100\nHybrid", rot = 90,
        gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      gaha_traffic_scale_2_latency_gg,
      right = textGrob("GAHA\nOffline", rot = 90,
        gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    arrangeGrob(
      genesis_traffic_scale_2_latency_gg,
      right = textGrob("GENESIS\nHybrid", rot = 90,
        gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
    ),
    nrow = 5,
    top = textGrob("Traffic Scale",
      gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
  ),
  ncol = 5,
  widths = c(1.2, 1, 1, 1, 1.4),
  bottom = textGrob("Generation",
    gp = gpar(fontsize = 12, fontfamily = "Times New Roman")),
  left = textGrob("Average Traffic Latency (ms)", rot = 90,
    gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
)

ggsave("experiments/surrogate_experiments/analysis/v3/latencies.png", width = 8, height = 5, plot = latencies)

combined <- grid.arrange(
  arrangeGrob(
    ars,
    top = textGrob("Acceptance Ratio", gp = gpar(fontsize = 12, text = element_text(fontfamily = "Times New Roman"),)),
    ncol = 1
  ),
  nullGrob(),
  arrangeGrob(
    latencies,
    ncol = 1,
    top = textGrob("Average Traffic Latency", gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))
  ),
  ncol = 3,
  widths = c(1, 0.01, 1)
)
ggsave(
  filename = "experiments/surrogate_experiments/analysis/v3/combined.png",
  width = 16,
  height = 4,
  plot = combined
)

# Time taken
time <- read.csv("experiments/surrogate_experiments/analysis/v3/time.csv") %>%
  mutate(time = as.numeric(time) / 60)
avgOnlineTime <- time %>%
  filter(algorithm == "BEGA Online") %>%
  group_by(algorithm) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

maxOnlineTime <- time %>%
  filter(algorithm == "BEGA Online") %>%
  group_by(algorithm) %>%
  summarise(max_time = max(time)) %>%
  pull(max_time)

minOnlineTime <- time %>%
  filter(algorithm == "BEGA Online") %>%
  group_by(algorithm) %>%
  summarise(min_time = min(time)) %>%
  pull(min_time)

avgHybrid2000Time <- time %>%
  filter(algorithm == "BEGA 2000 Hybrid") %>%
  group_by(experiment,algorithm) %>%
  summarise(tot_time = sum(time)) %>%
  group_by(algorithm) %>%
  summarise(avg_time = mean(tot_time)) %>%
  pull(avg_time)

maxHybrid2000Time <- time %>%
  filter(algorithm == "BEGA 2000 Hybrid") %>%
  group_by(experiment, algorithm) %>%
    summarise(tot_time = sum(time)) %>%
    group_by(algorithm) %>%
    summarise(avg_time = max(tot_time)) %>%
    pull(avg_time)

minHybrid2000Time <- time %>%
  filter(algorithm == "BEGA 2000 Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = min(tot_time)) %>%
      pull(avg_time)

avgHybrid100Time <- time %>%
  filter(algorithm == "BEGA 100 Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = mean(tot_time)) %>%
      pull(avg_time)

maxHybrid100Time <- time %>%
  filter(algorithm == "BEGA 100 Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = max(tot_time)) %>%
      pull(avg_time)

minHybrid100Time <- time %>%
  filter(algorithm == "BEGA 100 Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = min(tot_time)) %>%
      pull(avg_time)
  summarise(min_time = min(time)) %>%
  pull(min_time)

avgGENESISTime <- time %>%
  filter(algorithm == "GENESIS Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = mean(tot_time)) %>%
      pull(avg_time)

maxGENESISTime <- time %>%
  filter(algorithm == "GENESIS Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = max(tot_time)) %>%
      pull(avg_time)

minGENESISTime <- time %>%
  filter(algorithm == "GENESIS Hybrid") %>%
    group_by(experiment, algorithm) %>%
      summarise(tot_time = sum(time)) %>%
      group_by(algorithm) %>%
      summarise(avg_time = min(tot_time)) %>%
      pull(avg_time)

avgGAHATime <- time %>%
  filter(algorithm == "GAHA Offline") %>%
  group_by(algorithm) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

maxGAHATime <- time %>%
  filter(algorithm == "GAHA Offline") %>%
  group_by(algorithm) %>%
  summarise(max_time = max(time)) %>%
  pull(max_time)

minGAHATime <- time %>%
  filter(algorithm == "GAHA Offline") %>%
  group_by(algorithm) %>%
  summarise(min_time = min(time)) %>%
  pull(min_time)

avgGDATime <- time %>%
  filter(algorithm == "GDA") %>%
  group_by(algorithm) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

maxGDATime <- time %>%
  filter(algorithm == "GDA") %>%
  group_by(algorithm) %>%
  summarise(max_time = max(time)) %>%
  pull(max_time)

minGDATime <- time %>%
  filter(algorithm == "GDA") %>%
  group_by(algorithm) %>%
  summarise(min_time = min(time)) %>%
  pull(min_time)

avgRLTime <- time %>%
  filter(algorithm == "RL") %>%
  group_by(algorithm) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

maxRLTime <- time %>%
  filter(algorithm == "RL") %>%
  group_by(algorithm) %>%
  summarise(max_time = max(time)) %>%
  pull(max_time)

minRLTime <- time %>%
  filter(algorithm == "RL") %>%
  group_by(algorithm) %>%
  summarise(min_time = min(time)) %>%
  pull(min_time)

print(paste("Average Online Time (hr):", round(avgOnlineTime/60, 2)))
print(paste("Max Online Time (hr):", round(maxOnlineTime/60, 2)))
print(paste("Min Online Time (hr):", round(minOnlineTime/60, 2)))
print(paste("Average Hybrid 2000 Time (min):", round(avgHybrid2000Time, 2)))
print(paste("Max Hybrid 2000 Time (min):", round(maxHybrid2000Time, 2)))
print(paste("Min Hybrid 2000 Time (min):", round(minHybrid2000Time, 2)))
print(paste("Average Hybrid 100 Time (min):", round(avgHybrid100Time, 2)))
print(paste("Max Hybrid 100 Time (min):", round(maxHybrid100Time, 2)))
print(paste("Min Hybrid 100 Time (min):", round(minHybrid100Time, 2)))
print(paste("Average GENESIS Time (min):", round(avgGENESISTime, 2)))
print(paste("Max GENESIS Time (min):", round(maxGENESISTime, 2)))
print(paste("Min GENESIS Time (min):", round(minGENESISTime, 2)))
print(paste("Average GAHA Time (hr):", round(avgGAHATime/60, 2)))
print(paste("Max GAHA Time (hr):", round(maxGAHATime/60, 2)))
print(paste("Min GAHA Time (hr):", round(minGAHATime/60, 2)))
print(paste("Average GDA Time (s):", round(avgGDATime*60/60, 2)))
print(paste("Max GDA Time (s):", round(maxGDATime*60/60, 2)))
print(paste("Min GDA Time (s):", round(minGDATime*60/60, 2)))
print(paste("Average RL Time (min):", round(avgRLTime/60, 2)))
print(paste("Max RL Time (min):", round(maxRLTime/60, 2)))
print(paste("Min RL Time (min):", round(minRLTime/60, 2)))

avgNonSFCRHybridTime <- time %>%
  filter(evolution == "Hybrid", experiment != "SFCRs (Hybrid)") %>%
  group_by(experiment) %>%
  summarise(time = sum(time)) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

ggplot(
  time %>%
    mutate(algorithm = factor(algorithm, levels = c("GENESIS Hybrid", setdiff(unique(algorithm), "GENESIS Hybrid")))),
  aes(x = experiment, y = time, fill = type)
) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Experiments",
    y = "Time (minutes)"
  ) +
  facet_wrap(~algorithm, nrow=1) +
  scale_fill_manual(values = c("Offline" = "#a8cb8c", "Online" = "#375c63")) +
  guides(fill = guide_legend(title = NULL)) +
  scale_y_break(c(0.5, 5), scales = 1, space = 0.5) +
  scale_y_break(c(35, 250), scales = 1, space = 0.5) +
  scale_y_break(c(400, 550), scales = 1, space = 0.5) +
  scale_y_break(c(750, 1200), scales = 1, space = 0.5) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
  )

ggsave("experiments/surrogate_experiments/analysis/v3/time.png", width = 14, height = 8)

get_surrogate_latency <- function(data) {
  final_latency <- data %>%
    filter(method == "surrogate") %>%
    tail(1) %>%
    pull(max_latency)

  return(final_latency)
}

get_emulator_latency <- function(data) {
  final_latency <- data %>%
    filter(method == "emulator") %>%
    tail(1) %>%
    pull(max_latency)

  return(final_latency)
}

hybrid_baseline_offline_final_latency  <- get_surrogate_latency(hybrid_baseline_data)
hybrid_baseline_online_final_latency <- get_emulator_latency(hybrid_baseline_data)
hybrid_baseline_latency_diff <- hybrid_baseline_offline_final_latency - hybrid_baseline_online_final_latency

# hybrid_100_baseline_offline_final_latency  <- get_surrogate_latency(hybrid_100_baseline_data)
# hybrid_100_baseline_online_final_latency <- get_emulator_latency(hybrid_100_baseline_data)
# hybrid_100_baseline_latency_diff <- hybrid_100_baseline_offline_final_latency - hybrid_100_baseline_online_final_latency

genesis_baseline_offline_final_latency  <- get_surrogate_latency(genesis_baseline_data)
genesis_baseline_online_final_latency <- get_emulator_latency(genesis_baseline_data)
genesis_baseline_latency_diff <- genesis_baseline_offline_final_latency - genesis_baseline_online_final_latency

hybrid_bandwidth_offline_final_latency <- get_surrogate_latency(hybrid_bw_5_data)
hybrid_bandwidth_online_final_latency <- get_emulator_latency(hybrid_bw_5_data)
hybrid_bandwidth_latency_diff <- hybrid_bandwidth_offline_final_latency - hybrid_bandwidth_online_final_latency

hybrid_100_bandwidth_offline_final_latency <- get_surrogate_latency(hybrid_100_bw_5_data)
hybrid_100_bandwidth_online_final_latency <- get_emulator_latency(hybrid_100_bw_5_data)
hybrid_100_bandwidth_latency_diff <- hybrid_100_bandwidth_offline_final_latency - hybrid_100_bandwidth_online_final_latency

genesis_bandwidth_offline_final_latency <- get_surrogate_latency(genesis_bw_5_data)
genesis_bandwidth_online_final_latency <- get_emulator_latency(genesis_bw_5_data)
genesis_bandwidth_latency_diff <- genesis_bandwidth_offline_final_latency - genesis_bandwidth_online_final_latency

hybrid_cpus_offline_final_latency <- get_surrogate_latency(hybrid_cpus_1_data)
hybrid_cpus_online_final_latency <- get_emulator_latency(hybrid_cpus_1_data)
hybrid_cpus_latency_diff <- hybrid_cpus_offline_final_latency - hybrid_cpus_online_final_latency

# hybrid_100_cpus_offline_final_latency <- get_surrogate_latency(hybrid_100_cpus_1_data)
# hybrid_100_cpus_online_final_latency <- get_emulator_latency(hybrid_100_cpus_1_data)
# hybrid_100_cpus_latency_diff <- hybrid_100_cpus_offline_final_latency - hybrid_100_cpus_online_final_latency

genesis_cpus_offline_final_latency <- get_surrogate_latency(genesis_cpus_1_data)
genesis_cpus_online_final_latency <- get_emulator_latency(genesis_cpus_1_data)
genesis_cpus_latency_diff <- genesis_cpus_offline_final_latency - genesis_cpus_online_final_latency

hybrid_traffic_b_offline_final_latency <- get_surrogate_latency(hybrid_traffic_b_data)
hybrid_traffic_b_online_final_latency <- get_emulator_latency(hybrid_traffic_b_data)
hybrid_traffic_b_latency_diff <- hybrid_traffic_b_offline_final_latency - hybrid_traffic_b_online_final_latency

# hybrid_100_traffic_b_offline_final_latency <- get_surrogate_latency(hybrid_100_traffic_b_data)
# hybrid_100_traffic_b_online_final_latency <- get_emulator_latency(hybrid_100_traffic_b_data)
# hybrid_100_traffic_b_latency_diff <- hybrid_100_traffic_b_offline_final_latency - hybrid_100_traffic_b_online_final_latency

genesis_traffic_b_offline_final_latency <- get_surrogate_latency(genesis_traffic_b_data)
genesis_traffic_b_online_final_latency <- get_emulator_latency(genesis_traffic_b_data)
genesis_traffic_b_latency_diff <- genesis_traffic_b_offline_final_latency - genesis_traffic_b_online_final_latency

hybrid_traffic_scale_2_offline_final_latency <- get_surrogate_latency(hybrid_traffic_scale_2_data)
hybrid_traffic_scale_2_online_final_latency <- get_emulator_latency(hybrid_traffic_scale_2_data)
hybrid_traffic_scale_2_latency_diff <- hybrid_traffic_scale_2_offline_final_latency - hybrid_traffic_scale_2_online_final_latency

# hybrid_100_traffic_scale_2_offline_final_latency <- get_surrogate_latency(hybrid_100_traffic_scale_2_data)
# hybrid_100_traffic_scale_2_online_final_latency <- get_emulator_latency(hybrid_100_traffic_scale_2_data)
# hybrid_100_traffic_scale_2_latency_diff <- hybrid_100_traffic_scale_2_offline_final_latency - hybrid_100_traffic_scale_2_online_final_latency

genesis_traffic_scale_2_offline_final_latency <- get_surrogate_latency(genesis_traffic_scale_2_data)
genesis_traffic_scale_2_online_final_latency <- get_emulator_latency(genesis_traffic_scale_2_data)
genesis_traffic_scale_2_latency_diff <- genesis_traffic_scale_2_offline_final_latency - genesis_traffic_scale_2_online_final_latency

mean_latency_diff <- mean(c(
  abs(hybrid_baseline_latency_diff),
  abs(hybrid_bandwidth_latency_diff),
  abs(hybrid_cpus_latency_diff),
  abs(hybrid_traffic_b_latency_diff),
  abs(hybrid_traffic_scale_2_latency_diff),
  abs(genesis_baseline_latency_diff),
  abs(genesis_bandwidth_latency_diff),
  abs(genesis_cpus_latency_diff),
  abs(genesis_traffic_b_latency_diff),
  abs(genesis_traffic_scale_2_latency_diff),
  abs(hybrid_100_bandwidth_latency_diff)
))

gaha_offline <- read.csv("experiments/surrogate_experiments/analysis/v3/gaha_offline.csv") %>%
    mutate(Diff=Offline-Online)

mean(abs(gaha_offline$Diff))
max(abs(gaha_offline$Diff))
min(abs(gaha_offline$Diff))

data_file_names = c(
  "8_0.1_False_5_1",
  "8_0.1_False_5_2",
  "8_0.1_False_10_1",
  "8_0.1_False_10_2",
  "8_0.1_True_5_1",
  "8_0.1_True_5_2",
  "8_0.1_True_10_1",
  "8_0.1_True_10_2",
  "8_0.2_False_5_1",
  "8_0.2_False_5_2",
  "8_0.2_False_10_1",
  "8_0.2_False_10_2",
  "8_0.2_True_5_1",
  "8_0.2_True_5_2",
  "8_0.2_True_10_1",
  "8_0.2_True_10_2",
  "16_0.1_False_5_1",
  "16_0.1_False_5_2",
  "16_0.1_False_10_1",
  "16_0.1_False_10_2",
  "16_0.1_True_5_1",
  "16_0.1_True_5_2",
  "16_0.1_True_10_1",
  "16_0.1_True_10_2",
  "16_0.2_False_5_1",
  "16_0.2_False_5_2",
  "16_0.2_False_10_1",
  "16_0.2_False_10_2",
  "16_0.2_True_5_1",
  "16_0.2_True_5_2",
  "16_0.2_True_10_1",
  "16_0.2_True_10_2"
)

mean_diff_table <- data.frame(
  Experiment = character(),
  Surrogate = numeric(),
  Emulator = numeric(),
  Mean_Diff = numeric()
)

for (name in data_file_names) {
  data <- read.csv(paste0("experiments/surrogate_experiments/bega_2000_hybrid/v3/", name, "/data.csv"))
  surrogate_latency <- get_surrogate_latency(data)
  emulator_latency <- get_emulator_latency(data)
  if (length(emulator_latency) == 0) {
    next
  }
  specs = strsplit(name, "_")[[1]]
  sfcrs = ifelse(specs[1] == "16", "64", "32")
  scale = ifelse(specs[2] == "0.1", "1x", "2x")
  traffic = ifelse(specs[3] == "True", "B", "A")
  bw = specs[4]
  cpus = specs[5]
  experiment_name = paste0("", sfcrs, " ", scale, " ", traffic, " ", bw, " ", cpus)

  mean_diff <- abs(surrogate_latency - emulator_latency)
  mean_diff_table <- rbind(mean_diff_table, data.frame(
    Experiment = experiment_name,
    Surrogate = surrogate_latency,
    Emulator = emulator_latency,
    Mean_Diff = mean_diff
  ))
}

median(mean_diff_table$Mean_Diff)
quantile(mean_diff_table$Mean_Diff, 0.75)
quantile(mean_diff_table$Mean_Diff, 0.25)
mean(mean_diff_table$Mean_Diff)
max(mean_diff_table$Mean_Diff)
min(mean_diff_table$Mean_Diff)

mean_diff_plot <- mean_diff_table %>%
  pivot_longer(cols = c("Surrogate", "Emulator"), names_to = "Method", values_to = "Latency")

ggplot(mean_diff_plot, aes(x = Experiment, y = Latency, color = Method)) +
  geom_point(size=4) +
  labs(
    x = "Experiment",
    y = "Traffic Latency (ms)",
    color = "Method"
  ) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  scale_color_manual(values = c("#375c63", "#a8cb8c"), labels=c("Measured on OpenRASE", "Approximated by BENNS")) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 14, angle = 90),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

ggsave("experiments/surrogate_experiments/analysis/v3/mean_diff_plot.png", width = 6, height = 4)

ggplot(mean_diff_plot, aes(x = Experiment, y = Latency, color = Method)) +
  geom_point(size = 4) +
  labs(
    x = "Experiment",
    y = "Traffic Latency (ms)",
    color = "Method"
  ) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  scale_color_manual(values = c("#375c63", "#a8cb8c"), labels = c("Measured on OpenRASE", "Approximated by BENNS")) +
  theme_light() +
  theme(
    text = element_text(family = "Sans", color = "#29454a"),
    axis.text = element_text(size = 14, angle = 90, color = "#29454a"),
    axis.title = element_text(size = 14, color = "#29454a"),
    plot.title = element_text(size = 14, color = "#29454a"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, color = "#29454a"),
    plot.background = element_rect(fill = "#e9e7e7"),
    panel.background = element_rect(fill = "#e9e7e7"),
    legend.background = element_rect(fill = "#e9e7e7"),
    legend.key = element_rect(fill = "#e9e7e7")
  )

ggsave("experiments/surrogate_experiments/analysis/v3/mean_diff_plot_poster.png", width = 7, height = 5)

time_data  <-  read.csv("experiments/surrogate_experiments/analysis/v3/benchmark_time.csv") %>%
  filter(vnf != "dummy")

print(sum((time_data  %>%  filter(type == "total"))$time))

mean_time  <-  time_data %>%
  group_by(type) %>%
  summarise(mean_time = mean(time))

surrogate_data_time <- read.csv("experiments/surrogate_experiments/analysis/v3/surrogate_data_gen_time.csv") %>%
  filter(id != "4_2") %>%
  filter(id != "0.2_100") %>%
  filter(id != "0.2_5") %>%
  filter(id != "1_2") %>%
  filter(id != "0.5_10")

print(sum((surrogate_data_time %>% filter(type == "total_experiment"))$time))
print(mean((surrogate_data_time %>% filter(type == "total_experiment"))$time))
print(max((surrogate_data_time %>% filter(type == "total_experiment"))$time))
print(min((surrogate_data_time %>% filter(type == "total_experiment"))$time))

online_offline_data  <- read.csv("experiments/surrogate_experiments/analysis/v3/offline_online_diff.csv")  %>%
  group_by(experiment, algorithm)  %>%
  summarise(
    offline = sum(latency[type == "Offline"], na.rm = TRUE),
    online = sum(latency[type == "Online"], na.rm = TRUE),
    online_minus_offline = online - offline,
  )

print(mean(abs(online_offline_data$online_minus_offline)))
print(max(abs(online_offline_data$online_minus_offline)))
print(min(abs(online_offline_data$online_minus_offline)))
