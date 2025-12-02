library(tidyverse)
library(ggplot2)
library(ggbreak)
library(gridExtra)
library(grid) # Needed for textGrob and gpar

hybrid_baseline_data <- read.csv("experiments/surrogate_experiments/hybrid/v3/8_0.1_False_10_2/data.csv")
hybrid_bw_5_data <- read.csv("experiments/surrogate_experiments/hybrid/v3/8_0.1_False_5_2/data.csv")
hybrid_cpus_1_data <- read.csv("experiments/surrogate_experiments/hybrid/v3/8_0.1_False_10_1/data.csv")
hybrid_sfcrs_64_data <- read.csv("experiments/surrogate_experiments/hybrid/v3/16_0.1_False_10_2/data.csv")
hybrid_traffic_b_data <- read.csv("experiments/surrogate_experiments/hybrid/v3/8_0.1_True_10_2/data.csv")
hybrid_traffic_scale_2_data <- read.csv("experiments/surrogate_experiments/hybrid/v3/8_0.2_False_10_2/data.csv")
online_baseline_data <- read.csv("experiments/surrogate_experiments/online/v2/8_0.1_False_10_2/data.csv")
online_bw_5_data <- read.csv("experiments/surrogate_experiments/online/v2/8_0.1_False_5_2/data.csv")
online_cpus_1_data <- read.csv("experiments/surrogate_experiments/online/v2/8_0.1_False_10_1/data.csv")
online_sfcrs_64_data <- read.csv("experiments/surrogate_experiments/online/v2/16_0.1_False_10_2/data.csv")
online_traffic_b_data <- read.csv("experiments/surrogate_experiments/online/v2/8_0.1_True_10_2/data.csv")
online_traffic_scale_2_data <- read.csv("experiments/surrogate_experiments/online/v2/8_0.2_False_10_2/data.csv")

online_data <- bind_rows(
  mutate(online_baseline_data, experiment = "Baseline"),
  mutate(online_bw_5_data, experiment = "Bandwidth"),
  mutate(online_cpus_1_data, experiment = "CPU"),
  mutate(online_sfcrs_64_data, experiment = "SFCRs"),
  mutate(online_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(online_traffic_scale_2_data, experiment = "Traffic Scale"),
)

hybrid_data <- bind_rows(
  mutate(hybrid_baseline_data, experiment = "Baseline"),
  mutate(hybrid_bw_5_data, experiment = "Bandwidth"),
  mutate(hybrid_cpus_1_data, experiment = "CPU"),
  mutate(hybrid_sfcrs_64_data, experiment = "SFCRs"),
  mutate(hybrid_traffic_b_data, experiment = "Traffic Pattern"),
  mutate(hybrid_traffic_scale_2_data, experiment = "Traffic Scale"),
)

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
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_sfcrs_ar_gg <- ggplot(hybrid_data %>% filter(experiment == "SFCRs"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#dd5182", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#dd5182") +
  labs(
    x = NULL,
    y = NULL
  ) +
  geom_vline(xintercept = 217, linetype = "dashed", color = "#323232") +
  theme_minimal() +
  theme(
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
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_sfcrs_ar_gg <- ggplot(online_data %>% filter(experiment == "SFCRs"), aes(x = generation, y = average_ar)) +
  geom_ribbon(aes(ymax = max_ar, ymin = min_ar), fill = "#dd5182", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(color = "#dd5182") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
    nrow = 2,
    top = textGrob("Baseline", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_bandwidth_ar_gg,
    hybrid_bandwidth_ar_gg,
    nrow = 2,
    top = textGrob("Bandwidth", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_cpus_ar_gg,
    hybrid_cpus_ar_gg,
    nrow = 2,
    top = textGrob("CPU", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_sfcrs_ar_gg,
    hybrid_sfcrs_ar_gg,
    nrow = 2,
    top = textGrob("SFCRs", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_traffic_b_ar_gg,
    hybrid_traffic_b_ar_gg,
    nrow = 2,
    top = textGrob("Traffic Pattern",
      gp = gpar(fontsize = 12)
    )
  ),
  arrangeGrob(
    arrangeGrob(
      online_traffic_scale_2_ar_gg,
      right = textGrob("Online", rot = 90, gp = gpar(fontsize = 12))
    ),
    arrangeGrob(
      hybrid_traffic_scale_2_ar_gg,
      right = textGrob("Hybrid", rot = 90, gp = gpar(fontsize = 12))
    ),
    nrow = 2,
    top = textGrob("Traffic Scale", gp = gpar(fontsize = 12))
  ),
  ncol = 6,
  widths = c(1.2, 1, 1, 1, 1, 1),
  bottom = textGrob("Generation", gp = gpar(fontsize = 12)),
  left = textGrob("Acceptance Ratio", rot = 90, gp = gpar(fontsize = 12))
)

ggsave("experiments/surrogate_experiments/analysis/v3/acceptance_ratios.png", width = 8, height = 4, plot = ars)

# Hybrid Latency plots
hybrid_baseline_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "Baseline"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#003f5c", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#003f5c") +
  geom_vline(xintercept = 34, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#444e86") +
  geom_vline(xintercept = 11, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#955196") +
  geom_vline(xintercept = 16, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

hybrid_sfcrs_latency_gg <- ggplot(hybrid_data %>% filter(experiment == "SFCRs"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#dd5182", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#dd5182") +
  geom_vline(xintercept = 217, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#ff6e54") +
  geom_vline(xintercept = 12, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#ffa600") +
  geom_vline(xintercept = 14, linetype = "dashed", color = "#323232") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#003f5c") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#444e86") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#955196") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1)
  )

online_sfcrs_latency_gg <- ggplot(online_data %>% filter(experiment == "SFCRs"), aes(x = generation, y = average_latency)) +
  geom_ribbon(aes(ymax = max_latency, ymin = min_latency), fill = "#dd5182", linetype = 2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#dd5182") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#ff6e54") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
  coord_cartesian(ylim = c(0, 50)) +
  geom_line(color = "#ffa600") +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
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
    nrow = 2,
    top = textGrob("Baseline", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_bandwidth_latency_gg,
    hybrid_bandwidth_latency_gg,
    nrow = 2,
    top = textGrob("Bandwidth", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_cpus_latency_gg,
    hybrid_cpus_latency_gg,
    nrow = 2,
    top = textGrob("CPU", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_sfcrs_latency_gg,
    hybrid_sfcrs_latency_gg,
    nrow = 2,
    top = textGrob("SFCRs", gp = gpar(fontsize = 12))
  ),
  arrangeGrob(
    online_traffic_b_latency_gg,
    hybrid_traffic_b_latency_gg,
    nrow = 2,
    top = textGrob("Traffic Pattern",
      gp = gpar(fontsize = 12)
    )
  ),
  arrangeGrob(
    arrangeGrob(
      online_traffic_scale_2_latency_gg,
      right = textGrob("Online", rot = 90, gp = gpar(fontsize = 12))
    ),
    arrangeGrob(
      hybrid_traffic_scale_2_latency_gg,
      right = textGrob("Hybrid", rot = 90, gp = gpar(fontsize = 12))
    ),
    nrow = 2,
    top = textGrob("Traffic Scale", gp = gpar(fontsize = 12))
  ),
  ncol = 6,
  widths = c(1.2, 1, 1, 1, 1, 1),
  bottom = textGrob("Generation", gp = gpar(fontsize = 12)),
  left = textGrob("Average Traffic Latency (ms)", rot=90, gp = gpar(fontsize = 12))
)

ggsave("experiments/surrogate_experiments/analysis/v3/latencies.png", width = 8, height = 4, plot = latencies)

combined <- grid.arrange(
  arrangeGrob(
    ars,
    top = textGrob("Acceptance Ratio", gp = gpar(fontsize = 12)),
    ncol = 1
  ),
  nullGrob(),
  arrangeGrob(
    latencies,
    ncol = 1,
    top = textGrob("Average Traffic Latency", gp = gpar(fontsize = 12))
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
  filter(evolution == "Online") %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

maxOnlineTime  <- time %>%
  filter(evolution == "Online") %>%
  summarise(max_time = max(time)) %>%
  pull(max_time)

minOnlineTime <- time %>%
  filter(evolution == "Online") %>%
  summarise(min_time = min(time)) %>%
  pull(min_time)

avgHybridTime <- time %>%
  filter(evolution == "Hybrid") %>%
  group_by(experiment) %>%
  summarise(time = sum(time)) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

maxHybridTime <- time %>%
  filter(evolution == "Hybrid") %>%
  group_by(experiment) %>%
  summarise(time = sum(time)) %>%
  summarise(max_time = max(time)) %>%
  pull(max_time)

minHybridTime <- time %>%
  filter(evolution == "Hybrid") %>%
  group_by(experiment) %>%
  summarise(time = sum(time)) %>%
  summarise(min_time = min(time)) %>%
  pull(min_time)

print(paste("Average Online Time (min):", avgOnlineTime))
print(paste("Max Online Time (min):", maxOnlineTime))
print(paste("Min Online Time (min):", minOnlineTime))
print(paste("Average Hybrid Time (min):", avgHybridTime))
print(paste("Max Hybrid Time (min):", maxHybridTime))
print(paste("Min Hybrid Time (min):", minHybridTime))


avgNonSFCRHybridTime <- time %>%
  filter(evolution == "Hybrid", experiment != "SFCRs (Hybrid)") %>%
  group_by(experiment) %>%
  summarise(time = sum(time)) %>%
  summarise(avg_time = mean(time)) %>%
  pull(avg_time)

ggplot(time, aes(x = experiment, y = time, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Experiments",
    y = "Time (minutes)"
  ) +
  scale_fill_manual(values = c("Offline" = "#dd5182", "Online" = "#444e86")) +
  guides(fill = guide_legend(title = NULL)) +
  scale_y_break(c(180, 1000), scales = 1, space = 0.5) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 14),
  )

ggsave("experiments/surrogate_experiments/analysis/v3/time.png", width = 8, height = 6)

hybrid_baseline_offline_final_latency  <- hybrid_baseline_data %>%
  filter(method == "surrogate") %>% tail(1)  %>% pull(max_latency)
hybrid_baseline_online_final_latency <- hybrid_data %>%
  filter(method == "emulator") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_baseline_latency_diff <- hybrid_baseline_offline_final_latency - hybrid_baseline_online_final_latency

hybrid_bandwidth_offline_final_latency <- hybrid_bw_5_data %>%
  filter(method == "surrogate") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_bandwidth_online_final_latency <- hybrid_bw_5_data %>%
  filter(method == "emulator") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_bandwidth_latency_diff <- hybrid_bandwidth_offline_final_latency - hybrid_bandwidth_online_final_latency

hybrid_cpus_offline_final_latency <- hybrid_cpus_1_data %>%
  filter(method == "surrogate") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_cpus_online_final_latency <- hybrid_cpus_1_data %>%
  filter(method == "emulator") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_cpus_latency_diff <- hybrid_cpus_offline_final_latency - hybrid_cpus_online_final_latency

hybrid_sfcrs_offline_final_latency <- hybrid_sfcrs_64_data %>%
  filter(method == "surrogate") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_sfcrs_online_final_latency <- hybrid_sfcrs_64_data %>%
  filter(method == "emulator") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_sfcrs_latency_diff <- hybrid_sfcrs_offline_final_latency - hybrid_sfcrs_online_final_latency

hybrid_traffic_b_offline_final_latency <- hybrid_traffic_b_data %>%
  filter(method == "surrogate") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_traffic_b_online_final_latency <- hybrid_data %>%
  filter(method == "emulator") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_traffic_b_latency_diff <- hybrid_traffic_b_offline_final_latency - hybrid_traffic_b_online_final_latency

hybrid_traffic_scale_2_offline_final_latency <- hybrid_traffic_scale_2_data %>%
  filter(method == "surrogate") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_traffic_scale_2_online_final_latency <- hybrid_data %>%
  filter(method == "emulator") %>%
  tail(1) %>%
  pull(max_latency)
hybrid_traffic_scale_2_latency_diff <- hybrid_traffic_scale_2_offline_final_latency - hybrid_traffic_scale_2_online_final_latency

mean_latency_diff <- mean(c(
  abs(hybrid_baseline_latency_diff),
  abs(hybrid_bandwidth_latency_diff),
  abs(hybrid_cpus_latency_diff),
  abs(hybrid_sfcrs_latency_diff),
  abs(hybrid_traffic_b_latency_diff),
  abs(hybrid_traffic_scale_2_latency_diff)
))
