library(tidyverse)
library(ggplot2)
library(gridExtra)

segment_1_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/4_0.1_False_10_2_0/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_2_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/8_0.1_False_10_2_1/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_3_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/12_0.1_False_10_2_2/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segement_4_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/16_0.1_False_10_2_3/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_5_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/20_0.1_False_10_2_4/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_6_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/24_0.1_False_10_2_5/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_7_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/28_0.1_False_10_2_6/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_8_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/32_0.1_False_10_2_7/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_9_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/36_0.1_False_10_2_8/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))
segment_10_data <- read.csv("experiments/surrogate_experiments/genesis_dynamic/40_0.1_False_10_2_9/data.csv") %>%
    mutate(generation = ifelse(method == "emulator", round(generation + 1), generation))

segment_1_last_gen <- segment_1_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_2_data <- segment_2_data %>%
    mutate(generation = generation + segment_1_last_gen)
segment_2_last_gen <- segment_2_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_3_data <- segment_3_data %>%
    mutate(generation = generation + segment_2_last_gen)
segment_3_last_gen <- segment_3_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segement_4_data <- segement_4_data %>%
    mutate(generation = generation + segment_3_last_gen)
segment_4_last_gen <- segement_4_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_5_data <- segment_5_data %>%
    mutate(generation = generation + segment_4_last_gen)
segment_5_last_gen <- segment_5_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_6_data <- segment_6_data %>%
    mutate(generation = generation + segment_5_last_gen)
segment_6_last_gen <- segment_6_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_7_data <- segment_7_data %>%
    mutate(generation = generation + segment_6_last_gen)
segment_7_last_gen <- segment_7_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_8_data <- segment_8_data %>%
    mutate(generation = generation + segment_7_last_gen)
segment_8_last_gen <- segment_8_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_9_data <- segment_9_data %>%
    mutate(generation = generation + segment_8_last_gen)
segment_9_last_gen <- segment_9_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)
segment_10_data <- segment_10_data %>%
    mutate(generation = generation + segment_9_last_gen)
segment_10_last_gen <- segment_10_data %>%
    filter(generation == max(generation)) %>%
    pull(generation)

combined_data <- bind_rows(
    segment_1_data,
    segment_2_data,
    segment_3_data,
    segement_4_data,
    segment_5_data,
    segment_6_data,
    segment_7_data,
    segment_8_data,
    segment_9_data,
    segment_10_data
)
latency  <-  ggplot(combined_data, aes(x = generation, y = average_latency)) +
    geom_ribbon(aes(ymin = min_latency, ymax = max_latency), alpha = 0.2, linetype = 2, fill = "#375c63") +
    geom_line(color = "#375c63", size = 1) +
    labs(
        x = "Generation",
        y = "Average Traffic Latency (ms)"
    ) +
    geom_vline(xintercept = c(
        1,
        segment_1_last_gen,
        segment_2_last_gen,
        segment_3_last_gen,
        segment_4_last_gen,
        segment_5_last_gen,
        segment_6_last_gen,
        segment_7_last_gen,
        segment_8_last_gen,
        segment_9_last_gen
    ), linetype = "dashed", color = "#323232") +
    geom_text(aes(x = 1, label = "4 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_1_last_gen, label = "8 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_2_last_gen, label = "12 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_3_last_gen, label = "16 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_4_last_gen, label = "20 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_5_last_gen, label = "24 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_6_last_gen, label = "28 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_7_last_gen, label = "32 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_8_last_gen, label = "36 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_9_last_gen, label = "40 SFCRs\n", y = 50), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = 1, label = "\n14 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_1_last_gen, label = "\n14 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_2_last_gen, label = "\n14 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_3_last_gen, label = "\n14 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_4_last_gen, label = "\n14 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_5_last_gen, label = "\n13 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_6_last_gen, label = "\n12 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_7_last_gen, label = "\n11 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_8_last_gen, label = "\n10 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_9_last_gen, label = "\n9 Hosts", y = 50), colour = "#375c63", angle = 90, family = "Times New Roman") +
    theme_light() +
    scale_y_continuous(limits = c(0, 60)) +
    scale_x_continuous(limits = c(1, segment_10_last_gen), breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
    theme(text = element_text(size = 14, family = "Times New Roman"))


ar <- ggplot(combined_data, aes(x = generation, y = average_ar)) +
    geom_ribbon(aes(ymin = min_ar, ymax = max_ar), alpha = 0.2, linetype = 2, fill = "#a8cc8c") +
    geom_line(color = "#a8cc8c", size = 1) +
    labs(
        x = "Generation",
        y = "Acceptance Ratio"
    ) +
    geom_vline(xintercept = c(
        1,
        segment_1_last_gen,
        segment_2_last_gen,
        segment_3_last_gen,
        segment_4_last_gen,
        segment_5_last_gen,
        segment_6_last_gen,
        segment_7_last_gen,
        segment_8_last_gen,
        segment_9_last_gen
    ), linetype = "dashed", color = "#323232") +
    geom_text(aes(x = 1, label = "4 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_1_last_gen, label = "8 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_2_last_gen, label = "12 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_3_last_gen, label = "16 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_4_last_gen, label = "20 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_5_last_gen, label = "24 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_6_last_gen, label = "28 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_7_last_gen, label = "32 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_8_last_gen, label = "36 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_9_last_gen, label = "40 SFCRs\n", y = 0.3), colour = "#29454a", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = 1, label = "\n14 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_1_last_gen, label = "\n14 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_2_last_gen, label = "\n14 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_3_last_gen, label = "\n14 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_4_last_gen, label = "\n14 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_5_last_gen, label = "\n13 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_6_last_gen, label = "\n12 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_7_last_gen, label = "\n11 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_8_last_gen, label = "\n10 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    geom_text(aes(x = segment_9_last_gen, label = "\n9 Hosts", y = 0.3), colour = "#375c63", angle = 90, family = "Times New Roman") +
    theme_light() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, segment_10_last_gen), breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
    theme(text = element_text(size = 14, family = "Times New Roman"))

plots  <-  grid.arrange(latency, ar, ncol = 1)
ggsave("experiments/genesis_experiments/genesis_dynamic/dynamic_plot.png", width = 12, height = 8, dpi = 300, plot = plots)
