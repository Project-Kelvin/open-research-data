library(tidyverse)
library(ggplot2)
library(gridExtra)

no_of_segments <- 24
q4 <- 24 * 0.75
milan_hosts <- 30
random_hosts <- 25
power_factor  <-  60 * 60 # Convert from Wh to W by multiplying with 3600 (60 seconds * 60 minutes)

read_data <- function(dir) {
    sfcrs_count <- 2
    last_gen <- 0
    combined_data <- data.frame()
    cuts <- c(1)
    for (i in 0:(no_of_segments - 1)) {
        file_path <- paste0("experiments/genesis_experiments/genesis_power/", dir, "/", sfcrs_count, "_0.1_False_10_1_", i, "_", dir, "/data.csv")
        sfcrs_count <- sfcrs_count + 2
        data <- read_csv(file_path) %>%
            mutate(generation = ifelse(method == "emulator", round(generation) + 1, generation)) %>%
            mutate(generation = last_gen + generation)  %>%
            mutate(max_power = max_power * power_factor / 1000) %>%
            mutate(min_power = min_power * power_factor / 1000) %>%
            mutate(average_power = average_power * power_factor / 1000)
        last_gen <- max(data$generation)
        cuts <- c(cuts, last_gen)
        combined_data <- rbind(combined_data, data)
    }
    return(list(data = combined_data, cuts = cuts))
}

milan_data <- read_data("milan")
random_data <- read_data("25N50E")

milan_latency <- ggplot(milan_data$data, aes(x = generation, y = average_power)) +
    geom_ribbon(aes(ymin = min_power, ymax = max_power), alpha = 0.2, linetype = 2, fill = "#375c63") +
    geom_line(color = "#375c63", size = 1) +
    labs(
        x = "Generation",
        y = "Energy Consumed (kWh)"
    ) +
    geom_vline(xintercept = milan_data$cuts, linetype = "dashed", color = "#323232") +
    geom_vline(xintercept = milan_data$cuts[q4], linetype = "solid", color = "#ff0000") +
    theme_light() +
    scale_y_continuous(limits = c(0, 1080)) +
    scale_x_continuous(limits = c(1, tail(milan_data$cuts, n=1)), breaks = scales::pretty_breaks(n = 20), labels = scales::number_format(accuracy = 1)) +
    theme(text = element_text(size = 14, family = "Times New Roman"))


milan_ar <- ggplot(milan_data$data, aes(x = generation, y = average_ar)) +
    geom_ribbon(aes(ymin = min_ar, ymax = max_ar), alpha = 0.2, linetype = 2, fill = "#a8cc8c") +
    geom_line(color = "#a8cc8c", size = 1) +
    labs(
        x = "Generation",
        y = "Acceptance Ratio"
    ) +
    geom_vline(xintercept = milan_data$cuts, linetype = "dashed", color = "#323232") +
    geom_vline(xintercept = milan_data$cuts[q4], linetype = "solid", color = "#ff0000") +
    theme_light() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, tail(milan_data$cuts, n=1)), breaks = scales::pretty_breaks(n = 20), labels = scales::number_format(accuracy = 1)) +
    theme(text = element_text(size = 14, family = "Times New Roman"))

plots <- grid.arrange(milan_latency, milan_ar, ncol = 1)
ggsave("experiments/genesis_experiments/genesis_power/milan_dynamic_plot.png", width = 6, height = 5, dpi = 300, plot = plots)

rand_latency <- ggplot(random_data$data, aes(x = generation, y = average_power)) +
    geom_ribbon(aes(ymin = min_power, ymax = max_power), alpha = 0.2, linetype = 2, fill = "#375c63") +
    geom_line(color = "#375c63", size = 1) +
    labs(
        x = "Generation",
        y = "Energy Consumed (kWh)"
    ) +
    geom_vline(xintercept = random_data$cuts, linetype = "dashed", color = "#323232") +
    geom_vline(xintercept = random_data$cuts[q4], linetype = "solid", color = "#ff0000") +
    theme_light() +
    scale_y_continuous(limits = c(0, 1080)) +
    scale_x_continuous(limits = c(1, tail(random_data$cuts, n = 1)), breaks = scales::pretty_breaks(n = 20), labels = scales::number_format(accuracy = 1)) +
    theme(text = element_text(size = 14, family = "Times New Roman"))


rand_ar <- ggplot(random_data$data, aes(x = generation, y = average_ar)) +
    geom_ribbon(aes(ymin = min_ar, ymax = max_ar), alpha = 0.2, linetype = 2, fill = "#a8cc8c") +
    geom_line(color = "#a8cc8c", size = 1) +
    labs(
        x = "Generation",
        y = "Acceptance Ratio"
    ) +
    geom_vline(xintercept = random_data$cuts, linetype = "dashed", color = "#323232") +
    geom_vline(xintercept = random_data$cuts[q4], linetype = "solid", color = "#ff0000") +
    theme_light() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, tail(random_data$cuts, n = 1)), breaks = scales::pretty_breaks(n = 20), labels = scales::number_format(accuracy = 1)) +
    theme(text = element_text(size = 14, family = "Times New Roman"))

plots <- grid.arrange(rand_latency, rand_ar, ncol = 1)
ggsave("experiments/genesis_experiments/genesis_power/25N50E_dynamic_plot.png", width = 6, height = 5, dpi = 300, plot = plots)


time_taken <- read_csv("experiments/genesis_experiments/genesis_power/time_taken.csv") %>%
    mutate(milan = as.double(milan)/60) %>%
    mutate(`25N50E` = as.double(`25N50E`)/60)

time_taken_plot <- time_taken %>%
    pivot_longer(cols = c("milan", "25N50E"), names_to = "experiment", values_to = "time")

time <- ggplot(time_taken_plot, aes(x = segment, y = time, color = experiment)) +
    geom_line(size = 1) +
    labs(
        x = "Segment",
        y = "Time Taken (mins)"
    ) +
    geom_vline(xintercept = q4, linetype = "solid", color = "#ff0000") +
    theme_light() +
    theme(text = element_text(size = 14, family = "Times New Roman"), legend.position = "top", legend.title = element_blank()) +
    scale_color_manual(values = c("#375c63", "#a8cc8c"), labels = c("25N50E", "Milan City Centre")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave("experiments/genesis_experiments/genesis_power/time_taken_plot.png", width = 6, height = 3, dpi = 300, plot = time)

print("Time taken for Milan:")
print(quantile(time_taken$milan, probs = c(0.25, 0.5, 0.75)))
print("Time taken for 25N50E:")
print(quantile(time_taken$`25N50E`, probs = c(0.25, 0.5, 0.75)))

print(quantile(time_taken_plot$time, probs = c(0.25, 0.5, 0.75)))

milan_power <- milan_data$data  %>% filter(method == "emulator") %>% select(average_power)
quantile(milan_power$average_power, probs = c(0.25, 0.5, 0.75))

rand_power <- random_data$data  %>% filter(method == "emulator") %>% select(average_power)
quantile(rand_power$average_power, probs = c(0.25, 0.5, 0.75))

quantile(c(milan_power$average_power, rand_power$average_power), probs = c(0.25, 0.5, 0.75))

print(median(c(time_taken$milan, time_taken$`25N50E`)))
print(median(c(milan_power$average_power, rand_power$average_power)))
