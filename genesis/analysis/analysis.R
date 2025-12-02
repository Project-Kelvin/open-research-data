library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(patchwork)

data <- read.csv("experiments/genesis_experiments/analysis/analysis_results.csv") %>%
    mutate(convergence_time = as.numeric(convergence_time) / 60) %>%
    mutate(best_ar = ifelse(best_ar == "N/A", NA, as.numeric(best_ar))) %>%
    mutate(best_latency = ifelse(best_latency == "N/A", NA, as.numeric(best_latency))) %>%
    mutate(algorithm = as.factor(algorithm)) %>%
    mutate(algorithm = fct_relevel(algorithm, "GENESIS", "BEGA 100", "BEGA 2000", "GDA", "GAHA")) %>%
    mutate(did_converge = as.factor(did_converge)) %>%
    mutate(did_converge = fct_relevel(did_converge, "True", "False"))

first_stage <- data %>%
    filter(str_detect(experiment, "^32_"))
second_stage <- data %>%
    filter(str_detect(experiment, "^48_"))

first_stage_plt  <- ggplot(first_stage, aes(x = experiment, y = convergence_time, color = algorithm)) +
    geom_point(aes(shape = did_converge), size = 3) +
    scale_y_log10() +
    scale_shape_manual(values = c(17, 15), labels = c("Yes", "No")) +
    scale_color_brewer(palette = "Set1") +
    labs(title="First Stage", x = "Experiment", y = "Execution Time (mins)") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), plot.title = element_text(family = "Times New Roman", size = 14), legend.position = "top", axis.text = element_text(family = "Times New Roman", size = 14), legend.text = element_text(family = "Times New Roman", size = 14), legend.title = element_text(family = "Times New Roman", size = 14), axis.title = element_text(family = "Times New Roman", size = 14)) +
    labs(color = "", shape = "Did it converge?")

second_stage_plt <- ggplot(second_stage, aes(x = experiment, y = convergence_time, color = algorithm)) +
    geom_point(aes(shape = did_converge), size = 3) +
    scale_y_log10() +
    scale_shape_manual(values = c(17, 15), labels = c("Yes", "No")) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Second Stage", x = "Experiment", y = "Execution Time (mins)") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), plot.title = element_text(family = "Times New Roman", size = 14), legend.position = "none", axis.text = element_text(family = "Times New Roman", size = 14), legend.text = element_text(family = "Times New Roman", size = 14), legend.title = element_text(family = "Times New Roman", size = 14), axis.title = element_text(family = "Times New Roman", size = 14)) +
    labs(color = "", shape = "Did it converge?")

conv_plot <-
    guide_area() / first_stage_plt / second_stage_plt +
        plot_layout(guides = "collect", nrow = 3, heights = c(1, 10, 10)) +
        plot_annotation(
            theme = theme(
                plot.title = element_text(family = "Times New Roman", size = 16),
                legend.text = element_text(family = "Times New Roman", size = 12),
                legend.title = element_text(family = "Times New Roman", size = 12),
                legend.position = "top",        # shift legend to the left
                legend.justification = c(0, 1),      # anchor legend to the left-top
                legend.box = "horizontal"
            )
        ) &
    guides(color = guide_legend(nrow = 1, byrow = TRUE, order = 1), shape = guide_legend(nrow = 1, byrow = TRUE, order = 2))

ggsave("experiments/genesis_experiments/analysis/convergence_time.png", width = 10, height = 8, dpi = 300, plot = conv_plot)

ggplot(first_stage, aes(x = experiment, y = best_latency, fill = algorithm)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis_d(option = "viridis") +
    labs(x = "Experiment", y = "Best Average Traffic Latency(ms)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), legend.position = "top", axis.text = element_text(family = "Times New Roman", size = 14), legend.text = element_text(family = "Times New Roman", size = 14), legend.title = element_text(family = "Times New Roman", size = 14), axis.title = element_text(family = "Times New Roman", size = 14)) +
    labs(color = "Algorithm")

ggsave("experiments/genesis_experiments/analysis/first_stage_best_latency.png", width = 6, height = 4, dpi = 300)

ggplot(second_stage, aes(x = experiment, y = best_latency, fill = algorithm)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis_d(option = "viridis") +
    labs(x = "Experiment", y = "Best Average Traffic Latency(ms)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), legend.position = "top", axis.text = element_text(family = "Times New Roman", size = 14), legend.text = element_text(family = "Times New Roman", size = 14), legend.title = element_text(family = "Times New Roman", size = 14), axis.title = element_text(family = "Times New Roman", size = 14)) +
    labs(color = "Algorithm")

ggsave("experiments/genesis_experiments/analysis/second_stage_best_latency.png", width = 6, height = 4, dpi = 300)


converge_first <- first_stage %>%
    group_by(algorithm) %>%
    summarize(
        Yes = sum(did_converge == "True"),
        No = sum(did_converge == "False")
    ) %>%
    pivot_longer(cols = c("Yes", "No"), names_to = "Converged", values_to = "Count") %>%
    mutate(Converged = fct_relevel(Converged, "Yes", "No"))

fs_pi <- ggplot(converge_first, aes(x = 1.5, y = Count, fill = Converged)) +
    scale_fill_discrete(type = c("#6fc53a", "#ff4457")) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    xlim(c(0.2, 2)) +
    facet_wrap(~algorithm) +
    labs(fill = "Did it converge?") +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(family = "Times New Roman", size = 14),
        legend.title = element_text(family = "Times New Roman", size = 14),
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt")
    )

converge_second <- second_stage %>%
    group_by(algorithm) %>%
    summarize(
        Yes = sum(did_converge == "True"),
        No = sum(did_converge == "False")
    ) %>%
    pivot_longer(cols = c("Yes", "No"), names_to = "Converged", values_to = "Count")

ss_pi <- ggplot(converge_second, aes(x = 1.5, y = Count, fill = Converged)) +
    scale_fill_discrete(type = c("#ff4457", "#6fc53a")) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    xlim(c(0.2, 2)) +
    facet_wrap(~algorithm) +
    labs(fill = "Did it converge?") +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt")
    )

pies <- grid.arrange(
    arrangeGrob(fs_pi, top = textGrob("First Stage", just = "left", hjust = 1.6, gp = gpar(fontfamily = "Times New Roman", fontsize = 16))),
    arrangeGrob(ss_pi, top = textGrob("Second Stage", just = "left", hjust = 0.8, gp = gpar(fontfamily = "Times New Roman", fontsize = 16)), heights = c(0.86, 0.14)),
    ncol = 2,
    widths = c(1.45, 1)
)

ggsave("experiments/genesis_experiments/analysis/pies.png", width = 6, height = 4, dpi = 300, plot = pies)

average <- data %>%
    group_by(algorithm) %>%
    summarize(
        avg_time = mean(convergence_time, na.rm = TRUE),
        ci = qnorm(1 - (0.05 / 2)) * sd(convergence_time, na.rm = TRUE) / sqrt(n()),
        range = max(convergence_time, na.rm = TRUE) - min(convergence_time, na.rm = TRUE),
        max_time = round(max(convergence_time, na.rm = TRUE), 2),
        min_time = round(min(convergence_time, na.rm = TRUE), 2)
    )

ggplot(average, aes(x = algorithm, y = avg_time, color = algorithm)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = min_time, ymax = max_time), width = 0.5, size = 1) +
    scale_color_brewer(palette = "Set1") +
    scale_y_log10() +
    labs(x = "Algorithm", y = "Average Execution Time (mins)") +
    theme_light() +
    theme(axis.text.x = element_text(size = 12), legend.position = "none", axis.text = element_text(family = "Times New Roman", size = 14), axis.title = element_text(family = "Times New Roman", size = 14))

ggplot(data, aes(x = algorithm, y = convergence_time, color = algorithm)) +
    geom_boxplot(size = 0.2) +
    scale_color_brewer(palette = "Set1") +
    scale_y_log10() +
    theme_light() +
    labs(x = "Algorithm", y = "Execution Time (mins)") +
    theme(axis.text.x = element_text(size = 12), legend.position = "none", axis.text = element_text(family = "Times New Roman", size = 14), axis.title = element_text(family = "Times New Roman", size = 14))

ggsave("experiments/genesis_experiments/analysis/execution_time.png", width = 6, height = 3, dpi = 300)
