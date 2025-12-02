library(tidyverse)
library(ggplot2)
library(patchwork)

cpu <- read.csv("research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/cpu.csv") %>%
  filter(reqps > 0) %>%
  filter(latency > 0) %>%
  mutate(latency = latency - total_delay) %>%
  group_by(time) %>%
  summarise(
    max_cpu = median(max_cpu, na.rm = TRUE),
    latency = median(latency, na.rm = TRUE),
    total_delay = median(total_delay, na.rm = TRUE)
  )

q3 <- quantile(cpu$latency, 0.75, na.rm = TRUE)
q1 <- quantile(cpu$latency, 0.25, na.rm = TRUE)
iqr <- q3 - q1
# Remove outliers based on IQR
# cpu <- cpu %>% filter(latency >= (q1 - 1.5 * iqr) & latency <= (q3 + 1.5 * iqr))

cpu_plot <- ggplot(cpu, aes(x = max_cpu, y = latency)) +
  geom_point(color="#33658A") +
  theme_light() +
  scale_y_log10(limits = c(1, 6000), breaks=c(1, 10, 20, 50, 100, 500, 1000, 2000, 6000)) +
  scale_x_continuous(breaks = seq(0, 2.0, by = 0.2)) +
  theme(text = element_text(size = 14, family = "serif"), axis.text = element_text(size = 14), axis.text.x = element_text(angle=90)) +
  labs(x = "Maximum\nCPU Demand", y = "Traffic Latency (ms)")

ggsave(
  filename = "research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/cpu.png",
  plot = cpu_plot
)

memory <- read.csv("research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/memory.csv") %>%
  filter(reqps > 0) %>%
  filter(latency > 0) %>%
  mutate(latency = latency - total_delay) %>%
  group_by(time) %>%
  summarise(
    max_memory = median(max_memory, na.rm = TRUE),
    latency = median(latency, na.rm = TRUE),
    total_delay = median(total_delay, na.rm = TRUE)
  )

q3 <- quantile(memory$latency, 0.75, na.rm = TRUE)
q1 <- quantile(memory$latency, 0.25, na.rm = TRUE)
iqr <- q3 - q1
# Remove outliers based on IQR
# memory <- memory %>% filter(latency >= (q1 - 1.5 * iqr) & latency <= (q3 + 1.5 * iqr))

memoryPlot <- ggplot(memory, aes(x = max_memory, y = latency)) +
  geom_point(color = "#33658A") +
  theme_light() +
  scale_x_continuous(limits=c(1.30, 1.331)) +
  scale_y_log10(limits=c(1, 6000), breaks=c(1, 10, 20, 50, 100, 500, 1000, 2000, 6000)) +
  theme(text = element_text(size = 14, family = "serif"), axis.text = element_text(size = 14),axis.text.x = element_text(angle=90)) +
  labs(x = "Maximum\nMemory Demand", y=NULL)

ggsave("research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/memory.png")

links <- read.csv("research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/links.csv") %>%
  filter(reqps > 0) %>%
  filter(latency > 0) %>%
  mutate(latency = latency - total_delay) %>%
  group_by(time) %>%
  summarise(
    max_link_score = median(max_link_score, na.rm = TRUE),
    latency = median(latency, na.rm = TRUE),
    total_delay = median(total_delay, na.rm = TRUE)
  )

q3 <- quantile(links$latency, 0.75, na.rm = TRUE)
q1 <- quantile(links$latency, 0.25, na.rm = TRUE)
iqr <- q3 - q1
# Remove outliers based on IQR
#links <- links %>% filter(latency >= (q1 - 1.5 * iqr) & latency <= (q3 + 1.5 * iqr))

linkPlot  <- ggplot(links, aes(x = max_link_score, y = latency)) +
  geom_point(color = "#33658A") +
  theme_light() +
  scale_y_log10(limits=c(1, 6000), breaks=c(1, 10, 20, 50, 100, 500, 1000, 2000, 6000)) +
  theme(text = element_text(size = 14, family = "serif"), axis.text = element_text(size = 14), axis.text.x = element_text(angle=90)) +
  labs(x = "Maximum\nBandwidth Demand", y=NULL)

ggsave("research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/links.png")

combined_plot <- cpu_plot + memoryPlot + linkPlot + plot_layout(ncol = 3)

ggsave(
  filename = "research-analysis/latency-benchmarking/latency_benchmarking_2025_07_23/plots.png",
  plot = combined_plot,
  width = 8,
  height = 4,
  dpi = 300
)
