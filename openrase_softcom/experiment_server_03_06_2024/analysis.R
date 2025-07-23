library(tidyverse)
library(ggplot2)
library(gridExtra)

# Average Latency
average_latency <- read_csv("./experiments/experiment_server_03_06_2024/latency_data.csv")

upper_bound  <- quantile(average_latency$average_latency, 0.97)
average_latency <- average_latency %>% filter(average_latency <= upper_bound)
exp1 <- average_latency %>% filter(experiment == "SFCs:4-Topology:0.5")

plot1 <- ggplot(exp1, aes(x = ave(duration, sfc, FUN = cumsum), y = average_latency, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "4 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "Latency (ms)") +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5), legend.text = element_text(size = 16), legend.title = element_text(size = 16))

ggsave("./experiments/experiment_server_03_06_2024/exp1_latency_plot.png", plot1, width = 10, height = 4, dpi = 300)

plot1  <-  ggplot(exp1, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="4 SFCRs and 2 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp2 <- average_latency %>% filter(experiment == "SFCs:4-Topology:1")
plot2 <-  ggplot(exp2, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="4 SFCRs and 4 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp3 <- average_latency %>% filter(experiment == "SFCs:8-Topology:0.5")
plot3 <-  ggplot(exp3, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="8 SFCRs and 2 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp4 <- average_latency %>% filter(experiment == "SFCs:8-Topology:1")
plot4 <-  ggplot(exp4, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="8 SFCRs and 4 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp5 <- average_latency %>% filter(experiment == "SFCs:16-Topology:0.5")
plot5 <-  ggplot(exp5, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="16 SFCRs and 2 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp6 <- average_latency %>% filter(experiment == "SFCs:16-Topology:1")
plot6 <-  ggplot(exp6, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="16 SFCRs and 4 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp7 <- average_latency %>% filter(experiment == "SFCs:32-Topology:0.5")
plot7 <-  ggplot(exp7, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="32 SFCRs and 2 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

exp8 <- average_latency %>% filter(experiment == "SFCs:32-Topology:1")
plot8 <-  ggplot(exp8, aes(x=ave(duration, sfc, FUN=cumsum), y=average_latency, group=sfc, color=sfc)) +
    geom_line() +
    labs(title="32 SFCRs and 4 CPUs each in a host", x="Time (s)", y="Latency (ms)") +
    theme_minimal()

plot  <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2, top="Latency")
ggsave("./experiments/experiment_server_03_06_2024/average_latency_plots.png", plot, width = 20, height = 20, dpi = 300)

# Requests
requests <- read_csv("./experiments/experiment_server_03_06_2024/latency_data.csv")

upper_bound  <- quantile(requests$requests, 0.97)
requests <- requests %>% filter(requests <= upper_bound)
exp1 <- requests %>% filter(experiment == "SFCs:4-Topology:0.5")
plot1 <- ggplot(exp1, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "4 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp2 <- requests %>% filter(experiment == "SFCs:4-Topology:1")
plot2 <- ggplot(exp2, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "4 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp3 <- requests %>% filter(experiment == "SFCs:8-Topology:0.5")
plot3 <- ggplot(exp3, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "8 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp4 <- requests %>% filter(experiment == "SFCs:8-Topology:1")
plot4 <- ggplot(exp4, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "8 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp5 <- requests %>% filter(experiment == "SFCs:16-Topology:0.5")
plot5 <- ggplot(exp5, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "16 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp6 <- requests %>% filter(experiment == "SFCs:16-Topology:1")
plot6 <- ggplot(exp6, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "16 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp7 <- requests %>% filter(experiment == "SFCs:32-Topology:0.5")
plot7 <- ggplot(exp7, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "32 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

exp8 <- requests %>% filter(experiment == "SFCs:32-Topology:1")
plot8 <- ggplot(exp8, aes(x = ave(duration, sfc, FUN = cumsum), y = requests, group = sfc, color = sfc)) +
    geom_line() +
    labs(title = "32 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "no. of requests") +
    theme_minimal()

plot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2, top = "Number of Requests")
ggsave("./experiments/experiment_server_03_06_2024/no_of_requests_plots.png", plot, width = 20, height = 20, dpi = 300)

# CPU Usage
host <- read_csv("./experiments/experiment_server_03_06_2024/host_data.csv")

exp1 <- host %>%
    filter(experiment == "SFCs:4-Topology:0.5") %>%
    mutate(cpu_per = cpu / 2 * 100)

plot1 <- ggplot(exp1, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "4 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 20, hjust = 0.5), legend.text = element_text(size = 16), legend.title = element_text(size = 16))
    scale_y_continuous(limits = c(0, 100))

ggsave("./experiments/experiment_server_03_06_2024/exp1_cpu_usage_plot.png", plot1, width = 10, height = 4, dpi = 300)

plot1 <- ggplot(exp1, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "4 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

exp2 <- host %>%
    filter(experiment == "SFCs:4-Topology:1") %>%
    mutate(cpu_per = cpu / 4 * 100)
plot2 <- ggplot(exp2, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "4 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

exp3 <- host %>%
    filter(experiment == "SFCs:8-Topology:0.5") %>%
    mutate(cpu_per = cpu / 2 * 100)
plot3 <- ggplot(exp3, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "8 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

exp4 <- host %>%
    filter(experiment == "SFCs:8-Topology:1") %>%
    mutate(cpu_per = cpu / 4 * 100)
plot4 <- ggplot(exp4, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "8 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

exp5 <- host %>%
    filter(experiment == "SFCs:16-Topology:0.5") %>%
    mutate(cpu_per = cpu / 2 * 100)
plot5 <- ggplot(exp5, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "16 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
        scale_y_continuous(limits = c(0, 100))

exp6 <- host %>%
    filter(experiment == "SFCs:16-Topology:1") %>%
    mutate(cpu_per = cpu / 4 * 100)
plot6 <- ggplot(exp6, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "16 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

exp7 <- host %>%
    filter(experiment == "SFCs:32-Topology:0.5") %>%
    mutate(cpu_per = cpu / 2 * 100)
plot7 <- ggplot(exp7, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "32 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

exp8 <- host %>%
    filter(experiment == "SFCs:32-Topology:1") %>%
    mutate(cpu_per = cpu / 4 * 100)
plot8 <- ggplot(exp8, aes(x = ave(duration, host, FUN = cumsum), y = cpu_per, group = host, color = host)) +
    geom_line() +
    labs(title = "32 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "CPU usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))

plot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2, top = "CPU Usage")
ggsave("./experiments/experiment_server_03_06_2024/cpu_usage_plots.png", plot, width = 20, height = 20, dpi = 300)


# Memory Usage
host <- read_csv("./experiments/experiment_server_03_06_2024/host_data.csv") %>%
    mutate(mem_percent = memory / 64284 * 100)

exp1 <- host %>% filter(experiment == "SFCs:4-Topology:0.5")
plot1 <- ggplot(exp1, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "4 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp2 <- host %>% filter(experiment == "SFCs:4-Topology:1")
plot2 <- ggplot(exp2, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "4 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp3 <- host %>% filter(experiment == "SFCs:8-Topology:0.5")
plot3 <- ggplot(exp3, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "8 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp4 <- host %>% filter(experiment == "SFCs:8-Topology:1")
plot4 <- ggplot(exp4, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "8 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp5 <- host %>% filter(experiment == "SFCs:16-Topology:0.5")
plot5 <- ggplot(exp5, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "16 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp6 <- host %>% filter(experiment == "SFCs:16-Topology:1")
plot6 <- ggplot(exp6, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "16 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp7 <- host %>% filter(experiment == "SFCs:32-Topology:0.5")
plot7 <- ggplot(exp7, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "32 SFCRs and 2 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

exp8 <- host %>% filter(experiment == "SFCs:32-Topology:1")
plot8 <- ggplot(exp8, aes(x = ave(duration, host, FUN = cumsum), y = mem_percent, group = host, color = host)) +
    geom_line() +
    labs(title = "32 SFCRs and 4 CPUs each in a host", x = "Time (s)", y = "Memory usage (%)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 7))

plot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2, top = "Memory Usage (Total memory = 64284 MB)")
ggsave("./experiments/experiment_server_03_06_2024/memory_usage_plots.png", plot, width = 20, height = 20, dpi = 300)
