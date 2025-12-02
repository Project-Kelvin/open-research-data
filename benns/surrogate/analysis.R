library(tidyverse)
library(ggplot2)

data  <- read.csv("research-analysis/surrogate/v3/predictions.csv")

fit <- data %>%
    filter(max_cpu < 1.0, max_link_score < 150)

unfit <- data %>%
    filter(max_cpu >= 1.0 | max_link_score >= 150)

fit_error  <- mean(abs(fit$latency - fit$PredictedLatency))
unfit_error <- mean(abs(unfit$latency - unfit$PredictedLatency))

low_lat <- data %>%
    filter(latency < 150)

low_lat_error <- mean(abs(low_lat$latency - low_lat$PredictedLatency))

high_lat <- data %>%
    filter(latency >= 150)

high_lat_error <- mean(abs(high_lat$latency - high_lat$PredictedLatency))

overall_error <- mean(abs(data$latency - data$PredictedLatency))
cat("Fit Error: ", fit_error, "\n")
cat("Unfit Error: ", unfit_error, "\n")
cat("Low Latency Error: ", low_lat_error, "\n")
cat("High Latency Error: ", high_lat_error, "\n")
cat("Overall Error: ", overall_error, "\n")
