frustration_time <- data$taskcompletiontime[data$frustration == "Y"]

mean_time <- mean(frustration_time)
sd_time <- sd(frustration_time)
n <- length(frustration_time)
error <- qnorm(0.995) * sd_time / sqrt(n)
ci <- c(mean_time - error, mean_time + error)
ci
