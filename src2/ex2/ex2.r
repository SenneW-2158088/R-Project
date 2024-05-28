multimodal_time <- data$taskcompletiontime[data$adaptation == "D"]

mean_time <- mean(multimodal_time)
sd_time <- sd(multimodal_time)
n <- length(multimodal_time)
error <- qnorm(0.975) * sd_time / sqrt(n)
ci <- c(mean_time - error, mean_time + error)
ci
