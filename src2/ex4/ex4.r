no_frustration <- data$pressure[data$frustration == "N"]
prop <- mean(no_frustration > 3)
n <- length(no_frustration)
error <- qnorm(0.975) * sqrt(prop * (1 - prop) / n)
ci <- c(prop - error, prop + error)
ci
