time_frustration <- data$taskcompletiontime[data$frustration == "Y"]
time_no_frustration <- data$taskcompletiontime[data$frustration == "N"]

t.test(time_frustration, time_no_frustration, alternative = "greater", conf.level = 0.95)
