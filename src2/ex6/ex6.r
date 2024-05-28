pressure_haptic <- data$pressure[data$adaptation == "C"]
pressure_visual <- data$pressure[data$adaptation == "B"]

t.test(pressure_haptic, pressure_visual, alternative = "two.sided", conf.level = 0.90)
