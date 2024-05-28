data <- read.csv("dataset1.csv", sep=";")

# Algemene samenvatting
summary(data$pressure)

# Samenvatting voor mensen met opwekking van frustratie
summary(data$pressure[data$frustration == "Y"])

# Histogram voor druk
hist(data$pressure, main="Distribution of Pressure", xlab="Pressure", breaks=5)

# Histogram voor druk met frustratie opwekking
hist(data$pressure[data$frustration == "Y"], main="Distribution of Pressure with Frustration", xlab="Pressure", breaks=5)
