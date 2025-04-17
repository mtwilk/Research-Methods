# DISCLAIMER: DO NOT USE RSTUDIO. We heavily recommend using VSCode as it is an IDE that can run all of this at once

p_values_accuracy <- numeric(500)
p_values_fpr <- numeric(500)
p_values_fnr <- numeric(500)
m <- 94

# Loop for accuracy
for (i in 1:500) {
  # generate data for satellite data using a normal distribution
  satellite_data <- rbinom(m, 1, 0.5)
  
  # generate the data for accuracy
  accuracy <- runif(m, min = 0.54, max = 0.91)
  
  # t-test for accuracy
  result_1 <- t.test(accuracy[satellite_data == 1], accuracy[satellite_data == 0], var.equal = TRUE, alternative="greater")
  p_values_accuracy[i] = result_1$p.value
}

# Loop for false positive rate
for (i in 1:500) {
  # generate data for satellite data using a normal distribution
  satellite_data <- rbinom(m, 1, 0.5)
  
  # generate the data for false positive rate
  false_positive <- runif(m, min = 0.02, max = 0.45)
  
  # t-test for false positive rate
  result_2 <- t.test(false_positive[satellite_data == 1], false_positive[satellite_data == 0], var.equal = TRUE, alternative="less")
  p_values_fpr[i] = result_2$p.value
}

# Loop for false negative rate
for (i in 1:500) {
  # generate data for satellite data using a normal distribution
  satellite_data <- rbinom(m, 1, 0.5)
  
  # generate the data for false negative rate
  false_negative <- runif(m, min = 0.01, max = 0.38)
  
  # t-test for false negative rate
  result_3 <- t.test(false_negative[satellite_data == 1], false_negative[satellite_data == 0], var.equal = TRUE, alternative="less")
  p_values_fnr[i] = result_3$p.value
}

png("p_values_accuracy.png")
hist(p_values_accuracy, main = "p-values distribution for accuracy t-tests", xlab = "p-value", col = "#004cff", breaks = 20)
dev.off()
png("p_values_fpr.png")
hist(p_values_fpr, main = "p-values distribution for false_positive_rate t-tests", xlab = "p-value", col = "#1dc92c", breaks = 20)
dev.off()
png("p_values_fnr.png")
hist(p_values_fnr, main = "p-values distribution for false_negative_rate t-tests", xlab = "p-value", col = "#fa1c1c", breaks = 20)
dev.off()
