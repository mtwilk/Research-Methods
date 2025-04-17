# DISCLAIMER: DO NOT USE RSTUDIO. We heavily recommend using VSCode as it is an IDE that can run all of this at once

p_values_accuracy <- numeric(500)
p_values_fpr <- numeric(500)
p_values_fnr <- numeric(500)
m <- 94
set.seed(123) # Set seed for reproducibility

# Loop for accuracy with sequential stopping
for (i in 1:500) {
  for (j in 1:10) {
    satellite_data <- rbinom(m, 1, 0.5)
    accuracy <- runif(m, min = 0.54, max = 0.91)
    result_1 <- t.test(accuracy[satellite_data == 1], accuracy[satellite_data == 0], var.equal = TRUE, alternative = "greater")
    
    if (result_1$p.value < 0.05) {
      break
    }
  }
  p_values_accuracy[i] = result_1$p.value
}

# Loop for false positive rate with sequential stopping
for (i in 1:500) {
  for (j in 1:10) {
    satellite_data <- rbinom(m, 1, 0.5)
    false_positive <- runif(m, min = 0.02, max = 0.45)
    result_2 <- t.test(false_positive[satellite_data == 1], false_positive[satellite_data == 0], var.equal = TRUE, alternative = "less")
    
    if (result_2$p.value < 0.05) {
      break
    }
  }
  p_values_fpr[i] = result_2$p.value
}

# Loop for false negative rate with sequential stopping
for (i in 1:500) {
  for (j in 1:10) {
    satellite_data <- rbinom(m, 1, 0.5)
    false_negative <- runif(m, min = 0.01, max = 0.38)
    result_3 <- t.test(false_negative[satellite_data == 1], false_negative[satellite_data == 0], var.equal = TRUE, alternative = "less")
    
    if (result_3$p.value < 0.05) {
      break
    }
  }
  p_values_fnr[i] = result_3$p.value
}

# Plot histograms for the p-value distributions
png("p_values_accuracy.png")
hist(p_values_accuracy, main = "p-values distribution for accuracy t-tests", xlab = "p-value", col = "#004cff", breaks = 20)
dev.off()

png("p_values_fpr.png")
hist(p_values_fpr, main = "p-values distribution for false_positive_rate t-tests", xlab = "p-value", col = "#1dc92c", breaks = 20)
dev.off()

png("p_values_fnr.png")
hist(p_values_fnr, main = "p-values distribution for false_negative_rate t-tests", xlab = "p-value", col = "#fa1c1c", breaks = 20)
dev.off()