# DISCLAIMER: DO NOT USE RSTUDIO. We heavily recommend using VSCode as it is an IDE that can run all of this at once

p_values <- list(accuracy = numeric(500), fpr = numeric(500), fnr = numeric(500))
m <- 94
set.seed(123) # Set seed for reproducibility

# Function to perform the t-test and return p-values
perform_test <- function(m, min_val, max_val, alternative) {
  satellite_data <- rbinom(m, 1, 0.5)
  data <- runif(m, min = min_val, max = max_val)
  result <- t.test(data[satellite_data == 1], data[satellite_data == 0], var.equal = TRUE, alternative = alternative)
  return(result$p.value)
}

# Combined loop for all tests
for (i in 1:500) {
  p_values$accuracy[i] <- perform_test(m, min_val = 0.54, max_val = 0.91, alternative = "greater")
  p_values$fpr[i] <- perform_test(m, min_val = 0.02, max_val = 0.45, alternative = "less")
  p_values$fnr[i] <- perform_test(m, min_val = 0.01, max_val = 0.38, alternative = "less")
}

# Plot histograms for the p-value distributions
png("p_values_accuracy.png")
hist(p_values$accuracy, main = "p-values distribution for accuracy t-tests", xlab = "p-value", col = "#004cff", breaks = 20)
dev.off()

png("p_values_fpr.png")
hist(p_values$fpr, main = "p-values distribution for false_positive_rate t-tests", xlab = "p-value", col = "#1dc92c", breaks = 20)
dev.off()

png("p_values_fnr.png")
hist(p_values$fnr, main = "p-values distribution for false_negative_rate t-tests", xlab = "p-value", col = "#fa1c1c", breaks = 20)
dev.off()