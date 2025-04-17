# DISCLAIMER: DO NOT USE RSTUDIO. We heavily recommend using VSCode as it is an IDE that can run all of this at once

p_values_accuracy <- numeric(500)
p_values_fpr <- numeric(500)
p_values_fnr <- numeric(500)
m <- 94 # Maximum number of prediction models sampled
start <- 20
set.seed(123) # Set seed for reproducibility

# Loop for accuracy with sequential stopping
perform_test <- function(min_val, max_val, alternative, df, n_reps, variable) {
  p_values <- numeric(n_reps)
  for (i in 1:n_reps) {
    print(i)
    df <- rbinom(start - 1, 1, 0.5) # Generate a sample with the initial size - 1
    variable <- runif(start, min = min_val, max = max_val) # Generate the accuracy of this point
    for (j in start:m) {
      df <- c(rbinom(1, 1, 0.5), df) # Sample 1 more data point
      variable <- c(runif(1, min = min_val, max = max_val), variable) # Generate the accuracy of this point
      result <- t.test(variable[df == 1], variable[df == 0], var.equal = TRUE, alternative = alternative)
      if (result$p.value < 0.05) { # If the p value is significant, stop
        break
      }
    }
    p_values[i] = result$p.value # Add the p value to the table of accuracies
  }
  return(p_values)
}
p_values_accuracy <- perform_test(0.54, 0.91, "greater", satellite_data, 500, accuracy)

p_values_fpr <- perform_test(0.02, 0.45, "less", satellite_data, 500, false_positive)

p_values_fnr <- perform_test(0.01, 0.38, "less", satellite_data, 500, false_negative)

# Plot histograms for the p-value distributions
png("p_values_accuracy_qrp.png")
hist(p_values_accuracy, main = "p-values distribution for accuracy t-tests", xlab = "p-value", col = "#004cff", breaks = 20)
dev.off()

png("p_values_fpr_qrp.png")
hist(p_values_fpr, main = "p-values distribution for false_positive_rate t-tests", xlab = "p-value", col = "#1dc92c", breaks = 20)
dev.off()

png("p_values_fnr_qrp.png")
hist(p_values_fnr, main = "p-values distribution for false_negative_rate t-tests", xlab = "p-value", col = "#fa1c1c", breaks = 20)
dev.off()