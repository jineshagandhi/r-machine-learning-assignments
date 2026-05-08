# ==========================================
# ASSIGNMENT 2
# Descriptive Statistics and Hypothesis Testing
# ==========================================

# Load Dataset
data(mtcars)

# Select mpg column
x <- mtcars$mpg

# ==========================================
# DESCRIPTIVE STATISTICS
# ==========================================

mean_x   <- mean(x)
median_x <- median(x)
min_x    <- min(x)
max_x    <- max(x)
sd_x     <- sd(x)
var_x    <- var(x)

n  <- length(x)
df <- n - 1

cat("=========== DESCRIPTIVE STATISTICS ===========\n")

cat("Mean                 :", mean_x, "\n")
cat("Median               :", median_x, "\n")
cat("Minimum              :", min_x, "\n")
cat("Maximum              :", max_x, "\n")
cat("Standard Deviation   :", sd_x, "\n")
cat("Variance             :", var_x, "\n")
cat("Sample Size (n)      :", n, "\n")
cat("Degree of Freedom    :", df, "\n")

# ==========================================
# HISTOGRAM
# ==========================================

hist(x,
     main = "Histogram of Mileage (mpg)",
     xlab = "Miles per Gallon",
     col = "lightblue",
     border = "black")

abline(v = mean_x,
       col = "red",
       lwd = 2)

# ==========================================
# BOXPLOT
# ==========================================

boxplot(x,
        main = "Boxplot of Mileage (mpg)",
        col = "lightgreen",
        horizontal = TRUE)

# ==========================================
# HYPOTHESIS TESTING
# ==========================================

mu    <- 20
alpha <- 0.05

# ==========================================
# Z-TEST
# ==========================================

cat("\n=========== Z-TEST ===========\n")

z_value <- (mean_x - mu) / (sd_x / sqrt(n))
p_value_z <- 2 * (1 - pnorm(abs(z_value)))

cat("Z-Statistic :", z_value, "\n")
cat("P-Value     :", p_value_z, "\n")

if (p_value_z < alpha) {
  cat("Decision    : Reject Null Hypothesis\n")
} else {
  cat("Decision    : Fail to Reject Null Hypothesis\n")
}

# ==========================================
# MANUAL T-TEST
# ==========================================

cat("\n=========== MANUAL T-TEST ===========\n")

t_value <- (mean_x - mu) / (sd_x / sqrt(n))
p_value_t <- 2 * (1 - pt(abs(t_value), df))

cat("T-Statistic :", t_value, "\n")
cat("P-Value     :", p_value_t, "\n")

if (p_value_t < alpha) {
  cat("Decision    : Reject Null Hypothesis\n")
} else {
  cat("Decision    : Fail to Reject Null Hypothesis\n")
}

# ==========================================
# BUILT-IN T-TEST
# ==========================================

cat("\n=========== BUILT-IN t.test() OUTPUT ===========\n")

t.test(x, mu = 20)

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Basic statistical analysis, visualization, Z-test, and T-test performed successfully.\n")