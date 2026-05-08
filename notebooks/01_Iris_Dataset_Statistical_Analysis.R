# ==============================
# ASSIGNMENT 1
# Iris Dataset Statistical Analysis
# ==============================

# Load Dataset
df <- read.csv("datasets/iris.csv")

# Alternative Built-in Dataset
# data(iris)
# df <- iris

# ==============================
# DATASET DETAILS
# ==============================

cat("===== DATASET DETAILS =====\n")

cat("\nFirst 6 Rows:\n")
print(head(df))

cat("\nStructure of Dataset:\n")
str(df)

cat("\nDimensions (Rows, Columns):\n")
print(dim(df))

cat("\nColumn Names:\n")
print(colnames(df))

cat("\nSummary of Dataset:\n")
print(summary(df))

# ==============================
# FEATURES & DATA TYPES
# ==============================

cat("\n===== FEATURES & DATA TYPES =====\n")
print(sapply(df, class))

cat("\nTotal Features :", ncol(df), "\n")
cat("Numeric Features : 4\n")
cat("Categorical Feature : Species\n")

# Select Numeric Columns
numeric_data <- df[, 1:4]

# ==============================
# STATISTICAL ANALYSIS
# ==============================

cat("\n===== MINIMUM VALUES =====\n")
print(apply(numeric_data, 2, min))

cat("\n===== MAXIMUM VALUES =====\n")
print(apply(numeric_data, 2, max))

cat("\n===== MEAN VALUES =====\n")
print(apply(numeric_data, 2, mean))

cat("\n===== RANGE (MIN, MAX) =====\n")
print(apply(numeric_data, 2, range))

cat("\n===== STANDARD DEVIATION =====\n")
print(apply(numeric_data, 2, sd))

cat("\n===== VARIANCE =====\n")
print(apply(numeric_data, 2, var))

cat("\n===== PERCENTILES (0%, 25%, 50%, 75%, 100%) =====\n")
print(apply(numeric_data, 2, quantile))

# ==============================
# HISTOGRAMS
# ==============================

par(mfrow = c(2, 2))

hist(df$Sepal.Length,
     main = "Sepal Length",
     col = "lightblue",
     xlab = "Sepal Length")

hist(df$Sepal.Width,
     main = "Sepal Width",
     col = "lightgreen",
     xlab = "Sepal Width")

hist(df$Petal.Length,
     main = "Petal Length",
     col = "pink",
     xlab = "Petal Length")

hist(df$Petal.Width,
     main = "Petal Width",
     col = "orange",
     xlab = "Petal Width")

par(mfrow = c(1, 1))

# ==============================
# BOXPLOT
# ==============================

boxplot(numeric_data,
        main = "Iris Features Boxplot",
        col = c("lightblue", "lightgreen", "pink", "orange"),
        ylab = "Values")

# ==============================
# CONCLUSION
# ==============================

cat("\n===== CONCLUSION =====\n")
cat("Statistical analysis and visualization completed successfully.\n")