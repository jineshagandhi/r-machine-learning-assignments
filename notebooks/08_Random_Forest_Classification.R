# ==========================================
# ASSIGNMENT 8
# Random Forest Classification (Iris Dataset)
# ==========================================

# ==========================================
# LOAD LIBRARIES
# ==========================================

library(randomForest)
library(caTools)

# ==========================================
# LOAD DATASET
# ==========================================

data <- iris

cat("=========== DATA OVERVIEW ===========\n")
str(data)
summary(data)

# Convert target variable to factor
data$Species <- as.factor(data$Species)

# ==========================================
# TRAIN-TEST SPLIT
# ==========================================

set.seed(123)

split <- sample.split(data$Species, SplitRatio = 0.7)

train_data <- subset(data, split == TRUE)
test_data  <- subset(data, split == FALSE)

# ==========================================
# BUILD RANDOM FOREST MODEL
# ==========================================

model <- randomForest(Species ~ ., data = train_data, ntree = 100)

cat("\n=========== MODEL SUMMARY ===========\n")
print(model)

# ==========================================
# PREDICTIONS
# ==========================================

predictions <- predict(model, test_data)

# ==========================================
# RESULTS TABLE
# ==========================================

result <- data.frame(
  Actual = test_data$Species,
  Predicted = predictions
)

cat("\n=========== PREDICTIONS ===========\n")
print(head(result))

# ==========================================
# CONFUSION MATRIX
# ==========================================

conf_matrix <- table(test_data$Species, predictions)

cat("\n=========== CONFUSION MATRIX ===========\n")
print(conf_matrix)

# ==========================================
# ACCURACY
# ==========================================

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("\nAccuracy :", round(accuracy * 100, 2), "%\n")

# ==========================================
# FEATURE IMPORTANCE
# ==========================================

cat("\n=========== FEATURE IMPORTANCE ===========\n")
print(importance(model))

# Plot variable importance
par(mar = c(5, 4, 4, 2))
varImpPlot(model)

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Random Forest classification implemented successfully.\n")