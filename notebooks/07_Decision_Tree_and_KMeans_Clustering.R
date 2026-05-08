# ==========================================
# ASSIGNMENT 7
# Decision Tree Classification + K-Means Clustering
# ==========================================

# ==========================================
# PART A: DECISION TREE CLASSIFICATION
# ==========================================

# Install packages (run once)
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

set.seed(123)

# Load dataset
data(iris)

# ==========================================
# TRAIN-TEST SPLIT
# ==========================================

train_index <- sample(1:nrow(iris), 0.7 * nrow(iris))

train_data <- iris[train_index, ]
test_data  <- iris[-train_index, ]

# ==========================================
# BUILD DECISION TREE MODEL
# ==========================================

tree_model <- rpart(Species ~ ., data = train_data, method = "class")

# Plot tree
rpart.plot(tree_model)

# ==========================================
# PREDICTION
# ==========================================

pred <- predict(tree_model, test_data, type = "class")

# ==========================================
# CONFUSION MATRIX
# ==========================================

cm <- table(Predicted = pred, Actual = test_data$Species)

cat("\n=========== CONFUSION MATRIX ===========\n")
print(cm)

# Accuracy
accuracy <- sum(diag(cm)) / sum(cm)

cat("\nAccuracy :", round(accuracy * 100, 2), "%\n")

# ==========================================
# PART B: K-MEANS CLUSTERING
# ==========================================

library(cluster)

set.seed(123)

# Use only features
iris_features <- iris[, 1:4]

# Scale data
iris_scaled <- scale(iris_features)

# Apply K-Means
kmeans_result <- kmeans(iris_scaled, centers = 3, nstart = 10)

# Add cluster label
iris$Cluster <- kmeans_result$cluster

# ==========================================
# VISUALIZATION
# ==========================================

plot(
  iris$Sepal.Length,
  iris$Sepal.Width,
  col = iris$Cluster,
  pch = 19,
  xlab = "Sepal Length",
  ylab = "Sepal Width",
  main = "K-Means Clustering"
)

# ==========================================
# CLUSTER ANALYSIS
# ==========================================

cat("\n=========== CLUSTER TABLE ===========\n")
print(table(iris$Species, iris$Cluster))

# ==========================================
# SILHOUETTE SCORE
# ==========================================

sil <- silhouette(kmeans_result$cluster, dist(iris_scaled))

avg_sil <- mean(sil[, 3])

cat("\nSilhouette Score :", round(avg_sil, 3), "\n")

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Decision Tree classification and K-Means clustering implemented successfully.\n")