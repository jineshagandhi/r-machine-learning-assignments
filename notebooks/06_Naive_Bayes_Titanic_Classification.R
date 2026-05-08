# ==========================================
# ASSIGNMENT 6
# Naive Bayes Titanic Classification
# ==========================================

# ==========================================
# LOAD DATASET
# ==========================================

df <- read.csv("datasets/Titanic-Dataset.csv")

# ==========================================
# CHECK DATA
# ==========================================

cat("=========== FIRST 6 ROWS ===========\n")
print(head(df))

cat("\n=========== STRUCTURE OF DATASET ===========\n")
str(df)

# ==========================================
# HANDLE MISSING VALUES
# ==========================================

df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

df$Embarked[is.na(df$Embarked)] <- "S"

# ==========================================
# CONVERT VARIABLES TO FACTORS
# ==========================================

df$Survived <- as.factor(df$Survived)
df$Pclass   <- as.factor(df$Pclass)
df$Sex      <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

# ==========================================
# VISUALIZATION
# Survival by Gender
# ==========================================

gender_survival <- table(df$Sex, df$Survived)

barplot(
  gender_survival,
  beside = TRUE,
  legend = TRUE,
  main = "Survival by Gender",
  xlab = "Gender",
  ylab = "Number of Passengers"
)

# ==========================================
# TRAIN-TEST SPLIT
# ==========================================

set.seed(123)

index <- sample(1:nrow(df), 0.7 * nrow(df))

train <- df[index, ]
test  <- df[-index, ]

# ==========================================
# LOAD LIBRARY
# ==========================================

if (!require(e1071)) {
  install.packages("e1071")
}

library(e1071)

# ==========================================
# BUILD NAIVE BAYES MODEL
# ==========================================

model <- naiveBayes(
  Survived ~ Pclass + Sex + Age + Embarked,
  data = train
)

# ==========================================
# PREDICTIONS
# ==========================================

pred <- predict(model, test)

# ==========================================
# CONFUSION MATRIX
# ==========================================

cat("\n=========== CONFUSION MATRIX ===========\n")

conf_matrix <- table(
  Predicted = pred,
  Actual = test$Survived
)

print(conf_matrix)

# ==========================================
# ACCURACY
# ==========================================

accuracy <- sum(pred == test$Survived) / nrow(test)

cat("\nAccuracy :", round(accuracy * 100, 2), "%\n")

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Naive Bayes classification on Titanic dataset implemented successfully.\n")