# ==========================================
# ASSIGNMENT 5
# Logistic Regression and ROC Curve
# ==========================================

# Load Dataset
data("airquality")

# ==========================================
# HANDLE MISSING VALUES
# ==========================================

airquality <- na.omit(airquality)

# ==========================================
# CREATE BINARY TARGET VARIABLE
# ==========================================

airquality$HighOzone <- ifelse(airquality$Ozone > 50, 1, 0)

# ==========================================
# BUILD LOGISTIC REGRESSION MODEL
# ==========================================

log_model <- glm(
  HighOzone ~ Temp,
  data = airquality,
  family = binomial
)

# ==========================================
# MODEL SUMMARY
# ==========================================

cat("=========== MODEL SUMMARY ===========\n")
summary(log_model)

# ==========================================
# PREDICT PROBABILITIES
# ==========================================

prob <- predict(log_model, type = "response")

cat("\n=========== PREDICTED PROBABILITIES ===========\n")
print(head(prob))

# ==========================================
# PREDICT CLASSES
# ==========================================

class <- ifelse(prob > 0.5, 1, 0)

cat("\n=========== PREDICTED CLASS ===========\n")
print(head(class))

# ==========================================
# CONFUSION MATRIX
# ==========================================

cat("\n=========== CONFUSION MATRIX ===========\n")

print(
  table(
    Predicted = class,
    Actual = airquality$HighOzone
  )
)

# ==========================================
# LOGISTIC REGRESSION CURVE
# ==========================================

plot(
  airquality$Temp,
  airquality$HighOzone,
  xlab = "Independent Variable (Temp)",
  ylab = "Dependent Variable (0/1)",
  main = "Logistic Regression Curve"
)

curve(
  predict(
    log_model,
    data.frame(Temp = x),
    type = "response"
  ),
  add = TRUE,
  col = "red"
)

# ==========================================
# ROC CURVE
# ==========================================

# Install Package (Run Once)
# install.packages("pROC")

library(pROC)

roc_curve <- roc(airquality$HighOzone, prob)

plot(
  roc_curve,
  main = "ROC Curve"
)

auc_value <- auc(roc_curve)

cat("\nAUC Value :", auc_value, "\n")

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Logistic regression model implemented successfully.\n")