# ==========================================
# ASSIGNMENT 4
# Linear Regression Model
# ==========================================

# Load Dataset
data("airquality")

# ==========================================
# DATASET OVERVIEW
# ==========================================

cat("=========== FIRST 6 ROWS ===========\n")
print(head(airquality))

cat("\n=========== LAST 6 ROWS ===========\n")
print(tail(airquality))

cat("\n=========== STRUCTURE OF DATASET ===========\n")
str(airquality)

cat("\n=========== SUMMARY OF DATASET ===========\n")
summary(airquality)

# ==========================================
# HANDLE MISSING VALUES
# ==========================================

airquality <- na.omit(airquality)

# ==========================================
# BUILD LINEAR REGRESSION MODEL
# Ozone ~ Temp
# ==========================================

model <- lm(Ozone ~ Temp, data = airquality)

# ==========================================
# MODEL SUMMARY
# ==========================================

cat("\n=========== MODEL SUMMARY ===========\n")
summary(model)

# ==========================================
# PREDICTED VALUES
# ==========================================

predicted_values <- predict(model)

cat("\n=========== PREDICTED VALUES ===========\n")
print(head(predicted_values))

# ==========================================
# VISUALIZATION
# ==========================================

plot(
  airquality$Temp,
  airquality$Ozone,
  main = "Linear Regression Model",
  xlab = "Independent Variable (Temp)",
  ylab = "Dependent Variable (Ozone)",
  pch = 19,
  col = "blue"
)

abline(model,
       col = "red",
       lwd = 2)

# ==========================================
# REGRESSION EQUATION
# ==========================================

eq <- paste(
  "Y =",
  round(coef(model)[1], 2),
  "+",
  round(coef(model)[2], 2),
  "* X"
)

legend(
  "topleft",
  legend = eq,
  text.col = "darkred",
  bty = "n"
)

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Linear regression model implemented successfully.\n")