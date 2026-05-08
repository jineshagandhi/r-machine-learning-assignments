# ==========================================
# ASSIGNMENT 3
# Apriori Association Rule Mining
# ==========================================

# Install Packages (Run Once)
# install.packages("arules", dependencies = TRUE)
# install.packages("arulesViz")

# Load Libraries
library(arules)
library(arulesViz)

# ==========================================
# LOAD DATASET
# ==========================================

data("Groceries")

cat("=========== DATASET SUMMARY ===========\n")
summary(Groceries)

cat("\n=========== SAMPLE TRANSACTIONS ===========\n")
inspect(Groceries[1:10])

# ==========================================
# APPLY APRIORI ALGORITHM
# ==========================================

rules <- apriori(
  Groceries,
  parameter = list(
    supp = 0.01,
    conf = 0.3,
    minlen = 2
  )
)

# ==========================================
# GENERATED RULES
# ==========================================

cat("\n=========== GENERATED RULES ===========\n")
inspect(head(rules, 10))

# ==========================================
# SPECIFIC RULE
# other vegetables -> whole milk
# ==========================================

grocery_rule <- subset(
  rules,
  lhs %in% "other vegetables" &
    rhs %in% "whole milk"
)

cat("\n=========== RULE: other vegetables -> whole milk ===========\n")
inspect(grocery_rule)

# ==========================================
# STRONG RULES
# ==========================================

strong_rules <- subset(
  rules,
  lift > 2 & confidence > 0.5
)

strong_rules <- sort(strong_rules, by = "lift")

cat("\n=========== STRONG RULES ===========\n")
inspect(head(strong_rules, 10))

# ==========================================
# VISUALIZATIONS
# ==========================================

plot(strong_rules, method = "graph")

plot(strong_rules, method = "grouped")

plot(rules, method = "scatterplot")

# ==========================================
# CONCLUSION
# ==========================================

cat("\n=========== CONCLUSION ===========\n")
cat("Association rules generated using Apriori algorithm successfully.\n")