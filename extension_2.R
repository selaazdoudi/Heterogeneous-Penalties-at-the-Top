###############################################################################
######################## EXTENSION 2: GREEDY WORK ############################
######################## DML & CONVEX RETURNS      ############################
###############################################################################

# 1. Load necessary libraries
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)
library(ggplot2)
library(ranger)

# 2. Data Preparation
# Convert data to data.table for high-performance manipulation
dt <- as.data.table(acs2016gender)

# Define high-dimensional control variables
covariates <- c("married", "chld19", "region", "msa", "deg", "occ", "ind", "exp", "exp2", "yos")
y_var <- "lwwage"
baseline <- "40to49"
greedy_categories <- c("50to59", "60to69", "70plus")

# 3. Configure Machine Learning Learners
# Using Random Forest (ranger) to capture non-linear nuisance structures
lgbm_reg <- lrn("regr.ranger", num.trees = 500)
lgbm_class <- lrn("classif.ranger", num.trees = 500)

# 4. DML Loop: Estimate Market Premium per category
# Employs Partialling-out and 5-fold Cross-fitting
results_dml <- list()

for (cat in greedy_categories) {
  cat(paste0("\n--- DML Estimation: ", cat, " vs ", baseline, " ---\n"))
  
  # Filter: Compare specific "Greedy" category only against 40-49h baseline
  temp_dt <- dt[hw %in% c(baseline, cat)]
  temp_dt[, treatment := ifelse(hw == cat, 1, 0)]
  
  # Initialize DoubleML Data object
  dml_data <- DoubleMLData$new(temp_dt, y_col = y_var, d_cols = "treatment", x_cols = covariates)
  
  # Apply Partially Linear Regression (PLR) model
  dml_plr <- DoubleMLPLR$new(dml_data, ml_l = lgbm_reg, ml_m = lgbm_class, n_folds = 5)
  dml_plr$fit()
  
  # Store summary results
  results_dml[[cat]] <- dml_plr$summary()
}

# 5. Result Synthesis (DML Premium vs GRF Penalty)
# Incorporating your specific GRF coefficients for the final comparison
plot_data <- data.frame(
  Hours = factor(c("40-49", "50-59", "60-69", "70+"), 
                 levels = c("40-49", "50-59", "60-69", "70+")),
  # DML Results (Market Premium)
  Market_Premium = c(0, 0.2052, 0.2734, 0.2684), 
  # GRF Results (Gender Penalty)
  Gender_Penalty = c(0, 0.0146, -0.0273, -0.2090)
)

# 6. Generate Visualization: "The Greedy Work Trap"
ggplot(plot_data, aes(x = Hours, group = 1)) +
  # Market Premium (Blue Line - Convexity)
  geom_line(aes(y = Market_Premium, color = "Market Premium (DML)"), size = 1.2) +
  geom_point(aes(y = Market_Premium, color = "Market Premium (DML)"), size = 4) +
  # Gender Penalty (Red Line - Threshold)
  geom_line(aes(y = Gender_Penalty, color = "Gender Penalty (GRF)"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Gender_Penalty, color = "Gender Penalty (GRF)"), size = 4) +
  # Formatting
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Market Premium (DML)" = "#2c3e50", "Gender Penalty (GRF)" = "#e74c3c")) +
  labs(title = "The 70-Hour Threshold: Where Market Premiums Meet the Glass Ceiling",
       subtitle = "Market premiums are convex, but the gender penalty is a brutal threshold effect.",
       x = "Hours worked per week",
       y = "Effect on Salary (log points / %)",
       color = "Indicator") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# 7. Print final numerical summaries
print(results_dml)