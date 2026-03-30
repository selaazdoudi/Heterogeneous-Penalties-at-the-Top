library(readr)
library(dplyr)
library(hdm)
library(grf)
library(ggplot2)
library(rlang)
library(broom)
library(patchwork)
library(xtable)


# Import data
data_bach <- read_csv("acs2016gender.csv" )


###############################################################################
# Causal Forest
###############################################################################

X_grf <- model.matrix(
  ~ -1
  + married + nevermarried + chld19 + region + msa +
    deg + occ + ind + hw + yos + exp + exp2 + age,
  data = data_bach
)

D <- ifelse(data_bach$fem=="TRUE", 1, 0)
Y_grf <- data_bach$lwwage



########## Train 

cf <- causal_forest(
  X = X_grf,
  Y = Y_grf,
  W = D,
  num.trees = 2000
)


########### Predict
predictions <-  predict(cf, X_grf, estimate.variance=TRUE)
tau_hat <- predictions$predictions
standard.error <-  sqrt(predictions$variance.estimates)


# Predict multiple ATEs 
average_treatment_effect(cf)
ate_aipw <- average_treatment_effect(cf, method = "AIPW", target.sample = "all")
ate_tmle <- average_treatment_effect(cf, method = "TMLE", target.sample = "all")

# Predict ATE overlap
ate_aipw_overl <- average_treatment_effect(cf, method = "AIPW", target.sample = "overlap")
ate_tmle_overl <- average_treatment_effect(cf, method = "TMLE", target.sample = "overlap")


## Best linear projections
coeffs <- best_linear_projection(cf, X_grf)

# Convert to xtable
coef_mat_cf <- coeffs
pvals <- coef_mat_cf[,4]

stars <- ifelse(pvals < 0.01, "***",
                ifelse(pvals < 0.05, "**",
                       ifelse(pvals < 0.1, "*", "")))

estimates_with_stars_cf <- paste0(
  sprintf("%.3f", coef_mat_cf[,1]),
  stars
)

full_table_cf <- data.frame(
  Variable = rownames(coef_mat_cf),
  Estimate  = estimates_with_stars_cf,
  Std.Error = sprintf("%.3f", coef_mat_cf[,2]),
  t.value   = sprintf("%.3f", coef_mat_cf[,3]),
  p.value   = sprintf("%.3f", coef_mat_cf[,4])
)

xt_cf <- xtable(full_table_cf)

print(xt_cf,
      type = "latex",
      file = "cf_full_table.tex",
      include.rownames = TRUE,
      sanitize.text.function = identity,  # IMPORTANT for stars
      booktabs = TRUE)


###### Quantile plot ########################################################
df <- data.frame(
  tau_hat = tau_hat,
  se = standard.error
)

df$upper <- df$tau_hat + 1.96 * df$se
df$lower <- df$tau_hat - 1.96 * df$se


df_analysis <- cbind(data_bach, df[,c("tau_hat", "se", "upper", "lower")])


df$percentile <- rank(df$upper) 

df$is_significant <- df$upper < 0
ggplot(df, aes(x = percentile, y = tau_hat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_point(color = "navy") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Rank of Individual Effect", y = "Estimated Wage Gap (CATE)") +
  theme_minimal() + 
  labs(x = "Ranked Individual Effect", 
       y = "Estimated Wage Gap (CATE)", 
       title = "CATE Effects with 95% Confidence Interval") +
  
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()   )

#################################################################################
# Descriptive stats heterogeneity
#################################################################################

cov<- c("married", "nevermarried", "chld19",  "region",  "msa" , "deg" ,  "occ" ,  "ind" ,  "hw", "yos",  "exp",  "exp2",  "age" )
plot_list <- list()
for (name in cov){
  p <- ggplot(df_analysis, aes(x = !!sym(name), y = tau_hat, group =!!sym(name))) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
    labs(
      title = paste("Distribution of individual predicted penalty by", name),
      x = name,
      y = "prediction"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  # remove legend if fill is obvious from x-axis
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  plot_list[[name]] <- p
}


################################################################################
# Coefficient plots
################################################################################

tidy_coeffs <- tidy(coeffs) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    category = case_when(
      grepl("^region", term) ~ "Region",
      grepl("^deg", term)    ~ "Degree",
      grepl("^occ", term) ~ "Occupation",
      grepl("^ind", term) ~"Industry", 
      grepl("^hw", term) ~ "Hours Worked",
      grepl("^exp", term ) ~ "Experience",
      term %in% c("married", "chld19", "msa") ~ "Demographics",
      term %in% c("yos") ~ "Years of Studies", 
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(category))

categories <- unique(tidy_coeffs$category)
plot_list_coeffs <- list()

for (cat in categories) {
  
  data_subset <- tidy_coeffs %>% 
    filter(category == cat)
  
  p <- ggplot(data_subset,
              aes(x = reorder(term, estimate),
                  y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high),
                  width = 0.2) +
    geom_hline(yintercept = 0,
               linetype = "dashed") +
    coord_flip() +
    labs(title = paste("Point Estimates and 95% CIs:", cat),
         x = "",
         y = "Estimate") +
    theme_minimal()
  
  plot_list_coeffs[[cat]] <- p
}

###############################################################################
# INSPECTING TOP AND BOTTOM INDIVIDUALS IN TERMS OF WAGE PENALTY
###############################################################################
n <- length(df_analysis$tau_hat)
bottom20_idx <- order(df_analysis$tau_hat)[1:floor(0.2 * n)]          
top20_idx    <- order(df_analysis$tau_hat, decreasing = TRUE)[1:floor(0.2 * n)]  
bottom20_data <- df_analysis[bottom20_idx, ]
top20_data <- df_analysis[top20_idx, ]

# Identify main differences
summary(bottom20_data)
summary(top20_data)

# Assign groups to df_analysis
df_analysis$tau_group <- NA
df_analysis$tau_group[bottom20_idx] <- "Bottom 20%"
df_analysis$tau_group[top20_idx] <- "Top 20%"
df_analysis$tau_group[is.na(df_analysis$tau_group)] <- "Middle 60%"

# Veirfy distribution cat
prop.table(table(df_analysis$occ, df_analysis$tau_group), margin = 2)
prop.table(table(df_analysis$deg, df_analysis$tau_group), margin = 2)
prop.table(table(df_analysis$ind, df_analysis$tau_group), margin = 2)
prop.table(table(df_analysis$region, df_analysis$tau_group), margin = 2)
prop.table(table(df_analysis$hw, df_analysis$tau_group), margin = 2)

#################################################################################
# FEATURE IMPORTANCE
################################################################################
vars <- colnames(X_grf)
var_importance <- variable_importance(cf)
rownames(var_importance) <- vars
sort(var_importance, decreasing=TRUE)
feature_importance <- data.frame(Variable=rownames(var_importance), Importance=as.numeric(var_importance[,1]) )
xt_importance <- xtable(feature_importance)
print(xt_importance,
      type = "latex",
      file = "cf_importance.tex",
      include.rownames = TRUE,
      booktabs = TRUE)


###############################################################################
# PENALTY AND INCOME
##############################################################################
income_deciles <- cut(data_bach$incwage, 
                      breaks = quantile(data_bach$incwage, probs = seq(0, 1, 0.1)), 
                      include.lowest = TRUE, labels = FALSE)

df_plot <- data.frame(
  income = data_bach$incwage,
  penalty = predictions$predictions,
  variance = predictions$variance.estimates
)
summary_stats <- df_plot %>%
  group_by(decile = ntile(income, 10)) %>%
  summarize(
    mean_penalty = mean(penalty),
    robust_se = sqrt(mean(variance)) 
  )

ggplot(summary_stats, aes(x = factor(decile), y = mean_penalty)) +
  geom_linerange(aes(ymin = mean_penalty - 1.96 * robust_se, 
                     ymax = mean_penalty + 1.96 * robust_se), 
                 color = "grey70", linewidth = 1) +
  geom_point(color = "navy", size = 3) +
  geom_line(aes(group = 1), color = "navy", alpha = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Average Pay Gap by Income",
       subtitle="95% Confidence Intervals",
       x = "Income Decile", y = "Estimated Gap") +
  theme_minimal()
