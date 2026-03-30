library(readr)
library(dplyr)
library(hdm)
library(ggplot2)
library(rlang)
library(broom)
library(xtable)


# Import data
data_bach <- read_csv("acs2016gender.csv" )

################################################################################
# 1. Double Post-Lasso
################################################################################

# Prep
# create the model matrix for the covariates
X <- model.matrix(
  ~  - 1
  + fem:(married  + chld19 + region + msa +
           deg + occ + ind + hw + yos + exp + exp2 + age)
  + + (married  + chld19 + region + msa +
       deg + occ + ind + hw + yos + exp + exp2 + age)^2,
  data = data_bach
)




dim(X)
X <- X[, which(apply(X, 2, var) != 0)] # exclude all constant variables
demean<- function (x){ x- mean(x)}
X <- apply(X, 2, FUN=demean)
dim(X)

# target variables
index.gender <- grep("fem", colnames(X))
y <- data_bach$lwwage


##################################### Double Lasso #############################################
effects.female <- rlassoEffects(
  x = X,
  y = y,
  index = index.gender, 
  post=TRUE
)





# Summarize and extract coefficients
result <- summary(effects.female )
result$coef

# Export : Convert to xtable
coef_mat <- result$coefficients
pvals <- coef_mat[,4]

stars <- ifelse(pvals < 0.01, "***",
                ifelse(pvals < 0.05, "**",
                       ifelse(pvals < 0.1, "*", "")))

estimates_with_stars <- paste0(
  sprintf("%.3f", coef_mat[,1]),
  stars
)

full_table <- data.frame(
  Variable = rownames(coef_mat),
  Estimate  = estimates_with_stars,
  Std.Error = sprintf("%.3f", coef_mat[,2]),
  t.value   = sprintf("%.3f", coef_mat[,3]),
  p.value   = sprintf("%.3f", coef_mat[,4])
)
xt <- xtable(full_table)

print(xt,
      type = "latex",
      file = "rlasso_full_table.tex",
      include.rownames = TRUE,
      sanitize.text.function = identity,  
      booktabs = TRUE)

########################  CI #################################

#  Point-wise
pointwise.CI <- confint(effects.female, level = 0.95, joint = FALSE)
pointwise.CI
plot(effects.female, joint=FALSE, level=0.95) # plot of the effects

# Joint
joint.CI <- confint(effects.female, level = 0.95, joint = TRUE)
joint.CI
plot(effects.female, joint = TRUE, level = 0.95)  # plot of the effects (joint)
print(xtable(joint.CI), type = "latex", digits = 3)


# Extract standard errors
se_hat <- effects.female$se
