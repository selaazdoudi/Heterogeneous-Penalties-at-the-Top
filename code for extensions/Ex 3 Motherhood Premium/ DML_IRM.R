############################################################
# among full-time women, estimate the effect of having
# a child at home on log weekly wages,
# adjusting flexibly for observables.
############################################################

# -----------------------------
# 0) Packages
# -----------------------------

#pkgs <- c("data.table", "ggplot2", "DoubleML", "mlr3", "mlr3learners", "cobalt")
#to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
#if (length(to_install) > 0) install.packages(to_install)

library(data.table)
library(ggplot2)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(cobalt)

set.seed(123)

# -----------------------------
# 1) Load data
# -----------------------------

dt <- fread("/Users/rafi/Downloads/acs2016gender.csv")

# -----------------------------
# 2) Define outcome, treatment, sample
# -----------------------------

# Outcome: log weekly wage
y_col <- "lwwage"

# Treatment: motherhood indicator
d_col <- "chld19"

# Work on women only
dtw <- dt[fem == 1]

stopifnot(all(c(y_col, d_col) %in% names(dtw)))

# -----------------------------
# 3) Build covariates X
# -----------------------------

x_raw <- c("married", "nevermarried", "age", "yos", "exp", "exp2", "region", "msa", "deg", "occ", "ind", "hw")

missing_x <- setdiff(x_raw, names(dtw))
if (length(missing_x) > 0) stop(paste("Missing covariates:", paste(missing_x, collapse = ", ")))

# One-hot encode categorical variables
X <- model.matrix(
  ~ married + nevermarried + age + yos + exp + exp2 +
    factor(region) + factor(msa) + factor(deg) + factor(occ) + factor(ind) + factor(hw),
  data = dtw
)

X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

dml_df <- data.frame(
  y = dtw[[y_col]],
  d = dtw[[d_col]],
  X
)

# -----------------------------
# 4) Overlap check (propensity score distribution)
# -----------------------------

# We estimate e(X)=P(D=1|X) with lasso-logit (glmnet classifier)
task_ps <- TaskClassif$new(
  id = "ps",
  backend = data.frame(d = factor(dml_df$d), X),
  target = "d",
  positive = "1"
)

lrn_ps <- lrn("classif.glmnet", predict_type = "prob")

lrn_ps$train(task_ps)

ps_hat <- lrn_ps$predict(task_ps)$prob[, "1"]

ov_df <- data.frame(ps = ps_hat, d = dml_df$d)

p1 <- ggplot(ov_df, aes(x = ps, fill = factor(d))) +
  geom_density(alpha = 0.35) +
  labs(title = "Overlap check: propensity score density", fill = "D (child)") +
  xlim(0, 1)

print(p1)

# trim extreme propensity scores to improve comparability
trim_eps <- 0.1

keep <- ps_hat >= trim_eps & ps_hat <= (1 - trim_eps)

cat(sprintf("\nTrimming at eps=%.2f keeps %d / %d observations (%.1f%%)\n",
            trim_eps, sum(keep), length(keep), 100*mean(keep)))

dml_df_t <- dml_df[keep, , drop = FALSE]

ps_hat_t <- ps_hat[keep]

# -----------------------------
# 5) Balance diagnostics (before / after IPW)
# -----------------------------

# IPW weights for ATE:
w_ipw <- ifelse(dml_df_t$d == 1, 1 / ps_hat_t, 1 / (1 - ps_hat_t))

# Balance on original covariates (the one-hot matrix is huge). For a readable balance table,
# we balance on the raw covariates where possible (continuous + factors).
bal_dat <- dtw[keep, ..x_raw]
bal_dat$d <- dml_df_t$d
bal_dat$w <- w_ipw

# cobalt balance
# Unweighted:
b_unw <- bal.tab(d ~ ., data = bal_dat[, c("d", x_raw), with = FALSE], estimand = "ATE")
print(b_unw)

# Weighted:
b_w <- bal.tab(d ~ ., data = bal_dat[, c("d", x_raw), with = FALSE],
               weights = bal_dat$w, estimand = "ATE")
print(b_w)

# -----------------------------
# 6) Main estimate using DML
# -----------------------------

# Use DoubleMLIRM for binary treatment:
# Learns g0(x)=E[Y|D=0,X], g1(x)=E[Y|D=1,X], and m(x)=P(D=1|X)
# Produces doubly robust ATE with cross-fitting

data_dml <- DoubleMLData$new(
  data = dml_df_t,
  y_col = "y",
  d_cols = "d",
  x_cols = setdiff(names(dml_df_t), c("y", "d"))
)

# Learners 
ml_g <- lrn("regr.glmnet") # outcome regressions
ml_m <- lrn("classif.glmnet", predict_type = "prob")  # propensity

dml_irm <- DoubleMLIRM$new(
  data = data_dml,
  ml_g = ml_g,
  ml_m = ml_m,
  n_folds = 5,
  score = "ATE"
)

dml_irm$fit()

cat("\n==============================\n")
cat("DML-IRM (Doubly robust) ATE\n")
cat("==============================\n")
print(dml_irm$summary())

ate_hat <- dml_irm$coef
se_hat  <- dml_irm$se
cat(sprintf("\nATE (log points): %.4f (SE %.4f)\n", ate_hat, se_hat))
cat(sprintf("Approx. percent effect on wages: %.2f%%\n", 100*(exp(ate_hat)-1)))
