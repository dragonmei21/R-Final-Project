# ml_model.R — v2 (improved)
# Customer Personality Analysis — Model Training
#
# Improvements over v1:
#   1. log(Income) replaces raw Income in LR — captures non-linear relationship
#   2. Class weights in glm() — addresses 14.9% minority class imbalance
#   3. CampaignHistory × Recency interaction term — top-2 features combined
#   4. RF: mtry tuned via OOB AUC, maxnodes restriction removed
#   5. 5-fold stratified CV for reliable AUC estimates (not just one test split)

library(tidyverse)
library(randomForest)
library(pROC)

# ── 0. Load clean data ────────────────────────────────────────────────────────
customers <- readRDS("./DATA/customers_clean.rds")

# ── 1. Feature engineering ────────────────────────────────────────────────────
# Applied consistently to every split to avoid leakage.
engineer_features <- function(df) {
  df %>%
    mutate(
      log_Income                = log(Income + 1),
      Education_n               = as.integer(Education) - 1L,
      HasChildren               = as.integer(HasChildren),
      Response_01               = as.integer(Response == "Yes"),
      # Interaction: customers with high campaign history AND low recency
      # are much more likely to respond than either feature alone predicts.
      CampaignHistory_x_Recency = CampaignHistory * Recency
    )
}

customers <- engineer_features(customers)

# ── 2. Feature sets ───────────────────────────────────────────────────────────
# log_Income replaces raw Income for LR (linear model can't fit the curve).
# RF can handle non-linearity natively so log_Income is still fine to include
# (just treated as another split variable). Same feature set for both models
# keeps comparison clean and simplifies predict_response().

LR_FEATURES <- c(
  "log_Income", "Age", "Tenure", "Recency", "TotalSpend",
  "CampaignHistory", "HasChildren", "SpendPerPurchase",
  "DealsPct", "NumWebVisitsMonth", "Education_n",
  "CampaignHistory_x_Recency"
)
RF_FEATURES <- LR_FEATURES   # same set — keeps comparison clean

# ── 3. Stratified train / test split (80 / 20) ───────────────────────────────
set.seed(42)
test_idx <- c(
  sample(which(customers$Response_01 == 1),
         size = round(0.2 * sum(customers$Response_01 == 1))),
  sample(which(customers$Response_01 == 0),
         size = round(0.2 * sum(customers$Response_01 == 0)))
)
train_df <- customers[-test_idx, ]
test_df  <- customers[ test_idx, ]

cat("Train:", nrow(train_df), "rows |",
    round(mean(train_df$Response_01) * 100, 1), "% response\n")
cat("Test: ", nrow(test_df),  "rows |",
    round(mean(test_df$Response_01) * 100,  1), "% response\n")

# ── 4. Scale (fit on train only — no leakage) ─────────────────────────────────
X_train <- train_df[LR_FEATURES]
X_test  <- test_df[LR_FEATURES]
y_train <- train_df$Response_01
y_test  <- test_df$Response_01

train_means <- colMeans(X_train, na.rm = TRUE)
train_sds   <- apply(X_train, 2, sd, na.rm = TRUE)

# Store feature list inside the scaler so predict_response() knows what to build
model_scaler <- list(
  means       = train_means,
  sds         = train_sds,
  lr_features = LR_FEATURES
)

scale_features <- function(df, means, sds) {
  as.data.frame(sweep(sweep(as.data.frame(df), 2, means, "-"), 2, sds, "/"))
}

X_train_sc <- scale_features(X_train, train_means, train_sds)
X_test_sc  <- scale_features(X_test,  train_means, train_sds)

# ── 5. Class weights ──────────────────────────────────────────────────────────
# Inverse-frequency weighting so each class contributes equally to the
# likelihood. Effectively equivalent to upsampling the minority class.
n_pos <- sum(y_train == 1)
n_neg <- sum(y_train == 0)
n_tot <- length(y_train)
weights_train <- ifelse(y_train == 1,
                        n_tot / (2 * n_pos),
                        n_tot / (2 * n_neg))
cat(sprintf("\nClass weights: No=%.2f  Yes=%.2f\n",
            n_tot / (2 * n_neg), n_tot / (2 * n_pos)))

# ── 6. Train logistic regression ──────────────────────────────────────────────
train_sc_df          <- X_train_sc
train_sc_df$Response <- y_train

lr_formula <- as.formula(paste("Response ~",
                                paste(LR_FEATURES, collapse = " + ")))

lr_model <- glm(lr_formula, data = train_sc_df,
                family  = binomial(link = "logit"),
                weights = weights_train)

cat("\nLR coefficients (sorted by |coef|):\n")
coefs_sorted <- sort(abs(coef(lr_model)[-1]), decreasing = TRUE)
print(round(coefs_sorted, 3))

# ── 7. Tune RF mtry using OOB AUC ─────────────────────────────────────────────
# sqrt(12) ≈ 3.5 is the default; search ±2 either side.
cat("\nTuning RF mtry...\n")
mtry_candidates <- c(2, 3, 4, 5, 6)
oob_aucs        <- numeric(length(mtry_candidates))

train_df_rf          <- X_train          # unscaled — RF is scale-invariant
train_df_rf$Response <- factor(y_train, levels = c(0, 1),
                                labels = c("No", "Yes"))

for (i in seq_along(mtry_candidates)) {
  set.seed(42)
  rf_tune     <- randomForest(Response ~ ., data = train_df_rf,
                              ntree = 300, mtry = mtry_candidates[i],
                              importance = FALSE)
  oob_probs   <- rf_tune$votes[, "Yes"]
  oob_roc     <- roc(y_train, oob_probs, quiet = TRUE)
  oob_aucs[i] <- as.numeric(auc(oob_roc))
  cat(sprintf("  mtry=%d  OOB AUC=%.4f\n", mtry_candidates[i], oob_aucs[i]))
}

best_mtry <- mtry_candidates[which.max(oob_aucs)]
cat(sprintf("Best mtry: %d (OOB AUC=%.4f)\n", best_mtry, max(oob_aucs)))

# ── 8. Train final RF — best mtry, no maxnodes cap ────────────────────────────
set.seed(42)
rf_model <- randomForest(
  Response ~ .,
  data       = train_df_rf,
  ntree      = 300,
  mtry       = best_mtry,
  importance = TRUE
)

cat(sprintf("RF OOB error (final): %.1f%%\n",
            rf_model$err.rate[300, "OOB"] * 100))

# ── 9. 5-fold stratified cross-validation ─────────────────────────────────────
cat("\nRunning 5-fold stratified CV...\n")
n_folds <- 5
set.seed(42)
pos_idx  <- which(customers$Response_01 == 1)
neg_idx  <- which(customers$Response_01 == 0)
fold_pos <- sample(rep(1:n_folds, length.out = length(pos_idx)))
fold_neg <- sample(rep(1:n_folds, length.out = length(neg_idx)))
folds    <- integer(nrow(customers))
folds[pos_idx] <- fold_pos
folds[neg_idx] <- fold_neg

cv_lr_aucs <- numeric(n_folds)
cv_rf_aucs <- numeric(n_folds)

for (k in 1:n_folds) {
  cv_tr <- customers[folds != k, ]
  cv_te <- customers[folds == k, ]

  # Scale fitted on this fold's training portion only
  cv_means <- colMeans(cv_tr[LR_FEATURES], na.rm = TRUE)
  cv_sds   <- apply(cv_tr[LR_FEATURES], 2, sd, na.rm = TRUE)

  cv_tr_sc <- scale_features(cv_tr[LR_FEATURES], cv_means, cv_sds)
  cv_te_sc <- scale_features(cv_te[LR_FEATURES], cv_means, cv_sds)

  # Per-fold class weights
  n_pos_k <- sum(cv_tr$Response_01 == 1)
  n_neg_k <- sum(cv_tr$Response_01 == 0)
  n_tot_k <- nrow(cv_tr)
  w_k <- ifelse(cv_tr$Response_01 == 1,
                n_tot_k / (2 * n_pos_k),
                n_tot_k / (2 * n_neg_k))

  # LR fold
  cv_tr_df          <- cv_tr_sc
  cv_tr_df$Response <- cv_tr$Response_01
  cv_te_df          <- cv_te_sc
  cv_te_df$Response <- 0L   # dummy
  lr_k      <- glm(lr_formula, data = cv_tr_df,
                   family = binomial(), weights = w_k)
  lr_prob_k <- predict(lr_k, newdata = cv_te_df, type = "response")
  cv_lr_aucs[k] <- as.numeric(
    auc(roc(cv_te$Response_01, lr_prob_k, quiet = TRUE)))

  # RF fold
  cv_tr_rf          <- cv_tr[LR_FEATURES]
  cv_tr_rf$Response <- factor(cv_tr$Response_01,
                              levels = c(0, 1), labels = c("No", "Yes"))
  set.seed(42)
  rf_k      <- randomForest(Response ~ ., data = cv_tr_rf,
                             ntree = 200, mtry = best_mtry,
                             importance = FALSE)
  rf_prob_k <- predict(rf_k, newdata = cv_te[LR_FEATURES],
                        type = "prob")[, "Yes"]
  cv_rf_aucs[k] <- as.numeric(
    auc(roc(cv_te$Response_01, rf_prob_k, quiet = TRUE)))

  cat(sprintf("  Fold %d — LR: %.3f  RF: %.3f\n",
              k, cv_lr_aucs[k], cv_rf_aucs[k]))
}

cat(sprintf("\nCV LR AUC: %.3f ± %.3f\n", mean(cv_lr_aucs), sd(cv_lr_aucs)))
cat(sprintf("CV RF AUC: %.3f ± %.3f\n", mean(cv_rf_aucs), sd(cv_rf_aucs)))

# ── 10. Test set evaluation ────────────────────────────────────────────────────
lr_probs <- predict(lr_model,
                    newdata = cbind(X_test_sc, Response = y_test),
                    type    = "response")
rf_probs <- predict(rf_model, newdata = X_test, type = "prob")[, "Yes"]

lr_roc <- roc(y_test, lr_probs, quiet = TRUE)
rf_roc <- roc(y_test, rf_probs, quiet = TRUE)
lr_auc <- as.numeric(auc(lr_roc))
rf_auc <- as.numeric(auc(rf_roc))

cat("\n=== Test set AUC ===\n")
cat("Logistic Regression:", round(lr_auc, 4), "\n")
cat("Random Forest:      ", round(rf_auc, 4), "\n")

THRESHOLD <- 0.30

compute_metrics <- function(y_true, y_pred) {
  tp <- sum(y_pred == 1 & y_true == 1)
  fp <- sum(y_pred == 1 & y_true == 0)
  tn <- sum(y_pred == 0 & y_true == 0)
  fn <- sum(y_pred == 0 & y_true == 1)
  list(tp        = tp, fp = fp, tn = tn, fn = fn,
       precision = if (tp + fp > 0) tp / (tp + fp) else 0,
       recall    = if (tp + fn > 0) tp / (tp + fn) else 0,
       f1        = if (2*tp + fp + fn > 0) 2*tp / (2*tp + fp + fn) else 0,
       accuracy  = (tp + tn) / length(y_true))
}

lr_metrics <- compute_metrics(y_test, as.integer(lr_probs >= THRESHOLD))
rf_metrics <- compute_metrics(y_test, as.integer(rf_probs >= THRESHOLD))

cat(sprintf("\nAt threshold=%.2f:\n", THRESHOLD))
cat(sprintf("LR  Prec=%.3f  Rec=%.3f  F1=%.3f  Acc=%.3f\n",
            lr_metrics$precision, lr_metrics$recall,
            lr_metrics$f1, lr_metrics$accuracy))
cat(sprintf("RF  Prec=%.3f  Rec=%.3f  F1=%.3f  Acc=%.3f\n",
            rf_metrics$precision, rf_metrics$recall,
            rf_metrics$f1, rf_metrics$accuracy))

# ROC curve data frames
lr_roc_df <- data.frame(
  fpr       = 1 - lr_roc$specificities,
  tpr       = lr_roc$sensitivities,
  threshold = lr_roc$thresholds,
  model     = "Logistic Regression",
  stringsAsFactors = FALSE
)
rf_roc_df <- data.frame(
  fpr       = 1 - rf_roc$specificities,
  tpr       = rf_roc$sensitivities,
  threshold = rf_roc$thresholds,
  model     = "Random Forest",
  stringsAsFactors = FALSE
)
roc_df <- bind_rows(lr_roc_df, rf_roc_df)

# Feature importance
lr_coefs  <- coef(lr_model)[-1]
lr_imp_df <- data.frame(
  feature    = names(lr_coefs),
  importance = abs(lr_coefs),
  direction  = ifelse(lr_coefs > 0, "positive", "negative"),
  model      = "Logistic Regression",
  stringsAsFactors = FALSE
)

rf_imp_raw <- importance(rf_model, type = 1)
rf_imp_df  <- data.frame(
  feature    = rownames(rf_imp_raw),
  importance = rf_imp_raw[, "MeanDecreaseAccuracy"],
  direction  = "positive",
  model      = "Random Forest",
  stringsAsFactors = FALSE
)

# ── 11. Bundle and save ────────────────────────────────────────────────────────
model_metrics <- list(
  lr_auc        = lr_auc,
  rf_auc        = rf_auc,
  cv_lr_auc     = mean(cv_lr_aucs),
  cv_lr_sd      = sd(cv_lr_aucs),
  cv_rf_auc     = mean(cv_rf_aucs),
  cv_rf_sd      = sd(cv_rf_aucs),
  lr_metrics    = lr_metrics,
  rf_metrics    = rf_metrics,
  roc_df        = roc_df,
  lr_imp_df     = lr_imp_df,
  rf_imp_df     = rf_imp_df,
  threshold     = THRESHOLD,
  n_train       = nrow(train_df),
  n_test        = nrow(test_df),
  response_rate = mean(customers$Response_01),
  best_mtry     = best_mtry,
  lr_test_probs = as.numeric(lr_probs),
  y_test        = y_test
)

feature_meta <- data.frame(
  feature     = LR_FEATURES,
  label       = c(
    "Log annual income", "Age (years)", "Tenure (days as customer)",
    "Recency (days since last purchase)", "Total 2-year spend ($)",
    "Campaigns accepted previously (0–4)", "Has children at home (0/1)",
    "Avg spend per purchase ($)", "Deals % of purchases (0–1)",
    "Web visits per month", "Education level (0=Basic … 4=PhD)",
    "Campaign history × Recency (interaction)"
  ),
  stringsAsFactors = FALSE
)

saveRDS(lr_model,      "./DATA/lr_model.rds")
saveRDS(rf_model,      "./DATA/rf_model.rds")
saveRDS(model_scaler,  "./DATA/model_scaler.rds")
saveRDS(model_metrics, "./DATA/model_metrics.rds")
saveRDS(feature_meta,  "./DATA/feature_meta.rds")

cat("\n✓ All model files saved to DATA/\n")
cat(sprintf("  LR test AUC: %.3f  |  RF test AUC: %.3f\n", lr_auc, rf_auc))
cat(sprintf("  CV LR AUC:   %.3f ± %.3f\n", mean(cv_lr_aucs), sd(cv_lr_aucs)))
cat(sprintf("  CV RF AUC:   %.3f ± %.3f\n", mean(cv_rf_aucs), sd(cv_rf_aucs)))
