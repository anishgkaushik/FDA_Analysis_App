
# scripts/02_encode.R

# Dependencies
library(dplyr)
library(caret)        # for dummyVars() and nearZeroVar()
library(smotefamily)  # for SMOTE()

# ─── 1) One-hot encode + clean up predictors 
encode_predictors <- function(df) {
  # Response
  y <- factor(df$Classification)
  # All the predictors
  preds <- df %>% select(-Classification)
  # Drop any factor with just one level
  single <- sapply(preds, function(x) is.factor(x) && nlevels(x) < 2)
  preds  <- preds[, !single, drop = FALSE]

  # Build dummyVars and predict into a data.frame
  dummy_obj <- dummyVars(~ ., data = preds)
  X <- predict(dummy_obj, newdata = preds) %>% as.data.frame()

  # Drop near-zero variance
  nz <- nearZeroVar(X)
  if (length(nz)) X <- X[, -nz, drop = FALSE]

  # Fix column names
  names(X) <- make.names(names(X))

  return(list(X = X, y = y, obj = dummy_obj))
}

# ─── 2) Balance with SMOTE ──────────────────────────────────────────────────────
balance_smote <- function(X, y) {
  sm <- SMOTE(X = X, target = y, K = 5)
  dfb <- sm$data
  # last column is the class
  names(dfb)[ncol(dfb)] <- "Classification"
  dfb$Classification <- factor(dfb$Classification, levels = levels(y))

  # drop any new zero-var cols
  nz2 <- nearZeroVar(dfb[,-ncol(dfb)])
  if (length(nz2)) dfb <- dfb[, -nz2]

  return(dfb)
}

# ─── 3) Wrapper: run both and save the raw encoded object ───────────────────────
encode_and_balance <- function(df, cfg) {
  enc   <- encode_predictors(df)
  baldf <- balance_smote(enc$X, enc$y)

  # persist the raw encoding (so your dashboard can plot class balance, etc.)
  dir.create(dirname(cfg$paths$enc_data), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(X = enc$X, y = enc$y), cfg$paths$enc_data)

  return(baldf)
}
