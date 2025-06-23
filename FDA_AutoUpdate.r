#!/usr/bin/env Rscript

# FDA Classification Pipeline (Auto-Fetch from openFDA)
# Refactored for dynamic data retrieval

# 0. Setup: CRAN mirror and package loading
# options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("httr","jsonlite","tidyverse","caret","smotefamily","ranger","progress","MLmetrics","glmnet","lightgbm")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(new_pkgs)) install.packages(new_pkgs)

library(httr)
library(jsonlite)
library(tidyverse)
library(caret)
library(smotefamily)
library(ranger)
library(progress)
library(MLmetrics)
library(glmnet)
library(lightgbm)

# --- Helper Functions ---

# 1. Fetch latest FDA food enforcement data via openFDA API
fetch_fda_data <- function(limit = 1000) {
  url <- "https://api.fda.gov/food/enforcement.json"
  resp <- GET(url, query = list(limit = limit))
  stop_for_status(resp)
  data_list <- content(resp, "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
  df <- as_tibble(data_list$results)
  return(df)
}

raw = fetch_fda_data((limit = 1000))
names(raw)

head(raw)

str(raw)

# 2. Clean raw FDA data frame
clean_data <- function(df) {
  df_clean <- df %>%
    rename(
      Classification   = status,
      Country_Area      = country,
      Posted_Citations  = voluntary_mandated,
      Project_Area      = product_description,
      Product_Type      = product_type
    ) %>%
    select(
      Classification,
      State             = state,
      Country_Area,
      Posted_Citations,
      Project_Area,
      Product_Type,
      Fiscal_Year       = report_date
    ) %>%
    mutate(
      across(where(is.character), factor),
      Classification = factor(Classification),
      Fiscal_Year    = as.integer(substr(Fiscal_Year, 1, 4))
    ) %>%
    drop_na()
  levels(df_clean$Classification) <- make.names(levels(df_clean$Classification))
  return(df_clean)
}

# 3. Encode predictors via one-hot and sanitize names
#    also remove zero-variance columns
encode_predictors <- function(df, dummy_obj = NULL) {
  if (is.null(dummy_obj)) {
    dummy_obj <- dummyVars(Classification ~ ., data = df)
  }
  X <- predict(dummy_obj, df) %>% as.data.frame()
  # remove zero-variance
  nz <- nearZeroVar(X)
  if (length(nz) > 0) X <- X[, -nz, drop = FALSE]
  clean_names <- make.names(colnames(X))
  colnames(X)  <- clean_names
  y <- df$Classification
  return(list(X = X, y = y, obj = dummy_obj))
}

# 4. Balance classes via SMOTE
balance_smote <- function(X, y) {
  sm      <- SMOTE(X = X, target = y, K = 5)
  df_bal  <- sm$data
  names(df_bal)[ncol(df_bal)] <- "Classification"
  df_bal$Classification <- factor(df_bal$Classification, levels = levels(y))
  # reapply zero-variance removal
  nz2 <- nearZeroVar(df_bal[, -ncol(df_bal)])
  if (length(nz2) > 0) df_bal <- df_bal[, -nz2]
  return(df_bal)
}

# # 5. Train Random Forest model via ranger
# f_train_rf <- function(df_bal) {
#   ctrl <- trainControl(
#     method          = "cv",
#     number          = 5,
#     classProbs      = TRUE,
#     summaryFunction = multiClassSummary,
#     verboseIter     = TRUE
#   )
#   p <- ncol(df_bal) - 1
#   mtry_vals <- unique(floor(seq(1, p, length.out = 3)))
#   tuneGrid <- expand.grid(
#     mtry           = mtry_vals,
#     splitrule      = "gini",
#     min.node.size  = 1
#   )
#   set.seed(456)
#   rf <- train(
#     Classification ~ .,
#     data       = df_bal,
#     method     = "ranger",
#     trControl  = ctrl,
#     tuneGrid   = tuneGrid,
#     num.trees  = 500,
#     importance = 'impurity'
#   )
#   return(rf)
# }

# 6. Train Elastic Net (glmnet)
f_train_glm <- function(X, y) {
  x_mat <- as.matrix(X)
  y_vec <- as.numeric(y) - 1
  cvm   <- cv.glmnet(
    x_mat, y_vec,
    family = "multinomial",
    alpha  = 0.5
  )
  return(cvm)
}

# 7. Train LightGBM
# f_train_lgb <- function(X, y) {
#   dtrain <- lgb.Dataset(
#     data  = as.matrix(X),
#     label = as.numeric(y) - 1
#   )
#   params <- list(
#     objective = "multiclass",
#     num_class = length(levels(y)),
#     metric    = "multi_error"
#   )
#   model <- lgb.train(
#     params  = params,
#     data    = dtrain,
#     nrounds = 100,
#     verbose = -1
#   )
#   return(model)
# }

# --- Main Execution ---
# Progress bar setup
pb <- progress_bar$new(
  total  = 5,
  format = "[:bar] Step :current/:total :message",
  clear  = FALSE
)

# Step 1: Fetch
pb$tick(tokens = list(message = "Fetching FDA data..."))
raw <- fetch_fda_data(limit = 1000)

# Step 2: Clean
pb$tick(tokens = list(message = "Cleaning data..."))
data <- clean_data(raw)

# Step 3: Split
pb$tick(tokens = list(message = "Splitting data..."))
set.seed(123)
idx      <- createDataPartition(data$Classification, p = 0.7, list = FALSE)
train_df <- data[idx, ]
test_df  <- data[-idx, ]
test_df$Classification <- factor(
  test_df$Classification,
  levels = levels(data$Classification)
)

# Step 4: Encode & Balance
pb$tick(tokens = list(message = "Encoding & balancing..."))
enc    <- encode_predictors(train_df)
bal_df <- balance_smote(enc$X, enc$y)
test_enc <- encode_predictors(test_df)

# Step 5: Train & Save Models
pb$tick(tokens = list(message = "Training models..."))
# rf_model  <- f_train_rf(bal_df)
cv_glm    <- f_train_glm(enc$X, enc$y)
# lgb_model <- f_train_lgb(enc$X, enc$y)

# Persist models
dir.create("models", showWarnings = FALSE)
# saveRDS(
#   rf_model,
#   file = "models/rf_fda_model.rds"
# )
saveRDS(
  cv_glm,
  file = "models/cv_glmnet_fda_model.rds"
)
# lgb.save(
#   lgb_model,
#   filename = "models/lgb_fda_model.txt"
# )

pb$finish()
message("All models trained and saved in 'models/' directory.")
