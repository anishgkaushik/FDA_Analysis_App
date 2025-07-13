# #!/usr/bin/env Rscript

# # FDA Classification Pipeline (Auto-Fetch from openFDA)
# # Refactored for dynamic data retrieval with robust encoding and class reconciliation

# # 0. Setup: CRAN mirror and package loading
# options(repos = c(CRAN = "https://cloud.r-project.org"))

# # options(error = recover)
# # source("FDA_AutoUpdate.r", encoding="UTF-8")

# pkgs <- c(
#   "httr", "jsonlite", "tidyverse", "caret", "smotefamily",
#   "ranger", "progress", "MLmetrics", "glmnet", "lightgbm"
# )
# new_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
# if (length(new_pkgs)) install.packages(new_pkgs)

# library(httr)
# library(jsonlite)
# library(tidyverse)
# library(caret)
# library(smotefamily)
# library(ranger)
# library(progress)
# library(MLmetrics)
# library(glmnet)
# library(lightgbm)

# # --- Helper Functions ---

# # 1. Fetch data
# fetch_fda_data <- function(limit = 1000) {
#   resp <- GET("https://api.fda.gov/food/enforcement.json", query = list(limit = limit))
#   stop_for_status(resp)
#   fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)$results %>%
#     as_tibble()
# }

# # 2. Clean data
# clean_data <- function(df) {
#   df %>%
#     rename(
#       Classification   = status,
#       State            = state,
#       Country_Area     = country,
#       Posted_Citations = voluntary_mandated,
#       Project_Area     = product_description,
#       Product_Type     = product_type,
#       Report_Date      = report_date
#     ) %>%
#     transmute(
#       Classification   = factor(Classification),
#       State            = factor(State),
#       Country_Area     = factor(Country_Area),
#       Posted_Citations = factor(Posted_Citations),
#       Project_Area     = factor(Project_Area),
#       Product_Type     = factor(Product_Type),
#       Fiscal_Year      = as.integer(substr(Report_Date, 1, 4))
#     ) %>%
#     drop_na() %>%
#     group_by(Classification) %>%
#     filter(n() >= 2) %>%
#     ungroup() -> df_clean

#   levels(df_clean$Classification) <- make.names(levels(df_clean$Classification))
#   df_clean
# }

# # 3. Encode predictors via one-hot and sanitize names
# #    Drops single-level and zero-variance predictors, separates y
# encode_predictors <- function(df) {
#   # Extract response as a proper factor
#   y <- factor(df$Classification)
#   # Extract predictor columns
#   preds <- df %>% select(-Classification)
#   # Drop factor predictors with only one level
#   single <- sapply(preds, function(x) is.factor(x) && nlevels(x) < 2)
#   preds <- preds[, !single, drop = FALSE]
#   # Create dummyVars for predictors only
#   dummy_obj <- dummyVars(~ ., data = preds)
#   # Generate one-hot matrix
#   X <- predict(dummy_obj, newdata = preds) %>% as.data.frame()
#   # Remove near-zero variance columns
#   nz <- nearZeroVar(X)
#   if (length(nz)) X <- X[, -nz, drop = FALSE]
#   # Sanitize column names
#   names(X) <- make.names(names(X))
#   return(list(X = X, y = y, obj = dummy_obj))
# }


# # 4. Balance classes via SMOTE
# balance_smote <- function(X, y) {
#   sm <- SMOTE(X = X, target = y, K = 5)
#   dfb <- sm$data
#   names(dfb)[ncol(dfb)] <- "Classification"
#   dfb$Classification <- factor(dfb$Classification, levels = levels(y))
#   nz2 <- nearZeroVar(dfb[,-ncol(dfb)])
#   if (length(nz2)) dfb <- dfb[, -nz2]
#   dfb
# }

# # # 5. Train Random Forest
# # f_train_rf <- function(dfb) {
# #   dfb$Classification <- factor(dfb$Classification)
# #   ctrl <- trainControl(
# #     method = "cv", number = 5,
# #     classProbs = TRUE,
# #     summaryFunction = multiClassSummary,
# #     verboseIter = TRUE
# #   )
# #   p <- ncol(dfb) - 1
# #   tuneGrid <- expand.grid(
# #     mtry = unique(floor(seq(1, p, length.out = 3))),
# #     splitrule = "gini",
# #     min.node.size = 1
# #   )
# #   set.seed(456)
# #   train(
# #     Classification ~ ., data = dfb,
# #     method = "ranger",
# #     trControl = ctrl,
# #     tuneGrid = tuneGrid,
# #     num.trees = 500,
# #     importance = 'impurity'
# #   )
# # }


# # 5. Train Random Forest directly via ranger (bypass caret)
# # f_train_rf <- function(dfb) {
# #   dfb$Classification <- factor(dfb$Classification)
# #   num.trees <- 500
# #   mtry      <- floor(sqrt(ncol(dfb) - 1))
# #   # Train probability forest
# #   rf <- ranger::ranger(
# #     formula     = Classification ~ .,
# #     data        = dfb,
# #     num.trees   = num.trees,
# #     mtry        = mtry,
# #     importance  = 'impurity',
# #     probability = TRUE
# #   )
# #   return(rf)
# # }
# # debugonce(f_train_rf)
# # # then call it manually:
# # rf_model <- f_train_rf(baldf)

# # print(is.function(f_train_rf))  
# # # and also
# # print(ls())

# # traceback()


# # print(exists("ranger"))
# # print(typeof(ranger))
# # typeof(ranger)


# # 6. Train Elastic Net


# # f_train_glm <- function(X, y) {
# #   xmat <- as.matrix(X)
# #   yvec <- as.numeric(y) - 1
  
  
# # tryCatch({
# #     glm_model <- glmnet::cv.glmnet(
# #       x                 = xmat,
# #       y                 = yvec,
# #       family            = "multinomial",
# #       alpha             = 0.5,
# #       type.multinomial  = "ungrouped",
# #       maxit             = 1e5
# #     )
# #     return(glm_model)
# #   }, error = function(e) {
# #     cat("Error in cv.glmnet():\n")
# #     print(e)
# #     # drop into interactive debug
# #     browser()
# #     # if you want to return NULL on error:
# #     # return(NULL)
# #   })
# # }


# f_train_glm <- function(X, y) {
#   cat("▶ ENTER f_train_glm()\n")
#   xmat <- as.matrix(X)
#   yvec <- as.numeric(y) - 1
#   cat("  – xmat & yvec built (", nrow(xmat), "×", ncol(xmat), "); length(yvec)=", length(yvec), "\n")

#   cat("▶ about to enter tryCatch\n")
#   result <- tryCatch({
#     cat("    – inside TRY, before cv.glmnet()\n")
#     m <- glmnet::cv.glmnet(
#       x                 = xmat,
#       y                 = yvec,
#       family            = "multinomial",
#       alpha             = 0.5,
#       type.multinomial  = "ungrouped",
#       maxit             = 1e5
#     )
#     cat("    – cv.glmnet() returned successfully\n")
#     m
#   }, error = function(e) {
#     cat("🚨 CAUGHT ERROR in cv.glmnet:\n")
#     print(e)
#     browser()    # drop you into the debugger right here
#     NULL         # so the function returns NULL after you exit browser()
#   })

#   cat("▶ EXIT f_train_glm(), returning ", if (is.null(result)) "NULL\n" else "model object\n")
#   return(result)
# }




# # # 7. Train LightGBM
# # f_train_lgb <- function(X, y) {
# #   dtrain <- lgb.Dataset(data = as.matrix(X), label = as.numeric(y) - 1)
# #   params <- list(objective = "multiclass", num_class = length(levels(y)), metric = "multi_error")
# #   lgb.train(params, dtrain, nrounds = 100, verbose = -1)
# # }

# # --- Main Execution ---
# prog <- progress_bar$new(total = 5, format = "[:bar] Step :current/:total :message", clear = FALSE)

# # 1. Fetch
# d <- fetch_fda_data(1000)

# # 2. Clean
# prog$tick(tokens = list(message = "Cleaning data..."))
# d <- clean_data(d)

# # 3. Split
# prog$tick(tokens = list(message = "Splitting data..."))
# set.seed(123)
# idx <- createDataPartition(d$Classification, p = 0.7, list = FALSE)
# train_df <- droplevels(d[idx, ])
# test_df  <- droplevels(d[-idx, ])

# # Reconcile factor levels
# fac_cols <- names(train_df)[sapply(train_df, is.factor)]
# for (col in fac_cols) {
#   lv <- union(levels(train_df[[col]]), levels(test_df[[col]]))
#   train_df[[col]] <- factor(train_df[[col]], levels = lv)
#   test_df[[col]]  <- factor(test_df[[col]],  levels = lv)
# }

# # 4. Encode & Balance
# prog$tick(tokens = list(message = "Encoding & balancing..."))
# enc <- encode_predictors(train_df)
# # baldf <- balance_smote(enc$X, enc$y)

# # print(class(baldf))
# # str(baldf)


# #–– diagnostics:
# str(enc$X)
# str(enc$y)
# cat("dims X:", dim(enc$X), "; length y:", length(enc$y), "\n")

# # Test encoding
# # This should no longer error
# # enc <- encode_predictors(train_df)

# # # Encode test set
# # test_enc <- predict(enc$obj, newdata = test_df) %>% as.data.frame()
# # test_X <- test_enc[, intersect(names(test_enc), names(baldf)), drop = FALSE]
# # test_y <- test_df$Classification

# # 5. Train & Save
# prog$tick(tokens = list(message = "Training models..."))
# # rf_model   <- f_train_rf(baldf)
# glm_model <- f_train_glm(enc$X, enc$y)
# # lgbm_model <- f_train_lgb(enc$X, enc$y)
# # dir.create("models", showWarnings = FALSE)
# # # saveRDS(rf_model,   "models/rf_model.rds")
# # saveRDS(glm_model,  "models/glm_model.rds")
# # # lgb.save(lgbm_model, "models/lgbm_model.txt")

# cat("Got glm_model, saving now…\n")
# # # diagnostic prints
# cat("is dir.create() a function? ", is.function(dir.create), "\n")
# cat("typeof(dir.create): ", typeof(dir.create), "\n")
# cat("is saveRDS() a function?   ", is.function(saveRDS), "\n")
# cat("typeof(saveRDS):   ", typeof(saveRDS), "\n")

# # force base namespace
# base::dir.create("models", showWarnings = FALSE)
# base::saveRDS(glm_model, "models/glm_model.rds")
# cat("✅ glm_model.rds saved!\n")

# # prog$finish()
# # cat("→ class(prog):", class(prog),        "\n")
# # cat("→ names(prog):", paste(names(prog), collapse = ", "), "\n")
# # cat("→ is.function(prog$finish):", is.function(prog$finish), "\n")



# # message("✓ Done.")
# # # prog$finish()
# # # message("Models saved in 'models/' directory.")

