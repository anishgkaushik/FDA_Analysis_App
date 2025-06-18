# Set CRAN mirror to cloud for non-interactive installs
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load required packages
pkgs <- c("tidyverse", "caret", "smotefamily", "ranger", "progress", "MLmetrics")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)
library(tidyverse)
library(caret)
library(smotefamily)
library(ranger)
library(progress)
library(MLmetrics)
library(glmnet)
library(lightgbm)

# Initialize overall progress bar
pb <- progress_bar$new(
  total = 8,
  format = "[:bar] Step :current/:total :message",
  clear = FALSE
)

# 1. Load Data
pb$tick(tokens = list(message = "Loading data..."))
fda_raw <- read_csv("/Users/anishgkaushik/Desktop/FDA_Code_New/FDA Inspections.csv", show_col_types = FALSE)
names(fda_raw) <- str_replace_all(names(fda_raw), "[ /]", "_")

# 2. Transform & sanitize
pb$tick(tokens = list(message = "Transforming data..."))
fda <- fda_raw %>%
  select(Classification, State, Country_Area, Posted_Citations, Project_Area, Product_Type, Fiscal_Year) %>%
  mutate(across(where(is.character), factor),
         Classification = factor(Classification)) %>%
  drop_na()
# sanitize levels for valid names
levels(fda$Classification) <- make.names(levels(fda$Classification))

# 3. Split Train/Test
pb$tick(tokens = list(message = "Splitting data..."))
set.seed(123)
idx      <- createDataPartition(fda$Classification, p = 0.7, list = FALSE)
train_df <- fda[idx, ]
test_df  <- fda[-idx, ]
# ensure factor on test
test_df$Classification <- factor(test_df$Classification, levels = levels(fda$Classification))

# 4. Encode predictors
pb$tick(tokens = list(message = "Encoding predictors..."))
dummy_obj <- dummyVars(Classification ~ ., data = train_df)
train_X   <- predict(dummy_obj, train_df) %>% as.data.frame()
train_Y   <- train_df$Classification

test_X    <- predict(dummy_obj, test_df) %>% as.data.frame()
test_Y    <- test_df$Classification

# 5. SMOTE with progress
pb$tick(tokens = list(message = "Applying SMOTE..."))
pb_smote <- progress_bar$new(total = 1, format = "[:bar] SMOTE :percent", clear = FALSE)
smote_out <- SMOTE(X = train_X, target = train_Y, K = 5)
pb_smote$tick()
balanced  <- smote_out$data
names(balanced)[ncol(balanced)] <- "Classification"
balanced$Classification <- factor(balanced$Classification, levels = levels(train_Y))

# 6. Train model (caret with ranger)
pb$tick(tokens = list(message = "Training model..."))
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                     summaryFunction = multiClassSummary, verboseIter = TRUE)
set.seed(456)
rf_model <- train(
  Classification ~ ., data = balanced, method = "ranger",
  trControl = ctrl, tuneLength = 3,
  num.trees = 500, importance = 'impurity'
)

# 7. Evaluate
pb$tick(tokens = list(message = "Evaluating model..."))
preds <- predict(rf_model, test_X)
conf <- confusionMatrix(preds, test_Y)
print(conf)

# 8. Save
pb$tick(tokens = list(message = "Saving model..."))
saveRDS(rf_model, "rf_fda_model.rds")
message("All steps completed.")


# --- Faster Training Alternatives ---
# If training time is too long, consider these options:
# 1. Reduce the number of trees: lower num.trees (e.g., 100 or 200) to speed up training.
#    rf_model <- train(..., num.trees = 200)
# 2. Simplify hyperparameter search: use tuneLength = 1 or a single mtry value instead of multiple.
#    rf_model <- train(..., tuneLength = 1)
# 3. Parallelize caret training: enable doParallel backend.
#    library(doParallel)
#    cl <- makePSOCKcluster(detectCores() - 1); registerDoParallel(cl)
#    rf_model <- train(...)
#    stopCluster(cl)
# 4. Use a faster algorithm:
#    - xgboost via caret: method = "xgbTree" with early stopping rounds.
#    - lightgbm using lightgbm package for faster gradient boosting.
# 5. Try simpler models for quick baselines, e.g., logistic regression:
#    glm_model <- train(Classification ~ ., data = balanced, method = "glm", family = "binomial")
# 6. Subsample the training data: work on a random subset (e.g., 50%) to reduce N.

# Choose and combine these techniques based on your time and accuracy requirements.

