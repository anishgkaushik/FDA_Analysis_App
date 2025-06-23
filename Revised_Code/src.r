# Set CRAN mirror to cloud for non-interactive installs
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load required packages
pkgs = c("tidyverse", "caret", "smotefamily", "ranger", "progress", "MLmetrics", "glmnet", "lightgbm")
new_pkgs = pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)
library(tidyverse)
library(caret)
library(smotefamily)
library(ranger)
library(progress)
library(MLmetrics)
library(glmnet)
library(lightgbm)

options(scipen = 999)

# Initialize overall progress bar
pb = progress_bar$new(
  total = 8,
  format = "[:bar] Step :current/:total :message",
  clear = FALSE
)

# 1. Load Data
pb$tick(tokens = list(message = "Loading data..."))
fda_raw = read_csv("/Users/anishgkaushik/Desktop/FDA_Code_New/FDA Inspections.csv", show_col_types = FALSE)
names(fda_raw) = str_replace_all(names(fda_raw), "[ /]", "_")

# 2. Transform & sanitize
pb$tick(tokens = list(message = "Transforming data..."))
fda = fda_raw %>%
  select(Classification, State, Country_Area, Posted_Citations, Project_Area, Product_Type, Fiscal_Year) %>%
  mutate(across(where(is.character), factor),
         Classification = factor(Classification)) %>%
  drop_na()
# sanitize levels for valid names
levels(fda$Classification) = make.names(levels(fda$Classification))

# 3. Split Train/Test
pb$tick(tokens = list(message = "Splitting data..."))
set.seed(123)
idx      = createDataPartition(fda$Classification, p = 0.7, list = FALSE)
train_df = fda[idx, ]
test_df  = fda[-idx, ]
# ensure factor on test
test_df$Classification = factor(test_df$Classification, levels = levels(fda$Classification))

# 4. Encode predictors
pb$tick(tokens = list(message = "Encoding predictors..."))
dummy_obj = dummyVars(Classification ~ ., data = train_df)
train_X   = predict(dummy_obj, train_df) %>% as.data.frame()
train_Y   = train_df$Classification

test_X    = predict(dummy_obj, test_df) %>% as.data.frame() 
test_Y    = test_df$Classification

# 5. SMOTE with progress
pb$tick(tokens = list(message = "Applying SMOTE..."))
pb_smote = progress_bar$new(total = 1, format = "[:bar] SMOTE :percent", clear = FALSE)
smote_out = SMOTE(X = train_X, target = train_Y, K = 5)
pb_smote$tick()
balanced  = smote_out$data
names(balanced)[ncol(balanced)] = "Classification"
balanced$Classification = factor(balanced$Classification, levels = levels(train_Y))
colnames(balanced)[-ncol(balanced)] = clean_names


# # 6. Train model (caret with ranger)
# pb$tick(tokens = list(message = "Training model..."))
# ctrl = trainControl(method = "cv", number = 5, classProbs = TRUE,
#                      summaryFunction = multiClassSummary, verboseIter = TRUE)
# num_pred = ncol(balanced) - 1
# mtry_vals = unique(floor(seq(1, num_pred, length.out = 3)))
# message("Testing mtry values: ", paste(mtry_vals, collapse = ", "))
# tuneGrid = expand.grid(mtry = mtry_vals, splitrule = "gini", min.node.size = 1)

# set.seed(456)
# rf_model = train(
#   Classification ~ ., data = balanced, method = "ranger",
#   trControl = ctrl, tuneLength = 3,
#   num.trees = 500, importance = 'impurity'
# )

# # 7. Evaluate Random Forest
# pb$tick(tokens = list(message = "Evaluating Random Forest..."))
# preds_rf = predict(rf_model, test_X)
# conf_rf  = confusionMatrix(preds_rf, test_Y)
# print(conf_rf)

# # 8. Save
# pb$tick(tokens = list(message = "Saving model..."))
# saveRDS(rf_model, "rf_fda_model.rds")
# message("All steps completed.")

# Elastic Net (glmnet): regularized multinomial logistic regression
message("Training Elastic Net (glmnet) model...")
x_mat    = as.matrix(train_X)
y_vec    = as.numeric(train_Y) - 1  # 0-based labels
dtrain_glm = cv.glmnet(x_mat, y_vec, family = "multinomial", alpha = 0.5)
# Predict probabilities for test set
prob_array = predict(dtrain_glm, newx = as.matrix(test_X), s = "lambda.min", type = "response")
prob_mat   = prob_array[,,1]  # dimensions: rows x classes
# Determine predicted class indices
glm_idx    = apply(prob_mat, 1, which.max)
pred_glm   = factor(levels(train_Y)[glm_idx], levels = levels(train_Y))
conf_glm   = confusionMatrix(pred_glm, test_Y)
print(conf_glm)

# 8. Save
pb$tick(tokens = list(message = "Saving model..."))
saveRDS(dtrain_glm, "dtrain_Elasticmodel.rds")
message("All steps completed.")



# --- Example Prediction Test ---
# Load saved models
date_index = 1  # example row index from test set
sample_X   = test_X[date_index, , drop = FALSE]
sample_Y   = test_Y[date_index] # nolint

test_df$Classification[1]
predict(dtrain_glm, test_X[1, , drop = FALSE])

# Elastic Net prediction
loaded_glm = readRDS("dtrain_Elasticmodel.rds") # nolint
prob_glm   = predict(loaded_glm, newx = as.matrix(sample_X), s = "lambda.min", type = "class") # nolint # nolint
pred_glm   = factor(colnames(prob_glm)[max.col(prob_glm)], levels = levels(train_Y))
cat("GLM Prediction:", as.character(pred_glm), "| True:", as.character(sample_Y), "\n")

message("All models trained and evaluated using consistent train/test variables.")
