# scripts/03_train.R

library(dplyr)
library(glmnet)
library(ranger)

train_models <- function(bal, cfg) {
  cat("▶ ENTER train_models()\n")
  # split into X/y again
  X <- bal %>% select(-Classification)
  y <- bal$Classification
  cat("  – received balanced data: ", nrow(bal), "rows ×", ncol(X), "features\n")
  
  # 1) Train Elastic-Net
  cat("▶ Training Elastic-Net…\n")
  glm_model <- tryCatch({
    cat("    – calling cv.glmnet()\n")
    m_glm <- glmnet::cv.glmnet(
      x                 = as.matrix(X),
      y                 = as.numeric(y) - 1,
      family            = cfg$model$family,
      alpha             = cfg$model$alpha,
    #   type.multinomial  = cfg$model$glm$type.multinomial,
      maxit             = cfg$model$maxit
    )
    # fam <- cfg$model$glm$family
   
    cat("    – Elastic-Net trained OK\n")
    m_glm
  }, error = function(e) {
    cat("🚨 ERROR during Elastic-Net training:\n"); print(e)
    browser()
    stop("Elastic-Net training failed, aborting pipeline.")
  })
  
#   # 2) (Optionally) Train RandomForest
#   cat("▶ Training Random Forest…\n")
#   rf_model <- tryCatch({
#     cat("    – calling ranger()\n")
#     m_rf <- ranger::ranger(
#       formula     = Classification ~ .,
#       data        = bal,
#       num.trees   = 500,
#       mtry        = floor(sqrt(ncol(bal)-1)),
#       importance  = 'impurity',
#       probability = TRUE
#     )
#     cat("    – Random Forest trained OK\n")
#     m_rf
#   }, error = function(e) {
#     cat("🚨 ERROR during RF training:\n"); print(e)
#     browser()
#     stop("Random Forest training failed, aborting pipeline.")
#   })
  
  # 3) Persist models & record performance
  cat("▶ Saving models and performance…\n")
  dir.create(dirname(cfg$paths$glm_model), recursive=TRUE, showWarnings=FALSE)
  saveRDS(glm_model, cfg$paths$glm_model)
  # saveRDS(rf_model,  cfg$paths$rf_model)  # if you have rf_model path in config
  
  perf <- tibble(
    date     = Sys.time(),
    cv_error = min(glm_model$cvm),
    # rf_oob   = rf_model$prediction.error  # out-of-bag error
  )
  if (file.exists(cfg$paths$perf)) {
    old <- readRDS(cfg$paths$perf)
    perf <- bind_rows(old, perf)
  }
  saveRDS(perf, cfg$paths$perf)
  cat("performance snapshot:\n"); print(perf %>% tail(1))
  
  cat("▶ EXIT train_models(), returning list(glm, rf)\n")
  
  #I removed (rf = rf_model) as for now i am not using it. Add if rf_model is used
  return(list(glm = glm_model, perf = perf)) 
}
