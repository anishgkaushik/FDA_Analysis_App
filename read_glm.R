# 2. Read the model
loaded_glm <- readRDS("models/glm_model.rds")

# 3. Inspect it
print(class(loaded_glm))       # should be "cv.glmnet"
str(loaded_glm)                # view its components (lambda sequence, cv errors, etc.)

# 4. Make predictions on new data:
#    Suppose newX is a numeric matrix with the same columns as your training X.
pred_probs <- predict(
  loaded_glm,
  newx = as.matrix(enc$X),
  s    = "lambda.min",         # use the `lambda.min` that gave lowest CV error
  type = "response"            # for class‐probabilities
)

#    Or to get the predicted class labels:
pred_class <- predict(
  loaded_glm,
  newx = as.matrix(enc$X),
  s    = "lambda.min",
  type = "class"
)

print(pred_class)
print(pred_probs)


coef(loaded_glm, s = "lambda.min")

# e.g. [1] "Completed" "Ongoing"   "Other"
loaded_glm$glmnet.fit$classnames
ongoing_conf = coef(loaded_glm, s = "lambda.min")[[2]]
as.matrix(ongoing_conf)
