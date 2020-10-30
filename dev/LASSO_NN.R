

library(glmnet)
library(neuralnet)
library(e1071)

lassoReg = function(train.x, train.y){
  # train LASSO regression
  cv <- cv.glmnet(x = as.matrix(train.x), y = train.y,
                  family="gaussian", alpha = 1, intercept=TRUE)
  bestlambda <- cv$lambda.min
  lasso <- glmnet(x = as.matrix(train.x), y = train.y,
                  family="gaussian", alpha = 1, intercept=TRUE, lambda = bestlambda)
  return(lasso)
}


lassoPred = function(model, test.x){
  return(predict(model, as.matrix(test.x)))
}


nnReg = function(train_df){
  nn.fit = neuralnet(data = train_df,
                     formula = factor(HEALTH) ~ . ,
                     hidden = 10,
                     act.fct = "logistic",
                     threshold = 1,
                     linear.output = F)
  return(nn.fit)
}

nnPred = function(model, test.x){
  pred = compute(model, test.x)
  hat = pred$net.result
  return(max.col(hat[, 1:2]))
}

nbReg <- function(data){
  return(
    naiveBayes(formula = factor(HEALTH) ~ ., data = data, type = "response")
  )
}