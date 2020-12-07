##################################################
### Functions for Support Vector Machine Model ###
##################################################

library(e1071)

svm_tune <- function(mat.x, y){
  set.seed(0)
  return(
    tune(method = svm,
         train.x = mat.x,
         train.y = factor(y),
         kernel = "linear",
         ranges = list(
           gamma = seq(0.001, 0.021, 0.005),
           cost = seq(0.01, 0.21, 0.1)
         ))
  )
}

svm_train <- function(mat.x, y, opt.gamma, opt.cost){
  set.seed(0)
  return(
    svm (x = mat.x,
         y = y,
         scale = F,
         gamma = opt.gamma,
         cost = opt.cost,
         kernel = "linear",
         cross = 10
    )
  )
}

svm_test <- function(model, test.matx){
  test.x <- test.matx
  set.seed(0)
  return(
    predict(model, test.x, type = "response")
  )
}
