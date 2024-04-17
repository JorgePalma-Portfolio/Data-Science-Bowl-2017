
#

library(xgboost)
library(Matrix)
library(caret)
library(Metrics)
library(data.table)

train <- fread('input/LUNA/train.csv', header=TRUE)
test  <- fread('input/LUNA/test.csv', header=TRUE)

train <- data.matrix(train)
test  <- data.matrix(test)

xg_eval_mae <- function (yhat, xgtrain) {
  y = getinfo(xgtrain, "label")
  err= mae(yhat,y)
  return (list(metric = "error", value = err))
}

labelvar  <- train[,1]

k.fold <- 10
model <- list()
k <- 1
trainerror <- 0

for(k in 1:1){
  
  set.seed(11)
  folds <- createFolds(labelvar,k.fold)
  
  ul.folds <- unlist(folds)
  
  train.f      <- ul.folds[!ul.folds %in% folds[[k]]] #Set the training set
  validation.f <- folds[[k]]                          #Set the validation set
  
  rm(folds,ul.folds)
  
  xgtrain <- xgb.DMatrix(train[train.f,-1], label = labelvar[train.f])
  xgval   <- xgb.DMatrix(train[validation.f,-1])
  
  watchlist <- list('train_70' = xgtrain)
  
  param0 <- list(
    "objective" = "binary:logistic"
    , "booster" = "gbtree"
    , "eta" = 0.05
    , "subsample" = 0.7
    , "colsample_bytree" = 0.7
    , "min_child_weight" = 9
    , "max_depth" = 10
    , "n_estimators" = 2000
  )
  
  model_cv = xgb.cv(
    params = param0
    , nrounds = 2000
    , nfold = 10
    , data = xgtrain
    , early.stop.round = 200
    , maximize = TRUE
    , nthread = 5
    , print.every.n = 100
    , feval=xg_eval_mae
  )
  
  best <- min(model_cv$test.error.mean)
  bestIter <- which(model_cv$test.error.mean==best)
  
  cat("\n",best, bestIter,"\n")
  print(model_cv[bestIter])
  
  iter <- bestIter-1
  
  model[[k]] = xgb.train(nrounds = iter,
                         params = param0,
                         data = xgtrain,
                         watchlist = watchlist,
                         print.every.n = 100,
                         nthread = 6,
                         verbose = 1)
  
  predicted <- predict(model[[k]], xgval)
  
  yval <- test[,1]
  yhat <- predicted
  
  rst <- data.table(yval,yhat)
  
  #cat("Predicted ",k,mae(exp(yhat$loss)-shift.val, exp(yval$V1)-shift.val),"\n")
  #trainerror1 <- trainerror1 + mae(exp(yhat$loss)-shift.val,exp(yval$loss)-shift.val)
}  



trainerror <- trainerror /10
cat("Train error:",trainerror)

xgsub <- xgb.DMatrix(sparse.test)
predicted.s <- list()

for(k in 1:10){
  predicted.s[[k]] <- predict(model[[k]], xgsub)
}

predicted.s <- data.frame(predicted.s)
p.ensemble  <- rowMeans(predicted.s)

p.ensemble <- exp(denormalize(data.frame(loss=p.ensemble),mm.loss)$loss) -shift.val

submission <- data.frame(test_id,p.ensemble)
colnames(submission) <- c("id","loss")

write.csv(submission, "output/submission_xgb_ver05.csv", row.names=F, quote=F)


