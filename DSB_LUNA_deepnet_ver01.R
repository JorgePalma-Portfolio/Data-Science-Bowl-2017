
library(deepnet)
library(caret)
library(Metrics)
library(data.table)

train <- fread('input/LUNA/train.csv', header=TRUE)
test  <- fread('input/LUNA/test.csv', header=TRUE)

train <- data.matrix(train)
test  <- data.matrix(test)

labelvar  <- train[,1]

k.fold     <- 10
nn.model   <- list()
k          <- 1
trainerror <- 0

for(k in 1:k.fold) {
  
  set.seed(11)
  folds <- createFolds(labelvar,k.fold)
  
  ul.folds <- unlist(folds)
  
  train.f      <- ul.folds[!ul.folds %in% folds[[k]]] #Set the training set
  validation.f <- folds[[k]]                          #Set the validation set
  
  train_70 <- data.matrix(data.frame(label=train[train.f,1],train[train.f,-1]))
  train_30 <- data.matrix(data.frame(label=train[validation.f,1],train[validation.f,-1]))
 
  h <- round(ncol(train_70[,-1])/2,0) + 1
  h1 <- round(h/2,0) + 1
 
    nn.model[[k]] <- nn.train(train_70[,-1],
                              train_70[,1],
                              hidden=c(h,h1),
                              #batchsize=150,   
                              #numepochs=33,
                              #momentum=0.5,
                              #learningrate = 0.4,
                              activationfun="sigm", 
                              output="softmax")
    
    predicted <- data.frame(nn.predict(nn.model[[k]],train_30[,-1]))
    colnames(predicted) <- "label"
    
    result <- data.table(train_30[,1],round(predicted,0))
    table(result)
}  
trainerror <- trainerror /10
cat("Train error:",trainerror)

###

predicted.s <- list()

for(k in 1:1){
  predicted.s[[k]] <- nn.predict(nn.model[[k]], test.set[,fields.00])
}

predicted.s <- data.frame(predicted.s)
p.ensemble  <- rowMeans(predicted.s)

p.ensemble <- exp(denormalize(data.frame(loss=p.ensemble),mm.loss)$loss) - shift.val

submission <- data.frame(test_id,p.ensemble)
colnames(submission) <- c("id","loss")

write.csv(submission, "output/submission_nn_ver04.csv", row.names=F, quote=F)
