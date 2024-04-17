
library(mxnet)
library(data.table)
library(ROCR)

ConvFactory <- function(data, num_filter, kernel, stride=c(1,1), pad=c(0, 0), act_type="relu"){
  conv <- mx.symbol.Convolution(data=data, num_filter=num_filter, kernel=kernel, stride=stride, pad=pad)
  bn   <- mx.symbol.BatchNorm(data=conv)
  act  <- mx.symbol.Activation(data = bn, act_type=act_type)
  return(act)
}

ResidualFactory <- function(data){
  l1 <- ConvFactory(data, 32, kernel=c(3,3), pad=c(1,1))
  l2 <- ConvFactory(l1, 32, kernel=c(3,3), pad=c(1,1))
  return(l2 + data)
}

get_symbol <- function(num_classes = 2){
  data <- mx.symbol.Variable(name="data")
  o    <- ConvFactory(data=data, kernel=c(3,3), pad=c(1,1), num_filter=10, act_type="relu")
  for(i in 1:50){
    o <- ResidualFactory(o)
  }
  pool <- mx.symbol.Pooling(data=o, pool_type="avg", kernel=c(5,5), name="global_pool")
  
  flatten <- mx.symbol.Flatten(data=pool, name="flatten1")
  
  fc1        <- mx.symbol.FullyConnected(data=flatten, num_hidden=20)
  batchnorm3 <- mx.symbol.BatchNorm(data=fc1, fix_gamma=FALSE)
  relu3      <- mx.symbol.Activation(data=batchnorm3, act_type="relu")
  
  fc <- mx.symbol.FullyConnected(data=relu3, num_hidden=num_classes, name="fc1")
  
  label <- mx.symbol.Variable("softmax_label")
  softmax <- mx.symbol.SoftmaxOutput(data=fc, name="softmax", label=label)
  return(softmax)
}

train <- fread('input/LUNA/train.csv', header=TRUE)
test  <- fread('input/LUNA/test.csv', header=TRUE)

train <- data.matrix(train)
test  <- data.matrix(test)

train.x <- train[,-1]
train.y <- train[,1]

# My Model
mymodel <- get_symbol()

train.array <- train.x

train.array <- t(train.array)

dim(train.array) <- c(41, 41, 1, ncol(train.array))

test1 <- test[,-1]

test.array <- test1

test.array <- t(test.array)
dim(test.array) <- c(41, 41, 1, ncol(test.array))

# n.gpu <- 1
# device.gpu <- lapply(0:(n.gpu-1), function(i) {
#   mx.gpu(i)
# })

devices <- lapply(1:4, function(i) {
  mx.cpu(i)
})

mx.set.seed(11)
tic <- proc.time()
#MXNET_CPU_WORKER_NTHREADS <- 7

logger = mx.metric.logger()

model <- mx.model.FeedForward.create(mymodel, X=train.array, y=train.y, eval.data=list(data=test.array, label=test[,1]),
                                     ctx=devices, num.round=200, array.batch.size=100,
                                     learning.rate=0.05, momentum=0.9, wd=0.00001,
                                     eval.metric=mx.metric.accuracy,
                                     epoch.end.callback=mx.callback.early.stop(8, paste0(model.name,".Fold",i), F, logger), # custom callback func
                                     batch.end.callback=mx.callback.log.train.metric(100))



print(proc.time() - tic)

preds <- predict(model, test.array)
pred.label <- max.col(t(preds)) - 1

result  <- data.table(y=test[,1],yh=pred.label)
table(result)


pred <- prediction(result$yh,result$y)

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

# 0.9189218
prefix <- 'NewxLUNAmodel-ver02.1-25'
iteration <- 25

#mx.model.save(model, prefix=prefix, iteration=iteration)
 
model_01 <- mx.model.load(prefix,iteration=iteration)
 
preds <- predict(model_01, test.array)
pred.label <- max.col(t(preds)) - 1

result  <- data.table(y=test[,1],yh=pred.label)
table(result)
 
roc.perf = performance(preds, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

