
library(mxnet)
library(data.table)
library(ROCR)

train <- fread('input/LUNA/train.csv', header=TRUE)
test  <- fread('input/LUNA/test.csv', header=TRUE)

train <- data.matrix(train)
test  <- data.matrix(test)

train.x <- train[,-1]
train.y <- train[,1]

# input
data <- mx.symbol.Variable('data')

# first conv
conv1      <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=10, stride=c(2,2))
batchnorm1 <- mx.symbol.BatchNorm(data=conv1)
relu1      <- mx.symbol.Activation(data=batchnorm1, act_type="relu")
pool1      <- mx.symbol.Pooling(data=relu1, pool_type="max", kernel=c(2,2), stride=c(2,2))

# second conv
conv2      <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=20, stride=c(2,2))
batchnorm2 <- mx.symbol.BatchNorm(data=conv2)
relu2      <- mx.symbol.Activation(data=batchnorm2, act_type="relu")
pool2      <- mx.symbol.Pooling(data=relu2, pool_type="max", kernel=c(2,2), stride=c(2,2))

# Flatten
flatten <- mx.symbol.Flatten(data=pool2)

# Dropout 1
dropout1 <- mx.symbol.Dropout(data=flatten)

# first fullc
fc1        <- mx.symbol.FullyConnected(data=dropout1, num_hidden=40)
batchnorm3 <- mx.symbol.BatchNorm(data=fc1, fix_gamma=FALSE)
relu3      <- mx.symbol.Activation(data=batchnorm3, act_type="relu")

# Third fullc
fc2 <- mx.symbol.FullyConnected(data=relu3, num_hidden=2)
# loss
mymodel <- mx.symbol.SoftmaxOutput(data=fc2)

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

model <- mx.model.FeedForward.create(mymodel, X=train.array, y=train.y, eval.data=list(data=test.array, label=test[,1]),
                                     ctx=devices, num.round=25, array.batch.size=100,
                                     learning.rate=0.09, momentum=0.9, wd=0.00001,
                                     eval.metric=mx.metric.accuracy,
                                     #batch.end.callback=mx.callback.log.train.metric(100),
                                     epoch.end.callback=mx.callback.log.train.metric(100))



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

