
# 0.7695727 AUC

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
conv1 <- mx.symbol.Convolution(data=data, kernel=c(3,3), num_filter=10)
tanh1 <- mx.symbol.Activation(data=conv1, act_type="relu")
pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max", kernel=c(2,2), stride=c(1,1))

# second conv
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(3,3), num_filter=20)
tanh2 <- mx.symbol.Activation(data=conv2, act_type="relu")
pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max", kernel=c(2,2), stride=c(1,1))

# first fullc
flatten <- mx.symbol.Flatten(data=pool2)
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=20)
sigmoid1 <- mx.symbol.Activation(data=fc1, act_type="sigmoid")

# second fullc
fc2 <- mx.symbol.FullyConnected(data=sigmoid1, num_hidden=2)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2)

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

devices <- lapply(1:8, function(i) {
  mx.cpu(i)
})

mx.set.seed(11)
tic <- proc.time()
#MXNET_CPU_WORKER_NTHREADS <- 7

model <- mx.model.FeedForward.create(lenet, X=train.array, y=train.y,
                                     ctx=devices, num.round=15, array.batch.size=100,
                                     learning.rate=0.1, momentum=0.9, wd=0.00001,
                                     eval.metric=mx.metric.accuracy,
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


