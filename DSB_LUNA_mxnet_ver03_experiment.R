
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
conv1      <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=10, stride=c(1,1))
batchnorm1 <- mx.symbol.BatchNorm(data=conv1, fix_gamma=FALSE)
relu1      <- mx.symbol.Activation(data=batchnorm1, act_type="relu")
#pool1      <- mx.symbol.Pooling(data=relu1, pool_type="max", kernel=c(2,2), stride=c(1,1))

# second conv
conv2      <- mx.symbol.Convolution(data=relu1, kernel=c(7,7), num_filter=20, stride=c(1,1))
batchnorm2 <- mx.symbol.BatchNorm(data=conv2, fix_gamma=FALSE)
relu2      <- mx.symbol.Activation(data=batchnorm2, act_type="relu")
#pool2      <- mx.symbol.Pooling(data=relu2, pool_type="max", kernel=c(2,2), stride=c(2,2))

# third conv
conv3      <- mx.symbol.Convolution(data=relu2, kernel=c(9,9), num_filter=40, stride=c(2,2))
batchnorm3 <- mx.symbol.BatchNorm(data=conv3, fix_gamma=FALSE)
relu3      <- mx.symbol.Activation(data=batchnorm3, act_type="relu")
pool3      <- mx.symbol.Pooling(data=relu3, pool_type="max", kernel=c(2,2), stride=c(2,2))

# Flatten
flatten <- mx.symbol.Flatten(data=pool3)

# Dropout 1
dropout1 <- mx.symbol.Dropout(data=flatten,p=0.5)

# first fullc
fc1        <- mx.symbol.FullyConnected(data=dropout1, num_hidden=40)
batchnorm3 <- mx.symbol.BatchNorm(data=fc1, fix_gamma=FALSE)
relu3      <- mx.symbol.Activation(data=batchnorm3, act_type="relu")

# Dropout 2
dropout2 <- mx.symbol.Dropout(data=relu3,p=0.5)

# Second fullc
fc2        <- mx.symbol.FullyConnected(data=dropout2, num_hidden=20)
batchnorm4 <- mx.symbol.BatchNorm(data=fc2, fix_gamma=FALSE)
relu4      <- mx.symbol.Activation(data=batchnorm4, act_type="relu")

# Third fullc
fc3 <- mx.symbol.FullyConnected(data=relu4, num_hidden=2)
# loss
mymodel <- mx.symbol.SoftmaxOutput(data=fc3)

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

model <- mx.model.FeedForward.create(mymodel, X=train.array, y=train.y,
                                     ctx=devices, num.round=5, array.batch.size=100,
                                     learning.rate=0.05, momentum=0.9, wd=0.00001,
                                     eval.metric=mx.metric.accuracy,
                                     epoch.end.callback=mx.callback.log.train.metric(100))
                                       #mx.callback.early.stop(bad.steps = 2, maximize = TRUE, verbose = TRUE))



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

