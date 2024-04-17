
# First Pass
# sensitivity 0.8262132 true positive rate (w disease)
# specificity 0.9841429 true negative rate (w/o disease)
# cutoff      1.0000000
# AUC         0.905178

# Sencond Pass
# sensitivity 0.8064380 true positive rate (w disease)
# specificity 0.9830485 true negative rate (w/o disease) 
# cutoff      1.0000000
# AUC         0.8947432

library(mxnet)
library(data.table)
library(ROCR)

train <- fread('input/LUNA/train02.csv', header=TRUE)
test  <- fread('input/LUNA/test02.csv', header=TRUE)

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
pool1      <- mx.symbol.Pooling(data=relu1, pool_type="max", kernel=c(2,2), stride=c(2,2))

# second conv
conv2      <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=20, stride=c(1,1))
batchnorm2 <- mx.symbol.BatchNorm(data=conv2, fix_gamma=FALSE)
relu2      <- mx.symbol.Activation(data=batchnorm2, act_type="relu")
pool2      <- mx.symbol.Pooling(data=relu2, pool_type="max", kernel=c(2,2), stride=c(2,2))

# third conv
conv3      <- mx.symbol.Convolution(data=pool2, kernel=c(5,5), num_filter=40, stride=c(2,2))
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

# Second fullc
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

#learning.rate=0.05 for the first pass

model <- mx.model.FeedForward.create(mymodel, X=train.array, y=train.y, eval.data=list(data=test.array, label=test[,1]),
                                     ctx=devices, num.round=25, array.batch.size=100,
                                     learning.rate=0.09, momentum=0.9, wd=0.00001,
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

# 0.9051780 step 0
# 0.8947432 step 1

prefix <- '20170314xLUNAmodel-ver02-25-step1'
iteration <- 25

#mx.model.save(model, prefix=prefix, iteration=iteration)
 
model_01 <- mx.model.load(prefix,iteration=iteration)

preds <- predict(model_01, test.array)
pred.label <- max.col(t(preds)) - 1

result  <- data.table(y=test[,1],yh=pred.label)
table(result)


pred <- prediction(result$yh,result$y.V1)

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

boxplot(yh ~ y.V1, data = result)
