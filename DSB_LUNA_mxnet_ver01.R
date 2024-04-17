
# 15 or 20

library(mxnet)
library(data.table)
library(ROCR)

train <- fread('input/LUNA/train.csv', header=TRUE)
test  <- fread('input/LUNA/test.csv', header=TRUE)

train <- data.matrix(train)
test  <- data.matrix(test)

train.x <- t(train[,-1])
train.y <- train[,1]

data <- mx.symbol.Variable("data")

fc1  <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=600)
bn1  <- mx.symbol.BatchNorm(data=fc1)
act1 <- mx.symbol.Activation(bn1, name="relu1", act_type="relu")

# Dropout 1
drp1 <- mx.symbol.Dropout(data=act1,p=0.5)

fc2  <- mx.symbol.FullyConnected(drp1, name="fc2", num_hidden=64)
bn2  <- mx.symbol.BatchNorm(data=fc2)
act2 <- mx.symbol.Activation(bn2, name="relu2", act_type="relu")

# Dropout 2
# drp2 <- mx.symbol.Dropout(data=act2,p=0.5)
# 
# fc3  <- mx.symbol.FullyConnected(drp2, name="fc3", num_hidden=32)
# bn3  <- mx.symbol.BatchNorm(data=fc3)
# act3 <- mx.symbol.Activation(bn3, name="relu3", act_type="relu")

# Dropuot 3
# drp3 <- mx.symbol.Dropout(act3)
fc7  <- mx.symbol.FullyConnected(act2, name="fc7", num_hidden=2)

softmax <- mx.symbol.SoftmaxOutput(fc7, name="sm")

devices <- lapply(1:4, function(i) {
  mx.cpu(i)
})

mx.set.seed(11)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                     ctx=devices, num.round=100, array.batch.size=100,
                                     learning.rate=0.09, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))


preds <- predict(model, t(test[,-1]))
dim(preds)


pred.label <- max.col(t(preds)) - 1
table(pred.label)

result  <- data.table(y=test[,1],yh=pred.label)

table(result)

pred <- prediction(result$yh,result$y)

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

#prefix <- 'LUNAmodel-ver01-35'
#iteration <- 35
#mx.model.save(model, prefix=prefix, iteration=iteration)
