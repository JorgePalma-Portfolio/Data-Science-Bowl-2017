
library(caret)
library(data.table)

cd <- fread("output/LUNA/LUNA_candidates.csv", header=F, stringsAsFactors = F)

c1 <- subset(cd, V1 == 1)
c0 <- subset(cd, V1 == 0)

set.seed(11)
c0 <- cd[ sample( which( cd$V1==0 ) , nrow(c0)*0.5 ) , ]

dt01 <- fread("output/LUNA/LUNA_annotations.csv", header=F, stringsAsFactors = F)
dt02 <- fread("output/VIA/VIA_annotations.csv", header=F, stringsAsFactors = F)


set.seed(111)
dt03 <- c0[ sample( which( c0$V1==0 ) , nrow(c0)*0.50 ) , ]

dt04 <- rbind(dt01,dt02,c1,dt03)
dt04 <- dt04[order(dt04$V1000),]
dt04 <- dt04[order(dt04$V100),]
dt04 <- dt04[order(dt04$V10),]

set.seed(112)
trainIndex <- createDataPartition(dt04$V1, p = .8, list = FALSE, times = 1)

train <- dt04[trainIndex,]
test <- dt04[-trainIndex,]

write.csv(train,"input/LUNA/train.csv",row.names = F)
write.csv(test,"input/LUNA/test.csv",row.names = F)


