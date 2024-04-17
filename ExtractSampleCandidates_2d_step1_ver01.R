

library(caret)
library(data.table)

cd <- fread("output/LUNA/LUNA_2d_candidates.csv", header=F)

dt00 <- subset(cd, V1 == 0)
dt01 <- subset(cd, V1 == 1)

set.seed(1)
dt00 <- dt00[ sample( 1:nrow(dt00), nrow(dt00)*0.4),]
dt02 <- fread("output/LUNA/LUNA_2d_annotations.csv", header=F)
dt03 <- fread("output/VIA/VIA_2d_annotations.csv", header=F)
dt00 <- dt00[ sample(1:nrow(dt00) , nrow(dt00)*0.5),]

dt04 <- rbind(dt00,dt01,dt02,dt03) 
dt04 <- dt04[order(dt04$V1000),]

trainIndex <- createDataPartition(dt04$V1, p = .8, list = FALSE, times = 1)

train <- dt04[trainIndex,]
test  <- dt04[-trainIndex,]

write.csv(train,"input/LUNA/train01.csv",row.names = F)
write.csv(test,"input/LUNA/test01.csv",row.names = F)


