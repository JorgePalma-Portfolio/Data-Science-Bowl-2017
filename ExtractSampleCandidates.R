

library(caret)
library(data.table)

cd <- fread("output/LUNA/LUNA_candidates.csv", header=F, stringsAsFactors = F)

c1 <- subset(cd, V1 == 1)
write.table(c1,"output/LUNA/LUNA_candidates_1.csv",sep=",",row.names = F)
rm(c1)

c0 <- subset(cd, V1 == 0)

set.seed(1)
sc0 <- cd[ sample( which( cd$V1==0 ) , nrow(c0)*0.5 ) , ]
 
#write.table(sc0,"output/LUNA/LUNA_candidates_0_20p.csv",sep=",",row.names = F)

dt01 <- fread("output/LUNA/LUNA_annotations.csv", header=F, stringsAsFactors = F)

dt06 <- fread("output/VIA/VIA_annotations.csv", header=F, stringsAsFactors = F)

dt07 <- fread("output/KAGGLE/class0-v0.csv", header=F, stringsAsFactors = F)
dt08 <- fread("output/KAGGLE/class0-v1.csv", header=F, stringsAsFactors = F)

dt09 <- fread("output/KAGGLE/class0-v2.csv", header=F, stringsAsFactors = F)
dt10 <- fread("output/KAGGLE/class0-v3.csv", header=F, stringsAsFactors = F)
dt11 <- fread("output/KAGGLE/class0-v4.csv", header=F, stringsAsFactors = F)
dt12 <- fread("output/KAGGLE/class0-v5.csv", header=F, stringsAsFactors = F)
dt13 <- fread("output/KAGGLE/class0-v6.csv", header=F, stringsAsFactors = F)

dt02 <- fread("output/LUNA/LUNA_candidates_1.csv", header=T, stringsAsFactors = F)

#dt03 <- fread("output/LUNA/LUNA_candidates_0_20p.csv", header=T, stringsAsFactors = F)

dt03 <- sc0

set.seed(11)
dt04 <- dt03[ sample( which( dt03$V1==0 ) , nrow(dt03)*0.50 ) , ]

dt05 <- rbind(dt01,dt04,dt02,dt07,dt06,dt08,dt09,dt10,dt11)


dt05 <- dt05[order(dt05$V1000),]

dt05 <- dt05[order(dt05$V100),]

dt05 <- dt05[order(dt05$V10),]

set.seed(1)
trainIndex <- createDataPartition(dt05$V1, p = .8, list = FALSE, times = 1)

train <- dt05[trainIndex,]

test <- dt05[-trainIndex,]

train <- rbind(train,dt12,dt13)

write.csv(train,"input/LUNA/train.csv",row.names = F)

write.csv(test,"input/LUNA/test.csv",row.names = F)


