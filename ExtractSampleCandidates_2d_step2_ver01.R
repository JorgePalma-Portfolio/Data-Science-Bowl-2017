
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

dt11  <- fread("output/KAGGLE/Newxclass0-v1.csv", header=F)
dt12  <- fread("output/KAGGLE/Newxclass0-v2.csv", header=F)
dt13  <- fread("output/KAGGLE/Newxclass0-v3.csv", header=F)
dt14  <- fread("output/KAGGLE/Newxclass0-v4.csv", header=F)
dt16  <- fread("output/KAGGLE/Newxclass0-v6.csv", header=F)
dt19  <- fread("output/KAGGLE/Newxclass0-v9.csv", header=F)
dt111 <- fread("output/KAGGLE/Newxclass0-v11.csv", header=F)
dt112 <- fread("output/KAGGLE/Newxclass0-v12.csv", header=F)
dt113 <- fread("output/KAGGLE/Newxclass0-v13.csv", header=F)
dt115 <- fread("output/KAGGLE/Newxclass0-v15.csv", header=F)
dt116 <- fread("output/KAGGLE/Newxclass0-v16.csv", header=F)
dt117 <- fread("output/KAGGLE/Newxclass0-v17.csv", header=F)
dt118 <- fread("output/KAGGLE/Newxclass0-v18.csv", header=F)

dt04 <- rbind(dt00,dt01,dt02,dt03,dt11,dt12,dt13,dt14,dt16,dt19,dt111,dt112,dt113,dt115,dt116,dt117,dt118)

dt04 <- dt04[order(dt04$V1000),]

trainIndex <- createDataPartition(dt04$V1, p = .8, list = FALSE, times = 1)

train <- dt04[trainIndex,]
test  <- dt04[-trainIndex,]

write.csv(train,"input/LUNA/train02.csv",row.names = F)
write.csv(test,"input/LUNA/test02.csv",row.names = F)


