

library(caret)
library(data.table)

cd <- fread("output/LUNA/LUNA_candidates.csv", header=F)

dt00 <- subset(cd, V1 == 0)

set.seed(1)
dt00 <- dt00[ sample( 1:nrow(dt00), nrow(dt00)*0.2),]
dt01 <- fread("output/LUNA/LUNA_annotations.csv", header=F)
dt02 <- fread("output/VIA/VIA_annotations.csv", header=F)
dt00 <- dt00[ sample(1:nrow(dt00) , nrow(dt00)*0.1),]

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

dt21 <-  fread("output/KAGGLE/SecondPassxclass0-v1.csv", header=F)
dt22 <-  fread("output/KAGGLE/SecondPassxclass0-v2.csv", header=F)
dt23 <-  fread("output/KAGGLE/SecondPassxclass0-v3.csv", header=F)
dt24 <-  fread("output/KAGGLE/SecondPassxclass0-v4.csv", header=F)
dt26 <-  fread("output/KAGGLE/SecondPassxclass0-v6.csv", header=F)

dt29  <-  fread("output/KAGGLE/SecondPassxclass0-v9.csv", header=F)
dt211 <-  fread("output/KAGGLE/SecondPassxclass0-v11.csv", header=F)
dt212 <-  fread("output/KAGGLE/SecondPassxclass0-v12.csv", header=F)
dt213 <-  fread("output/KAGGLE/SecondPassxclass0-v13.csv", header=F)
dt215 <-  fread("output/KAGGLE/SecondPassxclass0-v15.csv", header=F)

dt216 <-  fread("output/KAGGLE/SecondPassxclass0-v16.csv", header=F)
dt217 <-  fread("output/KAGGLE/SecondPassxclass0-v17.csv", header=F)
dt218 <-  fread("output/KAGGLE/SecondPassxclass0-v18.csv", header=F)


dt05 <- rbind(dt00,dt01,dt02,dt11,dt12,dt13,dt14,dt16,dt19,dt111,dt112,dt113,dt115,dt116,dt117,dt118,dt21,dt22,dt23,dt24,dt26,dt29,
              dt211,dt212,dt213,dt215,dt216,dt217,dt218)

dt05 <- dt05[order(dt05$V1000),]

trainIndex <- createDataPartition(dt05$V1, p = .8, list = FALSE, times = 1)

train <- dt05[trainIndex,]
test  <- dt05[-trainIndex,]

write.csv(train,"input/LUNA/train_LUNA_VIA.csv",row.names = F)
write.csv(test,"input/LUNA/test_LUNA_VIA.csv",row.names = F)


