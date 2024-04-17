
library(data.table)
library(manipulate)
library(EBImage)
library(dplyr)

scoredfiles <- dir("output/Submission",pattern = "*.csv") %>% data.table()
colnames(scoredfiles) <- "id"

sfile  <- scoredfiles$id[1]
listid <- list()
probs  <- list()
listcnt <- 1

for(sfile in scoredfiles$id){
  
  images  <- fread(paste0("output/Submission/",sfile),header = F)
  
  probs[[listcnt]]   <- images[,c(2,3)]
  listid[[listcnt]]  <- images[,3:1683]
  listcnt <- listcnt + 1
  
}

dtimages <- matrix(unlist(listid), nrow=listcnt-1, byrow=T)
dtprobs  <- data.table(matrix(unlist(probs), nrow=listcnt-1, byrow=T))

dtimage <- t(dtimages) 
dim(dtimages) <- c(41,41,nrow(dtprobs))

base <- 1
viewer <- function(base){
  
  image(dtimages[,,base], col=grey(0:64/64), axes=FALSE)
  print(dtprobs[base,1:2])
}

manipulate(viewer(base), base = slider(1, nrow(dtprobs), step = 1))


image(dtimages, col=grey(0:64/64), axes=FALSE)
