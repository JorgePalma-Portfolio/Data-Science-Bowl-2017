
library(data.table)
library(manipulate)
library(EBImage)
library(dplyr)


submissionfiles <- dir("output/Submission") %>% data.table
colnames(submissionfiles) <- "id"

imageNumber <- 2

tokens <-strsplit(submissionfiles$id[imageNumber], "[-//.]")

file <- read.csv(paste0("output/Submission/",submissionfiles$id[imageNumber]),header = F)

newImage <- t(file[,-c(1,2)])

dim(newImage) <- c(41,41,nrow(file))

base <- 1
viewer <- function(base,file){
  
  image(newImage[,,base], col=grey(0:64/64), axes=FALSE)
  print(file[base,1:2])
 }

manipulate(viewer(base), base = slider(1, 2, step = 1))
