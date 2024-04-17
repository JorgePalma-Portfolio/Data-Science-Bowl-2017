
library(oro.dicom)
library(data.table)
library(dplyr)
library(manipulate)
library(EBImage)


Rcpp::sourceCpp('../Rcpp/rescale.cpp')

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom <- readDICOM(path = paste0("input/images/",Result[5]$id), verbose=TRUE)

dcmImage <-  create4D(dicom, sequence = TRUE)

img0 <- matrix()
base <- 1
viewer <- function(base){
  
  img2 <- dcmImage[,,base,1] 
  
  #img2 <- RescaleImage(img2,250,250)
  #img <- img2
  #img[(img >= 0) & (img <= 500) ] <- 10000
  
  #img[img != 10000 ] <- 0
  #img[img != 0] <- 1
  
  #img0 <<- img
  #for(i in 1:dim(img2)[1]){
  #  img0 <<- floodFill(img0,c(5,i),0)
  #  img0 <<- floodFill(img0,c(i,5),0)
  #}
  
  #image(1:nrow(img0),1:ncol(img0),img0, col=grey(0:64/64), axes=FALSE)
  image(img2, col=grey(0:64/64), axes=FALSE)
 }

manipulate(viewer(base), base = slider(1, dim(dcmImage)[3], step = 1))


#hist(dcmImage[,,18,1])
