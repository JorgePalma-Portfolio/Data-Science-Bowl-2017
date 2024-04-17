
library(oro.dicom)
library(oro.nifti)
library(data.table)
library(dplyr)
library(manipulate)
library(fslr)

Rcpp::sourceCpp('CPPFunctions.cpp')

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom <- readDICOM(path = paste0("input/images/",Result[1]$id), verbose=TRUE)

dcmImage <-  create4D(dicom, sequence = TRUE)


il <- searchPatches(dcmImage[,,18,1],41,1)



pos1 <<- list()
pos2 <<- list()

base <- 1
viewer <- function(base){
  
  image(1:nrow(dcmImage[,,base,1]),1:ncol(dcmImage[,,base,1]),dcmImage[,,base,1], col=grey(0:64/64), axes=FALSE)
  pos1 <<- manipulatorMouseClick()
  pos2 <<- manipulatorMouseClick()
  
}

manipulate(viewer(base), base = slider(1, dim(dcmImage)[3], step = 1))
