
library(oro.dicom)
library(oro.nifti)
library(data.table)
library(dplyr)
library(manipulate)
library(fslr)

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom <- readDICOM(path = paste0("input/images/",Result[1]$id), verbose=TRUE)

dcmImage <-  create4D(dicom, sequence = TRUE)

niftiImage <- dicom2nifti(dicom)

image(niftiImage[,,dim(dcmImage)[3]-18],col=gray(0:64/64))

orthographic(niftiImage, col.crosshairs = "green")

pos1 <<- list()
pos2 <<- list()

base <- 1
viewer <- function(base){
  
  image(1:nrow(dcmImage[,,base,1]),1:ncol(dcmImage[,,base,1]),dcmImage[,,base,1], col=grey(0:64/64), axes=FALSE)
  pos1 <<- manipulatorMouseClick()
  pos2 <<- manipulatorMouseClick()
  
}

manipulate(viewer(base), base = slider(1, dim(dcmImage)[3], step = 1))


rect(pos1$userX,pos1$userY,pos2$userX,pos2$userY,border = "red")



rect(250,100,300,150,border="red")
