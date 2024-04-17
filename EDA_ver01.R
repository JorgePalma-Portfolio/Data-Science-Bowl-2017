
library(oro.dicom)
library(data.table)
library(dplyr)

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom <- readDICOM(path = paste0("input/images/",Result[5]$id), verbose=TRUE)

graphics::image(t(dicom$img[[3]]), col=grey(0:255/255), axes=FALSE)

dcmImage <-  create3D(dicom)

graphics::image(dcmImage[,,1], col=grey(0:255/255), axes=FALSE)

IOP.dt <- data.table(extractHeader(dicom$hdr, "ImageOrientationPatient", numeric=FALSE))
IOP <- extractHeader(dicom$hdr, "ImageOrientationPatient", numeric=FALSE)

IPP.dt <- data.table(extractHeader(dicom$hdr, "ImagePositionPatient", numeric=FALSE))

ImagePositionPatient


summary(dicom$hdr[[1]]$group)

class(dicom$hdr[[1]]$group)
