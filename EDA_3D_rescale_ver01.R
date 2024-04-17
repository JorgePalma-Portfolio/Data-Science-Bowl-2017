
library(oro.dicom)
library(data.table)
library(dplyr)
library(EBImage)
library(abind)
library(manipulate)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

labels <- fread("input/stage1_labels.csv") %>% data.table
samplefiles <- dir("input/images") %>% data.table
colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom    <- readDICOM(path = paste0("input/images/",Result[15]$id), verbose=TRUE)
dcmImage <- create4D(dicom, sequence = TRUE)

PixelSpacing <- extractHeader(dicom$hdr, "PixelSpacing",numeric = FALSE)[1]
PixelSpacing <- as.numeric(unlist(strsplit(trim(PixelSpacing)," ")))

ImagePositionPatient <- extractHeader(dicom$hdr, "ImagePositionPatient",numeric = FALSE)
ImagePositionPatient <- as.numeric(unlist(strsplit(trim(ImagePositionPatient)," ")))
ImagePositionPatient <- matrix(ImagePositionPatient,ncol=3,byrow=T)
ImagePositionPatient <- ImagePositionPatient[order(ImagePositionPatient[,3]),]
thickness <- abs(ImagePositionPatient[2,3] - ImagePositionPatient[1,3]) 

spacing     <- c(PixelSpacing, thickness)
new_spacing <- c(1,1,1)
image.shape <- c(dim(dcmImage)[1],dim(dcmImage)[2],dim(dcmImage)[3])

resize_factor  <- spacing / new_spacing
new_real_shape <- round(image.shape * resize_factor,0)
new_shape      <- round(new_real_shape,0)

real_resize_factor <- new_shape / image.shape
new_spacing        <- spacing / real_resize_factor

imageList <- list()

for( i in 1:dim(dcmImage)[3]){
  
  imageList[[i]] <- resize(dcmImage[,,i,1],new_real_shape[1],new_real_shape[1])
}  

newImage <- abind( imageList, along=3 )

image(newImage[,,50], col=grey(0:64/64), axes=T)

cat(dim(dcmImage)[3])

imageList <- list()
 
for( i in 1:dim(newImage)[2]){
  
  imageList[[i]] <- resize(newImage[,i,],new_real_shape[2],new_real_shape[3])
}

newImage2 <- abind(imageList, along=3)

dim(newImage2)

imageList <- list()

for( i in 1:dim(newImage2)[2]){
  
  imageList[[i]] <- newImage2[,i,]
}

newImage3 <- abind(imageList, along=3)
dim(newImage3)


image(1:new_real_shape[1],1:new_real_shape[1],newImage3[,,55], col=grey(0:64/64), axes=T)


viewer <- function(base){
  image(1:new_real_shape[1],1:new_real_shape[2],newImage3[,,base], col=grey(0:64/64), axes=T)
}

manipulate(viewer(base), base = slider(1, dim(newImage3)[3], step = 1))


