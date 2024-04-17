
library(oro.dicom)
library(data.table)
library(dplyr)
library(manipulate)
library(EBImage)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

normalizePlanes <- function (npzarray){
  
  maxHU <- 400.00
  minHU <- -1000.00
  
  npzarray <-  (npzarray - minHU) / (maxHU - minHU)
  npzarray[npzarray > 1] <- 1.00
  npzarray[npzarray < 0] <- 0.00
  
  return(npzarray)
}

submissionfiles <- dir("D:/submission") %>% data.table
colnames(submissionfiles) <- "id"

dicom    <- readDICOM(path = paste0("D:/submission/",submissionfiles[3]$id), verbose=TRUE)
dcmImage <-  create4D(dicom, sequence = TRUE)

RescaleSlope     <- extractHeader(dicom$hdr, "RescaleSlope") 
RescaleIntercept <- extractHeader(dicom$hdr, "RescaleIntercept")
PixelSpacing     <-  extractHeader(dicom$hdr, "PixelSpacing",numeric = FALSE)[1]
ImagePositionPatient <-  extractHeader(dicom$hdr, "ImagePositionPatient",numeric = FALSE)

dcmImage <-  RescaleSlope * dcmImage
dcmImage <-  dcmImage + RescaleIntercept

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

for(i in 1:dim(dcmImage)[3]){
  dcmImage[,,i,1] <- normalizePlanes(dcmImage[,,i,1])
} 

newImage3 <- imager::resize(as.cimg(dcmImage),size_x=new_real_shape[1] , 
                            size_y=new_real_shape[2] , 
                            size_z=new_real_shape[3], 
                            interpolation_type = 3)

newImage3 <- as.array(newImage3[,,,1])

base <- 1
viewer <- function(base){
  
  image(newImage3[,,base], col=grey(0:64/64), axes=FALSE)
 }

manipulate(viewer(base), base = slider(1, dim(newImage3)[3], step = 1))
