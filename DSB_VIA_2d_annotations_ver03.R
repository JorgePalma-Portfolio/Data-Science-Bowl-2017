
library(data.table)
library(manipulate)
library(abind)
library(oro.dicom)
library(abind)
library(EBImage)

# Rotate Matrix 90 degrees
rotate <- function(x) t(apply(x, 2, rev))

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

normalizePlanes <- function (npzarray){
  
  maxHU <- 400 #max(npzarray)
  minHU <- -1000 #min(npzarray)
  
  npzarray <-  (npzarray - minHU) / (maxHU - minHU)
  npzarray[npzarray > 1] <- 1.00
  npzarray[npzarray < 0] <- 0.00
  
  return(npzarray)
}

path <- "F:/R Development/Data Science Bowl 2017/input/VIA/"
annotations <-read.csv(paste0(path,"annotations.csv"),header = T, stringsAsFactors = F)

outputFile <- "output/VIA/VIA_2d_annotations.csv"
outfile <- file(outputFile, open="w+")

seriesuid <- "" 

imageline <- 150

for( imageline in 1:nrow(annotations)) {
  
  if (annotations$FILE[imageline] != seriesuid) {
    
    seriesuid <- annotations$FILE[imageline]
    
    filepath <- "F:/R Development/Data Science Bowl 2017/input/VIA/files/"
    
    dicom <- readDICOM(path = paste0(filepath, annotations$FILE[imageline]), verbose=F)
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
    
    imageList <- list()
    
    for(i in 1:dim(dcmImage)[3]){
      imageList[[i]] <- resize(dcmImage[,,i,1],new_real_shape[1],new_real_shape[1])
    }  
    newImage <- abind( imageList, along=3 )
  }
    
  voxelCoord <- data.matrix(annotations[imageline,c(2,3,4)])
  voxelWidth <- 20
    
  x1 <- round(voxelCoord[1] * real_resize_factor[1],0)-voxelWidth
  x2 <- round(voxelCoord[1] * real_resize_factor[1],0)+voxelWidth
  y1 <- round(voxelCoord[2] * real_resize_factor[2],0)-voxelWidth
  y2 <- round(voxelCoord[2] * real_resize_factor[2],0)+voxelWidth
  z  <- round(voxelCoord[3],0)
  
  ImagePatch <- newImage[,,z]
  ImagePatch <- normalizePlanes(ImagePatch)
  
  if ((x1 >= 1) & (y1 >=1)  & (x2 <= new_shape[1]) & (y2 <= new_shape[2])) {
    
    patch1 <- ImagePatch[x1:x2, y1:y2]
    patch2 <- rotate(patch1)
    patch3 <- rotate(patch2)
    patch4 <- rotate(patch3)
    
    outputLine1 <- paste(c("1",as.vector(patch1)), collapse=",")
    outputLine2 <- paste(c("1",as.vector(patch2)), collapse=",")
    outputLine3 <- paste(c("1",as.vector(patch3)), collapse=",")
    outputLine4 <- paste(c("1",as.vector(patch4)), collapse=",")
    
    writeLines(outputLine1,outfile)
    writeLines(outputLine2,outfile)
    writeLines(outputLine3,outfile)
    writeLines(outputLine4,outfile)
  }
}

close(outfile)

image(patch1,col=gray(0:64/64))
