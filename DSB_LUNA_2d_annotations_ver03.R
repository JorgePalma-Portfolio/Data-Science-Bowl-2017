
library(data.table)
library(manipulate)
library(EBImage)
library(abind)

# Rotate Matrix 90 degrees
rotate <- function(x) t(apply(x, 2, rev))

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

annotations <-read.csv("input/LUNA/annotations.csv",header = T, stringsAsFactors = F)

outputFile <- "output/LUNA/LUNA_2d_annotations.csv"
outfile <- file(outputFile, open="w+")

seriesuid <- "" 

imageline <- 5

for( imageline in 1:nrow(annotations)) {
  
  if (annotations$seriesuid[imageline] != seriesuid) {
    
    seriesuid <- annotations$seriesuid[imageline]
    
    rawfile <- paste0(annotations$seriesuid[imageline],".raw")
    mhdfile <- paste0(annotations$seriesuid[imageline],".mhd")
    
    filepath <- "F:/R Development/Data Science Bowl 2017/input/LUNA/files/"
    
    readtext.filename <- file(paste0(filepath,mhdfile), "rt")
    textdata <- readLines(readtext.filename)
    
    splittext <- strsplit(textdata,"[=]")
    mhdvars <- data.table(matrix(unlist(splittext), nrow=length(splittext), ncol=2, byrow=T))
    
    offsetvar      <- as.numeric(unlist(strsplit(trim(mhdvars[V1 %like% "Offset", ]$V2)," ")))
    ElementSpacing <- as.numeric(unlist(strsplit(trim(mhdvars[V1 %like% "ElementSpacing", ]$V2)," ")))
    DimSize        <- as.numeric(unlist(strsplit(trim(mhdvars[V1 %like% "DimSize", ]$V2)," ")))
    
    blobsize <- DimSize[1]*DimSize[2]*DimSize[3]
    
    # Create a connection object to read the file in binary mode using "rb".
    readbin.filename  <- file(paste0(filepath,rawfile), "rb")
    bindata  <- readBin(readbin.filename, integer(),  size=2,n = blobsize)
    
    close(readbin.filename)
    close(readtext.filename)
    
    spacing     <- ElementSpacing
    new_spacing <- c(1,1,1)
    image.shape <- DimSize
    
    resize_factor  <- spacing / new_spacing
    new_real_shape <- round(image.shape * resize_factor,0)
    new_shape      <- round(new_real_shape,0)
    
    real_resize_factor <- new_shape / image.shape
    new_spacing        <- spacing / real_resize_factor
    
    dim(bindata) <- DimSize
    
    imageList <- list()
    
    for( i in 1:dim(bindata)[3]){
      imageList[[i]] <- resize(bindata[,,i],new_real_shape[1],new_real_shape[1])
    }  
    newImage <- abind( imageList, along=3 )
  }
  
  worldCoord <- data.matrix(annotations[imageline,c(2,3,4)])
  
  voxelCoord <- worldCoord
  stretchedVoxelCoord <- abs(worldCoord[1,] - offsetvar)
  voxelCoord[1,] <- as.double(stretchedVoxelCoord) %/% as.double(ElementSpacing)
  
  voxelWidth <- 20
    
  x1 <- round(voxelCoord[1,1] * ElementSpacing[1],0)-voxelWidth
  x2 <- round(voxelCoord[1,1] * ElementSpacing[1],0)+voxelWidth
  y1 <- round(voxelCoord[1,2] * ElementSpacing[2],0)-voxelWidth
  y2 <- round(voxelCoord[1,2] * ElementSpacing[2],0)+voxelWidth
  z  <- round(voxelCoord[1,3],0)
  
  ImagePatch <- newImage[,,z+1]
  ImagePatch <- normalizePlanes(ImagePatch)
  
  if ((x1 >= 1) & (y1 >=1)  & (x2 <= new_shape[1])  & (y2 <= new_shape[2])) {
    
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
