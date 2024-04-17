
library(plot3D)
library(plotly)
library(data.table)
library(manipulate)
library(EBImage)

Rcpp::sourceCpp('~/Rprojects/Rcpp/canny.cpp')
Rcpp::sourceCpp('~/Rprojects/Rcpp/ISEF.cpp')

# Rotate Matrix 90 degrees
rotate <- function(x) t(apply(x, 2, rev))

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

normalizePlanes <- function (npzarray){
  
  maxHU <- max(npzarray) #400.00
  minHU <- min(npzarray) #-1000.00
  
  npzarray <-  (npzarray - minHU) / (maxHU - minHU)
  npzarray[npzarray > 1] <- 1.00
  npzarray[npzarray < 0] <- 0.00
  
  return(npzarray)
}

annotations <-read.csv("input/LUNA/annotations.csv",header = T, stringsAsFactors = F)

outputFile <- "output/LUNA/LUNA_annotations_canny.csv"
outfile <- file(outputFile, open="w+")

seriesuid <- "" 

par(mfrow=c(4,1))

for( imageline in 1:nrow(annotations)) {
  
  if (annotations$seriesuid[imageline] != seriesuid) {
    
    seriesuid <- annotations$seriesuid[imageline]
    
    rawfile <- paste0(annotations$seriesuid[imageline],".raw")
    mhdfile <- paste0(annotations$seriesuid[imageline],".mhd")
  
    filepath <- "D:/R Development/Data Science Bowl 2017/input/LUNA/files/"
  
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
  }
  
  worldCoord <- data.matrix(annotations[imageline,c(2,3,4)])
  
  voxelCoord <- worldCoord
  for(i in 1:nrow(worldCoord)){
    stretchedVoxelCoord <- abs(worldCoord[i,] - offsetvar)
    voxelCoord[i,] <- stretchedVoxelCoord / ElementSpacing
  }
  
  dim(bindata) <- DimSize
 
  voxelWidth <- round(65/2,0)
  
  for(imgn in 1:nrow(voxelCoord)){
    
    x1 <- round(voxelCoord[imgn,1],0)-voxelWidth
    x2 <- round(voxelCoord[imgn,1],0)+voxelWidth
    
    y1 <- round(voxelCoord[imgn,2],0)-voxelWidth
    y2 <- round(voxelCoord[imgn,2],0)+voxelWidth
    
    z <- round(voxelCoord[imgn,3],0)
    
    if ((x1 >= 1) & (y1 >=1)  & (x2 <= DimSize[1])  & (y2 <= DimSize[2])) {
      
      nbindata <- normalizePlanes(bindata[,,z])
      
      nbindata <- Canny(nbindata *255,0.75)
      
      nbindata <- nbindata[,,2] / 255
      
      patch1 <- nbindata[x1:x2, y1:y2]
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
    # else
    # {
    #   cat(imgn, "-")
    # }
  }
}

close(outfile)

