
library(plot3D)
library(plotly)
library(data.table)

#1.3.6.1.4.1.14519.5.2.1.6279.6001.100621383016233746780170740405


# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

normalizePlanes <- function (npzarray){
  
  maxHU <- max(npzarray)
  minHU <-  min(npzarray)
  
  npzarray <-  (npzarray - minHU) / (maxHU - minHU)
  npzarray[npzarray > 1] <- 1.00
  npzarray[npzarray < 0] <- 0.00
  
  return(npzarray)
}


annotations <-  read.csv("https://www.dropbox.com/s/un1c5btca7cda4i/annotations_sample.csv?dl=1",header = T, stringsAsFactors = F)


viewer <- function(imageline){
  
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
  
  worldCoord <- data.matrix(annotations[imageline,c(2,3,4)])
  
  voxelCoord <- worldCoord
  for(i in 1:nrow(worldCoord)){
    stretchedVoxelCoord <- abs(worldCoord[i,] - offsetvar)
    voxelCoord[i,] <- stretchedVoxelCoord / ElementSpacing
  }
  
  
  dim(bindata) <- DimSize
  #image(1:nrow(bindata),1:ncol(bindata),bindata[,,88],col=grey(0:255/255))
  
  voxelWidth <- round(65/2,0)
  
  #par(mfrow=c(4,5))
  for(imgn in 1:nrow(voxelCoord)){
    
    x1 <- round(voxelCoord[imgn,1],0)-voxelWidth
    x2 <- round(voxelCoord[imgn,1],0)+voxelWidth
    
    y1 <- round(voxelCoord[imgn,2],0)-voxelWidth
    y2 <- round(voxelCoord[imgn,2],0)+voxelWidth
    
    z <- round(voxelCoord[imgn,3],0)
    
    if (imgn ==1){
      nbindata <- normalizePlanes2(bindata[,,z])
      
      #fHigh <- matrix(1, nc = 3, nr = 3)
      #fHigh[2, 2] <- -8
      #nbindata <- filter2(nbindata, fHigh)
      
      image(1:nrow(nbindata),1:ncol(nbindata),nbindata,col=grey(0:255/255))
    }
    
    cat( round(voxelCoord[imgn,1],0) , round(voxelCoord[imgn,2]) ,round(voxelCoord[imgn,3],0),annotations[imageline,5],"\n")
    
    #patch <- bindata[x1:x2, y1:y2, z]
    
    #patch <- normalizePlanes2(patch)
    
    #image(1:nrow(patch),1:ncol(patch),patch,col=grey(0:64/64))
    
    rect(x1,y1, x2,y2,border="red")
    
  }
  
}


manipulate(viewer(imageline), imageline = slider(1, nrow(annotations), step = 1) )





#readtext.filename <- file('https://www.dropbox.com/s/raa4vg2mp3klnan/1.3.6.1.4.1.14519.5.2.1.6279.6001.100621383016233746780170740405.mhd?dl=1',"rt")
#textdata <- readLines(readtext.filename)
#close(readtext.filename)

#readbin.filename  <- file('https://www.dropbox.com/s/8ko2q978u0u4944/1.3.6.1.4.1.14519.5.2.1.6279.6001.100621383016233746780170740405.raw?dl=1',"rb")
#bindata  <- readBin(readbin.filename, integer(),  size=2,n = 84148224)
#close(readbin.filename)


