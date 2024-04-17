
library(mxnet)
library(data.table)
library(ROCR)
library(oro.dicom)
library(data.table)
library(dplyr)
library(manipulate)
library(fslr)
library(EBImage)
library(abind)
library(biganalytics)

# Rotate Matrix 90 degrees
rotate <- function(x) t(apply(x, 2, rev))

mydist <- function(cx,cy,x,y){
  
  return(sqrt((cx-x)^2+(cy-y)^2 ))
}

Rcpp::sourceCpp('SearchPatches.cpp')
Rcpp::sourceCpp('../blobsearch/quickblob.cpp')

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

prefix <- '20170312xLUNAmodel-ver02-25'
iteration <- 25

model <- mx.model.load(prefix,iteration)

submissionfiles <- dir("D:/submission") %>% data.table
colnames(submissionfiles) <- "id"


for(dicomImage in 3:3){
  
  dicom    <- readDICOM(path = paste0("D:/submission/",submissionfiles[dicomImage]$id), verbose=TRUE)
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
    
    nimage <- normalizePlanes(dcmImage[,,i,1])
    imageList[[i]] <- resize(nimage,new_real_shape[1],new_real_shape[1])
  }  
  
  newImage <- abind( imageList, along=3 )
  imageList <- list()
  
  for( i in 1:dim(newImage)[2]){
    imageList[[i]] <- resize(newImage[,i,],new_real_shape[2],new_real_shape[3])
  }
  
  newImage2 <- abind(imageList, along=3)
  imageList <- list()
  
  for( i in 1:dim(newImage2)[2]){
    
    imageList[[i]] <- newImage2[,i,]
  }
  newImage3 <- abind(imageList, along=3)
  
  rm(newImage,newImage2)
  
  sliceNumber <- 50
  patchdata <- data.frame()
  probabilities <- list()
  probIndex <- 1
  
  tic <- proc.time()
  
  for(sliceNumber in 1:dim(newImage3)[3]){
    
    #image(1:nrow(newImage3[,,sliceNumber]),1:ncol(newImage3[,,sliceNumber]),newImage3[,,sliceNumber],col=gray(0:64/64))
    
    sdata <- scale(as.vector(t(newImage3[,,sliceNumber])))
    
    set.seed(11)
    clusters <- bigkmeans(sdata, 2, iter.max = 1000)
    cimage   <- matrix(clusters$cluster,nrow=dim(newImage3)[1],ncol=dim(newImage3)[2],byrow=T)
    
    #image(1:nrow(cimage),1:ncol(cimage),cimage,col=gray(0:64/64))
    cimage[cimage == 1] <- 0
    cimage[cimage == 2] <- 1
    
    kern   <- makeBrush(7, shape='diamond')
    cimage <- erode(cimage, kern)
    cimage <- dilate(cimage, kern)
    
    #image(1:nrow(cimage),1:ncol(cimage),cimage,col=gray(0:64/64))
    #image(1:nrow(newImage3[,,sliceNumber]),1:ncol(newImage3[,,sliceNumber]),newImage3[,,sliceNumber],col=gray(0:64/64))
    
    b <- blobsearch(cimage , b=0, minsize=1681)
    b <- b[order(b[,7],decreasing=T),]
    
    if (class(b) =="numeric"){
      b <- matrix(b,nrow=1,ncol=8) 
    }
    
    b <- cbind(b,rep(0,nrow(b)))
    
    if (nrow(b) > 1){
      cb <- b
      b  <- cb[1,]
      for(i in 1:I(nrow(cb)-1)){
        
        for(j in 2:nrow(cb)){
          
          if ( (cb[j,2] <= cb[i,2]) &
               (cb[j,1] <= cb[i,1]) &
               (cb[j,4] >= cb[i,4]) &
               (cb[j,3] >= cb[i,3]) ) {
            
            cb[j,9] <- 1
            
          } else {
            if (cb[j,9] != 1){
              b <- rbind(b,cb[j,])
            }
          }
        }
      }
      
      b <- unique(b[,1:8])
      b <- b[order(b[,7],decreasing=T),]
    }
    
    ImagePatches <- data.frame()
    
    for(i in 1:nrow(b)){
      
      #cat(mydist(dim(cimage)[1]/2, dim(cimage)[2]/2, b[i,5], b[i,6]),"\n")
      
      if ( mydist(dim(cimage)[1]/2, dim(cimage)[2]/2, b[i,5], b[i,6]) <= 120 & b[i,2] != 0 & b[i,1] != 0) {
        
        #rect(b[i,2],b[i,1],b[i,4],b[i,3],border="red")
        
        offset1 <-  b[i,1] - 10 
        if (offset1 >= 0) {
          offset1 <- -10
        }
        else {
          offset1 <- b[i,1]
        }
        
        offset2 <-  b[i,2] - 10 
        if (offset2 >= 0) {
          offset2 <- -10
        }
        else {
          offset2 <- b[i,2]
        }
        
        offset3 <-  b[i,3] + 10 
        if (offset3 >= dim(cimage)[1]) {
          offset3 <- 0
        }
        else {
          offset3 <- 10
        }
        
        offset4 <-  b[i,4] + 10 
        if (offset4 >= dim(cimage)[2]) {
          offset4 <- 0
        }
        else {
          offset4 <- 10
        }
        
        cPatch <- data.frame(V1=b[i,1]+offset1,V2=b[i,2]+offset2,V3=b[i,3]+offset3,V4=b[i,4]+offset4)
        
        ImagePatches <- rbind(ImagePatches,cPatch)
        
        #rect(cPatch$V2,cPatch$V1,cPatch$V4,cPatch$V3,border="red")
        
      }
    }
    
    if (nrow(ImagePatches) > 0){
      
      patchList <- list()
      patchsize <- 41
      stride    <- 10
      
      for(i in 1:nrow(ImagePatches)){
        
        img <- newImage3[ImagePatches$V2[i]:ImagePatches$V4[i],ImagePatches$V1[i]:ImagePatches$V3[i],sliceNumber]
        #image(1:nrow(img),1:ncol(img),img,col=grey(0:64/64))
        #dim(img) <- c(nrow(img),ncol(img))
        
        patchList[[i]] <- searchPatches(img,patchsize,stride)
      }
      
      patchArray <- abind(patchList, along=3)
      
      test.array <- patchArray
      
      if (dim(patchArray)[3] >=1){
        
        dim(test.array) <- c(41, 41, 1, dim(patchArray)[3])
        
        preds       <- predict(model, test.array)
        tpreds      <- t(preds)
        pred.label  <- ifelse(tpreds[,2] >= 0.95, 1,0)
        spred.label <- sum(pred.label == 1)
        
        if (spred.label > 0){
          
          pred.label.log <- as.logical(pred.label)
          
          candidates      <- patchArray[,,pred.label.log]
          candidatesProbs <- tpreds[pred.label.log,] 
          
          zdim <- table(pred.label.log)["TRUE"]
          
          if (!is.na(zdim)) {
            if (zdim > 1){ 
              dim(candidates) <- c(dim(candidates)[1]*dim(candidates)[2],dim(candidates)[3])
            } else {
              dim(candidates) <- c(dim(candidates)[1]*dim(candidates)[2],1)
            }   
            candidates <- t(candidates)
            patchdata <- rbind(patchdata,data.frame(candidates))
            
            probabilities[[probIndex]] <- candidatesProbs
            probIndex <- probIndex +1
          }
        }
      }
    }
  }
  print(proc.time() - tic)
  patchdata <- unique(patchdata)
  mprobs    <- matrix(unlist(probabilities),ncol=2,byrow=T)
  
  submissionResult <- cbind(mprobs,patchdata[,-1])
  
  write.table(submissionResult,paste0("output/Submission/",submissionfiles[dicomImage]$id,".csv"),row.names = F,col.names=F,sep=",")
  gc()
}

########################################################################################

candidates <- data.matrix(patchdata[,-1])

viewer <- function(base){
  m <- matrix(candidates[base,],ncol=41,byrow = F)
  image(1:41,1:41,m,col=gray(0:64/64))
}
manipulate(viewer(base), base = slider(1, dim(candidates)[1], step = 1))

