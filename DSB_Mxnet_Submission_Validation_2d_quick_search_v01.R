
library(mxnet)
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

prefix <- '20170408xLUNAmodel-ver02-30-step3-cpu'
iteration <- 30

model <- mx.model.load(prefix,iteration)

#submissionfiles <- dir("submission") %>% data.table
submissionfiles <- dir("F:/stage2") %>% data.table
colnames(submissionfiles) <- "id"

dicomImage <- 4

#1:83

# 2 has a problem

for(dicomImage in c(288:288)){
  
  #dicom    <- readDICOM(path = paste0("submission/",submissionfiles[dicomImage]$id), verbose=TRUE)
  dicom    <- readDICOM(path = paste0("F:/stage2/",submissionfiles[dicomImage]$id), verbose=TRUE)
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
  
  sliceNumber <- 50
  patchdata <- data.frame()
  probabilities <- list()
  probIndex <- 1
  
  tic <- proc.time()
  
  sliceNumber <- 100
  
  for(sliceNumber in 1:dim(newImage)[3]){
    
    #image(1:nrow(newImage[,,sliceNumber]),1:ncol(newImage[,,sliceNumber]),newImage[,,sliceNumber],col=gray(0:64/64))
    
    sdata <- scale(as.vector(t(newImage[,,sliceNumber])))
    
    set.seed(11)
    clusters <- bigkmeans(sdata, 2, iter.max = 1000)
    cimage   <- matrix(clusters$cluster,nrow=dim(newImage)[1],ncol=dim(newImage)[2],byrow=T)
    
    #image(1:nrow(cimage),1:ncol(cimage),cimage,col=gray(0:64/64))
    cimage[cimage == 1] <- 0
    cimage[cimage == 2] <- 1
    
    kern   <- makeBrush(7, shape='diamond')
    cimage <- EBImage::erode(cimage, kern)
    cimage <- EBImage::dilate(cimage, kern)
    
    #image(1:nrow(cimage),1:ncol(cimage),cimage,col=gray(0:64/64))
    #image(1:nrow(newImage[,,sliceNumber]),1:ncol(newImage[,,sliceNumber]),newImage[,,sliceNumber],col=gray(0:64/64))
    
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
      
      if (class(b) =="numeric"){
        b <- matrix(b,nrow=1,ncol=9) 
      }
      
      if (nrow(b) > 1){
         b <- unique(b[,1:8])
         b <- b[order(b[,7],decreasing=T),]
      }
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
        
        img <- newImage[ImagePatches$V2[i]:ImagePatches$V4[i],ImagePatches$V1[i]:ImagePatches$V3[i],sliceNumber]
        #image(1:nrow(img),1:ncol(img),img,col=grey(0:64/64))
        #dim(img) <- c(nrow(img),ncol(img))
        
        patchList[[i]] <- searchPatches(img,patchsize,stride)

        cleanlist <- apply(patchList[[i]],3,function(x) !all(x==0))
        patchList[[i]] <- patchList[[i]][,,cleanlist]
      }
      
      patchArray <- abind(patchList, along=3)
      
      test.array <- patchArray
      
      if (dim(patchArray)[3] >=1){
        
        dim(test.array) <- c(41, 41, 1, dim(patchArray)[3])
        
        #print(system.time(preds       <- predict(model, test.array)))
        #print(dim(patchArray)[3])
        
        preds       <- predict(model, test.array)
        
        tpreds      <- t(preds)
        
        tpreds[is.na(tpreds)] <- 0
        
        pred.label  <- ifelse(tpreds[,2] >= 0.30, 1,0)
        spred.label <- sum(pred.label == 1,na.omit=T)
        
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
            
            if (class(candidatesProbs) =="numeric"){
              if (max(candidatesProbs) >= 0.999){
                break
              }
            } else {
              if (max(candidatesProbs[,2]) >= 0.999){
                break
              }
              
            }
            
           
          }
          
        }
        
      }
      
    }
    
  }
  
  print(proc.time() - tic)
  
  if (!(is.data.frame(patchdata) & nrow(patchdata)==0)){
    
    mprobs <- abind(probabilities, along=1)
    submissionResult <- data.frame(mprobs,patchdata)
  } else {
    
    submissionResult <- t(rep(0,1683))
  }
  submissionResult <- submissionResult[which.max(submissionResult$X2.1),]
  write.table(submissionResult,paste0("output/FinalSubmission/",submissionfiles[dicomImage]$id,".csv"),row.names = F,col.names=F,sep=",")
  gc()
}