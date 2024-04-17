
library(oro.dicom)
library(data.table)
library(dplyr)
library(EBImage)
library(biganalytics)
library(manipulate)
library(caret)

Rcpp::sourceCpp('CPPFunctions.cpp')
Rcpp::sourceCpp('../blobsearch/quickblob.cpp')

mydist <- function(x,y){
  
  return(sqrt((256-x)^2+(256-y)^2 ))
}

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

FileResult <- samplefiles[labels, nomatch=0]

masklist <- list()

fileNumber <- 8

#nrow(FileResult)

for(fileNumber in 8:8){
  
  dicom <- readDICOM(path = paste0("input/images/",FileResult[fileNumber]$id), verbose=FALSE)
  
  RescaleSlope     <- extractHeader(dicom$hdr, "RescaleSlope") 
  RescaleIntercept <- extractHeader(dicom$hdr, "RescaleIntercept")
  PixelSpacing     <-  extractHeader(dicom$hdr, "PixelSpacing",numeric = FALSE)[1]
  ImagePositionPatient <-  extractHeader(dicom$hdr, "ImagePositionPatient",numeric = FALSE)
  
  
  
  zpos <- abs(as.numeric(unlist(strsplit(ImagePositionPatient, " "))[3]))
  
  PhotometricInterpretation <- extractHeader(dicom$hdr, "PhotometricInterpretation",numeric = FALSE)[1]
 
  dcmImage <-  create4D(dicom, sequence = TRUE)
  
  dcmImage <-  RescaleSlope * dcmImage

  imagenbr <- 1
  
  dcmImage <-  dcmImage + RescaleIntercept
  hist(dcmImage[,,imagenbr,1])
  image(1:512,1:512,dcmImage[,,imagenbr,1],col=gray(0:64/64))
 
#########################################################  
  
  for(imagenbr in 1:dim(dcmImage)[3]){
    
    set.seed(1)
    img <- dcmImage[,,imagenbr,1]
    
    data <- computeDataset(img)
    
    data[data[,3] == min(data[,3])] <- 0
    
    sdata <- scale(data[,3])
    
    clusters <- bigkmeans(sdata, 2, iter.max = 1000)
    
    cimage <- matrix(clusters$cluster,nrow=512,ncol=512,byrow=T)
    
    h <- hist(cimage,plot=F)

    if (h$density[1] < h$density[length(h$density)]){
      cimage[cimage == 2] <- 0
    } else {
      cimage[cimage == 1] <- 0
      cimage[cimage == 2] <- 1
    }
    # 
    #image(1:512,1:512,cimage,col=gray(0:64/64))
    #newimage <- as.vector(resize(cimage,100,100))
    
    kern = makeBrush(7, shape='diamond')
    cimage= erode(cimage, kern)
    #cimage= erode(cimage, kern)
    cimage = dilate(cimage, kern)
    #cimage = dilate(cimage, kern)
    
    masklist[[imagenbr]] <- cimage
    #    image(1:512,1:512,cimage,col=gray(0:64/64))
  }
}

imagenbr <- 18

maskmatrix <- array(unlist(masklist),c(512,512,134))
image(1:512,1:512,maskmatrix[,,imagenbr],col=gray(0:64/64))
b <- blobsearch(maskmatrix[,,imagenbr], b=0, minsize=1000)
b <- b[order(b[,7],decreasing=T),]

for(i in 1:nrow(b)){
  if ( (b[i,7] > 3000) & (b[i,7] < 50000) & (b[i,8] = 255)){
    cat(mydist(b[i,5],b[i,6]),"\n")
    if ( mydist(b[i,5],b[i,6]) <= 140 ) {
      rect(b[i,2],b[i,1],b[i,4],b[i,3],border="red")
    }
  }
}



maskmatrix <- array(unlist(masklist),c(512,512,dim(dcmImage)[3]))

viewer <- function(base){

  image(1:512,1:512,maskmatrix[,,base],col=gray(0:64/64))
  b <- blobsearch(maskmatrix[,,base], b=0)
  b <- b[order(b[,7],decreasing=T),]
  
  for(i in 1:nrow(b)){
    if ( (b[i,7] > 3000) & (b[i,7] < 100000) ) {  # & (b[i,8] = 255)){
      #cat(mydist(b[i,5],b[i,6]),"\n")
      if ( mydist(b[i,5],b[i,6]) <= 200 ) {
        rect(b[i,2],b[i,1],b[i,4],b[i,3],border="red")
      }
    }
  }
  
}
manipulate(viewer(base), base = slider(1, dim(dcmImage)[3], step = 1))






maskmatrix <- array(unlist(masklist),c(512,512,dim(dcmImage)[3]))

viewer1 <- function(base,ic){
  
  nm <- dcmImage[,,base,1]
 
  for(i in 1:I(ic-1)) {
   nm <- nm + dcmImage[,,I(base+i),1] 
  }
  
  nm <- nm / ic
  
  image(1:512,1:512,nm,col=gray(0:64/64))

}
manipulate(viewer1(base,ic), base = slider(1, dim(dcmImage)[3], step = 2), ic = slider(2,40,step=1))




