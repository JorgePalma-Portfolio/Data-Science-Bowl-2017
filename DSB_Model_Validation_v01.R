
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


# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

Rcpp::sourceCpp('CPPFunctions.cpp')

normalizePlanes <- function (npzarray){
  
  maxHU <- 400.00
  minHU <- -1000.00
  
  npzarray <-  (npzarray - minHU) / (maxHU - minHU)
  npzarray[npzarray > 1] <- 1.00
  npzarray[npzarray < 0] <- 0.00
  
  return(npzarray)
}

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom <- readDICOM(path = paste0("input/images/",Result[1]$id), verbose=TRUE)

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


image(newImage3[,,1],col=gray(0:64/64))

patchList <- searchPatches(newImage3[,,1],41,1)



#ix <- apply(patchList, 3, function(x) sd(x) != 0) 
#newPatchList <- patchList[,,ix]
#dim(newPatchList)

#prefix <- "Models/LUNAmodel-ver01-29"
#iteration <- 29

#prefix <- "LUNAmodel-ver02-19"
#iteration <- 19

prefix <- 'LUNAmodel-ver02-18'
iteration <- 18


model <- mx.model.load(prefix,iteration)

test.array <- patchList
dim(test.array) <- c(41, 41, 1, dim(patchList)[3])

preds <- predict(model, test.array)
pred.label <- max.col(t(preds)) - 1

Vista <- t(preds)

pred.label.log <- as.logical(pred.label)


candidates <- patchList[,,pred.label.log]
dim(candidates)

dim(candidates) <- c(dim(candidates)[1]*dim(candidates)[2],dim(candidates)[3])
dim(candidates)
candidates <- t(candidates)
dim(candidates)

patchdata <- data.frame(class=rep(0,dim(candidates)[1]),candidates)

#write.table(patchdata,"output/KAGGLE/class0-v2.csv",row.names = F,col.names=F,sep=",")


viewer <- function(base){
  m <- matrix(candidates[base,],ncol=41,byrow = F)
  image(1:41,1:41,m,col=gray(0:64/64))
}
manipulate(viewer(base), base = slider(1, dim(candidates)[1], step = 1))


image(1:41,1:41,test.array[,,1,3249],col=gray(0:64/64))
