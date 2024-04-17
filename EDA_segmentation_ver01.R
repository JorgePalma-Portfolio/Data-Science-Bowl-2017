
library(oro.dicom)
library(data.table)
library(dplyr)
library(manipulate)
library(EBImage)
library(ade4)

Rcpp::sourceCpp('~/Rprojects/Rcpp/canny.cpp')
Rcpp::sourceCpp('~/Rprojects/Rcpp/ISEF.cpp')
Rcpp::sourceCpp('CPPFunctions.cpp')

labels <- fread("input/stage1_labels.csv") %>% data.table

samplefiles <- dir("input/images") %>% data.table

colnames(samplefiles) <- "id"

setkey(labels,id)
setkey(samplefiles,id)

Result <- samplefiles[labels, nomatch=0]

dicom <- readDICOM(path = paste0("input/images/",Result[7]$id), verbose=TRUE)

dcmImage <-  create4D(dicom,sequence = TRUE)

graphics::image(dcmImage[,,18,1], col=grey(0:64/64), axes=FALSE)

nimg <- Canny(dcmImage[,,18,1],1)

nimg <- ISEF(dcmImage[,,18,1])

graphics::image(nimg, col=grey(0:64/64), axes=FALSE)

r <- dcmImage[,,18,1] - nimg

graphics::image(r, col=grey(0:64/64), axes=FALSE)


nimg <- max(dcmImage[,,18,1]) - dcmImage[,,18,1]
graphics::image(nimg, col=grey(0:64/64), axes=FALSE)

graphics::image(dcmImage[,,18,1], col=grey(0:64/64), axes=FALSE)
#***
nuc <- dcmImage[,,18,1]
nmask = thresh(nuc, w=4, h=4, offset=0.05)
nmask = opening(nmask, makeBrush(5, shape='disc'))
graphics::image(nmask, col=grey(0:64/64), axes=FALSE)
#***

r <- dcmImage[,,18,1] * nmask
graphics::image(r, col=grey(0:64/64), axes=FALSE)

nmask = fillHull(nmask)
nmask = bwlabel(nmask)

graphics::image(nmask, col=grey(0:64/64), axes=FALSE)

r <- dcmImage[,,18,1] * nmask

graphics::image(r, col=grey(0:64/64), axes=FALSE)

cel <- dcmImage[,,18,1]
ctmask = opening(cel>0.1, makeBrush(5, shape='disc'))
cmask = propagate(cel, seeds=nmask, mask=ctmask)

graphics::image(cmask, col=grey(0:64/64), axes=FALSE)

segmented = paintObjects(cmask, cells, col='#ff00ff')
segmented = paintObjects(nmask, segmented, col='#ffff00')


r <- (dcmImage[,,10] + nmask)/2

graphics::image(r, col=grey(0:64/64), axes=FALSE)



viewer <- function(base){
  
 graphics::image(dcmImage[,,base], col=grey(0:255/255), axes=FALSE)
  
}

manipulate(viewer(base), base = slider(1,203, step =1 ,initial=203))


par(mfrow = c(4, 4))

for(base in 17:32){
  
  
  graphics::image(dcmImage[base,,], col=grey(0:255/255), axes=FALSE)
  
}


