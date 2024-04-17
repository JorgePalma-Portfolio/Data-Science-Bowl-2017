

load(system.file("hk-40/hk40.RData", package="oro.dicom"))
dcmList <- hk40
dcmImage <- create3D(dcmList)
image(dcmImage[,,1], col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
      main=paste("First Slice from HK-40"))



imagePositionPatient <- attributes(dcmImage)$ipp
dSL <- abs(diff(imagePositionPatient[,3]))


plot(dSL, ylim=range(range(dSL) * 1.5, 0, 10), xlab="Image", ylab="mm",
     main="Difference in Slice Location")


dcmList <- readDICOM(system.file("hk-40", package="oro.dicom"),
                     pixelData=FALSE)


dcmImage <- create3D(dicom, pixelData=FALSE)
image(dcmImage[,,1], col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
      main=paste("First Slice from HK-40 (again)"))
