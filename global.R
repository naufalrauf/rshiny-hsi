#Shiny global environment development for Hyperspectral Image Processing
#naufalrauf@yahoo.co.id
#Developed by Rauf 13 June 2017
#Updated by Rauf 11 June 2018
library (EBImage)
library (abind)
library (jpeg)
library (tcltk)
library (shiny)

xa <- c((1:1040)*(600/1040)+400)
j <- c(1:1040)
k <- c(1:1040)
n <- c(1:1040)
m <- c(1:1040)

#Function for loading database
dbl <- function(){
  #Name of database when its been created was dbhsi
  #<<- use for assign global environment in shiny
  filed <<- tk_choose.files(caption = "Select HSI Database")
  load(file = filed)
  dbhsi <<- dbhsi
}

#Function for showing and saving image based on wavelength value input
fwl <- function(wln){
  
  h <- (-0.0002*wln*wln) + (1.9303*wln) - 732.12
  bmp <- dbhsi[1:1392, 1:ncol(dbhsi), h]
  savetarget <- sprintf('/Users/ampsd/Documents/R/App-01/www/result%i.jpeg', wln)
  writeImage(bmp, savetarget, quality=100)
  setwd(dir = "/Users/ampsd/Documents/R/App-01/www")
  
}

dwsp <- function(x1, y1, x2, y2){
  
  
  for (i in m){
    
    n[i] <- sum(dbhsi[x1:x2, y1:y2, i]) 
  } 
  
  plot(m,n,"l")
  
}
  
mosc <- function(wln){
  msc <- sprintf('/Users/ampsd/Documents/R/App-01/www/result%i.jpeg', wln)
  mscimg <- readImage(msc)
}  

saveph <- function(){
  sph550 <- readImage("/Users/ampsd/Documents/R/App-01/www/result550.jpeg")
  sph680 <- readImage("/Users/ampsd/Documents/R/App-01/www/result680.jpeg")
  sph750 <- readImage("/Users/ampsd/Documents/R/App-01/www/result750.jpeg")
  
  sphfolnm <- unlist(strsplit(filed,"/"))
  sphfl <- sphfolnm[length(sphfolnm)]
  sphfl2 <- substr(sphfl, 6, 10)
  writeImage(sph550, sprintf('/Users/ampsd/Documents/Rauf (Master Thesis)/Research Pictures/550/%s result550.jpeg', sphfl2), quality = 100)
  writeImage(sph680, sprintf('/Users/ampsd/Documents/Rauf (Master Thesis)/Research Pictures/680/%s result680.jpeg', sphfl2), quality = 100)
  writeImage(sph750, sprintf('/Users/ampsd/Documents/Rauf (Master Thesis)/Research Pictures/750/%s result750.jpeg', sphfl2), quality = 100)
}

#Function for image correction
corrc <- function(wln){
  h <- (-0.0002*wln*wln) + (1.9303*wln) - 732.12
  bmp <- dbhsi[1:1392, 1:ncol(dbhsi), h]
  #Slope
  #d1 <- dbhsi[1:1392, 1:ncol(dbhsi), 190]
  #d2 <- dbhsi[1:1392, 1:ncol(dbhsi), 161]
  #cimg <- 1/(bmp/(d1-d2))
  
  #Ratio 550/670
  #hlratio <- dbhsi[1:1392, 1:ncol(dbhsi), 260]/dbhsi[1:1392, 1:ncol(dbhsi), 468]
  
  
  #Average previous one is 139:174, 161:190
  #untuk leaf black sheet bilangan pengali = 0.025
  hsiavg <- apply(dbhsi[1:1392, 1:ncol(dbhsi), 1:43], 1:2, mean)
  cimg <- 0.005*bmp/hsiavg
  savetarget <- sprintf('/Users/ampsd/Documents/R/App-01/www/corrected%i.jpeg', wln)
  writeImage(cimg, savetarget, quality=100)
  
  
}

pseudo <- function(wln){
  h <- (-0.0002*wln*wln) + (1.9303*wln) - 732.12
  bmp <- sprintf('/Users/ampsd/Documents/R/App-01/www/corrected%i.jpeg', wln)
  pal <- colorRampPalette(c("darkblue", "skyblue", "yellow", "red", "darkred"),
                          bias=1)(max(brightness)+1)
  psdimg <- image(bmp, col = pal)
}

#Making NDVI image
genndvi <- function()
ndvi.img <- ((dbhsi[1:1392, 1:ncol(dbhsi), 693]/((crn[1424*xa[693]/1040])/717)) - (dbhsi[1:1392, 1:ncol(dbhsi), 468]/((crn[1424*xa[468]/1040])/717))) / ((dbhsi[1:1392, 1:ncol(dbhsi), 693]/((crn[1424*xa[693]/1040])/717)) + (dbhsi[1:1392, 1:ncol(dbhsi), 468]/((crn[1424*xa[468]/1040])/717))) - 0.1 #NDVI
savetarget <- ('/Users/ampsd/Documents/R/App-01/www/correctedndvi.jpeg')
writeImage(ndvi.img, savetarget, quality=100)
