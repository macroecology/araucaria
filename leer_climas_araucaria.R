library(sp)
library(raster)
library (dismo)
library (maptools)
library (rgdal)
library (terrain)

setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
datos<- read.table ("Fossil_pollen_data_Sara2.csv", sep=",", header=T)
rasterhead (datos)


setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria\\datos")
gtopo<- raster ("gt30w060s10.tif")
setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
gtopo
plot (gtopo)
slope<- terrain(gtopo, opt='tangent', unit='radians', neighbors=8) 
plot (slope)
aspect<- terrain(gtopo, opt='aspect', unit='degrees', neighbors=8) 
plot (aspect)
south<- reclassify (aspect, c(0, 112.5, 0, 112.5, 247.5, 1, 247.5, 500, 0))

plot (south, xlim=c(-57, -40), ylim=c(-35, -15))
points (pres_ara_lgm, col="blue", pch=16)
points (pres_ara_holo, col="green", pch=16)
points (coord, col="red", pch=16)

orien_lgm<- extract (aspect, pres_ara_lgm)
sum (orien_lgm >112.5 & orien_lgm<= 247.5)/length (orien_lgm)

orien_lgm<- extract (aspect, pres_ara_holo)
sum (orien_lgm >112.5 & orien_lgm<= 247.5)/length (orien_lgm)

orien_lgm<- extract (aspect, coord)
sum (orien_lgm >112.5 & orien_lgm<= 247.5)/length (orien_lgm)






pres_ara_lgm<- datos [datos$lgm==1 & datos$arauc==1, 2:3]
aus_ara_lgm<-datos [datos$lgm==1 & datos$arauc==0, 2:3] 

pres_ara_holo<- datos [datos$holo==1 & datos$arauc==1, 2:3]
aus_ara_holo<- datos [datos$holo==1 & datos$arauc==0, 2:3]

plot (pres_ara_lgm, col="blue", pch=16, xlim=c(-70, -20), ylim=c(-40, 0))
points (aus_ara_lgm, col="red", pch=16)

points (aus_ara_holo, col="coral", pch=16)
points (pres_ara_holo, col="green", pch=16)

points (pres_ara_lgm, col="blue", pch=16)


data (wrld_simpl)

plot (pres[[1]], xlim=c(-62, -35), ylim=c(-35, -10))
plot (wrld_simpl, add=T)

points (aus_ara_lgm, col="red", pch=16)
points (aus_ara_holo, col="coral", pch=16)
points (pres_ara_holo, col="green", pch=16)
points (pres_ara_lgm, col="blue", pch=16)


setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")


plot (frio[, 1:2], xlim=c(12, 25))
points (medio [, 1:2], col=2)

library (rgdal)
dat<- read.table ("present_data.csv", sep=",", header=T)
dat

coord<- data.frame (x=dat$Longitude_X, y=dat$Latitude_Y)


## read polygon present
require(rgdal)
library (raster)
araucaria <- readOGR("Mata_de_Araucaria.kml", "Araucaria")

### sample absence data for the present scenario
kk<-  pres[[1]]>0
plot (kk)
kkk<- rasterToPolygons(kk, fun=NULL, dissolve=TRUE)
plot (kkk)
outs<- kkk- araucaria
plot (outs)

library (sp)
abs_pres<- spsample(outs, 100, type="regular")
plot (abs_pres, add=T)


plot (araucaria, xlim=c(-55, -40), ylim=c(-30, -20))
points (coord)

setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\estudiantes\\julia_TFG")

modelos<- c("CCSM", "CNRM", "FGOALS", "GISS", "IPSL", "MIROC", "MRI")

#### calibration with JUST PRESENT DISTRIBUTION


for (i in 1:6){
print (modelos [i])
setwd ("C:/Users/sara.varela/Documents/CIENCIAS/ecoClimate/baseline_1950_futuro") 
climas<- list.files ()
presente <- read.table(climas [i], header = TRUE)
holo <- read.table(climas [i+6], header = TRUE)
lgm <- read.table(climas [i+12], header = TRUE)

gridded(presente) <- ~long+lat
gridded(holo) <- ~long+lat
gridded(lgm) <- ~long+lat

pres <- stack(presente)
hol <- stack(holo)
lgm21<- stack (lgm)

pres<- pres [[-1]]
hol<- hol [[-1]]
lgm21<- lgm21 [[-1]]

crs(pres) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(hol) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(lgm21) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

e<- extent(-60, -40,-40, -10)

pres<- crop (pres, e)
hol<- crop (hol, e)
lgm21<- crop (lgm21, e)

modelo_bioclim<- bioclim (pres, coord)
pred<- predict (modelo_bioclim, pres)
pred_holo<- predict (modelo_bioclim, hol)
pred_lgm<- predict (modelo_bioclim, lgm21)


modelo_maxent<- maxent (pres, coord, b=2)

pred_max<- predict (modelo_maxent, pres)
pred_holo_max<- predict (modelo_maxent, hol)
pred_lgm_max<- predict (modelo_maxent, lgm21)

setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")

pdf (paste (modelos [i], "map.pdf", sep=""))
par (mfrow =c(2,3))
plot (pred,  main= paste (modelos [i], "_present_bioclim"), xlim=c(-70, -40), ylim=c(-40, -10))
plot (wrld_simpl, add=T)
points (coord, pch=16, cex=0.2)

plot (pred_holo,  main= paste (modelos [i], "_holocene"), xlim=c(-70, -40), ylim=c(-40, -10))
plot (wrld_simpl, add=T)
points (pres_ara_holo, pch=16)
points (aus_ara_holo, col="coral", pch=16)

plot (pred_lgm,  main= paste (modelos [i], "_LGM"), xlim=c(-70, -40), ylim=c(-40, -10))
plot (wrld_simpl, add=T)
points (pres_ara_lgm, pch=16)
points (aus_ara_lgm, col="red", pch=16)


plot (pred_max,  main= paste (modelos [i], "_present_maxent"), xlim=c(-70, -40), ylim=c(-40, -10))
plot (wrld_simpl, add=T)
points (coord, pch=16, cex=0.2)

plot (pred_holo_max,  main= paste (modelos [i], "_holocene"), xlim=c(-70, -40), ylim=c(-40, -10))
plot (wrld_simpl, add=T)
points (pres_ara_holo, pch=16)
points (aus_ara_holo, col="coral", pch=16)

plot (pred_lgm_max,  main= paste (modelos [i], "_LGM"), xlim=c(-70, -40), ylim=c(-40, -10))
plot (wrld_simpl, add=T)
points (pres_ara_lgm, pch=16)
points (aus_ara_lgm, col="red", pch=16)

dev.off()





extract (pred_lgm, pres_ara_lgm)
extract (pred_holo, pres_ara_holo)

areas<- area(pred)
area_pres<- sum (areas[pred>0])
area_hol<- sum (areas[pred_holo>0])
area_lgm<- sum (areas[pred_lgm>0])


area_pres_max<- sum (areas[pred_max>0.05])
area_hol_max<<- sum (areas[pred_holo_max>0.05])
area_lgm_max<<- sum (areas[pred_lgm_max>0.05])

bio<- c(area_pres/area_pres, round (area_hol/area_pres, 2), round (area_lgm/area_pres, 2))
max<- c(area_pres_max/area_pres_max, area_hol_max/area_pres_max, area_lgm_max/area_pres_max)


pdf (paste (modelos [i], "area-suitability.pdf", sep=""))
par (mfrow =c(3,1))
boxplot (extract (pred_lgm_max, pres_ara_lgm), extract (pred_holo_max, pres_ara_holo),
         extract (pred_max, coord), col="grey", border="gray30",
         axes=F,  ylab="pred suitability", main=paste (modelos [i], "maxent"))
axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
axis (2)

boxplot (extract (pred_lgm, pres_ara_lgm), extract (pred_holo, pres_ara_holo),
         extract (pred, coord), col="grey", border="gray30",
         axes=F,  ylab="pred suitability", main= paste (modelos [i], "bioclim")) 
axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
axis (2)


plot (c(3:1), bio, type="o", ylim=c(0, 4), ylab="% change", axes=F, xlab="")
axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
axis (2)
points (c(3:1), max, type="o", ylim=c(0, 4), col="red")

dev.off()
}







summary (pres$bio.15@data@values)
#### calibration INTERTEMPORAL


for (i in 1:6){
  print (modelos [i])
  setwd ("C:/Users/sara.varela/Documents/CIENCIAS/ecoClimate/baseline_1950_futuro") 
  climas<- list.files ()
  presente <- read.table(climas [i], header = TRUE)
  holo <- read.table(climas [i+6], header = TRUE)
  lgm <- read.table(climas [i+12], header = TRUE)
  
  gridded(presente) <- ~long+lat
  gridded(holo) <- ~long+lat
  gridded(lgm) <- ~long+lat
  
  pres <- stack(presente)
  hol <- stack(holo)
  lgm21<- stack (lgm)
  
  pres<- pres [[-1]]
  hol<- hol [[-1]]
  lgm21<- lgm21 [[-1]]
  
  crs(pres) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(hol) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(lgm21) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  e<- extent(-70, -40,-40, -10)
  
  pres<- crop (pres, e)
  hol<- crop (hol, e)
  lgm21<- crop (lgm21, e)
  
  frio<- as.data.frame (extract (lgm21, pres_ara_lgm))
  medio<- as.data.frame (extract (hol, pres_ara_holo))
  presente<-  as.data.frame (extract (pres, araucaria))
  presente2<- as.data.frame (extract (pres, coord)) 
     
  datos<- rbind (frio, medio, presente, presente2)   
  
  pred<- pres$bio.1 > range (datos$bio.1) [1] &  pres$bio.1 <= range (datos$bio.1) [2] &
  pres$bio.12 > range (datos$bio.12) [1] &  pres$bio.12 <= range (datos$bio.12) [2] &
    pres$bio.11 > range (datos$bio.11) [1] &  pres$bio.11 <= range (datos$bio.11) [2] &
    pres$bio.18 > range (datos$bio.18) [1] &  pres$bio.18 <= range (datos$bio.18) [2] 
  


  pred_holo<- hol$bio.1 > range (datos$bio.1) [1] &  hol$bio.1 <= range (datos$bio.1) [2] &
    hol$bio.12 > range (datos$bio.12) [1] &  hol$bio.12 <= range (datos$bio.12) [2] &
    hol$bio.11 > range (datos$bio.11) [1] &  hol$bio.11 <= range (datos$bio.11) [2] &
    hol$bio.18 > range (datos$bio.18) [1] &  hol$bio.18 <= range (datos$bio.18) [2]  
  
  
  pred_lgm<- lgm21$bio.1 > range (datos$bio.1) [1] &  lgm21$bio.1 <= range (datos$bio.1) [2] &
    lgm21$bio.12 > range (datos$bio.12) [1] &  lgm21$bio.12 <= range (datos$bio.12) [2] &
    lgm21$bio.11 > range (datos$bio.11) [1] &  lgm21$bio.11 <= range (datos$bio.11) [2]  &
    lgm21$bio.18 > range (datos$bio.18) [1] &  lgm21$bio.18 <= range (datos$bio.18) [2] 
  
  
  pred<- mask (pred, sa)
  pred_holo<- mask (pred_holo, sa)
  pred_lgm<- mask (pred_lgm, sa)
  
  
  areas<- area(pred)
  area_pres<- sum (areas[pred>0])
  area_hol<- sum (areas[pred_holo>0])
  area_lgm<- sum (areas[pred_lgm>0])
  
  areas<- c(area_lgm,  area_hol,  area_pres)
  time<- c(1,2,3)
  poly.y<- c(0, areas, 0)
  poly.x<- c(1,time, 3)
  
  
setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")

pdf (paste (modelos [i], "bio1_11_12_18_map.pdf", sep=""))

par (mfrow=c(2, 2)) 
  
plot (pred_lgm, col=c("white", "darkolivegreen1"), legend=F, main=paste (modelos [i], "LGM"))
plot (sa, add=T)
plot (araucaria, add=T)
points (coord, pch=19, cex=0.5)
points (pres_ara_lgm, pch=16, cex=0.5, col="red")
points (aus_ara_lgm, pch=19, cex=0.5, col="blue")

plot (pred_holo, col=c("white", "darkolivegreen1"), legend=F, main="holocene")
plot (sa, add=T)
plot (araucaria, add=T)
points (coord, pch=16, cex=0.5)
points (pres_ara_holo, pch=19, cex=0.5, col="red")
points (aus_ara_holo, pch=19, cex=0.5, col="blue")

plot (pred, col=c("white", "darkolivegreen1"), legend=F, main= "present")
  plot (sa, add=T, border="gray30")
  plot (araucaria, add=T, border="gray30")
  points (coord, pch=19, col="gray30", cex=0.5)
  
plot (poly.x, poly.y, type="n", axes=F, ylab="Area (Km2)", xlab="")
polygon (poly.x, poly.y, density = NA,border = NA, col = "darkolivegreen1")
axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
axis (2)

dev.off()       
         
}


#### random forest and CART

library(rpart)
require(randomForest)

for (i in 1:6){
  print (modelos [i])
  setwd ("C:/Users/sara.varela/Documents/CIENCIAS/ecoClimate/baseline_1950_futuro") 
  climas<- list.files ()
  presente <- read.table(climas [i], header = TRUE)
  holo <- read.table(climas [i+6], header = TRUE)
  lgm <- read.table(climas [i+12], header = TRUE)
  
  gridded(presente) <- ~long+lat
  gridded(holo) <- ~long+lat
  gridded(lgm) <- ~long+lat
  
  pres <- stack(presente)
  hol <- stack(holo)
  lgm21<- stack (lgm)
  
  pres<- pres [[-1]]
  hol<- hol [[-1]]
  lgm21<- lgm21 [[-1]]
  
  crs(pres) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(hol) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(lgm21) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  e<- extent(-70, -40,-40, -10)
  
  pres<- crop (pres, e)
  hol<- crop (hol, e)
  lgm21<- crop (lgm21, e)
  
  frio<- as.data.frame (extract (lgm21, pres_ara_lgm))
  medio<- as.data.frame (extract (hol, pres_ara_holo))
  presente<-  as.data.frame (extract (pres, araucaria))
  presente2<- as.data.frame (extract (pres, coord)) 
  
  abs_p<- as.data.frame (extract (pres, abs_pres)) 
  
  datos<- rbind (frio, medio, presente, presente2)
  datos$araucaria<- 1
  abs_p$araucaria<- 0
  
  dat<- rbind (datos, abs_p)

  # create attractive postscript plot of tree 

  dat$araucaria <- as.factor (dat$araucaria)
  
  rf<- randomForest(araucaria  ~ ., data = dat, mtry=4,ntree=100)
 
  pred<- predict(pres, rf) 
  pred_holo<- predict(hol, rf) 
  pred_lgm<- predict(lgm21, rf) 
          
 
  pred<- mask (pred, sa)
  pred_holo<- mask (pred_holo, sa)
  pred_lgm<- mask (pred_lgm, sa)
  
  
  areas<- area(pred)
  area_pres<- sum (areas[pred>0])
  area_hol<- sum (areas[pred_holo>0])
  area_lgm<- sum (areas[pred_lgm>0])
  
  areas<- c(area_lgm,  area_hol,  area_pres)
  time<- c(1,2,3)
  poly.y<- c(0, areas, 0)
  poly.x<- c(1,time, 3)
  
  
  setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
  
  pdf (paste (modelos [i], "_RF.pdf", sep=""))
  
  par (mfrow=c(2, 2)) 
  
  plot (pred_lgm, col=c("white", "darkolivegreen1"), legend=F, main=paste (modelos [i], "LGM"))
  plot (sa, add=T)
  plot (araucaria, add=T)
  points (coord, pch=19, cex=0.5)
  points (pres_ara_lgm, pch=16, cex=0.5, col="red")
  points (aus_ara_lgm, pch=19, cex=0.5, col="blue")
  
  plot (pred_holo, col=c("white", "darkolivegreen1"), legend=F, main="holocene")
  plot (sa, add=T)
  plot (araucaria, add=T)
  points (coord, pch=16, cex=0.5)
  points (pres_ara_holo, pch=19, cex=0.5, col="red")
  points (aus_ara_holo, pch=19, cex=0.5, col="blue")
  
  plot (pred, col=c("white", "darkolivegreen1"), legend=F, main= "present")
  plot (sa, add=T, border="gray30")
  plot (araucaria, add=T, border="gray30")
  points (coord, pch=19, col="gray30", cex=0.5)
  
  plot (poly.x, poly.y, type="n", axes=F, ylab="Area (Km2)", xlab="")
  polygon (poly.x, poly.y, density = NA,border = NA, col = "darkolivegreen1")
  axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
  axis (2)
  
  dev.off()       
  
}


## LGM 

for (i in 1:6){
  print (modelos [i])
  setwd ("C:/Users/sara.varela/Documents/CIENCIAS/ecoClimate/baseline_1950_futuro") 
  climas<- list.files ()
  presente <- read.table(climas [i], header = TRUE)
  holo <- read.table(climas [i+6], header = TRUE)
  lgm <- read.table(climas [i+12], header = TRUE)
  
  gridded(presente) <- ~long+lat
  gridded(holo) <- ~long+lat
  gridded(lgm) <- ~long+lat
  
  pres <- stack(presente)
  hol <- stack(holo)
  lgm21<- stack (lgm)
  
  pres<- pres [[-1]]
  hol<- hol [[-1]]
  lgm21<- lgm21 [[-1]]
  
  crs(pres) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(hol) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(lgm21) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  e<- extent(-70, -40,-40, -10)
  
  pres<- crop (pres, e)
  hol<- crop (hol, e)
  lgm21<- crop (lgm21, e)
  
  frio<- as.data.frame (extract (lgm21, pres_ara_lgm))
  medio<- as.data.frame (extract (hol, pres_ara_holo))
  presente<-  as.data.frame (extract (pres, araucaria))
  presente2<- as.data.frame (extract (pres, coord)) 
  
  abs_p<- as.data.frame (extract (pres, abs_pres)) 
  
  datos<- rbind (frio, medio, presente, presente2)
  datos$araucaria<- 1
  abs_p$araucaria<- 0
  
  dat<- rbind (datos, abs_p)
  
  # create attractive postscript plot of tree 
  str (dat)
  
  fit<- step (glm (araucaria  ~ . , data = dat, family=binomial(link=logit)), trace=0)
  
  print (summary(fit)) # display results
  
  pred<- predict(pres, fit,  type="response") 
  pred_holo<- predict(hol, fit,  type="response")  
  pred_lgm<- predict(lgm21, fit,  type="response")  
  
  pred<- mask (pred, sa)
  pred_holo<- mask (pred_holo, sa)
  pred_lgm<- mask (pred_lgm, sa)
  
  

  areas<- area(pred)
  area_pres<- sum (areas[pred>min (extract (pred, coord))])
  area_hol<- sum (areas[pred_holo>min (extract (pred, coord))])
  area_lgm<- sum (areas[pred_lgm>min (extract (pred, coord))])
  
  areas<- c(area_lgm,  area_hol,  area_pres)
  time<- c(1,2,3)
  poly.y<- c(0, areas, 0)
  poly.x<- c(1,time, 3)
  
  
  setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
  
  pdf (paste (modelos [i], "_GLM.pdf", sep=""))
  
  par (mfrow=c(2, 2)) 
  
  plot (pred_lgm, legend=F, main=paste (modelos [i], "LGM"))
  plot (sa, add=T)
  plot (araucaria, add=T)
  points (coord, pch=19, cex=0.5)
  points (pres_ara_lgm, pch=16, cex=0.5, col="red")
  points (aus_ara_lgm, pch=19, cex=0.5, col="blue")
  
  plot (pred_holo, legend=F, main="holocene")
  plot (sa, add=T)
  plot (araucaria, add=T)
  points (coord, pch=16, cex=0.5)
  points (pres_ara_holo, pch=19, cex=0.5, col="red")
  points (aus_ara_holo, pch=19, cex=0.5, col="blue")
  
  plot (pred, legend=F, main= "present")
  plot (sa, add=T, border="gray30")
  plot (araucaria, add=T, border="gray30")
  points (coord, pch=19, col="gray30", cex=0.5)
  
  plot (poly.x, poly.y, type="n", axes=F, ylab="Area (Km2)", xlab="")
  polygon (poly.x, poly.y, density = NA,border = NA, col = "darkolivegreen1")
  axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
  axis (2)
  
  dev.off()       
  
}





