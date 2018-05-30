setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")

## read present sampled sites

dat<- read.table ("present_data.csv", sep=",", header=T)
dat
coord<- data.frame (x=dat$Longitude_X, y=dat$Latitude_Y)

## read polygon for the present distribution
require(rgdal)
araucaria <- readOGR("Mata_de_Araucaria.kml", "Araucaria")
araucaria
library (maptools)
sa<- readShapePoly ("South_America.shp")

## do the first plot to see present data and distribution of Araucaria

plot (araucaria, xlim=c(-60, -40), ylim=c(-35, -20))
plot (sa, add=T, col="grey", border="white")
plot (araucaria, add=T, col="gray30", border="gray30")
points (coord, pch=19, col="black")

## to see how climate changed during the las 21000 years in the places where the species lives today. 
setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\estudiantes\\julia_TFG")

modelos<- c("CCSM", "CNRM", "FGOALS", "GISS", "IPSL", "MIROC", "MRI")


par (mfrow=c(3,2))
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
  
  pres_data<- as.data.frame (extract (pres, araucaria))
  hol_data<- as.data.frame (extract (hol, araucaria))
  lgm21_data<- as.data.frame (extract (lgm21, araucaria))
  
  pres_data2<- as.data.frame (extract (pres, coord))
  hol_data2<- as.data.frame (extract (hol, coord))
  lgm21_data2<- as.data.frame (extract (lgm21, coord))
  
  pres_data3<- rbind (pres_data, pres_data2)
  hol_data3<- rbind (hol_data, hol_data2)
  lgm21_data3<- rbind (lgm21_data, lgm21_data2)
  
  boxplot (lgm21_data3[,1], hol_data3[,1], pres_data3[,1], col="grey", border="gray30",
           axes=F, ylim=c(12, 22), ylab="Annual Mean Temperature", main=modelos [i])
  axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
  axis (2)
}


par (mfrow=c(3,2))
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
  
  pres_data<- as.data.frame (extract (pres, araucaria))
  hol_data<- as.data.frame (extract (hol, araucaria))
  lgm21_data<- as.data.frame (extract (lgm21, araucaria))
  
  pres_data2<- as.data.frame (extract (pres, coord))
  hol_data2<- as.data.frame (extract (hol, coord))
  lgm21_data2<- as.data.frame (extract (lgm21, coord))
  
  pres_data3<- rbind (pres_data, pres_data2)
  hol_data3<- rbind (hol_data, hol_data2)
  lgm21_data3<- rbind (lgm21_data, lgm21_data2)
  
  boxplot (lgm21_data3[,12], hol_data3[,12], pres_data3[,12], col="grey", border="gray30",
           axes=F, ylim=c(500, 2500), ylab="Annual Precipitation", main=modelos [i])
  axis (1, labels =c("LGM", "Holocene", "Present"), at =c(1, 2,3), col="gray30")
  axis (2)
}


## map difference in climate from lgm to the present
col1<- colorRampPalette(c("white", "darkred"))

for (i in 1:6){
  print (modelos [i])
  setwd ("C:/Users/sara.varela/Documents/CIENCIAS/ecoClimate/baseline_1950_futuro") 
  climas<- list.files ()
  presente <- read.table(climas [i], header = TRUE)
  lgm <- read.table(climas [i+12], header = TRUE)
  
  gridded(presente) <- ~long+lat
  gridded(lgm) <- ~long+lat
  
  pres <- stack(presente)
  lgm21<- stack (lgm)
  
  pres<- pres [[-1]]
  lgm21<- lgm21 [[-1]]
  
  crs(pres) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(lgm21) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  df = data.frame(a = 1:nrow (coord))
  sp_co<- SpatialPointsDataFrame(coord, df, proj4string= crs(pres))
  
  ?SpatialPointsDataFrame
  
  ara<-  araucaria + coord
  
  pres_data<- mask (pres, ara)
  lgm21_data<- mask (lgm21, ara)
  
  deltas<- lgm21_data- pres_data
  
  plot (deltas[[1]], main = paste (modelos [i], "_AMT")) 
  plot (deltas [[12]], main = paste (modelos [i], "_AP")) 
  
}


  
  