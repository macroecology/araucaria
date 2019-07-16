## modelos con ecoclimate
dat<- read.table ("present_data.csv", sep=",", header=T)
dat

coord<- data.frame (x=dat$Longitude_X, y=dat$Latitude_Y)
setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
polen<- read.table ("Fossil_pollen_data_Sara2.csv", sep=",", head=T)
names (polen)
#past distribution
coord_glm<- data.frame (lon=polen$longitude [polen$lgm==1], lat=polen$latitude [polen$lgm==1])
coord_hol<- data.frame (lon=polen$longitude [polen$holo==1], lat=polen$latitude [polen$holo==1])
modelos
# download coast shapefile to remove the ocean from our maps. 
setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
costa<- readShapePoly ("ne_10m_land.shp")

# load 7 AOGCMs for present, holocene and lgm  
i<- 7
for (i in 1:7){
  print (modelos [i])
  # here I have all aogcms for all scenarios. take care with the order. 
  # this only works if you have them in the same order than me. 
  # It should be the same if you did not change their names 
  # after downloaded from ecoclimate
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
  
  e<- extent(-65, -30,-40, 5)
  
  pres<- crop (pres, e)
  hol<- crop (hol, e)
  lgm21<- crop (lgm21, e)
  
  foo<-paste(modelos [i],"pres","<- pres", sep="")
  eval(parse(text=foo))
  foo<-paste( modelos [i],"hol","<-hol", sep="")
  eval(parse(text=foo))
  foo<-paste( modelos [i],"lgm21","<-lgm21", sep="")
  eval(parse(text=foo))
  
}

# make a list, only works if your aogcms follow the same order than mines!
lala<-list (CCSMpres, CCSMhol, CCSMlgm21,
            CNRMpres, CNRMhol, CNRMlgm21, 
            FGOALSpres, FGOALShol, FGOALSlgm21, 
            GISSpres, GISShol, GISSlgm21,
            IPSLpres, IPSLhol, IPSLlgm21,
            MIROCpres, MIROChol, MIROClgm21, 
            MRIpres, MRIhol, MIROClgm21)


# download etopo1 model, save it in your computer, before this: 
# read altitude map
setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
e<- extent(-65, -30, -40, 5)
alt<- raster ("ETOPO1_Bed_g_geotiff.tif")
alt<- crop (alt, e)




## bioclim con ecoclimate
## you need to run the loop from here
## first, build blank maps that will enter in the loop below, and then run the loop
map_presente<- CCSMpres[[1]]$bio.1*0
map_holocene<- CCSMpres[[1]]$bio.1*0
map_lgmaximum<- CCSMpres[[1]]$bio.1*0
sta<- 0
areas_res<- NULL
res_maps<- list ()
x<- 1
for (i in 1:7){
  nicho_pres<- as.data.frame (extract (lala[[x]], coord))
  nicho_hol<- as.data.frame (extract (lala[[x+1]], coord_hol))
  nicho_glm<- as.data.frame (extract (lala[[x+2]], coord_glm))
  
  nicho_pres<- data.frame ( time="pres", nicho_pres)
  nicho_hol<- data.frame (time="holocene", nicho_hol) 
  nicho_glm<- data.frame (time="glm", nicho_glm)
  
  datos<- rbind (nicho_pres, nicho_hol, nicho_glm)
  
  datos<- datos [complete.cases(datos), ]
  # set percentile to be drop out
  x1<- 0.01
  
  min_t<- quantile (datos$bio.1, x1)
  max_t<- quantile (datos$bio.1, 1-x1)
  
  min_ts<- quantile (datos$bio.4, x1)
  max_ts<- quantile (datos$bio.4, 1-x1)
  
  min_tmin<- quantile (datos$bio.11, x1)
  max_tmin<- quantile (datos$bio.11, 1-x1)
  
  min_tmax<- quantile (datos$bio.10, x1)
  max_tmax<- quantile (datos$bio.10, 1-x1)
  
  min_p<- quantile (datos$bio.12, x1)
  max_p<- quantile (datos$bio.12, 1-x1)
  
  min_pmax<- quantile (datos$bio.17, x1)
  max_pmax<- quantile (datos$bio.17, 1-x1)
  
  min_pmin<- quantile (datos$bio.16, x1)
  max_pmin<- quantile (datos$bio.16, 1-x1)
  
  # make maps
  map_pres<- reclassify (lala[[x]]$bio.1, c(-Inf, min_t, 0, min_t, max_t, 1, max_t, +Inf, 0))
  map_pres_tmin<- reclassify (lala[[x]]$bio.11, c(-Inf, min_tmin, 0, min_tmin, max_tmin, 1, max_tmin, +Inf, 0))
  map_pres_tmax<- reclassify (lala[[x]]$bio.10, c(-Inf, min_tmax, 0, min_tmax, max_tmax, 1, max_tmax, +Inf, 0))
  map_pres_precip<- reclassify (lala[[x]]$bio.12, c(-Inf, min_p, 0, min_p, max_p, 1, max_p, +Inf, 0))
  map_pres_precipmax<- reclassify (lala[[x]]$bio.17, c(-Inf, min_pmax, 0, min_pmax, max_pmax, 1, max_pmax, +Inf, 0))
  map_pres_precipmin<- reclassify (lala[[x]]$bio.16, c(-Inf, min_pmin, 0, min_pmin, max_pmin, 1, max_pmin, +Inf, 0))
  map_ts<- reclassify (lala[[x]]$bio.4, c(-Inf, min_ts, 0, min_ts, max_ts, 1, max_ts, +Inf, 0))
  
  map1<- map_pres * map_pres_tmin * map_pres_tmax * map_ts * map_pres_precip * map_pres_precipmax * map_pres_precipmin 
  map1<- mask (map1, costa)
  
  map_pres<- reclassify (lala[[x+1]]$bio.1, c(-Inf, min_t, 0, min_t, max_t, 1, max_t, +Inf, 0))
  map_pres_tmin<- reclassify (lala[[x+1]]$bio.11, c(-Inf, min_tmin, 0, min_tmin, max_tmin, 1, max_tmin, +Inf, 0))
  map_pres_tmax<- reclassify (lala[[x+1]]$bio.10, c(-Inf, min_tmax, 0, min_tmax, max_tmax, 1, max_tmax, +Inf, 0))
  map_pres_precip<- reclassify (lala[[x+1]]$bio.12, c(-Inf, min_p, 0, min_p, max_p, 1, max_p, +Inf, 0))
  map_pres_precipmax<- reclassify (lala[[x+1]]$bio.17, c(-Inf, min_pmax, 0, min_pmax, max_pmax, 1, max_pmax, +Inf, 0))
  map_pres_precipmin<- reclassify (lala[[x+1]]$bio.16, c(-Inf, min_pmin, 0, min_pmin, max_pmin, 1, max_pmin, +Inf, 0))
  map_ts<- reclassify (lala[[x]]$bio.4, c(-Inf, min_ts, 0, min_ts, max_ts, 1, max_ts, +Inf, 0))
  
  map2<- map_pres * map_pres_tmin * map_pres_tmax * map_ts *
    map_pres_precip * map_pres_precipmax * map_pres_precipmin 
  
  map2<- mask (map2, costa)
  
  map_pres<- reclassify (lala[[x+2]]$bio.1, c(-Inf, min_t, 0, min_t, max_t, 1, max_t, +Inf, 0))
  map_pres_tmin<- reclassify (lala[[x+2]]$bio.11, c(-Inf, min_tmin, 0, min_tmin, max_tmin, 1, max_tmin, +Inf, 0))
  map_pres_tmax<- reclassify (lala[[x+2]]$bio.10, c(-Inf, min_tmax, 0, min_tmax, max_tmax, 1, max_tmax, +Inf, 0))
  map_pres_precip<- reclassify (lala[[x+2]]$bio.12, c(-Inf, min_p, 0, min_p, max_p, 1, max_p, +Inf, 0))
  map_pres_precipmax<- reclassify (lala[[x+2]]$bio.17, c(-Inf, min_pmax, 0, min_pmax, max_pmax, 1, max_pmax, +Inf, 0))
  map_pres_precipmin<- reclassify (lala[[x+2]]$bio.16, c(-Inf, min_pmin, 0, min_pmin, max_pmin, 1, max_pmin, +Inf, 0))
  map_ts<- reclassify (lala[[x]]$bio.4, c(-Inf, min_ts, 0, min_ts, max_ts, 1, max_ts, +Inf, 0))
  
  map3<- map_pres * map_pres_tmin * map_pres_tmax * map_ts *
    map_pres_precip * map_pres_precipmax * map_pres_precipmin 
  map3<- mask (map3, costa)
  
  stab<- map1*map2*map3
  stab2<- map1+map2+map3
  res_maps [[i]]<- stab2
  todo_1_0<- extract (stab, coord)
  matri_1_0<- extract (stab, coord2)
  big_area_1_0<- extract (stab, araucaria) [[1]]
  stab_matri<- round (sum (matri_1_0)/length (matri_1_0)*100,2)
  stab_big_area<- round (sum (big_area_1_0)/length (big_area_1_0)*100,2)
  
  area_tot<- area (map1)
  area_map1<- area_tot*map1
  area_map2<- area_tot*map2
  area_map3<- area_tot*map3
  
  areas<- rev(c(sum (area_map1@data@values, na.rm=T), 
                sum (area_map2@data@values, na.rm=T), 
                sum (area_map3@data@values, na.rm=T)))
  
  stable_coord<- coord [todo_1_0==1, ]
  inst_coord<- coord [todo_1_0==0, ]
  
  coord3<- data.frame (coord, stable=0)
  coord3$stable [todo_1_0==1]<- 1
  
  write.table (coord3, paste (modelos[i], "stable_points.csv", sep=""), sep=",", row.names = F)
  
  pdf(file= paste (modelos[i], "_bioclim.pdf", sep=""))
  
  
  par (mfrow=c(2,3), mar=c(2,5,2,2))
  
  plot (map3,xlim=c(-65, -30), 
        ylim=c(-40, 5), 
        col=c("#00000000", "#00006090"), legend=F, 
        axes=F, box=F, main="LGM")
  plot (wrld_simpl, add=T)
  points (coord_glm, col="coral", pch=16)
  
  plot (map2, xlim=c(-65, -30), 
        ylim=c(-40, 5),
        col=c("#00000000", "#00606060", "#60000060", "#00006090"), legend=F, 
        axes=F, box=F, main="holocene")
  plot (wrld_simpl, add=T)
  points (coord_hol, col="coral", pch=16)
  
  plot (map1, xlim=c(-65, -30), 
        ylim=c(-40, 5),
        col=c("#00000000", "#00606060", "#60000060", "#00006090"), legend=F, 
        axes=F, box=F, main="present")
  plot (wrld_simpl, add=T)
  plot (araucaria, add=T)
  points (coord2)
  
  plot (inst_coord, xlim=c(-65, -30), 
        ylim=c(-40, 5), col=c("#90000090"), pch=16,
        axes=F, main="Stable populations (blue)")
  points (stable_coord, col=c("#00009090"), pch=16)
  plot (wrld_simpl, add=T)
  text (-48, -30, paste (stab_matri, "% mantiqueira", sep=""), pos=4)
  text (-48, -35, paste (stab_big_area, "% core", sep=""), pos=4)
  
  
  plot (areas, type="o", axes=F, main=modelos [i], ylab="Area Km2", col="#00006090", 
        ylim=c(300000, 10000000), pch=16)
  axis (1, labels=c("LGM", "Holo", "Pres"), at=c(1,2,3))
  axis (2)

  
  dev.off()
  
  map_presente<-  map_presente + map1
  map_holocene<- map_holocene + map2
  map_lgmaximum<- map_lgmaximum + map3
  sta<- sta + coord3$stable
  areas_res<- rbind (areas,  areas_res)
  x<- x+3
}


## the other figures
pdf ("stab_ecoclimate.pdf")
par (mfrow =c(2,4))
for (i in 1:7){
plot (res_maps [[1]], ylim=c(-45, -10), axes=F, box=F, 
      legend=F, main=modelos [i])
plot (araucaria, add=T)
points (coord2)
plot (wrld_simpl, add=T)
}
dev.off()

alt2<- resample (alt, map1, method="bilinear")
alt_pres<- data.frame (alt= extract (alt2, coord), time= 0)
alt_hol<- data.frame (alt=extract (alt2, coord_hol),time= 6)
alt_glm<- data.frame (alt=extract (alt2, coord_glm), time=23)

alt_datos<- rbind (alt_pres, alt_hol, alt_glm)
dev.off()

boxplot (alt_datos$alt ~alt_datos$time, 
         ylab="Altitude", xlab="time kypb",
         col="grey", border="gray50", axes=F) 
axis (1, labels=c("present", "Mid-Holocene", "LGM"), at=c(1,2,3))
axis(2)

pdf("alt_tiempo.pdf")
par (mfrow=c(1,3))
plot (alt2, zlim=c(0, 1500), xlim=c(-65, 30), 
      ylim=c(-40, 5), axes=F, box=F, main="past pollen records")
points (coord_glm, col="#00006060", pch=16)
points (coord_hol, col="#00006060", pch=16)
plot (wrld_simpl, add=T)


plot (alt2, zlim=c(0, 1500), xlim=c(-65, 30), 
      ylim=c(-40, 5), axes=F, box=F, main="present distribution", legend=F)
plot (araucaria, add=T)
points (coord2)
plot (wrld_simpl, add=T)

boxplot (alt_datos$alt ~alt_datos$time, 
         ylab="Altitude", xlab="time kypb",
         col="grey", border="gray50", axes=F) 
axis (1, labels=c("present", "Mid-Holocene", "LGM"), at=c(1,2,3))
axis(2)

dev.off()

setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
# ensemble 

pdf("ensemble.pdf")
par (mfrow=c(1,3), mar=c(0,0,5,0))
plot (map_lgmaximum, xlim=c(-65, 30), 
      ylim=c(-40, 5), axes=F, box=F, 
      main="LGM distribution", legend=F, zlim=c(2,7))
points (coord_glm, col="#00006090", pch=16)
plot (wrld_simpl, add=T)
plot (map_holocene, xlim=c(-65, 30), 
      ylim=c(-40, 5), axes=F, box=F, 
      main="Holocene distribution", legend=F, zlim=c(2,7))
points (coord_hol, col="#00006090", pch=16)
plot (wrld_simpl, add=T)
plot (map_presente, xlim=c(-65, 30), 
      ylim=c(-40, 5), axes=F, box=F, 
      main="present distribution", zlim=c(2,7))
plot (araucaria, add=T)
points (coord2)
plot (wrld_simpl, add=T)
dev.off()


pdf("stability.pdf")
palette (colorRampPalette(c("red", "blue"))(7))
plot (wrld_simpl, xlim=c(-60, -30), 
      ylim=c(-40, -20), main="stable populations (blue)")
points (coord, col=sta, pch=16)
dev.off()

nombres<- rep (modelos, 3)

library (ggplot2)
variable<- nombres [order (rep (modelos, 3))]
length (variable)
dim (a)
a<- data.frame (area=as.vector (t(areas_res)), time=c(23,6,0), variable) 


areas_plot<- ggplot(a, aes(x=time,y=area)) + geom_smooth(color="#00006090") + geom_point (color="#00006090")
pdf("area.pdf")
plot (areas_plot)
dev.off()

pdf("area_modelos.pdf")
ggplot(a, aes(x=time,y=area)) + geom_line(aes(colour=variable))
dev.off()

