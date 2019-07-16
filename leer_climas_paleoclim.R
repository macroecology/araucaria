library (raster)
library (rgdal)
library (sp)

setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria\\paleoclim_org")
# download coast shapefile to remove the ocean from our maps. 
setwd("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
costa<- readShapePoly ("ne_10m_land.shp")

# load paleoclim layers
setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria\\paleoclim_org")
carpetas<- list.files ()
i<- 8
for (i in 1:8){
setwd (paste ("./", carpetas [i], sep=""))
variables<- list.files (, pattern="\\.tif$")
XX<- stack (variables)
foo<-paste("stack_", i, "<-XX", sep="")
eval(parse(text=foo))
setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria\\paleoclim_org")
}


e<- extent(-70, -40,-40, -10)
stack_1<- crop (stack_1, e)
stack_2<- crop (stack_2, e)
stack_3<- crop (stack_3, e)
stack_4<- crop (stack_4, e)
stack_5<- crop (stack_5, e)
stack_6<- crop (stack_6, e)
stack_7<- crop (stack_7, e)
stack_8<- crop (stack_8, e)

lalala<- list (stack_1, stack_2, stack_3, stack_4, stack_5, stack_6, 
                stack_7, stack_8)

# read pollen data
setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\mariana_araucaria")
polen<- read.table ("Fossil_pollen_data_Sara2.csv", sep=",", head=T)
names (polen)

## modelos con paleoClim
x<- 1
nicho_pres<- as.data.frame (extract (lalala[[x]], coord))
nicho_hol1<- as.data.frame (extract (lalala[[x+1]], polen [polen$X0.3_4.2 ==1,2:3 ]))
nicho_hol2<- as.data.frame (extract (lalala[[x+2]], polen [polen$X4.2_8.3 ==1, 2:3 ]))
nicho_hol3<- as.data.frame (extract (lalala[[x+3]], polen [polen$X8.3_11.7 ==1,2:3  ]))
nicho_hol4<- as.data.frame (extract (lalala[[x+4]], polen [polen$X11.7_12.9 ==1, 2:3 ]))
nicho_hol5<- as.data.frame (extract (lalala[[x+5]], polen [polen$X12.9_14.7 ==1, 2:3 ]))
nicho_hol6<- as.data.frame (extract (lalala[[x+6]], polen [polen$X14.7_17 ==1, 2:3 ]))
nicho_lgm<- as.data.frame (extract (lalala[[x+7]], polen [polen$lgm ==1, 2:3 ]))

datos<- rbind (nicho_pres, nicho_hol1, nicho_hol2, nicho_hol3,nicho_hol4,nicho_hol5,
nicho_hol6, nicho_lgm)

datos<- datos [complete.cases(datos), ]
summary (datos)

# set percentile to drop out
x1<- 0.01

min_t<- quantile (datos$bio_1, x1)
max_t<- quantile (datos$bio_1, 1-x1)

min_ts<- quantile (datos$bio_4, x1)
max_ts<- quantile (datos$bio_4, 1-x1)

min_tmin<- quantile (datos$bio_11, x1)
max_tmin<- quantile (datos$bio_11, 1-x1)

min_tmax<- quantile (datos$bio_10, x1)
max_tmax<- quantile (datos$bio_10, 1-x1)

min_p<- quantile (datos$bio_12, x1)
max_p<- quantile (datos$bio_12, 1-x1)

min_pmax<- quantile (datos$bio_16, x1)
max_pmax<- quantile (datos$bio_16, 1-x1)

min_pmin<- quantile (datos$bio_17, x1)
max_pmin<- quantile (datos$bio_17, 1-x1)


# build maps
x<- 0
for (i in 1:8){
x<- x+1  
map_pres<- reclassify (lalala[[x]]$bio_1, c(-Inf, min_t, 0, min_t, max_t, 1, max_t, +Inf, 0))
map_pres_tmin<- reclassify (lalala[[x]]$bio_11, c(-Inf, min_tmin, 0, min_tmin, max_tmin, 1, max_tmin, +Inf, 0))
map_pres_tmax<- reclassify (lalala[[x]]$bio_10, c(-Inf, min_tmax, 0, min_tmax, max_tmax, 1, max_tmax, +Inf, 0))
map_pres_precip<- reclassify (lalala[[x]]$bio_12, c(-Inf, min_p, 0, min_p, max_p, 1, max_p, +Inf, 0))
map_pres_precipmax<- reclassify (lalala[[x]]$bio_16, c(-Inf, min_pmax, 0, min_pmax, max_pmax, 1, max_pmax, +Inf, 0))
map_pres_precipmin<- reclassify (lalala[[x]]$bio_17, c(-Inf, min_pmin, 0, min_pmin, max_pmin, 1, max_pmin, +Inf, 0))
map_ts<- reclassify (lalala[[x]]$bio_4, c(-Inf, min_ts, 0, min_ts, max_ts, 1, max_ts, +Inf, 0))

map<- map_pres * map_pres_tmin * map_pres_tmax *map_ts *map_pres_precip * map_pres_precipmax * map_pres_precipmin 

map<- mask (map, costa)
foo<-paste("map_", i, "<-map", sep="")
eval(parse(text=foo))

}

# build raw figure

tiff ("PaleoClim_envelope.tif",  width = 500, height = 1000, units = "px")
par (mfrow=c(2,4))
plot (map_1, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="present", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
plot (araucaria, add=T)
points (coord2)

plot (map_2, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="0.3-4.2 kybp", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$X0.3_4.2 ==1,2:3 ])

plot (map_3, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="4.2-8.3 kybp", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$X4.2_8.3 ==1, 2:3 ])

plot (map_4, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="8.3-11.7 kybp", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$X8.3_11.7 ==1,2:3  ])

plot (map_5, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="11.7-12.9 kybp", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$X11.7_12.9 ==1, 2:3 ])

plot (map_6, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="12.9-14.7 kybp", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$X12.9_14.7 ==1, 2:3 ])

plot (map_7, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="14.7-17 kybp", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$X14.7_17 ==1, 2:3 ])

plot (map_8, col=c("#00000000","#00006090"), legend=F, axes=F, box=F, 
      main="LGM", xlim=c(-55, -30), ylim=c(-45, -20))
plot (wrld_simpl, add=T)
points (polen [polen$lgm ==1, 2:3 ])

dev.off()

stabi<- (map_1 + map_2 + map_3 + map_4 + map_5 + map_6 + map_7 + map_8)

pdf ("stability_paleoclim.pdf")
plot (stabi)
plot (araucaria, add=T)
points (coord2)
plot (wrld_simpl, add=T)
dev.off()