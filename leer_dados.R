library (neotoma)
library (sp)
library (raster)

list.files ()
data<- read.table ("AraucariaSamples.csv", sep=",", header=T)
coord_p<- data.frame(data$Longitude, data$Latitude)
coord_p


library(ecospat)
setwd ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\ecoClimate\\baseline_1950\\")
clima_0<- list.files ("C:\\Users\\sara.varela\\Documents\\CIENCIAS\\ecoClimate\\baseline_1950\\", pattern=".txt")
# to read the same models that we have for the future

presente <- read.table(clima_0 [1], header = TRUE)
gridded(presente) <- ~long+lat
pres <- stack(presente)[[-1]]
pres

cnrm <- read.table(clima_0 [2], header = TRUE)
gridded(cnrm) <- ~long+lat
cnrm_pres <- stack(cnrm)[[-1]]
cnrm_pres

lgm<- read.table(clima_0 [8], header = TRUE)
gridded(lgm) <- ~long+lat
lgm2 <- stack(lgm)[[-1]]

cnrm_lgm<- read.table(clima_0 [9], header = TRUE)
gridded(cnrm_lgm) <- ~long+lat
cnrm_lgm2 <- stack (cnrm_lgm)[[-1]]





nicho_pres<- extract (pres, coord_p)
nicho_lgm<- extract (lgm2, coord_p)

nicho_pres_cnrm<- extract (cnrm_pres, coord_p)
nicho_lgm_cnrm<- extract (cnrm_lgm2, coord_p)



plot (nicho_pres[, 1],  nicho_pres[, 12], pch=16, col="#50505050", 
      xlim=c(10, 25), ylim=c(1000, 2000))
points(nicho_lgm [, 1],  nicho_lgm[, 12], pch=16, col="#50000050")


points (nicho_pres_cnrm [, 1],  nicho_pres[, 12], pch=16, col="#20305050")
points (nicho_lgm_cnrm [, 1],  nicho_pres[, 12], pch=16, col="#20605050")
