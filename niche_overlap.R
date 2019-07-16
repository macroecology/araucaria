library (ecospat)

ecospat.grid.clim.dyn (glob, glob1, sp, R, th.sp, th.env, geomask)


library (hypervolume)

pres_hyper<- hypervolume(nicho_pres, method = "gaussian")

# present distribution
coord
#past distribution
coord_glm<- data.frame (lon=polen$longitude [polen$lgm==1], lat=polen$latitude [polen$lgm==1])
coord_hol<- data.frame (lon=polen$longitude [polen$holo==1], lat=polen$latitude [polen$holo==1])
modelos

par (mfrow=c(4,2), mar=c(5, 4, 2, 0))
x<- 1
for (i in 1:7){
nicho_pres<- as.data.frame (extract (lala[[x]], coord))
nicho_hol<- as.data.frame (extract (lala[[x+1]], coord_hol))
nicho_glm<- as.data.frame (extract (lala[[x+2]], coord_glm))
x<- x+3

plot (nicho_pres$bio.1, nicho_pres$bio.12, xlim=c(10, 30), 
      ylim=c(500, 2500), col="#90000060", cex=2,
      pch=16, xlab="AMT (ºC)", ylab="AP (mm)", axes=F)
axis(1)
axis(2)
text (10, 2200, pos=4, modelos[i])
points (nicho_hol$bio.1, nicho_hol$bio.12,  cex=2,col="#00900060", pch=16)
points (nicho_glm$bio.1, nicho_glm$bio.12,  cex=2,col="#00009060", pch=16)
}

plot (c(15, 15, 15), c(700, 1200, 1700), 
      cex=2, pch=16, xlim=c(10, 30), 
      ylim=c(500, 2500),
        col=c(col="#900000","#009000", "#000090"), 
      axes=F, xlab="", ylab="")
text (c(16, 16, 16), c(700, 1200, 1700), pos=4,  
      c("Present","Holocene", "Last Glacial Maximum"))


?text
library(dynRB)
library(ggplot2)
library(reshape2)
library(vegan)
library(RColorBrewer)

x<- 1
for (i in 1:7){
  nicho_pres<- as.data.frame (extract (lala[[x]], coord))
  nicho_hol<- as.data.frame (extract (lala[[x+1]], coord_hol))
  nicho_glm<- as.data.frame (extract (lala[[x+2]], coord_glm))
  x<- x+3
  
nicho_pres<- data.frame ( time="pres", nicho_pres)
nicho_hol<- data.frame (time="holocene", nicho_hol) 
nicho_glm<- data.frame (time="glm", nicho_glm)

datos<- rbind (nicho_pres, nicho_hol, nicho_glm)

# 'result$port_prod' may be changed to 'result$port_mean' or 'result$port_gmean'
## lo hacemos solo con bio1 y bio12
r <- dynRB_VPa(datos[,c(1,2,12)])  
theme_change <- theme(
  plot.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_text(colour="black", size = rel(1.5), angle=35, hjust = 1),
  axis.text.y = element_text(colour="black", size = rel(1.5)),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

result <- r$result
Overlap <- as.numeric(ifelse(result$V1 == result$V2, 
                             "NA", result$port_prod))  

is.numeric(Overlap)
Result2<-cbind(result, Overlap)
breaks <- seq(0,1, by=5)

col1 <- colorRampPalette(c("white", "navyblue")) #define color gradient
p<- ggplot(Result2, aes(x = V1, y = V2)) + ggtitle(modelos [i]) +
  geom_tile(data = subset(Result2, !is.na(Overlap)), aes(fill = Overlap), color="black") +
  geom_tile(data = subset(Result2,  is.na(Overlap)), fill = "lightgrey", color="black") +
  scale_fill_gradientn(colours=col1(8), breaks=breaks, guide="colorbar",  
                       limits=c(min(Overlap, na.rm=TRUE),max(Overlap, na.rm=TRUE))) +
  theme_change

foo<-paste("p", i, "<-p", sep="")
eval(parse(text=foo))

}


library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6, p7)

