#################
#Code for making map

library(raster)
library(fasterize)
library(sf)
library(sp)
library(latticeExtra)
library(rasterVis)
library(terra)
library(ggplot2)
library(pals)
library(geojsonio)
library(ggspatial)

cpal = 'RdYlBu'

resultDir = "./results"
inputDir = "./data"

pts = read.csv(paste0(inputDir,"/ExemplarLocations.csv"))

pts = subset(pts,!grepl("Service",label,fixed=T))
nrow(pts)
#base raster from https://osf.io/bxvd4/
H = raster(paste0(inputDir,"/allCostsLayer.tif"))

provsA =  getData('GADM', country=c('CAN'), level=1)

usa <- getData("GADM", country = "USA", level = 1)
usa <- usa[usa$NAME_1 == "Alaska",]

provsA = rbind(provsA,usa)
provsA= spTransform(provsA,crs(H))

plot(provsA)
#Treaty info
#https://r-graph-gallery.com/325-background-map-from-geojson-format-in-r.html
#treatiesJson = FROM_GeoJson(paste0(inputDir,"/indigenousTreaties.json"))

#OSDP modern and historic treaties

historicTreaties = st_read(paste0(inputDir,"/Traite_historique_Historic_Treaty_SHP.shp"))

historicTreaties = st_transform(historicTreaties,crs(H))

modernTreaties = st_read(paste0(inputDir,"/Traite_moderne_Modern_Treaty_SHP.shp"))
modernTreaties = st_transform(modernTreaties,crs(H))

labPts = as.data.frame(rbind(historicTreaties,modernTreaties))

ptsAdd <- subset(pts,select=c(longitude,latitude,label))
names(ptsAdd)=c("x","y","Study")
ptsAdd$Study[ptsAdd$Study=="Polfus et al. 2016"]="Polfus et al. 2016 & 2017"
ptsAdd=subset(ptsAdd,Study!="Polfus et al. 2017")

ptsAdd$Study=paste0(" ",ptsAdd$Study)
str(pts)
range(ptsAdd$x)
range(ptsAdd$y)

#https://paleolimbot.github.io/ggspatial/articles/ggspatial.html
png(paste0(resultDir,"/mapWTreatiesN.png"),
    width=17*0.5,height=13.5*0.4, units="in",res=600)
ggplot(labPts,aes(x=x,y=y)) +
  layer_spatial(data=provsA,fill="white",col="white")+
  layer_spatial(data = historicTreaties,aes(fill = "#66c2a5",col="#66c2a5"),alpha=0.1) +
  layer_spatial(data = modernTreaties,aes(fill = "#8da0cb",col="#8da0cb"),alpha=0.1) +
  geom_spatial_point(data=ptsAdd,aes(x=x,y=y))+
  #geom_spatial_text(data=ptsAdd,aes(label=Study),col="black",size=6,vjust=0,hjust=0)+
  coord_sf(crs = crs(H))+
   theme(legend.position = "none")+ylab("")+xlab("")
dev.off()

