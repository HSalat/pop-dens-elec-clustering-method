library(rgdal)

gridSenegal <- c(-17.54319,-11.34247,12.30786,16.69207,744,527)
holesSenegal <- which(is.na(grid_senComPop))


#######################################################
##### Spatial data in Voronoi cells around towers #####
#######################################################

#####-------------
##### Census data 
#####-------------


individus <- read.csv("/Users/Hadrien/Desktop/Data/NewOrange/individus.csv",header = T, sep=";")
habitat <- read.csv("/Users/Hadrien/Desktop/Data/NewOrange/habitat.csv",header = T, sep=";")


#####----------------------------------------
##### Data in Shapefiles (dens, dens2, elec) 
#####----------------------------------------

senCommune3 <- readOGR("Limite_Commune_Senegal.shp")

senCommune3@data$rand <- sample(1:552)

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune3)

senComPop2 <- rasterize(senCommune3, r, 'rand')
extent(senComPop2) <- extent(senCommune)

plot(senComPop2)
plot(senCommune,add=T)
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)


plot(senComPop2,xlim=c(-14.5,-11.34),ylim=c(12.3,14.5))
plot(senCommune,add=T)
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)

plot(senComPop2,xlim=c(-14.5,-1134),ylim=c(14.5,16.70))
plot(senCommune,add=T)
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)

plot(senComPop2,xlim=c(-17.55,-14.5),ylim=c(12.3,14.5))
plot(senCommune,add=T)
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)

plot(senComPop2,xlim=c(-17.55,-14.5),ylim=c(14.5,16.70))
plot(senCommune,add=T)
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)



cents <- coordinates(senCommuneProper)

cents[129,]
cover

dat2 <- data.frame(long = cents[129,1], lat = cents[129,2], name = senCommuneProper@data$COD_ENTITE[129])
coordinates(dat2) <- ~ long + lat
proj4string(dat2) <- proj4string(senCommuneProper)

test <- over(dat2,senCommune)





dat <- data.frame(long = cents[,1], lat = cents[,2], name = senCommuneProper@data$COD_ENTITE)
coordinates(dat) <- ~ long + lat
proj4string(dat) <- proj4string(senCommuneProper)

test <- over(dat,senCommune)

proj4string(senCommune) <- proj4string(senCommuneProper)
proj4string(dat)

View(test)
View(senCommune@data)

length(unique(test$CC_45))
length(unique(test$COD_ENTITE))


length(unique(senCommune3@data$COD_ENTITE))


lookUpTable <- data.frame(com552 = senCommuneProper@data$COD_ENTITE, com433 = test$CC_4)

write.csv(lookUpTable,"lookUpTable.csv")

senCommune3@data$CC_45 <- test$CC_45
senCommune3@data$check <- senCommune3@data$COD_ENTITE


popelec <- read.csv("popelec.csv")
for(i in 1:552){
  if(nchar(popelec$cacr[i]) < 8){
    popelec$cacr[i] <- paste("0",popelec$cacr[i],sep="")
  }
}

senCommune3@data$id <- 1:552
senCommune3@data <- merge(senCommune3@data,popelec,by.x="COD_ENTITE",by.y="cacr",sort=F)



senCommune@data <- merge(senCommune@data,temp,by.x="CC_45",by.y="CC_45",sort=F,all.x=T)
senCommune@data$pop[is.na(senCommune@data$pop)] <- senCommune@data$popsize[is.na(senCommune@data$pop)]
senCommune@data$Density2 <- senCommune@data$pop/senCommune@data$gadm36__15
  
View(senCommune@data)
  



plot(senComPop2)

senComPop2 <- rasterize(senCommune, r, 'Density2')

grid_senComPop2 <- as.matrix(senComPop2)
grid_senComPop2 <- grid_senComPop2[nrow(grid_senComPop2):1,]
grid_senComPop2[which(is.na(grid_senComPop2))] <- 0
grid_senComPop2[which(is.na(grid_senComPop))] <- NA

vor_data$dens3 <- avg_voronoi_ref_NA(grid_senComPop2,ref2)[[2]]


cor(vor_data$dens,vor_data$night)^2
cor(vor_data$dens3,vor_data$night)^2

subsettemp <- which(vor_data$night >0)

cor(log(vor_data$dens[subsettemp]),log(vor_data$night[subsettemp]))^2
cor(log(vor_data$dens3[subsettemp]),log(vor_data$night[subsettemp]))^2





senCommune3@data$CC_45 <- test$CC_45
temp <- aggregate(popsize ~ CC_45, data=senCommune3@data, FUN = "sum")
colnames(temp)[2] <- "pop"

senCommune3@data <- senCommune3@data[order(senCommune3@data$id),]
row.names(senCommune3@data) <- as.character(0:551)

senCommune3@data$density <- senCommune3@data$popsize/senCommune3@data$SUPERFICIE

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune3)

senComPop2 <- rasterize(senCommune3, r, 'COD_ENTITE')

senComPop2 <- rasterize(senCommune3, r, 'density')

senComPop3 <- rasterize(senCommune3, r, 'shelec')

extent(senComPop2) <- extent(senCommune)


grid_senComPop2 <- as.matrix(senComPop2)
grid_senComPop2 <- grid_senComPop2[nrow(grid_senComPop2):1,]
grid_senComPop2[which(is.na(grid_senComPop2))] <- 0
grid_senComPop2[which(is.na(grid_senComPop))] <- NA

vor_data$dens3 <- avg_voronoi_ref_NA(grid_senComPop2,ref2)[[2]]


cor(vor_data$dens,vor_data$night)^2
cor(vor_data$dens3,vor_data$night)^2


cor(vor_data$texts,vor_data$dens3)^2
cor(vor_data$calls,vor_data$dens3)^2
cor(vor_data$length,vor_data$dens3)^2

which(vor_data$texts == 0)
which(vor_data$dens3 == 0)

subset <- setdiff(1:1298,which(vor_data$texts == 0))
subset <- setdiff(subset,which(vor_data$dens3 == 0))


cor(log(vor_data$texts[subset]),log(vor_data$dens3[subset]))^2
cor(log(vor_data$calls[subset]),log(vor_data$dens3[subset]))^2
cor(log(vor_data$length[subset]),log(vor_data$dens3[subset]))^2


senCommune3@data[which(senCommune3@data$COD_ENTITE %in% c(10120204,10110200)),]

senCommune3@data$ident <- 0
senCommune3@data$popsize[which(senCommune3@data$COD_ENTITE == 10120204)]
senCommune3@data$popsize[which(senCommune3@data$COD_ENTITE == 10110200)]

senCommune3@data$popsize[which(senCommune3@data$COD_ENTITE == 11310100)]
senCommune3@data$popsize[which(senCommune3@data$COD_ENTITE == 11320103)]


senComPop3 <- rasterize(senCommune4, r, 'ident')
extent(senComPop3) <- extent(senCommune)
plot(senComPop3)


sum(popelec$popsize)
sum(senCommune@data$pop)
sum(senCommune@data$popsize)

sum(vor_data$dens*vor_data$cells)
sum(vor_data$dens3*vor_data$cells)


plot(senCommune3)

plot(senComPop2)
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)



plot(senComPop2,xlim=c(-15,-14),ylim=c(12.5,14))
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)

plot(senComPop3,xlim=c(-15,-14),ylim=c(12.5,14))
text(senCommune3@data$LONGITUDE,senCommune3@data$LATITUDE,senCommune3@data$COD_ENTITE)




text(senCommune3@data$LONGITUDE[403],senCommune3@data$LATITUDE[403],senCommune3@data$COD_ENTITE[403])


senCommune3@data$ident[403]
senCommune3@data$COD_ENTITE[417]

10220301

cents <- coordinates(senCommune)

plot(senCommune,xlim=c(-15,-14),ylim=c(12.5,14))
text(cents[,1],cents[,2],senCommune@data$OBJECTID_1)


setdiff(senCommune3@data)

setdiff(senCommune3@data$COD_ENTITE,popelec$cacr)

View(popelec)
View(senCommune3@data)
head(popelec)

senCommune <- readShapePoly("C:/Users/Hadrien/Desktop/Data/Export_Output_2.shp",delete_null_obj=TRUE)
senCommune2 <- readShapePoints("C:/Users/Hadrien/Desktop/Data/Export_Output_5.shp")

# See "Estimated consumption" for "habitat2"
habitat3 <- aggregate(habitat2,by=list(habitat2$Code),FUN="mean")
senCommune@data <- merge(senCommune@data,habitat3,by.x="CC_4",by.y="Group.1",all.x=T)

View(senCommune2@data)

length(unique(senCommune2@data$CC_45))

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune)

# Grid "dens"
senComPop <- rasterize(senCommune, r, 'Density')
grid_senComPop <- as.matrix(senComPop)
grid_senComPop <- grid_senComPop[nrow(grid_senComPop):1,]

# Grid "elec"
senComElec <- rasterize(senCommune, r, 'shelec')
grid_senComElec <- as.matrix(senComElec)
grid_senComElec <- grid_senComElec[nrow(grid_senComElec):1,]

# Grid "estCons"
senComEstCons <- rasterize(senCommune, r, 'estConsumption')
grid_senComEstCons <- as.matrix(senComEstCons)
grid_senComEstCons <- grid_senComEstCons[nrow(grid_senComEstCons):1,]

# Grid "numApp"
senComnumApp <- rasterize(senCommune, r, 'numApp')
grid_senComnumApp <- as.matrix(senComnumApp)
grid_senComnumApp <- grid_senComnumApp[nrow(grid_senComnumApp):1,]

# Grid "dens2"
senComDens <- rasterize(senCommune2, r, 'Density')
grid_senComDens <- as.matrix(senComDens)
grid_senComDens <- grid_senComDens[nrow(grid_senComDens):1,]
grid_senComDens[which(is.na(grid_senComDens))] <- 0
grid_senComDens[which(is.na(grid_senComPop))] <- NA


#####-----------------------------------------------
##### [OLD] Mobile phone data (texts, calls, length) 
#####-----------------------------------------------


towerloc <- read.csv("/Users/Hadrien/Desktop/Data/ContextData/SITE_ARR_LONLAT.csv",header = T, sep=",")

# Texts
i=12
assign(paste("sptt",i,sep="_"),monthlyText(i,folderin))

sptt_all <- cbind(sptt_1,sptt_2,sptt_3,sptt_4,sptt_5,sptt_6,
                  sptt_7,sptt_8,sptt_9,sptt_10,sptt_11,sptt_12)
sptt_all <- sptt_all[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)]
colnames(sptt_all) <- c("tower",monthslist)

sptt_all$year <- sptt_all$jan+sptt_all$feb+sptt_all$mar+sptt_all$apr+sptt_all$may+sptt_all$jun+sptt_all$jul+sptt_all$aug+sptt_all$sep+sptt_all$oct+sptt_all$nov+sptt_all$dec

# Calls
i=12
assign(paste("sptv",i,sep="_"),monthlyVoice(i,folderin))

sptv_all <- cbind(sptv_1,sptv_2,sptv_3,sptv_4,sptv_5,sptv_6,
                  sptv_7,sptv_8,sptv_9,sptv_10,sptv_11,sptv_12)
sptv_all_n <- sptv_all[,c(1,2,5,8,11,14,17,20,23,26,29,32,35)]
sptv_all_t <- sptv_all[,c(1,3,6,9,12,15,18,21,24,27,30,33,36)]
colnames(sptv_all_n) <- c("tower",monthslist)
colnames(sptv_all_t) <- c("tower",monthslist)

sptv_all_n$year <- sptv_all_n$jan+sptv_all_n$feb+sptv_all_n$mar+sptv_all_n$apr+sptv_all_n$may+sptv_all_n$jun+sptv_all_n$jul+sptv_all_n$aug+sptv_all_n$sep+sptv_all_n$oct+sptv_all_n$nov+sptv_all_n$dec
sptv_all_t$year <- sptv_all_t$jan+sptv_all_t$feb+sptv_all_t$mar+sptv_all_t$apr+sptv_all_t$may+sptv_all_t$jun+sptv_all_t$jul+sptv_all_t$aug+sptv_all_t$sep+sptv_all_t$oct+sptv_all_t$nov+sptv_all_t$dec

# Gridding
sptt_all <- merge(sptt_all,towerloc,by.x="tower",by.y="site_id")
sptv_all_n <- merge(sptv_all_n,towerloc,by.x="tower",by.y="site_id")
sptv_all_t <- merge(sptv_all_t,towerloc,by.x="tower",by.y="site_id")

grid_Sptt_y <- newrandtoGrid(sptt_all$lon,sptt_all$lat,sptt_all$year,744,527,"sum")
grid_Sptv_n_y <- newrandtoGrid(sptv_all_n$lon,sptv_all_n$lat,sptv_all_n$year,744,527,"sum")
grid_Sptv_t_y <- newrandtoGrid(sptv_all_t$lon,sptv_all_t$lat,sptv_all_t$year,744,527,"sum")

for(i in 1:12){
  assign(paste("grid_Sptt",monthslist[i],sep="_"),newrandtoGrid(sptt_all$lon,sptt_all$lat,sptt_all[,i+1],744,527,"sum"))
  assign(paste("grid_Sptv_n",monthslist[i],sep="_"),newrandtoGrid(sptv_all_n$lon,sptv_all_n$lat,sptv_all_n[,i+1],744,527,"sum"))
  assign(paste("grid_Sptv_t",monthslist[i],sep="_"),newrandtoGrid(sptv_all_t$lon,sptv_all_t$lat,sptv_all_t[,i+1],744,527,"sum"))
}

grid_Sptt_y[which(is.na(grid_senComPop))] <- NA
grid_Sptv_n_y[which(is.na(grid_senComPop))] <- NA
grid_Sptv_t_y[which(is.na(grid_senComPop))] <- NA


#####---------------------------------------
##### Nighttime lights data (night, nightp) 
#####---------------------------------------


imported_raster <- raster("/Users/Hadrien/Desktop/Data/Nighttime_lights_visible_x_pct/F182013.v4c.avg_lights_x_pct.tif")
subset <- crop(imported_raster,extent(senCommune))
nightlightxpct <- as.matrix(subset)
nightlightxpct <- nightlightxpct[nrow(nightlightxpct):1,]

imported_raster2 <- raster("/Users/Hadrien/Desktop/Data/Nighttime_lights_visible/F182013.v4c_web.stable_lights.avg_vis.tif")
subset2 <- crop(imported_raster2,extent(senCommune))
nightlight <- as.matrix(subset2)
nightlight <- nightlight[nrow(nightlight):1,]

nightlightxpct[which(is.na(grid_senComPop))] <- NA
nightlight[which(is.na(grid_senComPop))] <- NA


#####-----------------------------------------
##### Estimated consumption (numApp, estCons) 
#####-----------------------------------------


consumptionRef <- data.frame(code=c("E13_1","E13_2","E13_3","E13_4","E13_5","E13_6","E13_7","E13_8","E13_9",
                                    "E13_10","E13_11","E13_12","E13_13","E13_14","E13_15","E13_16","E13_17","E13_18"),
                             appliance=c("Radio","TV","DVD","Fridge","Phone","Mobile","Fireplace","Clim","Sew",
                                         "Fan","Broadband","PC","PV","GE","Fax","Iron","Boiler","Cooker"),
                             pct=pct,
                             consumptionHour=c(1.5,120,14,275,4,5.5,0,2500,5,
                                               50,10,200,0,0,14,1000,3000,1200),
                             hourPerDay=c(2,2,2,24,24,3,0,6,0.5,
                                          6,24,2,0,0,0.1,0.3,3,1)
)
consumptionRef$consumptionDay <- consumptionRef$consumptionHour*consumptionRef$hourPerDay

pct <- rep(0,18)
for(i in 27:44){
  pct[i-26] <- length(which(habitat[,i] > 0))/nrow(habitat)
}

habitat2 <- habitat[,c(27:44,78)]
habitat2$numApp <- rep(0,nrow(habitat2))
for(i in 1:nrow(habitat2)){
  habitat2$numApp[i] <- sum(habitat2[i,1:18])
}
habitat2 <- habitat2[which(!is.na(habitat2$numApp)),]
habitat2$estConsumption <- rep(0,nrow(habitat2))
for(i in 1:nrow(habitat2)){
  habitat2$estConsumption[i] <- sum(habitat2[i,1:18]*consumptionRef$consumptionDay)
}


#####------------------------------
##### [OLD] Voronoi reference table 
#####------------------------------


vor_data <- data.frame(grid_id=which(grid_Sptv_n_y>0))

ref <- which(grid_Sptv_n_y>0, arr.ind=T)
vor_data$cell_count <- avg_voronoi_ref_NA(grid_Sptv_n_y,ref)[[4]]
vor_data$dens <- avg_voronoi_ref_NA(grid_senComPop,ref)[[2]]
vor_data$night <- avg_voronoi_ref_NA(nightlight,ref)[[2]]
vor_data$nightp <- avg_voronoi_ref_NA(nightlightxpct,ref)[[2]]
vor_data$texts <- avg_voronoi_ref_NA(grid_Sptt_y,ref)[[2]]
vor_data$calls <- avg_voronoi_ref_NA(grid_Sptv_n_y,ref)[[2]]
vor_data$length <- avg_voronoi_ref_NA(grid_Sptv_t_y,ref)[[2]]
vor_data$elec <- avg_voronoi_ref_NA(grid_senComElec,ref)[[2]]
vor_data$numApp <- avg_voronoi_ref_NA(grid_senComnumApp,ref)[[2]]
vor_data$estCons <- avg_voronoi_ref_NA(grid_senComEstCons,ref)[[2]]

vor_data$dens2 <- avg_voronoi_ref_NA(grid_senComDens,ref)[[2]]
for(i in 1:nrow(vor_data)){
  if(vor_data$dens2[i] == 0 & vor_data$dens[i] > 1000){
    vor_data$dens2[i] <- vor_data$dens[i]
  }
}

vor_data2 <- vor_data[,2:12]


#####--------------------------------------------------------------
##### Mobile phone data (texts, calls, length) and Reference tables
#####--------------------------------------------------------------


towerloc$mati <- rep(0,1666)
towerloc$matj <- rep(0,1666)

country <- gridSenegal
x <- seq(country[1],country[2],length.out = country[5]+1)
y <- seq(country[3],country[4],length.out = country[6]+1)

for(i in 1:1666){
  towerloc$mati[i] <- max(which(towerloc$lat[i] > y))
  towerloc$matj[i] <- max(which(towerloc$lon[i] > x))
}

tower_data <- data.frame(tId=1:1666,tLong=towerloc$lon,tLat=towerloc$lat)
tower_data$tMatRow <- towerloc$mati
tower_data$tMatCol <- towerloc$matj
tower_data$tMatCoor <- towerloc$mati+(towerloc$matj-1)*527
tower_data$tTexts <- sptt_all$year
tower_data$tCalls <- sptv_all_n$year
tower_data$tLength <- sptv_all_t$year
tower_data$tvId <- rep(NA,1666)
attach(tower_data)

grid_texts <- griddingNation(tLong,tLat,tTexts,gridSenegal,holesSenegal,"sum")
grid_calls <- griddingNation(tLong,tLat,tCalls,gridSenegal,holesSenegal,"sum")
grid_length <- griddingNation(tLong,tLat,tLength,gridSenegal,holesSenegal,"sum")

ref <- which(grid_calls>0)
ref2 <- which(grid_calls>0,arr.ind=T)
for(k in 1:1666){
  if(length(which(ref == tower_data$tMatCoor[k]))>0)
    tower_data$tvId[k] <- which(ref == tower_data$tMatCoor[k])
}

for(i in 1:12){
  assign(paste("grid_Sptt",monthslist[i],sep="_"),griddingNation(tLong,tLat,sptt_all[,i+1],gridSenegal,holesSenegal,"sum"))
  assign(paste("grid_Sptv_n",monthslist[i],sep="_"),griddingNation(tLong,tLat,sptv_all_n[,i+1],gridSenegal,holesSenegal,"sum"))
  assign(paste("grid_Sptv_t",monthslist[i],sep="_"),griddingNation(tLong,tLat,sptv_all_t[,i+1],gridSenegal,holesSenegal,"sum"))
}

vor_data <- data.frame(vid=1:length(ref))
vor_data$vMatRow <- ref2[,1]
vor_data$vMatCol <- ref2[,2]
vor_data$vMatCoor <- ref
vor_data$cells <- avg_voronoi_ref_NA(grid_calls,ref2)[[4]]
vor_data$dens <- avg_voronoi_ref_NA(grid_senComPop,ref2)[[2]]
vor_data$dens2 <- avg_voronoi_ref_NA(grid_senComDens,ref2)[[2]]
vor_data$night <- avg_voronoi_ref_NA(nightlight,ref2)[[2]]
vor_data$nightp <- avg_voronoi_ref_NA(nightlightxpct,ref2)[[2]]
vor_data$texts <- avg_voronoi_ref_NA(grid_texts,ref2)[[2]]
vor_data$calls <- avg_voronoi_ref_NA(grid_calls,ref2)[[2]]
vor_data$length <- avg_voronoi_ref_NA(grid_length,ref2)[[2]]
vor_data$elec <- avg_voronoi_ref_NA(grid_senComElec,ref2)[[2]]

for(i in 1:nrow(vor_data)){
  if(vor_data$dens2[i] == 0 & vor_data$dens[i] > 1000){
    vor_data$dens2[i] <- vor_data$dens[i]
  }
}


#########################
##### Temporal data #####
#########################


#####--------------
##### Senelec data
#####--------------


for(i in 1:12){
  assign(paste("senelec",i,sep=""),read.csv(paste(folderin,"senelec/DemandSummary2013_",i,".csv",sep="")))
}

i=12
colnames(senelec12) <- c("hour",as.character(i*100+(1:(ncol(get(paste("senelec",i,sep="")))-1))))

senelec <- cbind(senelec1,senelec2[,2:ncol(senelec2)],senelec3[,2:ncol(senelec3)],
                 senelec4[,2:ncol(senelec4)],senelec5[,2:ncol(senelec5)],senelec6[,2:ncol(senelec6)],
                 senelec7[,2:ncol(senelec7)],senelec8[,2:ncol(senelec8)],senelec9[,2:ncol(senelec9)],
                 senelec10[,2:ncol(senelec10)],senelec11[,2:ncol(senelec11)],senelec12[,2:ncol(senelec12)])

plot(1:24,senelec[,2],pch="",ylab="MWh",ylim=c(0,500))
for(i in sample(1:365,365)){
  lines(1:24,senelec[,i+1],col=alpha(palettespe[refyear[i]],0.4))
}

senelec_yearavg <- rep(0,24)
for(i in 1:24){
  senelec_yearavg[i] <- mean(as.numeric(senelec[i,2:366]))
}
plot(1:24,senelec_yearavg)
lines(1:24,senelec_yearavg)


#####-------------------
##### Mobile phone data
#####-------------------


i=12
assign(paste("tst",i,sep="_"),hourlyText(i,folderin))

i=12
assign(paste("tsv",i,sep="_"),hourlyVoice(i,folderin))

tst_y <- rbind(tst_1,tst_2,tst_3,tst_4,tst_5,tst_6,
               tst_7,tst_8,tst_9,tst_10,tst_11,tst_12)

tsv_y <- rbind(tsv_1,tsv_2,tsv_3,tsv_4,tsv_5,tsv_6,
               tsv_7,tsv_8,tsv_9,tsv_10,tsv_11,tsv_12)



