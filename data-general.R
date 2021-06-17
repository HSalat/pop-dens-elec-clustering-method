library(rgdal)
### Required data:
###   <individus.csv> and <habitat.csv> are extracts from the 2013 Census, see data section of the general documentation
###   <Senegal_Communes_552.shp> is a shapefile of Senegal at Commune level (522 entities according to the post DEC 2013 definitions)
###   <popelec.csv> contains population density and electrification rates at Commune level estimated from the Census by Georges Vivien Houngbonon (ghoungbonon@ifc.org)
###   <SITE_ARR_LONLAT.csv> contains coordinates of all antenna sites in Senegal. Request access from Orange/Sonatel

folderin <- "data/"

gridSenegal <- c(-17.54319,-11.34247,12.30786,16.69207,744,527) # coordinates of a bounding box around Senegal
monthslist <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")


#######################################################
##### Spatial data in Voronoi cells around towers #####
#######################################################


#####-------------
##### Census data 
#####-------------


individus <- read.csv("/data/individus.csv",header = T, sep=";")
habitat <- read.csv("/data/habitat.csv",header = T, sep=";")
popelec <- read.csv("/data/popelec.csv")


#####------------------
##### Geographic data 1
#####------------------


# A gridded version of the data is produced to match the format of the satellite data.
# Note that this was replaced in later works by using raster tools directly

senCommune <- readOGR("Senegal_Communes_552.shp")

# visualisation
senCommune@data$rand <- sample(1:552) 

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune)

senComRand <- rasterize(senCommune, r, 'rand')
extent(senComRand) <- extent(senCommune)

plot(senComRand)
plot(senCommune,add=T)
text(senCommune@data$LONGITUDE,senCommune@data$LATITUDE,senCommune@data$COD_ENTITE)

# standardisation of Commune codes in popelec
for(i in 1:552){
  if(nchar(popelec$cacr[i]) < 8){
    popelec$cacr[i] <- paste("0",popelec$cacr[i],sep="")
  }
}

colnames(senCommune@data)[which(colnames(senCommune@data) == "pop")] <- "popOld"

senCommune@data$id <- 1:552
senCommune@data <- merge(senCommune@data,popelec,by.x="COD_ENTITE",by.y="cacr",sort=F)

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune)
senComPop <- rasterize(senCommune, r, 'density')

grid_senComPop <- as.matrix(senComPop)
grid_senComPop <- grid_senComPop[nrow(grid_senComPop):1,]

holesSenegal <- which(is.na(grid_senComPop))


#####--------------------------------------------------------------
##### Mobile phone data (texts, calls, length) and Reference tables
#####--------------------------------------------------------------


# Similarly, gridded versions of mobile phone activity are produced.
# This was also replaced by more usual GIS tools at a later stage

# Loading texts
i=12
assign(paste("sptt",i,sep="_"),monthlyText(i,folderin))

sptt_all <- cbind(sptt_1,sptt_2,sptt_3,sptt_4,sptt_5,sptt_6,
                  sptt_7,sptt_8,sptt_9,sptt_10,sptt_11,sptt_12)
sptt_all <- sptt_all[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)]
colnames(sptt_all) <- c("tower",monthslist)

sptt_all$year <- sptt_all$jan+sptt_all$feb+sptt_all$mar+sptt_all$apr+sptt_all$may+sptt_all$jun+sptt_all$jul+sptt_all$aug+sptt_all$sep+sptt_all$oct+sptt_all$nov+sptt_all$dec

# Loading calls
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
towerloc <- read.csv("data/SITE_ARR_LONLAT.csv",header = T, sep=",")

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

# Towers that fall within the same px in the grid are merged, this is recorded by a common 'tvId'
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


#####------------------
##### Geographic data 2 
#####------------------


senComPop2 <- rasterize(senCommune, r, 'COD_ENTITE')

senComPop2 <- rasterize(senCommune, r, 'density')

senComPop3 <- rasterize(senCommune, r, 'shelec')

extent(senComPop2) <- extent(senCommune)

grid_senComPop2 <- as.matrix(senComPop2)
grid_senComPop2 <- grid_senComPop2[nrow(grid_senComPop2):1,]
grid_senComPop2[which(is.na(grid_senComPop2))] <- 0
grid_senComPop2[which(is.na(grid_senComPop))] <- NA

vor_data$dens3 <- avg_voronoi_ref_NA(grid_senComPop2,ref2)[[2]]

subset <- setdiff(1:1298,which(vor_data$texts == 0))
subset <- setdiff(subset,which(vor_data$dens3 == 0))


#####---------------------------------------
##### Nighttime lights data (night, nightp) 
#####---------------------------------------


imported_raster <- raster("/data/F182013.v4c.avg_lights_x_pct.tif")
subset <- crop(imported_raster,extent(senCommune))
nightlightxpct <- as.matrix(subset)
nightlightxpct <- nightlightxpct[nrow(nightlightxpct):1,]

imported_raster2 <- raster("/data/F182013.v4c_web.stable_lights.avg_vis.tif")
subset2 <- crop(imported_raster2,extent(senCommune))
nightlight <- as.matrix(subset2)
nightlight <- nightlight[nrow(nightlight):1,]

nightlightxpct[which(is.na(grid_senComPop))] <- NA
nightlight[which(is.na(grid_senComPop))] <- NA


#####------------------
##### Geographic data 3 
#####------------------


# Final aligned grids for main parameters

habitat3 <- aggregate(habitat,by=list(habitat$Code),FUN="mean")
senCommune@data <- merge(senCommune@data,habitat3,by.x="CC_4",by.y="Group.1",all.x=T)

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


##########################
##### Discarded work #####
##########################


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


#####--------------
##### Temporal data
#####--------------


##### Senelec data

# Data not provided

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

##### Mobile phone data

i=12
assign(paste("tst",i,sep="_"),hourlyText(i,folderin))

i=12
assign(paste("tsv",i,sep="_"),hourlyVoice(i,folderin))

tst_y <- rbind(tst_1,tst_2,tst_3,tst_4,tst_5,tst_6,
               tst_7,tst_8,tst_9,tst_10,tst_11,tst_12)

tsv_y <- rbind(tsv_1,tsv_2,tsv_3,tsv_4,tsv_5,tsv_6,
               tsv_7,tsv_8,tsv_9,tsv_10,tsv_11,tsv_12)
