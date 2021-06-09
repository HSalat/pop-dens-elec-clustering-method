



senCommune <- readOGR("Senegal_Communes_552.shp")
popelec <- read.csv("popelec.csv")

for(i in 1:552){
  if(nchar(popelec$cacr[i]) == 7){
    popelec$cacr[i] <- paste("0",popelec$cacr[i],sep="")
  }
}

ref <- data.frame(cacr=senCommune@data$COD_ENTITE)
test <- merge(ref,popelec,sort=F)
senCommune@data$pop <- test$popsize
senCommune@data$density <- senCommune@data$pop/senCommune@data$SUPERFICIE

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune)
senComPop <- rasterize(senCommune,r,"density")

plot(senComPop)
plot(senCommune,add=T)

grid_senComPop <- as.matrix(senComPop)
grid_senComPop <- grid_senComPop[nrow(grid_senComPop):1,]

meltedgrid <- melt(grid_senComPopProper)
ggplot(meltedgrid, aes(x = Var2, y = Var1, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4")

vor_data$dens2 <- avg_voronoi_ref_NA(grid_senComPopProper,ref2)[[2]]


cor(vor_data$dens2,vor_data$night)^2
cor(vor_data$dens2,vor_data$texts)^2
cor(vor_data$dens2,vor_data$calls)^2
cor(vor_data$dens2,vor_data$length)^2

subset <- setdiff(1:1298,which(vor_data$night == 0))
subset <- setdiff(subset,which(vor_data$dens == 0))
subset <- setdiff(subset,which(vor_data$dens2 == 0))

cor(log(vor_data$dens2[subset]),log(vor_data$night[subset]))^2
cor(log(vor_data$dens2[subset]),log(vor_data$texts[subset]))^2
cor(log(vor_data$dens2[subset]),log(vor_data$calls[subset]))^2
cor(log(vor_data$dens2[subset]),log(vor_data$length[subset]))^2


subsetC2 <- intersect(subset,which(vor_data$dens2 > 5000))
subsetV2 <- intersect(subset,which(vor_data$dens2 <= 5000))

cor(vor_data$dens2[subsetC2],vor_data$texts[subsetC2])^2
cor(vor_data$dens2[subsetC2],vor_data$calls[subsetC2])^2
cor(vor_data$dens2[subsetC2],vor_data$length[subsetC2])^2
cor(vor_data$dens2[subsetC2],vor_data$night[subsetC2])^2

cor(log(vor_data$dens2[subsetC2]),log(vor_data$night[subsetC2]))^2
cor(log(vor_data$dens2[subsetC2]),log(vor_data$texts[subsetC2]))^2
cor(log(vor_data$dens2[subsetC2]),log(vor_data$calls[subsetC2]))^2
cor(log(vor_data$dens2[subsetC2]),log(vor_data$length[subsetC2]))^2

cor(vor_data$dens2[subsetV2],vor_data$night[subsetV2])^2
cor(vor_data$dens2[subsetV2],vor_data$texts[subsetV2])^2
cor(vor_data$dens2[subsetV2],vor_data$calls[subsetV2])^2
cor(vor_data$dens2[subsetV2],vor_data$length[subsetV2])^2

cor(log(vor_data$dens2[subsetV2]),log(vor_data$night[subsetV2]))^2
cor(log(vor_data$dens2[subsetV2]),log(vor_data$texts[subsetV2]))^2
cor(log(vor_data$dens2[subsetV2]),log(vor_data$calls[subsetV2]))^2
cor(log(vor_data$dens2[subsetV2]),log(vor_data$length[subsetV2]))^2


cor(vor_data$night,vor_data$texts)^2
cor(vor_data$night,vor_data$calls)^2
cor(vor_data$night,vor_data$length)^2
cor(vor_data$night[subsetC2],vor_data$texts[subsetC2])^2
cor(vor_data$night[subsetC2],vor_data$calls[subsetC2])^2
cor(vor_data$night[subsetC2],vor_data$length[subsetC2])^2
cor(vor_data$night[subsetV2],vor_data$texts[subsetV2])^2
cor(vor_data$night[subsetV2],vor_data$calls[subsetV2])^2
cor(vor_data$night[subsetV2],vor_data$length[subsetV2])^2

cor(log(vor_data$night[subset]),log(vor_data$texts[subset]))^2
cor(log(vor_data$night[subset]),log(vor_data$calls[subset]))^2
cor(log(vor_data$night[subset]),log(vor_data$length[subset]))^2
cor(log(vor_data$night[subsetC2]),log(vor_data$texts[subsetC2]))^2
cor(log(vor_data$night[subsetC2]),log(vor_data$calls[subsetC2]))^2
cor(log(vor_data$night[subsetC2]),log(vor_data$length[subsetC2]))^2
cor(log(vor_data$night[subsetV2]),log(vor_data$texts[subsetV2]))^2
cor(log(vor_data$night[subsetV2]),log(vor_data$calls[subsetV2]))^2
cor(log(vor_data$night[subsetV2]),log(vor_data$length[subsetV2]))^2


rsqP(night,texts,dens2,subset)
rsqP(night,texts,dens,subset)

attach(vor_data)

temp <- glm(night[subset] ~ log(texts[subset]) + log(dens[subset]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*texts^temp$coefficients[2]*dens^temp$coefficients[3])^2


temp <- glm(night[subset] ~ log(texts[subset]) + log(dens2[subset]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*texts^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subset],exp(temp$coefficients[1])*texts[subset]^temp$coefficients[2]*dens2[subset]^temp$coefficients[3])^2

predicted <- exp(temp$coefficients[1])*texts[subset]^temp$coefficients[2]*dens2[subset]^temp$coefficients[3]
predicted[which(predicted > 63)] <- 63
cor(night[subset],predicted)^2

temp <- glm(night[subset] ~ log(calls[subset]) + log(dens2[subset]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*calls^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subset],exp(temp$coefficients[1])*calls[subset]^temp$coefficients[2]*dens2[subset]^temp$coefficients[3])^2

predicted <- exp(temp$coefficients[1])*calls[subset]^temp$coefficients[2]*dens2[subset]^temp$coefficients[3]
predicted[which(predicted > 63)] <- 63
cor(night[subset],predicted)^2

temp <- glm(night[subset] ~ log(length[subset]) + log(dens2[subset]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*length^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subset],exp(temp$coefficients[1])*length[subset]^temp$coefficients[2]*dens2[subset]^temp$coefficients[3])^2

predicted <- exp(temp$coefficients[1])*length^temp$coefficients[2]*dens2^temp$coefficients[3]
predicted[which(predicted > 63)] <- 63
cor(night[subset],predicted)^2


vor_data$predicted <- predicted




temp <- glm(night[subsetC2] ~ log(texts[subsetC2]) + log(dens2[subsetC2]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*texts^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subsetC2],exp(temp$coefficients[1])*texts[subsetC2]^temp$coefficients[2]*dens2[subsetC2]^temp$coefficients[3])^2

temp <- glm(night[subsetC2] ~ log(calls[subsetC2]) + log(dens2[subsetC2]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*calls^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subsetC2],exp(temp$coefficients[1])*calls[subsetC2]^temp$coefficients[2]*dens2[subsetC2]^temp$coefficients[3])^2

temp <- glm(night[subsetC2] ~ log(length[subsetC2]) + log(dens2[subsetC2]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*length^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subsetC2],exp(temp$coefficients[1])*length[subsetC2]^temp$coefficients[2]*dens2[subsetC2]^temp$coefficients[3])^2


temp <- glm(night[subsetV2] ~ log(texts[subsetV2]) + log(dens2[subsetV2]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*texts^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subsetV2],exp(temp$coefficients[1])*texts[subsetV2]^temp$coefficients[2]*dens2[subsetV2]^temp$coefficients[3])^2

temp <- glm(night[subsetV2] ~ log(calls[subsetV2]) + log(dens2[subsetV2]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*calls^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subsetV2],exp(temp$coefficients[1])*calls[subsetV2]^temp$coefficients[2]*dens2[subsetV2]^temp$coefficients[3])^2

temp <- glm(night[subsetV2] ~ log(length[subsetV2]) + log(dens2[subsetV2]),family = poisson(link = "log"),na.action = na.exclude)
cor(night,exp(temp$coefficients[1])*length^temp$coefficients[2]*dens2^temp$coefficients[3])^2
cor(night[subsetV2],exp(temp$coefficients[1])*length[subsetV2]^temp$coefficients[2]*dens2[subsetV2]^temp$coefficients[3])^2


length(subsetC2)


rsq <- function(a,b,subset=1:length(a),l=1){
  if(l==1){
    return(cor(a[subset],b[subset])^2)
  } else{
    return(cor(log(a[subset]),log(b[subset]))^2)
  }
}

rsqP <- function(a,b,c=NULL,subset=1:length(a)){
  if(!is.null(c)){
    temp <- glm(a[subset] ~ log(b[subset]) + log(c[subset]),family = poisson(link = "log"),na.action = na.exclude)
    rsq(a,exp(temp$coefficients[1])*b^temp$coefficients[2]*c^temp$coefficients[3])
  }else{
    temp <- glm(a[subset] ~ log(b[subset]),family = poisson(link = "log"),na.action = na.exclude)
    rsq(a,exp(temp$coefficients[1])*b^temp$coefficients[2])
  }
}






##########################################


write.dbf(senCommune@data,"Senegal_Communes_552.dbf")

head(senCommuneTemp@data)

senCommuneTemp@data$CODE433 <- lookUpTable[,2]

View(senCommuneTemp@data)

write.dbf(senCommuneTemp@data,"test4-2.dbf")

senCommuneProper <- readOGR("test4.shp")
plot(senCommuneProper)

View(senCommuneProper@data)

proj4string(senCommuneProper)


test1 <- readOGR("Senegal_Communes_The_Definitive_Edition/Senegal_Communes_552.shp")
test2 <- readOGR("Senegal_Communes_The_Definitive_Edition/Senegal_Communes_433.shp")

proj4string(test2)
extent(test2)



diff <- (area(test1)/1000000-test1@data$SUPERFICIE)/test1@data$SUPERFICIE
plot(1:552,diff)




senCommune <- readOGR("test.shp")


diff <- (area(senCommune)/1000000-senCommune@data$SUPERFICIE)/senCommune@data$SUPERFICIE
plot(1:552,diff)
plot(1:552,diff,ylim=c(-1,1))


senCommuneProper@data <- merge(senCommuneProper@data,popelec,by.x="COD_ENTITE",by.y="cacr",sort=F)
senCommuneProper@data$density <- senCommuneProper@data$popsize/senCommuneProper@data$SUPERFICIE

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommuneProper)
senComPopProper <- rasterize(senCommuneProper, r, 'density')
plot(senComPopProper)




grid_senComPopProper <- as.matrix(senComPop)
grid_senComPopProper <- grid_senComPopProper[nrow(grid_senComPopProper):1,]
grid_senComPopProper[which(is.na(grid_senComPopProper))] <- 0
grid_senComPopProper[which(is.na(grid_senComPop))] <- NA

meltedgrid <- melt(grid_senComPopProper)
ggplot(meltedgrid, aes(x = Var2, y = Var1, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4")

vor_data$dens3 <- avg_voronoi_ref_NA(grid_senComPopProper,ref2)[[2]]

cor(vor_data$dens,vor_data$night)^2
cor(vor_data$dens3,vor_data$night)^2

subset <- setdiff(1:1298,which(vor_data$night == 0))
subset <- setdiff(subset,which(vor_data$dens == 0))
subset <- setdiff(subset,which(vor_data$dens3 == 0))

cor(log(vor_data$dens[subset]),log(vor_data$night[subset]))^2
cor(log(vor_data$dens3[subset]),log(vor_data$night[subset]))^2














senCommuneTest@data$area_sqkm <- area(senCommuneTest) / 1000000

senCommuneTest@data$diff <- senCommuneTest@data$area_sqkm-senCommuneTest@data$SUPERFICIE


plot(1:552,senCommuneTest@data$diff,ylim=c(-1,1))


View(senCommuneTest@data)










senCommune2 <- readOGR("shape2.shp")




plot(senCommune2)

View(senCommune2@data)


extent(senCommune)
extent(senCommune2)

summary(senCommune)
summary(senCommune2)




extentref <- extent(c(-17.53319,-11.33247,12.31786,16.70207))


range(senCommune2$LONGITUDE)



senCommune <- readOGR("gadm36_SEN_4.shp")
plot(senCommune)


proj4string(senCommune)
proj4string(senCommuneProper)


cents <- coordinates(senCommuneProper)

dat <- data.frame(long = cents[,1], lat = cents[,2], name = senCommuneProper@data$COD_ENTITE)
coordinates(dat) <- ~ long + lat
proj4string(dat) <- proj4string(senCommuneProper)

test <- over(dat,senCommune)

View(test)
View(senCommune@data)

length(unique(test$CC_45))
length(unique(test$COD_ENTITE))


length(unique(lookUpTable$com552))
length(unique(lookUpTable$com433))

setdiff(senCommune@data$CC_4,lookUpTable$com433)

lookUpTable <- data.frame(com552 = senCommuneProper@data$COD_ENTITE, com433 = test$CC_4)

lookUpTable[,1] <- as.character(lookUpTable[,1])
lookUpTable[,2] <- as.character(lookUpTable[,2])

lookUpTable[129,2] <- lookUpTable[129,1]
lookUpTable[8,2] <- substr(lookUpTable[8,1],2,8)
lookUpTable[326,2] <- substr(lookUpTable[326,1],2,8)


count <- rep(0,552)
for(i in 1:552){
  temp <- senCommuneProper@data$COD_ENTITE[i]
  for(j in 1:552){
    if(senCommuneProper@data$COD_ENTITE[j]==temp){
      count[i] <- count[i]+1
    }
  }
}


names <- as.character(senCommuneProper@data$COD_ENTITE)
for(i in 1:552){
  if(substr(names[i],1,1) == "0"){
    names[i] <- substr(names[i],2,8)
  }
}


length(setdiff(unique(names),unique(lookUpTable$com433)))

length(which(names != lookUpTable$com433))


write.csv(lookUpTable,"lookUpTable.csv",row.names = F)

552-433

length(unique(names))


View(senCommune@data)



senCommune2 <- readOGR("Limite_Commune_Senegal.shp")
senCommune2 <- spTransform(senCommune2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

proj4string(senCommune2)

r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune2)

senCommune2@data$rand <- sample(1:552)

senComPop2 <- rasterize(senCommune2, r, 'rand')
#extent(senComPop2) <- extent(senCommune)
#extent(senComPop2) <- extentref

plot(senCommune2,col=4)
plot(senCommune,add=T)

extent(senCommuneProper)
extent(senCommune)

plot(senCommune2,xlim=c(-17.6,-16.5),ylim=c(14.5,15),col=4)
plot(senCommune)

senCommune2@data <- merge(senCommune2@data,popelec,by.x="COD_ENTITE",by.y="cacr",sort=F)
senCommune2@data$density <- senCommune2@data$popsize/senCommune2@data$SUPERFICIE

senCommune2@data$area_sqkm <- area(senCommune2) / 1000000

sum(senCommune2@data$popsize)
sum(senCommune2@data$density*senCommune2@data$area_sqkm)
sum(senCommune2@data$density*senCommune2@data$SUPERFICIE)

plot(senCommune2@data$area_sqkm,senCommune2@data$SUPERFICIE)


sum(senCommune2@data$area_sqkm)

plot(1:552,senCommune2@data$diff)
lines(c(0,512),c(-50,-50))
lines(c(0,512),c(50,50))

senCommune2@data$diff <- senCommune2@data$SUPERFICIE-senCommune2@data$area_sqkm


senCommune2.subset <- senCommune2[senCommune2@data$diff < 50 & senCommune2@data$diff > -50,]
plot(senCommune2.subset)

View(senCommune2.subset@data)


senCommune2.subset2 <- senCommune2[!(senCommune2@data$diff < 50 & senCommune2@data$diff > -50),]
plot(senCommune2.subset2)



temp <- habitat$IDDR
temp2 <- rep("0",length(temp))

nchar("zzzz")

range(nchar(temp))


for(i in 1:length(temp)){
  if(nchar(temp[i]) == 11){
    temp2[i] <- paste("0",substr(temp[i],1,7),sep="")
  }else{
      temp2[i] <- substr(temp[i],1,8)
    }
}

length(unique(temp2))

senCommune2@data$COD_ENTITE[which(!(senCommune2@data$diff < 50 & senCommune2@data$diff > -50))]



686*515
744*525

2151*196889

mean(senCommune2@data$density)

senComPop2 <- rasterize(senCommune2, r, 'density')
senComPop2 <- rasterize(senCommune2.subset, r, 'density')

grid_senComPop2 <- as.matrix(senComPop2)
grid_senComPop2 <- grid_senComPop2[nrow(grid_senComPop2):1,]
grid_senComPop2[which(is.na(grid_senComPop2))] <- 0
grid_senComPop2[which(is.na(grid_senComPop))] <- NA


meltedgrid <- melt(grid_senComPop2)
ggplot(meltedgrid, aes(x = Var2, y = Var1, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4")


vor_data$dens3 <- avg_voronoi_ref_NA(grid_senComPop2,ref2)[[2]]

grid <- grid_senComPop2
ref <- ref2

avg_voronoi_ref_NA <- function(grid,ref){
  result <- matrix(NA,nrow=nrow(grid),ncol=ncol(grid))
  whichm <- matrix(0,nrow=nrow(grid),ncol=ncol(grid))
  meanr <- rep(0,nrow(ref))
  sumr <- rep(0,nrow(ref))
  countr <- rep(0,nrow(ref))
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      whichm[i,j] <- voronoi(i,j,ref)[1]
    }
  }
  one <- matrix(NA,nrow=nrow(grid),ncol=ncol(grid))
  one[which(!is.na(grid))] <- 1
  for(i in 1:nrow(ref)){
    meanr[i] <- mean(grid[which(whichm == i)],na.rm=T)
    sumr[i] <- sum(grid[which(whichm == i)],na.rm=T)
    countr[i] <- sum(one[which(whichm == i)],na.rm=T)
  }
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      if(!is.na(grid[i,j]))
        result[i,j] <- meanr[whichm[i,j]]
    }
  }
  return(list(result,meanr,sumr,countr))
}

meltedgrid <- melt(result)

sum(meanr*countr)/4

sum(countr)

mean(meanr)


plot(1:1298,vor_data$dens)
points(1:1298,vor_data$dens2,pch=2)
points(1:1298,vor_data$dens3,pch=3)
for(i in 1:1298){
  lines(c(i,i),c(vor_data$dens[i],vor_data$dens3[i]))
}

cor(vor_data$dens,vor_data$night)^2
cor(vor_data$dens3,vor_data$night)^2

subset <- setdiff(1:1298,which(vor_data$night == 0))
subset <- setdiff(subset,which(vor_data$dens == 0))
subset <- setdiff(subset,which(vor_data$dens3 == 0))

cor(log(vor_data$dens[subset]),log(vor_data$night[subset]))^2
cor(log(vor_data$dens3[subset]),log(vor_data$night[subset]))^2


sum(vor_data$dens3*vor_data$cells)
sum(vor_data$dens*vor_data$cells)

View(senCommune2@data)









