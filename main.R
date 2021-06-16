library(rgdal)

source(functions.R)
source(data-general.R)

attach(vor_data)


### Required data:

###   <comData.csv> data frame with population density and nighttime lights intensity at Commune level
###   <vorData.csv> same information as <comData.csv> at Voronoi level around each of the active antenna sites (trimmed to 1298 from 1666 sites) and mobile phone activity


vor_data <- read.csv("vorData.csv")


# standardisation of Commune codes
for(i in 1:552){
  if(nchar(popelec$cacr[i]) == 7){
    popelec$cacr[i] <- paste("0",popelec$cacr[i],sep="")
  }
}

# data integrated to shp
ref <- data.frame(cacr=senCommune@data$COD_ENTITE)
test <- merge(ref,popelec,sort=F)
senCommune@data$pop <- test$popsize
senCommune@data$density <- senCommune@data$pop/senCommune@data$SUPERFICIE

# visualisation
r <- raster(ncol=744, nrow=527)
extent(r) <- extent(senCommune)
senComPop <- rasterize(senCommune,r,"density")

plot(senComPop) # colour map by pop density
plot(senCommune,add=T) # boundaries

# alternative
# grid_senComPop <- as.matrix(senComPop)
# grid_senComPop <- grid_senComPop[nrow(grid_senComPop):1,]
# meltedgrid <- melt(grid_senComPop)
# ggplot(meltedgrid, aes(x = Var2, y = Var1, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4")






vor_data$dens2 <- avg_voronoi_ref_NA(grid_senComPop,ref2)[[2]]
dens2 <- vor_data$dens2

subset <- setdiff(1:1298,which(night == 0))
subset <- setdiff(subset,which(dens == 0))
subset <- setdiff(subset,which(dens2 == 0))
subsetC2 <- intersect(subset,which(vor_data$dens2 > 5000)) # outdated definition of cities (reduced to 1000 in later work)
subsetV2 <- intersect(subset,which(vor_data$dens2 <= 5000)) # outdated definition of rural villages

cor(dens2,night)^2
cor(dens2,texts)^2
cor(dens2,calls)^2
cor(dens2,length)^2
cor(log(dens2[subset]),log(night[subset]))^2
cor(log(dens2[subset]),log(texts[subset]))^2
cor(log(dens2[subset]),log(calls[subset]))^2
cor(log(dens2[subset]),log(length[subset]))^2
cor(dens2[subsetC2],texts[subsetC2])^2
cor(dens2[subsetC2],calls[subsetC2])^2
cor(dens2[subsetC2],length[subsetC2])^2
cor(dens2[subsetC2],night[subsetC2])^2
cor(dens2[subsetV2],night[subsetV2])^2
cor(dens2[subsetV2],texts[subsetV2])^2
cor(dens2[subsetV2],calls[subsetV2])^2
cor(dens2[subsetV2],length[subsetV2])^2
cor(night,texts)^2
cor(night,calls)^2
cor(night,length)^2
cor(night[subsetC2],texts[subsetC2])^2
cor(night[subsetC2],calls[subsetC2])^2
cor(night[subsetC2],length[subsetC2])^2
cor(night[subsetV2],texts[subsetV2])^2
cor(night[subsetV2],calls[subsetV2])^2
cor(night[subsetV2],length[subsetV2])^2

rsqP(night,texts,dens2,subset)
rsqP(night,texts,dens,subset)


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










##########################################






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


senCommune2@data$COD_ENTITE[which(!(senCommune2@data$diff < 50 & senCommune2@data$diff > -50))]


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


library(igraph)
library(ggplot2)
library(scales)
library(reshape2)
library(stats)
library(tseries)
library(moments)
library(rgdal)

source("C:/Users/Hadrien/Desktop/PersonalToolbox.R")

palette <- c("steelblue4","slateblue3","palevioletred3","lightcoral","orange","chartreuse2","aquamarine4","turquoise3","royalblue1")
palettespe <- c("tomato4","tomato3","tomato1",
                "khaki1","khaki2","khaki3",
                "palegreen3","palegreen4","seagreen4",
                "slateblue1","slateblue3","slateblue4")
palettespe2 <- c("tomato4","tomato3","tomato2","tomato1",
                 "khaki2","khaki3","palegreen3","palegreen4",
                 "slateblue1","slateblue2","slateblue3","slateblue4")

folderin <- "/Users/TRCQ7387/Desktop/Data/"
folderout <- "/Users/TRCQ7387/Desktop/Results/"

i=1
towerloc <- read.csv("tower.csv")
towerloc <- towerloc[order(towerloc$tId),]
row.names(towerloc) <- 1:1666
D_jan <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)

country <- c(1,10,1,10,10,10)
X <- rep(1:10,10)
Y <- floor((0:99)/10)+1
Z <- rep(1,100)

griddingNation(X,Y,Z,country,holes=NULL,fun="mean")


compl <- read.csv("SI-dataset_one.csv")
vorData$densOld <- vorData$dens
vorData$dens <- compl$dens

vorData <- towerloc
vorData <- vorData[which(vorData$tvId>0),]
vorData <- data.frame(Id=vorData$tvId,long=vorData$tLong,lat=vorData$tLat,dens=vorData$dens,nightlight=vorData$night,
                      Ntexts=vorData$texts,Ncalls=vorData$calls,length=vorData$length)
vorData <- aggregate(vorData, by=list(vorData$Id), mean)
vorData <- vorData[,-1]
write.table(vorData,"SI-Data1.csv")


dataV2 <- dataV

dataV2 <- dataV2[which(dataV2$vorId>0),]
dataV2 <- dataV2[order(dataV2$V1),]
row.names(dataV2) <- 1:12344151
dataV2 <- dataV2[1:1021052,]

tsData <- data.frame(Id=dataV2$vorId,date=dataV2$V1,Ncalls=dataV2$V4)
write.table(tsData,"SI-Data2.csv")



#################################
##### Building the networks #####
#################################

#
# Outputs:
#    G_jan     = full network
#    G_jan2    = no self-loops
#    G_jan_sub = only edges with Texts > 1000
#    lay       = nodes coordinates
#

# Geographical layout
X <- floor((18+towerloc$tLong)*1000)
Y <- floor((towerloc$tLat-12)*1000)
lay <- matrix(0,ncol=2,nrow=1666)
lay[,1] <- X
lay[,2] <- Y


X <- towerloc$tLong
Y <- towerloc$tLat
lay2 <- matrix(0,ncol=2,nrow=1666)
lay2[,1] <- X
lay2[,2] <- Y



# Full network
jan <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(D_jan)){
  jan[D_jan$V2[i],D_jan$V3[i]] <- jan[D_jan$V2[i],D_jan$V3[i]] + D_jan$V4[i]
}
G_jan <- graph_from_adjacency_matrix(jan,mode="directed",weighted=T)

# No self-loop
jan2 <- jan
diag(jan2) <- 0
G_jan2 <- graph_from_adjacency_matrix(jan2,mode="directed",weighted=T)

# Physical distance as weights
jan3 <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:1666){
  for(j in 1:1666){
    jan3[i,j] <- sqrt((X[i]-X[j])^2+(Y[i]-Y[j])^2)
  }
}
G_jan_dist <- graph_from_adjacency_matrix(jan2,mode="undirected",weighted=T)

# Sub-network (texts > 1000, keep all vertices)
G_jan_sub <- subgraph.edges(graph=G_jan, eids=which(floor(E(G_jan)$weight/1000)>0),delete.vertices=F)
#G_jan_sub <- induced.subgraph(graph=G_jan,vids=sample(V(G_jan),50,replace=F))


#################################
##### Plotting the networks #####
#################################


### Only edge if #Texts > 5000
g <- G_jan
g <- subgraph.edges(graph=g, eids=which(floor(E(g)$weight/5000)>0),delete.vertices=F)

l <- length(V(g))
v <- c(rep(2:0,l/3),2)

lab.locs <- radian.rescale(x=1:l, direction=-1, start=0)
plot(g, layout=layout.circle(g), vertex.size=1, vertex.label.dist=v*1.5+2,
     vertex.label.degree=lab.locs,vertex.label.cex=0.3,
     edge.arrow.size=0.1,
     edge.width=floor(E(g)$weight/5000)/10)

plot(g, layout=lay, vertex.size=1, #vertex.label.dist=v*1.5+2,
     vertex.label="",
     edge.arrow.size=0.1,
     edge.width=floor(E(g)$weight/5000)/10)

### No self-loops, #Texts > 2000
g <- G_jan2
g <- subgraph.edges(graph=g, eids=which(floor(E(g)$weight/2000)>0),delete.vertices=F)

l <- length(V(g))
v <- c(rep(2:0,l/3),2)

#lab.locs <- radian.rescale(x=1:l, direction=-1, start=0)
#plot(g, layout=layout.circle(g), vertex.size=1, vertex.label.dist=v*1.5+2,
#     vertex.label.degree=lab.locs,vertex.label.cex=0.3,
#     edge.arrow.size=0.1,
#     edge.width=floor(E(g)$weight/5000)/10)



sen <- readOGR(dsn = ".", layer = "SEN_outline")



plot(sen,col="cornsilk1")#,col="darkseagreen1")

plot(g, layout=lay2, vertex.size=4,vertex.color="black",
     vertex.label="",
     edge.arrow.size=0.1,
     edge.width=floor(E(g)$weight/2000)/10,
     add = TRUE,rescale = FALSE,edge.color="azure4")



# No vertex
plot(g, layout=lay, vertex.size=1, vertex.shape="none",
     vertex.label="",
     edge.arrow.size=0.1,
     edge.width=floor(E(g)$weight/1000)/10)


##############################################
##### Clustering features (full network) #####
##############################################


############ Centrality measures

# Betweenness
bet_uu <- betweenness(G_jan,directed=F,weights=rep(1,length(E(G_jan)$weight)),normalized=F)
bet_du <- betweenness(G_jan,directed=T,weights = rep(1,length(E(G_jan)$weight)),normalized=F)
bet_uw <- betweenness(G_jan,directed=F,weights = E(G_jan)$weight,normalized=F)
bet_dw <- betweenness(G_jan,directed=T,weights = E(G_jan)$weight,normalized=F)

plot(1:1666,sort(bet_uu,dec=T),pch="",ylim=c(0,1),ylab="Sorted betweenness")
lines(1:1666,sort(bet_uu,dec=T)/max(bet_uu),col=palettespe[1],lwd=2)
lines(1:1666,sort(bet_du,dec=T)/max(bet_du),col=palettespe[4],lwd=2)
lines(1:1666,sort(bet_uw,dec=T)/max(bet_uw),col=palettespe[7],lwd=2)
lines(1:1666,sort(bet_dw,dec=T)/max(bet_dw),col=palettespe[10],lwd=2)
legend("topright",c("X/X","Dir./X","X/weigh.","Dir./weigh."),lwd=c(2,2,2,2),col=palettespe[c(1,4,7,10)])

# Degree
deg_u <- degree(G_jan,mode="all",normalized=F)
deg_i <- degree(G_jan,mode="in",normalized=F)
deg_o <- degree(G_jan,mode="out",normalized=F)
rdeg <- deg_i/(deg_i+deg_o)
rdeg[which(is.na(rdeg))] <- 0

plot(1:1666,sort(deg_u,dec=T),pch="",ylim=c(0,1),ylab="Sorted degree")
lines(1:1666,sort(deg_u,dec=T)/max(deg_u),col=palettespe[1],lwd=2)
lines(1:1666,sort(deg_i,dec=T)/max(deg_i),col=palettespe[4],lwd=2)
lines(1:1666,sort(deg_o,dec=T)/max(deg_o),col=palettespe[7],lwd=2)
lines(1:1666,sort(rdeg,dec=T),col=palettespe[10],lwd=2)
legend("bottomleft",c("X","In","Out","Ratio"),lwd=c(2,2,2,2),col=palettespe[c(1,4,7,10)])

# Katz
alpha <- alpha_centrality(G_jan,nodes=V(G_jan),alpha = 1,loops=T,exo=1,weights=E(G_jan)$weight,tol=1e-07,sparse=T)

############ Self-loops/rest

rSelfLoop <- rep(0,nrow(jan))
for(i in 1:nrow(jan)){
  if(sum(jan[i,]) > 0){
    rSelfLoop[i] <- jan[i,i]/sum(jan[i,])
  }
}

plot(1:1666,sort(rSelfLoop,dec=T),pch="",ylim=c(0,1),ylab="Self-Loop/Total")
lines(1:1666,sort(rSelfLoop,dec=T),col=1,lwd=2)


############ Largest component

# Belong to largest component
is_connected(g)
compJan <- components(G_jan,mode="weak")
varBMC <- compJan$membership
varBMC[which(varBMC>1)] <- 0

# Selection of largest component
ref <- which(varBMC==1)
g <- induced.subgraph(graph=G_jan,vids=ref)

# Closeness
clos_uu <- closeness(g,mode="all",weights=rep(1,length(E(g)$weight)),normalized=F)
clos_iu <- closeness(g,mode="in",weights = rep(1,length(E(g)$weight)),normalized=F)
clos_ou <- closeness(g,mode="out",weights = rep(1,length(E(g)$weight)),normalized=F)
clos_uw <- closeness(g,mode="all",weights = E(g)$weight,normalized=F)
clos_iw <- closeness(g,mode="in",weights = E(g)$weight,normalized=F)
clos_ow <- closeness(g,mode="out",weights = E(g)$weight,normalized=F)

plot(1:1505,sort(clos_uu,dec=T),pch="",ylim=c(0,1),ylab="Sorted closeness")
lines(1:1505,sort(clos_uu,dec=T)/max(clos_uu),col=palettespe2[1],lwd=2)
lines(1:1505,sort(clos_iu,dec=T)/max(clos_iu),col=palettespe2[3],lwd=2)
lines(1:1505,sort(clos_ou,dec=T)/max(clos_ou),col=palettespe2[5],lwd=2)
lines(1:1505,sort(clos_uw,dec=T)/max(clos_uw),col=palettespe2[7],lwd=2)
lines(1:1505,sort(clos_iw,dec=T)/max(clos_iw),col=palettespe2[9],lwd=2)
lines(1:1505,sort(clos_ow,dec=T)/max(clos_ow),col=palettespe2[11],lwd=2)
legend("bottomleft",c("X/X","In/X","Out/X","X/weigh.","In/weigh.","Out/weigh."),lwd=c(2,2,2,2,2,2),col=palettespe2[c(1,3,5,7,9,11)])

closuu <- rep(0,1666)
closuw <- rep(0,1666)
closuu[ref] <- clos_uu/max(clos_uu)
closuw[ref] <- clos_uw/max(clos_uw)


############ Average (physical) distance

dist <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:1666){
  for(j in 1:1666){
    dist[i,j] <- sqrt((X[i]-X[j])^2+(Y[i]-Y[j])^2)
  }
}
sumdist <- dist*jan
di <- rep(0,1666)
for(i in 1:1666){
  di[i] <- (sum(sumdist[i,])+sum(sumdist[,i])-sumdist[i,i])/(sum(jan[i,])+sum(jan[,i])-jan[i,i])
}
length(which(is.na(di)))
di[which(is.na(di))] <- 0
di <- di/max(di)

plot(1:1666,di)


############ Summary
networkClustFeat <- data.frame(Id=1:1666)
networkClustFeat$Deg <- deg_u/max(deg_u)
networkClustFeat$rInOut <- rdeg
networkClustFeat$BetwUnw <- bet_uu/max(bet_uu)
networkClustFeat$BetwW <- bet_uw/max(bet_uw)
networkClustFeat$SelfLoop <- rSelfLoop
networkClustFeat$belongLC <- varBMC
networkClustFeat$closUnw <- closuu
networkClustFeat$closW <- closuw
networkClustFeat$avgd <- di
networkClustFeat$DENS <- towerloc$dens
networkClustFeat$DENS2 <- towerloc$dens2
networkClustFeat$NIGHT <- towerloc$night
networkClustFeat$ELEC <- towerloc$elec
networkClustFeat$vorID <- towerloc$tvId
networkClustFeat$X <- X
networkClustFeat$Y <- Y

write.csv(networkClustFeat,"featureData.csv")

# Visuals
temp <- c(1,3,4,6,7,9,10,12)
plot(1:1666,1:1666,ylim=c(0,1),xlim=c(0,2200),xlab="Tower Id",ylab="Feature",main="Independently sorted")
for(i in 2:ncol(networkClustFeat)){
  lines(1:1666,sort(networkClustFeat[,i],dec=T),lwd=2,col=palettespe[temp[i-1]])
}
legend("topright",colnames(networkClustFeat)[2:ncol(networkClustFeat)],
       lwd=rep(2,ncol(networkClustFeat)),col=palettespe[temp],cex=0.7)

ref <- as.numeric(rownames(networkClustFeat[order(networkClustFeat$closW),]))
plot(1:1666,1:1666,ylim=c(0,1),xlim=c(0,2200),xlab="Tower Id",ylab="Feature",main="Only closW sorted")
for(i in 2:ncol(networkClustFeat)){
  lines(1:1666,networkClustFeat[ref,i],lwd=2,col=palettespe[temp[i-1]])
}
legend("topright",colnames(networkClustFeat)[2:ncol(networkClustFeat)],
       lwd=rep(2,ncol(networkClustFeat)),col=palettespe[temp],cex=0.7)


#################################################
##### Clustering features (partial network) #####
#################################################


############ Centrality measures

# Betweenness
bet_uu2 <- betweenness(G_jan_sub,directed=F,weights=rep(1,length(E(G_jan_sub)$weight)),normalized=F)
#bet_du2 <- betweenness(G_jan_sub,directed=T,weights = rep(1,length(E(G_jan_sub)$weight)),normalized=F)
bet_uw2 <- betweenness(G_jan_sub,directed=F,weights = E(G_jan_sub)$weight,normalized=F)
#bet_dw2 <- betweenness(G_jan_sub,directed=T,weights = E(G_jan_sub)$weight,normalized=F)

plot(1:1666,sort(bet_uu2,dec=T),pch="",ylim=c(0,1),ylab="Sorted betweenness")
lines(1:1666,sort(bet_uu2,dec=T)/max(bet_uu2),col=palettespe[1],lwd=2)
#lines(1:1666,sort(bet_du2,dec=T)/max(bet_du2),col=palettespe[4],lwd=2)
lines(1:1666,sort(bet_uw2,dec=T)/max(bet_uw2),col=palettespe[7],lwd=2)
#lines(1:1666,sort(bet_dw2,dec=T)/max(bet_dw2),col=palettespe[10],lwd=2)
#legend("topright",c("X/X","Dir./X","X/weigh.","Dir./weigh."),lwd=c(2,2,2,2),col=palettespe[c(1,4,7,10)])

# Degree
deg_u2 <- degree(G_jan_sub,mode="all",normalized=F)
deg_i2 <- degree(G_jan_sub,mode="in",normalized=F)
deg_o2 <- degree(G_jan_sub,mode="out",normalized=F)
rdeg2 <- deg_i2/(deg_i2+deg_o2)
rdeg2[which(is.na(rdeg2))] <- 0

plot(1:1666,sort(deg_u2,dec=T),pch="",ylim=c(0,1),ylab="Sorted degree")
lines(1:1666,sort(deg_u2,dec=T)/max(deg_u2),col=palettespe[1],lwd=2)
lines(1:1666,sort(deg_i2,dec=T)/max(deg_i2),col=palettespe[4],lwd=2)
lines(1:1666,sort(deg_o2,dec=T)/max(deg_o2),col=palettespe[7],lwd=2)
lines(1:1666,sort(rdeg2,dec=T),col=palettespe[10],lwd=2)
legend("topright",c("X","In","Out","Ratio"),lwd=c(2,2,2,2),col=palettespe[c(1,4,7,10)])

# Katz
alpha <- alpha_centrality(G_jan_sub,nodes=V(G_jan_sub),alpha = 1,loops=T,exo=1,weights=E(G_jan_sub)$weight,tol=1e-07,sparse=T)


############ Self-loops/rest

jan_2 <- jan
jan_2[which(jan<=1000)] <- 0

rSelfLoop2 <- rep(0,nrow(jan_2))
for(i in 1:nrow(jan_2)){
  if(sum(jan_2[i,]) > 0){
    rSelfLoop2[i] <- jan_2[i,i]/sum(jan_2[i,])
  }
}

plot(1:1666,sort(rSelfLoop2,dec=T),pch="",ylim=c(0,1),ylab="Self-Loop/Total")
lines(1:1666,sort(rSelfLoop2,dec=T),col=1,lwd=2)


############ Largest component

# Belong to largest component
is_connected(g)
compJan2 <- components(G_jan_sub,mode="weak")
varBMC2 <- compJan2$membership
varBMC2[which(varBMC2!=2)] <- 0

# Selection of largest component
ref2 <- which(varBMC2==2)
g <- induced.subgraph(graph=G_jan_sub,vids=ref2)

# Closeness
clos_uu2 <- closeness(g,mode="all",weights=rep(1,length(E(g)$weight)),normalized=F)
#clos_iu2 <- closeness(g,mode="in",weights = rep(1,length(E(g)$weight)),normalized=F)
#clos_ou2 <- closeness(g,mode="out",weights = rep(1,length(E(g)$weight)),normalized=F)
clos_uw2 <- closeness(g,mode="all",weights = E(g)$weight,normalized=F)
#clos_iw2 <- closeness(g,mode="in",weights = E(g)$weight,normalized=F)
#clos_ow2 <- closeness(g,mode="out",weights = E(g)$weight,normalized=F)

plot(1:955,sort(clos_uu2,dec=T),pch="",ylim=c(0,1),ylab="Sorted closeness")
lines(1:955,sort(clos_uu2,dec=T)/max(clos_uu2),col=palettespe2[1],lwd=2)
#lines(1:1505,sort(clos_iu2,dec=T)/max(clos_iu2),col=palettespe2[3],lwd=2)
#lines(1:1505,sort(clos_ou2,dec=T)/max(clos_ou2),col=palettespe2[5],lwd=2)
lines(1:955,sort(clos_uw2,dec=T)/max(clos_uw2),col=palettespe2[7],lwd=2)
#lines(1:1505,sort(clos_iw2,dec=T)/max(clos_iw2),col=palettespe2[9],lwd=2)
#lines(1:1505,sort(clos_ow2,dec=T)/max(clos_ow2),col=palettespe2[11],lwd=2)
#legend("bottomleft",c("X/X","In/X","Out/X","X/weigh.","In/weigh.","Out/weigh."),lwd=c(2,2,2,2,2,2),col=palettespe2[c(1,3,5,7,9,11)])

closuu2 <- rep(0,1666)
closuw2 <- rep(0,1666)
closuu2[ref2] <- clos_uu2/max(clos_uu2)
closuw2[ref2] <- clos_uw2/max(clos_uw2)


############ Average (physical) distance

dist <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:1666){
  for(j in 1:1666){
    dist[i,j] <- sqrt((X[i]-X[j])^2+(Y[i]-Y[j])^2)
  }
}
sumdist2 <- dist*jan_2
di2 <- rep(0,1666)
for(i in 1:1666){
  di2[i] <- (sum(sumdist2[i,])+sum(sumdist2[,i])-sumdist2[i,i])/(sum(jan_2[i,])+sum(jan_2[,i])-jan_2[i,i])
}
length(which(is.na(di2)))
di2[which(is.na(di2))] <- 0
di2 <- di2/max(di2)

plot(1:1666,di2)


############ Summary
networkClustFeat2 <- data.frame(Id=1:1666)
networkClustFeat2$Deg <- deg_u2/max(deg_u2)
networkClustFeat2$rInOut <- rdeg2
networkClustFeat2$BetwUnw <- bet_uu2/max(bet_uu2)
networkClustFeat2$BetwW <- bet_uw2/max(bet_uw2)
networkClustFeat2$SelfLoop <- rSelfLoop2
networkClustFeat2$belongLC <- varBMC2/2
networkClustFeat2$closUnw <- closuu2
networkClustFeat2$closW <- closuw2
networkClustFeat2$avgd <- di2
networkClustFeat2$DENS <- towerloc$dens
networkClustFeat2$DENS2 <- towerloc$dens2
networkClustFeat2$NIGHT <- towerloc$night
networkClustFeat2$ELEC <- towerloc$elec
networkClustFeat2$vorID <- towerloc$tvId
networkClustFeat2$X <- X
networkClustFeat2$Y <- Y
write.csv(networkClustFeat2,"featureDataSub.csv")

# Visuals
temp <- c(1,3,4,6,7,9,10,12)
plot(1:1666,1:1666,ylim=c(0,1),xlim=c(0,2200),xlab="Tower Id",ylab="Feature",main="Independently sorted, T>1000")
for(i in 2:ncol(networkClustFeat2)){
  lines(1:1666,sort(networkClustFeat2[,i],dec=T),lwd=2,col=palettespe[temp[i-1]])
}
legend("topright",colnames(networkClustFeat2)[2:ncol(networkClustFeat2)],
       lwd=rep(2,ncol(networkClustFeat2)),col=palettespe[temp],cex=0.7)

ref <- as.numeric(rownames(networkClustFeat2[order(networkClustFeat2$closW),]))
plot(1:1666,1:1666,ylim=c(0,1),xlim=c(0,2200),xlab="Tower Id",ylab="Feature",main="Only closW sorted, T>1000")
for(i in 2:ncol(networkClustFeat2)){
  lines(1:1666,networkClustFeat2[ref,i],lwd=2,col=palettespe[temp[i-1]])
}
legend("topright",colnames(networkClustFeat2)[2:ncol(networkClustFeat2)],
       lwd=rep(2,ncol(networkClustFeat2)),col=palettespe[temp],cex=0.7)


###############################
##### Direct correlations #####
###############################


corVec <- rep(0,8)
for(i in 1:8){
  corVec[i] <- cor(networkClustFeat[which(!is.na(towerloc$dens)),i+1],towerloc$texts[which(!is.na(towerloc$dens))])^2
}

cor(towerloc$tTexts[which(!is.na(towerloc$dens))],towerloc$texts[which(!is.na(towerloc$dens))])^2

plot(1:8,corVec)


#####################
##### Functions #####
#####################


radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}



finger <- function(adj,hyp,opp){
  mean(asin(opp/hyp),acos(adj/hyp),atan(opp/adj))*180/pi
}

1-finger(2.9,3.2,1.3)/finger(2.9,3.6,2.1)

finger(3.6,4.85,3.3)
finger(3.9,4.7,2.6)
