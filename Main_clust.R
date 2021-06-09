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






