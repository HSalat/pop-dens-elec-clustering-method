
h <- hist(dataN$N[which(dataN$N < 2000)])

library(poweRlaw)

fit_power_law(h$counts)

h$counts

#################################
##### Building the networks #####
#################################



# Data building
ref <- data.frame(vorId=towerloc$tvId,towId=towerloc$tId)

i=1
dataN <- read.csv(paste(folderin,"Sonatel/SET1/SET1S_",i,".csv",sep=""),header = F)
dataN <- aggregate(V4~V2+V3, dataN, sum)
#dataN <- merge(dataN,ref,by.x="V2",by.y="towId",all.x = T)
#dataN <- dataN[which(!is.na(dataN$vorId)),]
dataN <- data.frame(O=dataN$V2,D=dataN$V3,N=dataN$V4)
#dataN <- merge(dataN,ref,by.x="D",by.y="towId",all.x = T)
#dataN <- dataN[which(!is.na(dataN$vorId)),]
#dataN <- data.frame(O=dataN$O,D=dataN$vorId,N=dataN$N)
dataN <- aggregate(N~O+D, dataN, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"Sonatel/SET1/SET1S_",i,".csv",sep=""),header = F)
  temp <- aggregate(V4~V2+V3, temp, sum)
#  temp <- merge(temp,ref,by.x="V2",by.y="towId",all.x = T)
#  temp <- temp[which(!is.na(temp$vorId)),]
  temp <- data.frame(O=temp$V2,D=temp$V3,N=temp$V4)
#  temp <- merge(temp,ref,by.x="D",by.y="towId",all.x = T)
#  temp <- temp[which(!is.na(temp$vorId)),]
#  temp <- data.frame(O=temp$O,D=temp$vorId,N=temp$N)
  temp <- aggregate(N~O+D, temp, sum)
  dataN <- rbind(dataN,temp)
  dataN <- aggregate(N~O+D, dataN, sum)
}

i=1
dataNV <- read.csv(paste(folderin,"Sonatel/SET1/SET1V_",i,".csv",sep=""),header = F)
dataNV <- aggregate(V4~V2+V3, dataNV, sum)
#dataNV <- merge(dataNV,ref,by.x="V2",by.y="towId",all.x = T)
#dataNV <- dataNV[which(!is.na(dataNV$vorId)),]
dataNV <- data.frame(O=dataNV$V2,D=dataNV$V3,N=dataNV$V4)
#dataNV <- merge(dataNV,ref,by.x="D",by.y="towId",all.x = T)
#dataNV <- dataNV[which(!is.na(dataNV$vorId)),]
#dataNV <- data.frame(O=dataNV$O,D=dataNV$vorId,N=dataNV$N)
dataNV <- aggregate(N~O+D, dataNV, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"Sonatel/SET1/SET1V_",i,".csv",sep=""),header = F)
  temp <- aggregate(V4~V2+V3, temp, sum)
#  temp <- merge(temp,ref,by.x="V2",by.y="towId",all.x = T)
#  temp <- temp[which(!is.na(temp$vorId)),]
  temp <- data.frame(O=temp$V2,D=temp$V3,N=temp$V4)
#  temp <- merge(temp,ref,by.x="D",by.y="towId",all.x = T)
#  temp <- temp[which(!is.na(temp$vorId)),]
#  temp <- data.frame(O=temp$O,D=temp$vorId,N=temp$N)
  temp <- aggregate(N~O+D, temp, sum)
  dataNV <- rbind(dataNV,temp)
  dataNV <- aggregate(N~O+D, dataNV, sum)
}

i=1
dataNL <- read.csv(paste(folderin,"Sonatel/SET1/SET1V_",i,".csv",sep=""),header = F)
dataNL <- aggregate(V5~V2+V3, dataNL, sum)
#dataNL <- merge(dataNL,ref,by.x="V2",by.y="towId",all.x = T)
#dataNL <- dataNL[which(!is.na(dataNL$vorId)),]
dataNL <- data.frame(O=dataNL$V2,D=dataNL$V3,N=dataNL$V5)
#dataNL <- merge(dataNL,ref,by.x="D",by.y="towId",all.x = T)
#dataNL <- dataNL[which(!is.na(dataNL$vorId)),]
#dataNL <- data.frame(O=dataNL$O,D=dataNL$vorId,N=dataNL$N)
dataNL <- aggregate(N~O+D, dataNL, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"Sonatel/SET1/SET1V_",i,".csv",sep=""),header = F)
  temp <- aggregate(V5~V2+V3, temp, sum)
#  temp <- merge(temp,ref,by.x="V2",by.y="towId",all.x = T)
#  temp <- temp[which(!is.na(temp$vorId)),]
  temp <- data.frame(O=temp$V2,D=temp$V3,N=temp$V5)
#  temp <- merge(temp,ref,by.x="D",by.y="towId",all.x = T)
#  temp <- temp[which(!is.na(temp$vorId)),]
#  temp <- data.frame(O=temp$O,D=temp$vorId,N=temp$N)
  temp <- aggregate(N~O+D, temp, sum)
  dataNL <- rbind(dataNL,temp)
  dataNL <- aggregate(N~O+D, dataNL, sum)
}



# Visualisations
netT <- matrix(0,nrow=1298,ncol=1298)
for(i in 1:nrow(dataN)){
  netT[dataN$O[i],dataN$D[i]] <- dataN$N[i]
}
G_T <- graph_from_adjacency_matrix(netT,mode="directed",weighted=T)

netT2 <- netT
diag(netT2) <- 0
G_T_2 <- graph_from_adjacency_matrix(netT2,mode="directed",weighted=T)

G_T_sub <- subgraph.edges(graph=G_T, eids=which(floor(E(G_T)$weight/60000)>0),delete.vertices=F)
G_T_sub_2 <- subgraph.edges(graph=G_T_2, eids=which(floor(E(G_T)$weight/100000)>0),delete.vertices=F)

X <- aggregate(tLong ~ tvId, towerloc, mean)$tLong
Y <- aggregate(tLat ~ tvId, towerloc, mean)$tLat
X <- floor((18+X)*1000)
Y <- floor((Y-12)*1000)
lay <- matrix(0,ncol=2,nrow=1298)
lay[,1] <- X
lay[,2] <- Y

plot(G_T_sub, layout=lay, vertex.size=1,
     vertex.label="",
     edge.arrow.size=0.1)#,
     #edge.width=floor(E(G_T)$weight/5000)/10)

plot(G_T_sub_2, layout=lay, vertex.size=1, vertex.shape="none",
     vertex.label="",
     edge.arrow.size=0.1)

write.table(dataN,"dataN.csv")
write.table(dataNV,"dataNV.csv")
write.table(dataNL,"dataNL.csv")

library(igraph)

##################################################
##### Clustering features (partial networks) #####
##################################################

dataN <- read.csv("dataN.csv",sep=" ")
dataNV <- read.csv("dataNV.csv",sep=" ")
dataNL <- read.csv("dataNL.csv",sep=" ")

netT <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(dataN)){
  netT[dataN$O[i],dataN$D[i]] <- dataN$N[i]
}
G_T <- graph_from_adjacency_matrix(netT,mode="directed",weighted=T)

netTV <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(dataNV)){
  netTV[dataNV$O[i],dataNV$D[i]] <- dataNV$N[i]
}
G_TV <- graph_from_adjacency_matrix(netTV,mode="directed",weighted=T)

netTL <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(dataNL)){
  netTL[dataNL$O[i],dataNL$D[i]] <- dataNL$N[i]
}
G_TL <- graph_from_adjacency_matrix(netTL,mode="directed",weighted=T)


G_test <- subgraph.edges(graph=G_T,eids=which(E(G_T)$weight>10000),delete.vertices=F)
length(V(G_test))

range(E(G_T)$weight)
range(E(G_TV)$weight)
range(E(G_TL)$weight)

hist(E(G_T)$weight[E(G_T)$weight < 500],breaks=20)
hist(E(G_T)$weight)

hist(E(G_TV)$weight[E(G_TV)$weight < 500],breaks=20)
hist(E(G_TL)$weight[E(G_TL)$weight < 15000],breaks=20)

length(E(G_T)$weight)

60000/365

clustFeat(G_T,c(0,75,150,300,600),dist)
clustFeat(G_TV,c(0,75,150,300,600),dist)
clustFeat(G_TL,c(0,200,400,800,1200),dist)

dist <- read.csv("dist.csv")
dist <- dist[,2:1667]
dist <- as.matrix(dist)


G <- G_T
t <- 0

i = 1

clustFeat <- function(G,t,dist){
  # G is the original graph.
  # t is the edge threshold(s) to build subgraph. Please start with 0 to include the original graph.
  for(i in 1:length(t)){
    g <- subgraph.edges(graph=G,eids=which(E(G)$weight>t[i]),delete.vertices=F)
    M <- as_adjacency_matrix(g,attr="weight")
    deg <- degree(g,mode="all",normalized=F)
    nClustFeat <- data.frame(deg = deg/max(deg))
    betu <- betweenness(g,directed=F,weights=rep(1,length(E(g)$weight)),normalized=F)
    betw <- betweenness(g,directed=F,weights=E(g)$weight,normalized=F)
    nClustFeat$BetU <- betu/max(betu)
    nClustFeat$BetW <- betw/max(betw)
    rSelfLoop <- rep(0,nrow(M))
    rInOut <- rep(0,nrow(M))
    for(j in 1:nrow(M)){
      if(sum(M[j,]) > 0){
        rSelfLoop[j] <- M[j,j]/sum(M[j,])
        rInOut[j] <- sum(M[,j])/sum(M[j,])
      }
    }
    nClustFeat$SelfLoop <- rSelfLoop
    nClustFeat$rInOut <- rInOut
    comp <- components(g,mode="weak")
    varBMC <- comp$membership
    varBMC[which(varBMC!=1)] <- 0
    nClustFeat$belongLC <- varBMC
    ref <- which(varBMC==1)
    g2 <- induced.subgraph(graph=g,vids=ref)
    clos_u <- closeness(g2,mode="all",weights=rep(1,length(E(g2)$weight)),normalized=F)
    clos_w <- closeness(g2,mode="all",weights = E(g2)$weight,normalized=F)
    closu <- rep(0,nrow(M))
    closw <- rep(0,nrow(M))
    closu[ref] <- clos_u/max(clos_u)
    closw[ref] <- clos_w/max(clos_w)
    nClustFeat$closU <- closu
    nClustFeat$closW <- closw
    sumdist <- dist*M
    di <- rep(0,nrow(M))
    for(j in 1:nrow(M)){
      di[j] <- (sum(sumdist[j,])+sum(sumdist[,j])-sumdist[j,j])/(sum(M[j,])+sum(M[,j])-M[j,j])
    }
    di[which(is.na(di))] <- 0
    nClustFeat$avgd <- di/max(di)
    nClustFeat[is.na(nClustFeat)] <- 0
    write.table(nClustFeat,paste("featureNetSub",i,".csv",sep=""),row.names=F,col.names=F,sep=",")
  }
}











