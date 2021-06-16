library(igraph)

### Required data (same as data-clustering-curves.R):
###   <SET1S_XX> are a series of Call Details Record (CDR) for text messages in Senegal in 2013 operated by Sonatel, sorted by month. Access needs to be requested from Orange/Sonatel.
###   <SET1V_XX> are a series of Call Details Record (CDR) for calls in Senegal in 2013 operated by Sonatel, sorted by month. Access needs to be requested from Orange/Sonatel.


#################################
##### Building the networks #####
#################################


# Data building

ref <- data.frame(vorId=towerloc$tvId,towId=towerloc$tId)
# Allows selection between all antenna sites (towId, 1666 entities) or active antenna sites (vorId, 1298 entities).
# Comment out accordingly in the lines below. Most recent analyses use active antenna sites (vorId).

# Texts
i=1
dataN <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)
dataN <- aggregate(V4~V2+V3, dataN, sum)
#dataN <- merge(dataN,ref,by.x="V2",by.y="towId",all.x = T)
#dataN <- dataN[which(!is.na(dataN$vorId)),]
dataN <- data.frame(O=dataN$V2,D=dataN$V3,N=dataN$V4)
#dataN <- merge(dataN,ref,by.x="D",by.y="towId",all.x = T)
#dataN <- dataN[which(!is.na(dataN$vorId)),]
#dataN <- data.frame(O=dataN$O,D=dataN$vorId,N=dataN$N)
dataN <- aggregate(N~O+D, dataN, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)
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

# Calls
i=1
dataNV <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
dataNV <- aggregate(V4~V2+V3, dataNV, sum)
#dataNV <- merge(dataNV,ref,by.x="V2",by.y="towId",all.x = T)
#dataNV <- dataNV[which(!is.na(dataNV$vorId)),]
dataNV <- data.frame(O=dataNV$V2,D=dataNV$V3,N=dataNV$V4)
#dataNV <- merge(dataNV,ref,by.x="D",by.y="towId",all.x = T)
#dataNV <- dataNV[which(!is.na(dataNV$vorId)),]
#dataNV <- data.frame(O=dataNV$O,D=dataNV$vorId,N=dataNV$N)
dataNV <- aggregate(N~O+D, dataNV, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
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

# Call lenghts
i=1
dataNL <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
dataNL <- aggregate(V5~V2+V3, dataNL, sum)
#dataNL <- merge(dataNL,ref,by.x="V2",by.y="towId",all.x = T)
#dataNL <- dataNL[which(!is.na(dataNL$vorId)),]
dataNL <- data.frame(O=dataNL$V2,D=dataNL$V3,N=dataNL$V5)
#dataNL <- merge(dataNL,ref,by.x="D",by.y="towId",all.x = T)
#dataNL <- dataNL[which(!is.na(dataNL$vorId)),]
#dataNL <- data.frame(O=dataNL$O,D=dataNL$vorId,N=dataNL$N)
dataNL <- aggregate(N~O+D, dataNL, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
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

#write.table(dataN,"dataN.csv")
#write.table(dataNV,"dataNV.csv")
#write.table(dataNL,"dataNL.csv")

# Visualisations

netT <- matrix(0,nrow=1298,ncol=1298) # Compatible with vorId only
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


##################################################
##### Clustering features (partial networks) #####
##################################################


# Compatible with towId only

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

hist(E(G_T)$weight[E(G_T)$weight < 500],breaks=20)
hist(E(G_T)$weight)

hist(E(G_TV)$weight[E(G_TV)$weight < 500],breaks=20)
hist(E(G_TL)$weight[E(G_TL)$weight < 15000],breaks=20)

clustFeat(G_T,c(0,75,150,300,600),dist)
clustFeat(G_TV,c(0,75,150,300,600),dist)
clustFeat(G_TL,c(0,200,400,800,1200),dist)

dist <- read.csv("dist.csv")
dist <- dist[,2:1667]
dist <- as.matrix(dist)
