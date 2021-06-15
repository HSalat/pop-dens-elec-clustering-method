### Required data:
###   <SET1S_XX> are a series of Call Details Record (CDR) for text messages in Senegal in 2013 operated by Sonatel, sorted by month. Access needs to be requested from Orange/Sonatel.
###   <SET1V_XX> are a series of Call Details Record (CDR) for calls in Senegal in 2013 operated by Sonatel, sorted by month. Access needs to be requested from Orange/Sonatel.


folderin <- "data/"


########################################
##### Prepared data for clustering #####
########################################


# data averaged per antenna site
ref <- data.frame(vorId=towerloc$tvId,towId=towerloc$tId)

i=1
data <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)
data <- aggregate(V4~V1+V2, data, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)
  temp <- aggregate(V4~V1+V2, temp, sum)
  data <- rbind(data,temp)
}
data <- merge(data,ref,by.x="V2",by.y="towId",all.x = T)

i=1
dataV <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
dataV <- aggregate(V4~V1+V2, dataV, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
  temp <- aggregate(V4~V1+V2, temp, sum)
  dataV <- rbind(dataV,temp)
}
dataV <- merge(dataV,ref,by.x="V2",by.y="towId",all.x = T)

i=1
dataL <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
dataL <- aggregate(V5~V1+V2, dataL, sum)
for(i in 2:12){
  temp <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
  temp <- aggregate(V5~V1+V2, temp, sum)
  dataL <- rbind(dataL,temp)
}
dataL <- merge(dataL,ref,by.x="V2",by.y="towId",all.x = T)


#####------------------
##### Distance matrices 
#####------------------


# Produces distance matrices in the format: <z_dMat_DSd_T_V>, where
#   z means NA values are treated as zeros
#   {D,W,Y} means daily, weekly or yearly aggregates
#   {Sd,Cor} is the distance chosen between 'standard deviation' and 'correlation' between any two curves
#   {T,C,L} means number of text messages, number of calls or call length
#   {T,V} is the geographical unit considered, only V for Voronoi has been used


# Choose what to do with "missing values" in the table:
#mv <- NA
mv <- 0


#### Daily

### Texts

#data2 <- data
#data2$V3 <- substr(data2$V1,12,13)
#data2$V3 <- as.numeric(data2$V3)
#data2 <- aggregate(V4~V2+V3, data2, mean)
#premat <- matrix(mv,ncol=24,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat[data2$V2[i],data2$V3[i]+1] <- data2$V4[i]
#}

#prematDT <- premat

#z_dMat_DSd_T_T <- distMat(premat,0,method="sd")
#z_dMat_DCor_T_T <- distMat(premat,0,method="cor")
#z_dMat_DCor_T_T[is.na(z_dMat_DCor_T_T)] <- 1 # set NA values to maximum distance

data2 <- data
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V4~V1+vorId,data2,sum)
data2$V3 <- substr(data2$V1,12,13)
data2$V3 <- as.numeric(data2$V3)
data2 <- aggregate(V4~vorId+V3,data2,mean)
premat <- matrix(mv,ncol=24,nrow=1298)
for(i in 1:nrow(data2)){
  premat[data2$vorId[i],data2$V3[i]+1] <- data2$V4[i]
}

#plot(1:24,1:24,pch="",ylim=c(0,20000))
#for(i in 1:1298){
#  lines(1:24,premat[i,],col=1)
#}
#for(i in 1:1298){
#  lines(1:24,premat[i,],col=alpha(palettespe[i %% 12],0.2))
#}

z_dMat_DSd_T_V <- distMat(premat,0,method="sd")

count <- 0
id <- NULL
for(i in 1:1298){
  if(sum(premat[i,]) == 0){
    count <- count + 1
    id <- c(id,i)
  }
}

z_dMat_DCor_T_V <- distMat(premat,0,method="cor")
z_dMat_DCor_T_V[is.na(z_dMat_DCor_T_V)] <- 1 # set NA values to maximum distance

write.table(z_dMat_DCor_T_V,"data/z_dMat_DCor_T_V.csv",row.names=F,col.names=F,sep=",")

### Calls

#data2 <- dataV
#data2$V3 <- substr(data2$V1,12,13)
#data2$V3 <- as.numeric(data2$V3)
#data2 <- aggregate(V4~V2+V3, data2, mean)
#premat <- matrix(mv,ncol=24,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat[data2$V2[i],data2$V3[i]+1] <- data2$V4[i]
#}

#prematDC <- premat

#z_dMat_DSd_C_T <- distMat(premat,0,method="sd")
#z_dMat_DCor_C_T <- distMat(premat,0,method="cor")

data2 <- dataV
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V4~V1+vorId,data2,sum)
data2$V3 <- substr(data2$V1,12,13)
data2$V3 <- as.numeric(data2$V3)
data2 <- aggregate(V4~vorId+V3,data2,mean)
prematDC <- matrix(mv,ncol=24,nrow=1298)
for(i in 1:nrow(data2)){
  prematDC[data2$vorId[i],data2$V3[i]+1] <- data2$V4[i]
}

# plot(1:24,1:24,pch="",ylim=c(0,20000))
# for(i in 1:1298){
#   lines(1:24,premat[i,],col=1)
# }
# for(i in 1:1298){
#   lines(1:24,premat[i,],col=alpha(palettespe[i %% 12],0.2))
# }

z_dMat_DSd_C_V <- distMat(premat,0,method="sd")
z_dMat_DCor_C_V <- distMat(premat,0,method="cor")

head(z_dMat_DCor_C_V[,1:20])

which(diag(z_dMat_DCor_C_V)>0)

### Length

#data2 <- dataL
#data2$V3 <- substr(data2$V1,12,13)
#data2$V3 <- as.numeric(data2$V3)
#data2 <- aggregate(V5~V2+V3, data2, mean)
#premat <- matrix(mv,ncol=24,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat[data2$V2[i],data2$V3[i]+1] <- data2$V5[i]
#}

#prematDL <- premat

#z_dMat_DSd_L_T <- distMat(premat,0,method="sd")
#z_dMat_DCor_L_T <- distMat(premat,0,method="cor")

data2 <- dataL
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V5~V1+vorId,data2,sum)
data2$V3 <- substr(data2$V1,12,13)
data2$V3 <- as.numeric(data2$V3)
data2 <- aggregate(V5~vorId+V3,data2,mean)
prematDL <- matrix(mv,ncol=24,nrow=1298)
for(i in 1:nrow(data2)){
  prematDL[data2$vorId[i],data2$V3[i]+1] <- data2$V5[i]
}

z_dMat_DSd_L_V <- distMat(premat,0,method="sd")
z_dMat_DCor_L_V <- distMat(premat,0,method="cor")


#### Weekly

# Number of days in a month
ref2 <- c(0,31,28,31,30,31,30,31,31,30,31,30)
ref <- ref2
for(i in 1:12){
  ref[i] <- sum(ref2[1:i])
}

### Texts

#data2 <- data
#data2$m <- substr(data2$V1,6,7)
#data2$m <- as.numeric(data2$m)
#data2$d <- substr(data2$V1,9,10)
#data2$d <- as.numeric(data2$d)
#data2$h <- substr(data2$V1,12,13)
#data2$day <- (ref[data2$m]+data2$d) %% 7 + 1
#data2$day <- as.character(data2$day)
#data2$dh <- paste(data2$day,data2$h,sep="")
#data2$dh <- as.integer(data2$dh)
#data2 <- aggregate(V4~V2+dh, data2, mean, na.rm=T)
#data2$d <- as.integer(substr(data2$dh,1,1))-1
#data2$h <- as.integer(substr(data2$dh,2,3))+1
#data2$temp <- data2$d*24+data2$h
#data2$temp <- as.integer(data2$temp)
#premat2 <- matrix(mv,ncol=24*7,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat2[data2$V2[i],data2$temp[i]] <- data2$V4[i]
#}

#prematWT <- premat2

#z_dMat_WSd_T_T <- distMat(premat2,0,method="sd")
#z_dMat_WCor_T_T <- distMat(premat2,0,method="cor")

# plot(1:168,1:168,pch="",ylim=c(0,8000))
# for(i in 1:1666){
#   lines(1:168,premat2[i,],col=alpha(palettespe[i %% 12],0.2))
# }

data2 <- data
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V4~V1+vorId,data2,sum)
data2$m <- substr(data2$V1,6,7)
data2$m <- as.numeric(data2$m)
data2$d <- substr(data2$V1,9,10)
data2$d <- as.numeric(data2$d)
data2$h <- substr(data2$V1,12,13)
data2$day <- (ref[data2$m]+data2$d) %% 7 + 1
data2$day <- as.character(data2$day)
data2$dh <- paste(data2$day,data2$h,sep="")
data2$dh <- as.integer(data2$dh)
data2 <- aggregate(V4~vorId+dh, data2, mean, na.rm=T)
data2$d <- as.integer(substr(data2$dh,1,1))-1
data2$h <- as.integer(substr(data2$dh,2,3))+1
data2$temp <- data2$d*24+data2$h
data2$temp <- as.integer(data2$temp)
prematWT <- matrix(mv,ncol=24*7,nrow=1298)
for(i in 1:nrow(data2)){
  prematWT[data2$vorId[i],data2$temp[i]] <- data2$V4[i]
}

# plot(1:168,1:168,pch="",ylim=c(0,20000))
# for(i in 1:1298){
#   lines(1:168,premat2[i,],col=alpha(palettespe[i %% 12],0.2))
# }

z_dMat_WSd_T_V <- distMat(premat2,0,method="sd")
z_dMat_WCor_T_V <- distMat(premat2,0,method="cor")

z_dMat_WCor_T_V[is.na(z_dMat_WCor_T_V)] <- 1

write.table(z_dMat_WCor_T_V,"Data/z_dMat_WCor_T_V.csv",row.names=F,col.names=F,sep=",")

### Calls

#data2 <- dataV
#data2$m <- substr(data2$V1,6,7)
#data2$m <- as.numeric(data2$m)
#data2$d <- substr(data2$V1,9,10)
#data2$d <- as.numeric(data2$d)
#data2$h <- substr(data2$V1,12,13)
#data2$day <- (ref[data2$m]+data2$d) %% 7 + 1
#data2$day <- as.character(data2$day)
#data2$dh <- paste(data2$day,data2$h,sep="")
#data2$dh <- as.integer(data2$dh)
#data2 <- aggregate(V4~V2+dh, data2, mean, na.rm=T)
#data2$d <- as.integer(substr(data2$dh,1,1))-1
#data2$h <- as.integer(substr(data2$dh,2,3))+1
#data2$temp <- data2$d*24+data2$h
#data2$temp <- as.integer(data2$temp)
#premat2 <- matrix(mv,ncol=24*7,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat2[data2$V2[i],data2$temp[i]] <- data2$V4[i]
#}

#prematWC <- premat2

#z_dMat_WSd_C_T <- distMat(premat2,0,method="sd")
#z_dMat_WCor_C_T <- distMat(premat2,0,method="cor")

#plot(1:168,1:168,pch="",ylim=c(0,4500))
#for(i in 1:1666){
#  lines(1:168,premat2[i,],col=alpha(palettespe[i %% 12],0.2))
#}

data2 <- dataV
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V4~V1+vorId,data2,sum)
data2$m <- substr(data2$V1,6,7)
data2$m <- as.numeric(data2$m)
data2$d <- substr(data2$V1,9,10)
data2$d <- as.numeric(data2$d)
data2$h <- substr(data2$V1,12,13)
data2$day <- (ref[data2$m]+data2$d) %% 7 + 1
data2$day <- as.character(data2$day)
data2$dh <- paste(data2$day,data2$h,sep="")
data2$dh <- as.integer(data2$dh)
data2 <- aggregate(V4~vorId+dh, data2, mean, na.rm=T)
data2$d <- as.integer(substr(data2$dh,1,1))-1
data2$h <- as.integer(substr(data2$dh,2,3))+1
data2$temp <- data2$d*24+data2$h
data2$temp <- as.integer(data2$temp)
prematWC <- matrix(mv,ncol=24*7,nrow=1298)
for(i in 1:nrow(data2)){
  prematWC[data2$vorId[i],data2$temp[i]] <- data2$V4[i]
}

z_dMat_WSd_C_V <- distMat(premat2,0,method="sd")
z_dMat_WCor_C_V <- distMat(premat2,0,method="cor")

### Length

#data2 <- dataL
#data2$m <- substr(data2$V1,6,7)
#data2$m <- as.numeric(data2$m)
#data2$d <- substr(data2$V1,9,10)
#data2$d <- as.numeric(data2$d)
#data2$h <- substr(data2$V1,12,13)
#data2$day <- (ref[data2$m]+data2$d) %% 7 + 1
#data2$day <- as.character(data2$day)
#data2$dh <- paste(data2$day,data2$h,sep="")
#data2$dh <- as.integer(data2$dh)
#data2 <- aggregate(V5~V2+dh, data2, mean, na.rm=T)
#data2$d <- as.integer(substr(data2$dh,1,1))-1
#data2$h <- as.integer(substr(data2$dh,2,3))+1
#data2$temp <- data2$d*24+data2$h
#data2$temp <- as.integer(data2$temp)
#premat2 <- matrix(mv,ncol=24*7,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat2[data2$V2[i],data2$temp[i]] <- data2$V5[i]
#}

#prematWL <- premat2

#z_dMat_WSd_L_T <- distMat(premat2,0,method="sd")
#z_dMat_WCor_L_T <- distMat(premat2,0,method="cor")

data2 <- dataL
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V5~V1+vorId,data2,sum)
data2$m <- substr(data2$V1,6,7)
data2$m <- as.numeric(data2$m)
data2$d <- substr(data2$V1,9,10)
data2$d <- as.numeric(data2$d)
data2$h <- substr(data2$V1,12,13)
data2$day <- (ref[data2$m]+data2$d) %% 7 + 1
data2$day <- as.character(data2$day)
data2$dh <- paste(data2$day,data2$h,sep="")
data2$dh <- as.integer(data2$dh)
data2 <- aggregate(V5~vorId+dh, data2, mean, na.rm=T)
data2$d <- as.integer(substr(data2$dh,1,1))-1
data2$h <- as.integer(substr(data2$dh,2,3))+1
data2$temp <- data2$d*24+data2$h
data2$temp <- as.integer(data2$temp)
prematWL <- matrix(mv,ncol=24*7,nrow=1298)
for(i in 1:nrow(data2)){
  prematWL[data2$vorId[i],data2$temp[i]] <- data2$V5[i]
}

z_dMat_WSd_L_V <- distMat(premat2,0,method="sd")
z_dMat_WCor_L_V <- distMat(premat2,0,method="cor")


names <- c("dMat_DSd_T_T","dMat_DCor_T_T","dMat_DSd_T_V","dMat_DCor_T_V",
           "dMat_DSd_C_T","dMat_DCor_C_T","dMat_DSd_C_V","dMat_DCor_C_V",
           "dMat_DSd_L_T","dMat_DCor_L_T","dMat_DSd_L_V","dMat_DCor_L_V",
           "dMat_WSd_T_T","dMat_WCor_T_T","dMat_WSd_T_V","dMat_WCor_T_V",
           "dMat_WSd_C_T","dMat_WCor_C_T","dMat_WSd_C_V","dMat_WCor_C_V",
           "dMat_WSd_L_T","dMat_WCor_L_T","dMat_WSd_L_V","dMat_WCor_L_V"
           )

names <- c("z_dMat_DSd_T_T","z_dMat_DCor_T_T","z_dMat_DSd_T_V","z_dMat_DCor_T_V",
           "z_dMat_DSd_C_T","z_dMat_DCor_C_T","z_dMat_DSd_C_V","z_dMat_DCor_C_V",
           "z_dMat_DSd_L_T","z_dMat_DCor_L_T","z_dMat_DSd_L_V","z_dMat_DCor_L_V",
           "z_dMat_WSd_T_T","z_dMat_WCor_T_T","z_dMat_WSd_T_V","z_dMat_WCor_T_V",
           "z_dMat_WSd_C_T","z_dMat_WCor_C_T","z_dMat_WSd_C_V","z_dMat_WCor_C_V",
           "z_dMat_WSd_L_T","z_dMat_WCor_L_T","z_dMat_WSd_L_V","z_dMat_WCor_L_V"
)

for(i in names[13:24]){
  write.table(get(i),paste("Data/",i,".csv",sep=""),row.names=F,col.names=F,sep=",")
}


#### Yearly

### Texts

#data2 <- data
#data2$m <- substr(data2$V1,6,7)
#data2$m <- as.numeric(data2$m)
#data2$d <- substr(data2$V1,9,10)
#data2$d <- as.numeric(data2$d)-1
#data2$h <- substr(data2$V1,12,13)
#data2$h <- as.numeric(data2$h)+1
#data2$index <- (ref[data2$m]+data2$d)*24+data2$h
#data2$index <- as.integer(data2$index)
#premat3 <- matrix(mv,ncol=24*365,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat3[data2$V2[i],data2$index[i]] <- data2$V4[i]
#}

#prematYT <- premat3

#fMat_T_T <- curveFeatures(premat3)
#z_dMat_YSd_T_T <- distMat(premat3,100,method="sd")
#z_dMat_YCor_T_T <- distMat(premat3,100,method="cor")


# plot(1:8760,1:8760,pch="",ylim=c(0,5000),xlim=c(2000,4000))
# temp <- sample(1:1666,20)
# for(i in temp){
#   lines(1:8760,premat3[i,],col=alpha(palettespe[i %% 12],0.2))
# }

data2 <- data
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V4~V1+vorId,data2,sum)
data2$m <- substr(data2$V1,6,7)
data2$m <- as.numeric(data2$m)
data2$d <- substr(data2$V1,9,10)
data2$d <- as.numeric(data2$d)-1
data2$h <- substr(data2$V1,12,13)
data2$h <- as.numeric(data2$h)+1
data2$index <- (ref[data2$m]+data2$d)*24+data2$h
data2$index <- as.integer(data2$index)
prematYT <- matrix(mv,ncol=24*365,nrow=1298)
for(i in 1:nrow(data2)){
  prematYT[data2$vorId[i],data2$index[i]] <- data2$V4[i]
}

fMat_T_V <- curveFeatures(premat3)
z_dMat_YSd_T_V <- distMat(premat3,100,method="sd")
z_dMat_YCor_T_V <- distMat(premat3,100,method="cor")

z_dMat_YCor_T_V[which(is.na(z_dMat_YCor_T_V))] <- 1

nrow(z_dMat_YSd_T_V)

### Calls

#data2 <- dataV
#data2$m <- substr(data2$V1,6,7)
#data2$m <- as.numeric(data2$m)
#data2$d <- substr(data2$V1,9,10)
#data2$d <- as.numeric(data2$d)-1
#data2$h <- substr(data2$V1,12,13)
#data2$h <- as.numeric(data2$h)+1
#data2$index <- (ref[data2$m]+data2$d)*24+data2$h
#data2$index <- as.integer(data2$index)
#premat3 <- matrix(mv,ncol=24*365,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat3[data2$V2[i],data2$index[i]] <- data2$V4[i]
#}

#prematYC <- premat3

#fMat_C_T <- curveFeatures(premat3)
#z_dMat_YSd_C_T <- distMat(premat3,100,method="sd")
#z_dMat_YCor_C_T <- distMat(premat3,100,method="cor")

data2 <- dataV
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V4~V1+vorId,data2,sum)
data2$m <- substr(data2$V1,6,7)
data2$m <- as.numeric(data2$m)
data2$d <- substr(data2$V1,9,10)
data2$d <- as.numeric(data2$d)-1
data2$h <- substr(data2$V1,12,13)
data2$h <- as.numeric(data2$h)+1
data2$index <- (ref[data2$m]+data2$d)*24+data2$h
data2$index <- as.integer(data2$index)
prematYC <- matrix(mv,ncol=24*365,nrow=1298)
for(i in 1:nrow(data2)){
  prematYC[data2$vorId[i],data2$index[i]] <- data2$V4[i]
}

fMat_C_V <- curveFeatures(premat3)
z_dMat_YSd_C_V <- distMat(premat3,100,method="sd")
z_dMat_YCor_C_V <- distMat(premat3,100,method="cor")

z_dMat_YCor_C_V[which(is.na(z_dMat_YCor_C_V))] <- 1

### Length

#data2 <- dataL
#data2$m <- substr(data2$V1,6,7)
#data2$m <- as.numeric(data2$m)
#data2$d <- substr(data2$V1,9,10)
#data2$d <- as.numeric(data2$d)-1
#data2$h <- substr(data2$V1,12,13)
#data2$h <- as.numeric(data2$h)+1
#data2$index <- (ref[data2$m]+data2$d)*24+data2$h
#data2$index <- as.integer(data2$index)
#premat3 <- matrix(mv,ncol=24*365,nrow=1666)
#for(i in 1:nrow(data2)){
#  premat3[data2$V2[i],data2$index[i]] <- data2$V5[i]
#}

#prematYL <- premat3

#fMat_L_T <- curveFeatures(premat3)
#z_dMat_YSd_L_T <- distMat(premat3,100,method="sd")
#z_dMat_YCor_L_T <- distMat(premat3,100,method="cor")

data2 <- dataL
data2 <- data2[which(!is.na(data2$vorId)),]
data2 <- aggregate(V5~V1+vorId,data2,sum)
data2$m <- substr(data2$V1,6,7)
data2$m <- as.numeric(data2$m)
data2$d <- substr(data2$V1,9,10)
data2$d <- as.numeric(data2$d)-1
data2$h <- substr(data2$V1,12,13)
data2$h <- as.numeric(data2$h)+1
data2$index <- (ref[data2$m]+data2$d)*24+data2$h
data2$index <- as.integer(data2$index)
prematYL <- matrix(mv,ncol=24*365,nrow=1298)
for(i in 1:nrow(data2)){
  prematYL[data2$vorId[i],data2$index[i]] <- data2$V5[i]
}

fMat_L_V <- curveFeatures(premat3)
z_dMat_YSd_L_V <- distMat(premat3,100,method="sd")
z_dMat_YCor_L_V <- distMat(premat3,100,method="cor")

z_dMat_YCor_L_V[which(is.na(z_dMat_YCor_L_V))] <- 1

names2 <- c("z_dMat_YSd_T_T","z_dMat_YCor_T_T","z_dMat_YSd_T_V","z_dMat_YCor_T_V",
            "z_dMat_YSd_C_T","z_dMat_YCor_C_T","z_dMat_YSd_C_V","z_dMat_YCor_C_V",
            "z_dMat_YSd_L_T","z_dMat_YCor_L_T","z_dMat_YSd_L_V","z_dMat_YCor_L_V")

names3 <- c("fMat_T_T","fMat_T_V","fMat_C_T","fMat_C_V","fMat_L_T","fMat_L_V")

for(i in names2[c(11,12)]){
  write.table(get(i),paste("Data/",i,".csv",sep=""),row.names=F,col.names=F,sep=",")
}

for(i in names3[1:6]){
  write.table(get(i),paste("Data/",i,".csv",sep=""),row.names=F,col.names=F,sep=",")
}


##########################
##### Curve Features #####
##########################


# Alternative method to cluster on a feature table instead of a distance matrix. This is not used, although potentially interesting.


test <- premat3[750,]
test <- rnorm(100,0,1)+seq(0,50,length.out=100)

# Trend & seasonality
test <- ts(test,frequency=365,start=c(2013,1))

# vect <- rep(0,1666)
# sum <- 0
# for(i in 1:1666){
#   vect[i] <- length(which(is.na(premat3[i,])))
#   sum <- sum+vect[i]
# }
# plot(1:1666,vect)
# nrow(data)+sum
# sum/(1666*24*365)

decomposedtest <- stats::decompose(test)
t <- decomposedtest$trend
s <- decomposedtest$seasonal

decomposedtest <- stl(test,s.window=24)
t <- decomposedtest$time.series[,2]
s <- decomposedtest$time.series[,1]

degtrend <- 1-var(test-t-s,na.rm=T)/var(test-s,na.rm=T)
degseas <- 1-var(test-t-s,na.rm=T)/var(test-t,na.rm=T)

# plot(1:100,decomposedtest$seasonal,pch="",ylim=c(-2,52))
# lines(1:100,decomposedtest$seasonal)
# lines(1:100,decomposedtest$trend,col=2)
# lines(1:100,decomposedtest$random,col=3)
# lines(1:100,test,lwd=2)

# Non-linearity
test <- rnorm(100,0,1)+seq(0,50,length.out=100)
test[1] <- NA
test <- as.ts(test[!is.na(test)])
terasvirta.test(test)$statistic[1]

#terasvirta.test(test,type="F")

# Skewness
test <- c(1:50,50:1)
test <- c(1:50,1:50)
test <- c(1:50,rnorm(50))
test[1] <- NA
skewness(test,na.rm=T)
kurtosis(test,na.rm=T)

curveFeatures(premat3)

nrow(premat3[1:2,])
tseries <- premat3
n <- 0
i

curveFeatures <- function(tseries){
  # tseries is a matrix with each row being a time series
  # n is the minimal of non NA valures needed for a row to be taken into account
  ni <- nrow(tseries)
  feattable <- data.frame(Ytrend=rep(NA,ni),Yseas=rep(NA,ni),DNL=rep(NA,ni),WNL=rep(NA,ni),
                          YNL=rep(NA,ni),Dskew=rep(NA,ni),Wskew=rep(NA,ni),Dkurt=rep(NA,ni)
                          )
  for(i in 1:ni){
    if(sum(tseries[i,])>0){
      year <- tseries[i,]
      week <- rep(NA,24*7)
      day <- rep(NA,24)
      for(j in 1:24){
        day[j] <- mean(year[j+(0:364)*24])
        week[j] <- mean(year[j+(0:51)*168])
      }
      for(j in 25:168){
        week[j] <- mean(year[j+(0:51)*168])
      }
      yeartemp <- ts(year,frequency=365,start=c(1,1))
      decomposedtest <- stl(test,s.window=24)
      t <- decomposedtest$time.series[,2]
      s <- decomposedtest$time.series[,1]
      feattable[i,1] <- 1-var(year-t-s,na.rm=T)/var(year-s,na.rm=T)
      feattable[i,2] <- 1-var(year-t-s,na.rm=T)/var(year-t,na.rm=T)
      yeartemp <- as.ts(year[!is.na(year)])
      weektemp <- as.ts(week[!is.na(week)])
      daytemp <- as.ts(day[!is.na(day)])
      feattable[i,5] <- terasvirta.test(yeartemp)$statistic[1]
      feattable[i,4] <- terasvirta.test(weektemp)$statistic[1]
      feattable[i,3] <- terasvirta.test(daytemp)$statistic[1]
      feattable[i,7] <- skewness(week,na.rm=T)
      feattable[i,6] <- skewness(day,na.rm=T)
      feattable[i,8] <- kurtosis(day,na.rm=T)
    }
  }
  return(feattable)
}


#########################
##### Visualisation #####
#########################


library(fpc)

test <- read.csv(file = "Data/z_dMat_DSd_C_V.csv",header=F)

sampl <- which(!is.na(test[,5]))
test2 <- test[sampl,sampl]
test2 <- as.dist(test2)
test3 <- hclust(test2)

plot(test3)
t <- identify(test3)
dev.off()

vorData$clust <- rep(NA,1298)
for(i in 1:length(sampl)){
  if(any(sapply(t, function(y) i %in% y)))
    vorData$clust[sampl[i]] <- which(sapply(t, function(y) i %in% y))
}

samp <- which(!is.na(vorData$dens) & !is.na(vorData$clust))
#sampS <- sample(samp,length(samp))
plot(1:24,1:24,ylim=c(0,10000),pch="",xlab="Hour",ylab="# Texts")
for(i in samp){
  lines(1:24,prematT[i,],col=alpha(palettespe[towerloc$clust[i]*3-1],0.4))
}

plot(1:1100,1:1100,pch="",ylim=c(0,50000),xlab="Shuffled Id",ylab="Density")
for(i in samp){
  points(runif(1,1,1000),vorData$dens[i],col=alpha(palettespe[vorData$clust[i]*3-1],0.4),pch=20)
}
legend("topright",c("1","2","3","4","5","6"),col=alpha(palettespe[c(2,5,8,11,1,1)],0.4),pch=c(20,20,20,20,20,20))


C <- matrix(0,ncol=24,nrow=4)
for(i in 1:4){
  for(j in 1:24){
    C[i,j] <- mean(prematC[sampl[t[[i]]],j])
  }
}
plot(1:24,1:24,pch="",ylim=c(0,2400),ylab="Typical curve (calls)")
for(i in 1:4){
  lines(1:24,C[i,],col=palettespe[i*3-1],lwd=2)
}

C <- matrix(0,ncol=24,nrow=4)
for(i in 1:4){
  for(j in 1:24){
    C[i,j] <- mean(prematT[sampl[t[[i]]],j])
  }
}
plot(1:24,1:24,pch="",ylim=c(0,4000),ylab="Typical curve (Texts)")
for(i in 1:4){
  lines(1:24,C[i,],col=palettespe[i*3-1],lwd=2)
}

plot(1:24,1:24,pch="",ylim=c(0,500),ylab="Typical curve (Texts)")
for(i in 1:4){
  lines(1:24,C[i,],col=palettespe[i*3-1],lwd=2)
}

# Normalised
plot(1:24,1:24,pch="",ylim=c(0,3),ylab="Typical curve (Texts) - normalised")
for(i in 1:4){
  lines(1:24,C[i,]/mean(C[i,]),col=palettespe[i*3-1],lwd=2)
}

# Max
plot(1:24,1:24,pch="",ylim=c(0,1),ylab="Typical normalised curve (Texts)")
for(i in 1:4){
  lines(1:24,C[i,]/max(C[i,]),col=palettespe[i*3-1],lwd=2)
}

# 14h
plot(1:24,1:24,pch="",ylim=c(0,2.6),ylab="Typical curve (Texts) - ref 14h")
for(i in 1:4){
  lines(1:24,C[i,]/max(C[i,14]),col=palettespe[i*3-1],lwd=2)
}

# Derivative
D <- C
for(i in 2:23){
  D[,i] <- (C[,i+1]-C[,i-1])/2
}

for(i in 1:4){
  D[i,] <- diff(C[i,])
}

library(xts)
D <- C
A <- C
for(i in 1:4){
  TS <- ts(C[i,])
  TS <- as.xts(TS)
  tt <- time(TS)
  D[i,] <- as.vector(xts(splinefun(tt, TS)(tt, 1), tt))
  A[i,] <- as.vector(xts(splinefun(tt, TS)(tt, 2), tt))
}

plot(1:24,1:24,pch="",ylim=c(-4,1.5),ylab="Typical curve (Texts)")
for(i in 1:4){
  lines(1:24,D[i,1:24],col=palettespe[i*3-1],lwd=2)
}
lines(c(0,25),c(0,0),lty=3)

plot(1:24,1:24,pch="",ylim=c(-0.01,0.007),ylab="Typical curve (Texts)")
for(i in 1:4){
  lines(1:24,A[i,1:24],col=palettespe[i*3-1],lwd=2)
}
lines(c(0,25),c(0,0),lty=3)


require(plotrix)
lowdens <- which(towerloc$dens < 2000)
l <- list(towerloc$dens[intersect(lowdens,t[[1]])],towerloc$dens[intersect(lowdens,t[[2]])],towerloc$dens[intersect(lowdens,t[[3]])],towerloc$dens[intersect(lowdens,t[[4]])])
multhist(l,breaks=10,col=palettespe[c(2,5,8,11)],freq=F)
multhist(l,breaks=10,col=palettespe[c(2,5,8,11)])

l <- list(towerloc$dens[t[[1]]],towerloc$dens[t[[2]]],towerloc$dens[t[[3]]],towerloc$dens[t[[4]]])
multhist(l,breaks=10,col=palettespe[c(2,5,8,11)],freq=F)
multhist(l,breaks=10,col=palettespe[c(2,5,8,11)])

plot(1:1100,1:1100,pch="",ylim=c(0,500))
for(i in samp){
  points(runif(1,1,1000),towerloc$dens[samp[i]],col=palettespe[towerloc$clust[samp[i]]*2])
}
legend("topright",c("1","2","3","4","5","6"),col=palettespe[c(2,4,6,8,10,12)],pch=c(1,1,1,1,1,1))

avg <- rep(0,6)
for(i in 1:6){
  avg[i] <- mean(towerloc$dens[t[[i]]],na.rm=T)
}
plot(1:6,avg)
for(i in 1:6){
  points(i,avg[i],col=palettespe[2*i])
}

####

test <- read.csv(file = "Data/z_dMat_YSd_C_V.csv",header=F)

sampl <- which(!is.na(test[,5]))
test2 <- test[sampl,sampl]
test2 <- as.dist(test2)
test3 <- hclust(test2)

plot(test3)

list <- identify(test3)

dend <- as.dendrogram(test3)
sampTree(dend,6)
dev.off()

####

test <- read.csv(file = "Data/z_dMat_DSd_C_T.csv",header=F)

sampl <- which(!is.na(test[,5]))
test2 <- test[sampl,sampl]
test2 <- as.dist(test2)
test3 <- hclust(test2)

plot(test3)
t2 <- identify(test3)
dev.off()

towerloc$clust2 <- rep(NA,1666)
for(i in 1:length(sampl)){
  if(any(sapply(t2, function(y) sampl[i] %in% y)))
    towerloc$clust2[which(towerloc$tvId == i)] <- which(sapply(t2, function(y) sampl[i] %in% y))
}

samp2 <- which(!is.na(towerloc$dens) & !is.na(towerloc$clust2))
plot(1:24,1:24,ylim=c(0,12000),pch="",xlab="Hour",ylab="Length Calls")
for(i in samp2){
  lines(1:24,premat[towerloc$tvId[i],],col=alpha(palettespe[towerloc$clust2[samp2[i]]*2],0.4))
}

plot(1:1100,1:1100,pch="",ylim=c(0,50000),ylab="Density",xlab="Shuffled Id")
for(i in samp2){
  points(runif(1,1,1000),towerloc$dens[samp2[i]],col=palettespe[towerloc$clust2[samp2[i]]*2])
}
legend("topright",c("1","2","3","4","5","6"),col=palettespe[c(2,4,6,8,10,12)],pch=c(1,1,1,1,1,1))

plot(1:1100,1:1100,pch="",ylim=c(0,500),ylab="Density (Zoom)",xlab="Shuffled Id")
for(i in samp){
  points(runif(1,1,1000),towerloc$dens[samp2[i]],col=alpha(palettespe[towerloc$clust2[samp2[i]]*2],0.4),pch=20)
}
legend("topright",c("1","2","3","4","5","6"),col=alpha(palettespe[c(2,4,6,8,10,12)],0.4),pch=c(20,20,20,20,20,20))

avg <- rep(0,6)
for(i in 1:6){
  avg[i] <- mean(towerloc$dens[t2[[i]]],na.rm=T)
}
plot(1:6,avg)
for(i in 1:6){
  points(i,avg[i],col=palettespe[2*i])
}


#####################################################
test <- read.csv(file = "Data/fMat_C_V.csv",header=F)
test <- test[1:1298,]

test2 <- dist(test)

head(test2[,1:10])

which(is.na(test))

clust <- kmeans(test,6)

clust$cluster

plot(1:1100,1:1100,ylim=c(0,60000),pch="")
for(i in 1:1666){
  if(!is.na(towerloc$tvId[i])){
    points(runif(1,1,1000),towerloc$dens[i],col=alpha(palettespe[2*clust$cluster[towerloc$tvId[i]]],0.4),pch=20)
  }
}
legend("topright",c("1","2","3","4","5","6"),col=alpha(palettespe[seq(2,16,by=2)],0.4),pch=20)
