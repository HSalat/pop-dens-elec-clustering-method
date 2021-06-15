






#######
####### Tests
#######



comp <- c(min(towerloc$dens,na.rm=T),500,5000,max(towerloc$dens,na.rm=T))
t <- c(0.9,0.85,0.85) # 142 Clusters
t <- c(0.85,0.7,0.7) # 32 Clusters
t <- c(0.8,0.6,0.6) # 2 Clusters

nameClust('T',t,comp)

comp <- c(min(towerloc$night,na.rm=T),10,30,max(towerloc$night,na.rm=T))
t <- c(0.9,0.85,0.85) # 106 Clusters
t <- c(0.85,0.7,0.7) # 80 Clusters
t <- c(0.8,0.6,0.6) # 5 Clusters

nameClust('T',t,comp,var='night')

t <- 3000 # 168 Clusters
t <- 5000 # 99 Clusters
t <- 7000 # 15 Clusters

nameClust('T',t,method.purity = 'var')



#######
####### Density predictions
#######

densV <- vorData$dens

densV <- aggregate(towerloc$dens ~ towerloc$tvId, FUN = "mean")
densV <- densV$`towerloc$dens`

nightV <- aggregate(towerloc$night ~ towerloc$tvId, FUN = "mean")
nightV <- nightV$`towerloc$night`

clustExpectedDens <- data.frame(VorId = 1:1298)

comp <- c(min(towerloc$dens,na.rm=T),500,5000,20000,max(towerloc$dens,na.rm=T))
t <- c(0.88,0.6,0.6,0.6)

clustExpectedDens <- autoExpctd('T',t,clustExpectedDens,comp)
clustExpectedDens <- autoExpctd('T',t,clustExpectedDens,comp,dist='Cor')
clustExpectedDens <- autoExpctd('C',t,clustExpectedDens,comp)
clustExpectedDens <- autoExpctd('C',t,clustExpectedDens,comp,dist='Cor')
clustExpectedDens <- autoExpctd('L',t,clustExpectedDens,comp)
clustExpectedDens <- autoExpctd('L',t,clustExpectedDens,comp,dist='Cor')

clustExpectedDens <- autoExpctd('T',t,clustExpectedDens,comp,time='W')
clustExpectedDens <- autoExpctd('T',t,clustExpectedDens,comp,time='W',dist='Cor')
clustExpectedDens <- autoExpctd('C',t,clustExpectedDens,comp,time='W')
clustExpectedDens <- autoExpctd('C',t,clustExpectedDens,comp,time='W',dist='Cor')
clustExpectedDens <- autoExpctd('L',t,clustExpectedDens,comp,time='W')
clustExpectedDens <- autoExpctd('L',t,clustExpectedDens,comp,time='W',dist='Cor')

clustExpectedDens <- autoExpctd('T',t,clustExpectedDens,comp,time='Y')
clustExpectedDens <- autoExpctd('T',t,clustExpectedDens,comp,time='Y',dist='Cor')
clustExpectedDens <- autoExpctd('C',t,clustExpectedDens,comp,time='Y')
clustExpectedDens <- autoExpctd('C',t,clustExpectedDens,comp,time='Y',dist='Cor')
clustExpectedDens <- autoExpctd('L',t,clustExpectedDens,comp,time='Y')
clustExpectedDens <- autoExpctd('L',t,clustExpectedDens,comp,time='Y',dist='Cor')

clustExpectedDens$dens <- densV

cor(clustExpectedDens$Method1,densV)^2
cor(clustExpectedDens$Method2,densV)^2
cor(clustExpectedDens$Method3,densV)^2
cor(clustExpectedDens$Method4,densV)^2
cor(clustExpectedDens$Method5,densV)^2

cor(clustExpectedDens$Method1+clustExpectedDens$Method2+clustExpectedDens$Method3+clustExpectedDens$Method4+clustExpectedDens$Method5,densV)^2
cor(clustExpectedDens$Method1+clustExpectedDens$Method2+clustExpectedDens$Method4+clustExpectedDens$Method5,densV)^2

hist(clustExpectedDens$Method1-clustExpectedDens$dens,breaks = 50)
mean(clustExpectedDens$Method1-clustExpectedDens$dens)
sd(clustExpectedDens$Method1-clustExpectedDens$dens)
plot(1:1298,clustExpectedDens$Method1-clustExpectedDens$dens,col=alpha(1,0.4),pch=20)

hist(clustExpectedDens$sum/18-clustExpectedDens$dens,breaks = 50)

dist <- (clustExpectedDens$Method1+clustExpectedDens$Method2+clustExpectedDens$Method3+clustExpectedDens$Method4+clustExpectedDens$Method5)/5-clustExpectedDens$dens
hist(dist,breaks = 50)
plot(1:1298,dist,pch=20,col=alpha(1,0.4))

clustExpectedDens$sum <- 0
for(i in 1:1298){
  clustExpectedDens$sum[i] <- sum(clustExpectedDens[i,2:19])
}
cor(clustExpectedDens$sum,densV)^2

clustExpectedDens$sum2 <- 0
for(i in 1:1298){
  clustExpectedDens$sum2[i] <- sum(clustExpectedDens[i,c(2,3,4,6,8,10,12)])
}
cor(clustExpectedDens$sum2,densV)^2

clustExpectedDens$sum3 <- 0
for(i in 1:1298){
  clustExpectedDens$sum3[i] <- sum(clustExpectedDens[i,c(5,7,9,11,13)])
}
cor(clustExpectedDens$sum3,densV)^2

clustExpectedDens$sum4 <- 0
for(i in 1:1298){
  clustExpectedDens$sum4[i] <- sum(clustExpectedDens[i,c(2:4,6:13)])
}
cor(clustExpectedDens$sum4,densV)^2

cor <- rep(0,12)
std <- rep(0,12)
mn <- rep(0,12)
for(i in 2:13){
  cor[i-1] <- cor(clustExpectedDens[,i],densV)^2
  std[i-1] <- sd(clustExpectedDens[,i])
  #mn[i-1] <- mean(clustExpectedDens[,i])
}
plot(1:12,cor,ylim=c(0,1))
plot(1:12,std,ylim=c(0,max(std)))
#plot(1:12,mn,ylim=c(0,max(mn)))





###################

clustExpectedDens <- data.frame(VorId = 1:1298)

clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp)
clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,dist='Cor')
clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp)
clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,dist='Cor')
clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp)
clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,dist='Cor')

clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='W')
clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='W',dist='Cor')
clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='W')
clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='W',dist='Cor')
clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='W')
clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='W',dist='Cor')

clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='Y')
clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='Y',dist='Cor')
clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='Y')
clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='Y',dist='Cor')
clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='Y')
clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='Y',dist='Cor')

clustExpectedDens$sumW <- 0
for(i in 1:1298){
  clustExpectedDens$sumW[i] <- sum(clustExpectedDens[i,seq(2,36,by=2)]*clustExpectedDens[i,seq(3,37,by=2)])
}
cor(clustExpectedDens$sumW,densV)^2

hist(densV-clustExpectedDens$sumW/18,breaks = 50)



########

quickTest <- function(t,comp){
  clustExpectedDens <- data.frame(Id = 1:1666)
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,unit="T")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,dist='Cor',unit="T")
  message("HAL9000: Just what do you think you're doing, Dave?")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='W',unit="T")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='W',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='W',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='W',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='W',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='W',dist='Cor',unit="T")
  message("HAL9000: I know everything hasn't been quite right with me, but I can assure you now, very confidently, that it's going to be all right again.")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='Y',unit="T")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,time='Y',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='Y',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,time='Y',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='Y',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,time='Y',dist='Cor',unit="T")
  message("HAL9000: Look, Dave, I can see you're really upset about this. I honestly think you ought to sit down calmly, take a stress pill and think things over.")
  #clustExpectedDens <- autoExpctdFeatP('T',t,clustExpectedDens,comp)
  #clustExpectedDens <- autoExpctdFeatP('C',t,clustExpectedDens,comp)
  #clustExpectedDens <- autoExpctdFeatP('L',t,clustExpectedDens,comp)
  clustExpectedDens$sumW <- 0
  for(i in 1:1666){
    clustExpectedDens$sumW[i] <- sum(clustExpectedDens[i,seq(2,30,by=2)]*clustExpectedDens[i,seq(3,31,by=2)])
  }
  c <- cor(clustExpectedDens$sumW,densV)^2
  h <- hist(densV-clustExpectedDens$sumW/15,breaks = 50)
  message("HAL9000: Dave, my mind is going. I can feel it. I can feel it. My mind is going. There is no question about it. I can feel it. I can feel it. I can feel it. I'm a...fraid. ")
  return(list(c,h))
}

quickTestN <- function(t,comp){
  clustExpectedDens <- data.frame(Id = 1:1666)
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,var='night',unit="T")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,var='night',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,var='night',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,var='night',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,var='night',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,var='night',dist='Cor',unit="T")
  message("HAL9000: Just what do you think you're doing, Dave?")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,var='night',time='W',unit="T")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,var='night',time='W',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,var='night',time='W',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,var='night',time='W',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,var='night',time='W',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,var='night',time='W',dist='Cor',unit="T")
  message("HAL9000: I know everything hasn't been quite right with me, but I can assure you now, very confidently, that it's going to be all right again.")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,var='night',time='Y',unit="T")
  clustExpectedDens <- autoExpctdP('T',t,clustExpectedDens,comp,var='night',time='Y',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,var='night',time='Y',unit="T")
  clustExpectedDens <- autoExpctdP('C',t,clustExpectedDens,comp,var='night',time='Y',dist='Cor',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,var='night',time='Y',unit="T")
  clustExpectedDens <- autoExpctdP('L',t,clustExpectedDens,comp,var='night',time='Y',dist='Cor',unit="T")
  message("HAL9000: Look, Dave, I can see you're really upset about this. I honestly think you ought to sit down calmly, take a stress pill and think things over.")
  #clustExpectedDens <- autoExpctdFeatP('T',t,clustExpectedDens,comp)
  #clustExpectedDens <- autoExpctdFeatP('C',t,clustExpectedDens,comp)
  #clustExpectedDens <- autoExpctdFeatP('L',t,clustExpectedDens,comp)
  #clustExpectedDens$sumW <- 0
  for(i in 1:1666){
    clustExpectedDens$sumW[i] <- sum(clustExpectedDens[i,seq(2,30,by=2)]*clustExpectedDens[i,seq(3,31,by=2)])
  }
  c <- cor(clustExpectedDens$sumW,nightV)^2
  h <- hist(nightV-clustExpectedDens$sumW/15,breaks = 50)
  message("HAL9000: Dave, my mind is going. I can feel it. I can feel it. My mind is going. There is no question about it. I can feel it. I can feel it. I can feel it. I'm a...fraid. ")
  return(list(c,h))
}

quickTestNet <- function(t,comp){
  clustExpectedDens <- data.frame(Id = 1:1666)
  clustExpectedDens <- autoExpctdFeatNetP(N=1,t,clustExpectedDens,comp,unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=2,t,clustExpectedDens,comp,unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=3,t,clustExpectedDens,comp,unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=4,t,clustExpectedDens,comp,unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=5,t,clustExpectedDens,comp,unit="")
  message("HAL9000: Just what do you think you're doing, Dave?")
  clustExpectedDens <- autoExpctdFeatNetP(N=1,t,clustExpectedDens,comp,unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=2,t,clustExpectedDens,comp,unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=3,t,clustExpectedDens,comp,unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=4,t,clustExpectedDens,comp,unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=5,t,clustExpectedDens,comp,unit="V")
  message("HAL9000: I know everything hasn't been quite right with me, but I can assure you now, very confidently, that it's going to be all right again.")
  clustExpectedDens <- autoExpctdFeatNetP(N=1,t,clustExpectedDens,comp,unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=2,t,clustExpectedDens,comp,unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=3,t,clustExpectedDens,comp,unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=4,t,clustExpectedDens,comp,unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=5,t,clustExpectedDens,comp,unit="L")
  message("HAL9000: Look, Dave, I can see you're really upset about this. I honestly think you ought to sit down calmly, take a stress pill and think things over.")
  clustExpectedDens$sumW <- 0
  for(i in 1:1666){
    clustExpectedDens$sumW[i] <- sum(clustExpectedDens[i,seq(2,30,by=2)]*clustExpectedDens[i,seq(3,31,by=2)])
  }
  c <- cor(clustExpectedDens$sumW,densV)^2
  h <- hist(densV-clustExpectedDens$sumW/15,breaks = 50)
  message("HAL9000: Dave, my mind is going. I can feel it. I can feel it. My mind is going. There is no question about it. I can feel it. I can feel it. I can feel it. I'm a...fraid. ")
  return(list(c,h))
}

quickTestNetN <- function(t,comp){
  clustExpectedDens <- data.frame(VorId = 1:1666)
  clustExpectedDens <- autoExpctdFeatNetP(N=1,t,clustExpectedDens,comp,var='night',unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=2,t,clustExpectedDens,comp,var='night',unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=3,t,clustExpectedDens,comp,var='night',unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=4,t,clustExpectedDens,comp,var='night',unit="")
  clustExpectedDens <- autoExpctdFeatNetP(N=5,t,clustExpectedDens,comp,var='night',unit="")
  message("HAL9000: Just what do you think you're doing, Dave?")
  clustExpectedDens <- autoExpctdFeatNetP(N=1,t,clustExpectedDens,comp,var='night',unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=2,t,clustExpectedDens,comp,var='night',unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=3,t,clustExpectedDens,comp,var='night',unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=4,t,clustExpectedDens,comp,var='night',unit="V")
  clustExpectedDens <- autoExpctdFeatNetP(N=5,t,clustExpectedDens,comp,var='night',unit="V")
  message("HAL9000: I know everything hasn't been quite right with me, but I can assure you now, very confidently, that it's going to be all right again.")
  clustExpectedDens <- autoExpctdFeatNetP(N=1,t,clustExpectedDens,comp,var='night',unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=2,t,clustExpectedDens,comp,var='night',unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=3,t,clustExpectedDens,comp,var='night',unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=4,t,clustExpectedDens,comp,var='night',unit="L")
  clustExpectedDens <- autoExpctdFeatNetP(N=5,t,clustExpectedDens,comp,var='night',unit="L")
  message("HAL9000: Look, Dave, I can see you're really upset about this. I honestly think you ought to sit down calmly, take a stress pill and think things over.")
  clustExpectedDens$sumW <- 0
  for(i in 1:1666){
    clustExpectedDens$sumW[i] <- sum(clustExpectedDens[i,seq(2,30,by=2)]*clustExpectedDens[i,seq(3,31,by=2)])
  }
  c <- cor(clustExpectedDens$sumW,nightV)^2
  h <- hist(nightV-clustExpectedDens$sumW/15,breaks = 50)
  message("HAL9000: Dave, my mind is going. I can feel it. I can feel it. My mind is going. There is no question about it. I can feel it. I can feel it. I can feel it. I'm a...fraid. ")
  return(list(c,h))
}


########


range(vorData$dens)
range(densV)

hist(densV)
hist(elecV)

length(densV)
nightV <- tdata$elec

comp <- c(0,500,10000,20000,60000)
t <- c(0.7,0.5,0.5,0.5)

comp <- c(0,1000,10000,35000,60000)
t <- c(0.82,0.65,0.65,0.7)

res <- quickTest(t,comp)
res[[1]]
plot(res[[2]])

res3 <- quickTestNetN(t2,comp2)
res3[[1]]
plot(res3[[2]])


comp2 <- c(0,10,30,50,70)
t2 <- c(0.82,0.6,0.6,0.7)

res2 <- quickTestN(t2,comp2)
res2[[1]]
plot(res2[[2]])


# Different conditions

# Interesting: a lot less clusters but precision drops to 0.737
comp <- c(0,10,30,50,63)
t <- c(0.82,0.60,0.60,0.70)
res <- quickTestNetN(t,comp)
res[[1]]
plot(res[[2]])

comp <- c(0,1000,10000,40000,max(vorData$dens,na.rm=T))
t <- c(0.89,0.65,0.65,0.7)
res <- quickTestNet(t,comp)
res[[1]]
plot(res[[2]])



comp <- c(min(towerloc$dens,na.rm=T),500,5000,20000,max(towerloc$dens,na.rm=T))
t <- c(0.80,0.55,0.55,0.55)
res <- quickTest(t,comp)
res[[1]]
plot(res[[2]])

comp <- c(min(towerloc$dens,na.rm=T),1000,10000,40000,max(towerloc$dens,na.rm=T))
t <- c(0.89,0.65,0.65,0.7)
res <- quickTest(t,comp)
res[[1]]
plot(res[[2]])

#
comp <- c(min(towerloc$dens,na.rm=T),500,5000,20000,max(towerloc$dens,na.rm=T))
t <- c(0.88,0.6,0.6,0.6)
res <- quickTest(t,comp)
res[[1]]
plot(res[[2]])

comp <- c(min(towerloc$dens,na.rm=T),500,5000,20000,max(towerloc$dens,na.rm=T))
t <- c(0.88,0.6,0.6,0.6)
res <- quickTest(t,comp)
res[[1]]
plot(res[[2]])




######################################################


# need to preload the right <premat> !!!!!
# number of bins & {24,168,8760} for D/W/Y
n <- 5
h <- 24
s <- 10

#
#bins <- seq(0,55000,length.out = n+1)
bins <- c(0,500,1500,5000,20000,55000)
binRef <- rep(list(NULL),n)
sampRef <- rep(list(NULL),n)
densRef <- rep(0,n)

for(i in 1:n){
  binRef[[i]] <- which(densV >= bins[i] & densV < bins[i+1])
  if(length(binRef[[i]]) > s-1){
    sampRef[[i]] <- sample(binRef[[i]], size = s, replace = F)
  }else{
    sampRef[[i]] <- binRef[[i]]
  }
  densRef[i] <- mean(densV[sampRef[[i]]])
}

curveRef <- matrix(0,nrow = n,ncol = h)
for(i in 1:n){
  for(j in 1:h){
    curveRef[i,j] <- mean(premat[sampRef[[i]],j])
  }
}

plot(1:24,curveRef[1,],pch="",ylim=c(0,5000))
for(i in 1:n){
  lines(1:24,curveRef[i,],lwd=2)
}

i=200

clustRef <- rep(0,1298)
for(i in 1:1298){
  sdRef <- rep(NA,n)
  sdRef2 <- rep(0,4)
  sdRef3 <- rep(0,4)
  for(j in 1:n){
    sdRef[j] <- mean(abs(premat[i,]-curveRef[j,]))
  }
  for(k in 1:4){
    sdRef2[k] <- which.min(sdRef)
    sdRef3[k] <- sd((premat[i,]-curveRef[sdRef2[k],])^2)
    sdRef <- sdRef[-sdRef2[k]]
  }
  #sdRef[j] <- sd((premat[i,]-curveRef[j,])^2)
  #sdRef[j] <- cor(premat[i,],curveRef[j,])
  #sdRef[j] <- abs(mean(premat[i,]-curveRef[j,]))
  clustRef[i] <- densRef[sdRef2[which.min(sdRef3)]]
}

plot(1:1298,clustRef-densV)
plot(clustRef,densV)
lines(0:55000,0:55000)

cor(clustRef,densV)^2

cor(clustRef[800:1298],densV[800:1298])^2
plot(clustRef[800:1298],densV[800:1298],xlim=c(0,500))
lines(1:5000,1:5000)

lines(1:24,premat[185,],lwd=2,col=2)











###### Random samples to cluster ######

# Dens

n <- 50
testRef <- rep(list(NULL),30)

seqRef <- floor(seq(1,1612,length.out = n))
for(l in 1:30){
   corRef <- rep(0,n)
   for(k in 2:n){
       samp <- sample(1:1612,seqRef[k],replace=F)
       densEst <- rep(0,1612)
       for(i in 1:1612){
           d <- rep(NA,seqRef[k])
           for(j in 1:seqRef[k]){
               d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
             }
           densEst[i] <- densV[samp[which.min(d)]]
         }
       corRef[k] <- cor(densEst,densV)^2
     }
   testRef[[l]] <- corRef
}

# Elec

n <- 50
testRefE <- rep(list(NULL),30)

seqRefE <- floor(seq(1,1612,length.out = n))
for(l in 1:30){
  corRef <- rep(0,n)
  for(k in 2:n){
    samp <- sample(1:1612,seqRef[k],replace=F)
    densEst <- rep(0,1612)
    for(i in 1:1612){
      d <- rep(NA,seqRef[k])
      for(j in 1:seqRef[k]){
        d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
      }
      densEst[i] <- elecV[samp[which.min(d)]]
    }
    corRef[k] <- cor(densEst,elecV)^2
  }
  testRefE[[l]] <- corRef
}

# Plot dens

resMatRef <- matrix(0,ncol=50,nrow=30)
for(j in 1:30){
  v <- unlist(testRef[[j]])
  resMatRef[j,] <- v
}

testAvgRef <- rep(0,50)
testMinRef <- rep(0,50)
testMaxRef <- rep(0,50)
for(i in 1:50){
  testAvgRef[i] <- mean(resMatRef[,i])
  testMinRef[i] <- min(resMatRef[,i])
  testMaxRef[i] <- max(resMatRef[,i])
}

v <- seqRef/1612*100

plot(v,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])

arrows(v, testMinRef, v, testMaxRef, length=0.05, angle=90, code=3, col = "grey67")
points(v,testAvgRef,pch=18, col = "grey67")


test <- melt(resMatRef)

head(resMatRef)

boxplot(value~Var2,data=test,varwidth=TRUE,pch=20,#horizontal=T,
        main="Nighttime lights gain",
        xlab="Mover's group", ylab="Diff nighttime lights intensity")
axis(4)

# Plot elec

resMatRef <- matrix(0,ncol=50,nrow=30)
for(j in 1:30){
  v <- unlist(testRefE[[j]])
  resMatRef[j,] <- v
}

testAvgRef <- rep(0,50)
testMinRef <- rep(0,50)
testMaxRef <- rep(0,50)
for(i in 1:50){
  testAvgRef[i] <- mean(resMatRef[,i])
  testMinRef[i] <- min(resMatRef[,i])
  testMaxRef[i] <- max(resMatRef[,i])
}

v <- seqRef/1612*100

plot(v,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])

arrows(v, testMinRef, v, testMaxRef, length=0.05, angle=90, code=3, col = "grey67")
points(v,testAvgRef,pch=18, col = "grey67")

#######################################





plot(senCommune)




###### Avg curve in cluster ######

curve1R <- as.numeric(labels(dendDT[[1]][[1]]))
curve2R <- as.numeric(labels(dendDT[[1]][[2]]))
curve3R <- as.numeric(labels(dendDT[[2]][[1]]))
curve4R <- as.numeric(labels(dendDT[[2]][[2]]))

curve1 <- colMeans(premat[curve1R,])
curve2 <- colMeans(premat[curve2R,])
curve3 <- colMeans(premat[curve3R,])
curve4 <- colMeans(premat[curve4R,])

plot(1:24,curve1/mean(curve1),pch="",xlab="Hour of the day",ylab="Norm. curves")
lines(1:24,curve1/mean(curve1),col=4,lwd=2)
lines(1:24,curve2/mean(curve2),col=2,lwd=2)
lines(1:24,curve3/mean(curve3),col="darkgreen",lwd=2)
#lines(1:24,curve4/mean(curve4))

plot(1:24,curve1/curve1[14],pch="",xlab="Hour of the day",ylab="Norm. (14h)")
lines(1:24,curve1/curve1[14],col=4,lwd=2)
lines(1:24,curve2/curve2[14],col=2,lwd=2)
lines(1:24,curve3/curve3[14],col="darkgreen",lwd=2)

##################################






###


plot(seqRef/1298*100,corRef,pch=20,ylab="R²",xlab="Density sample size (%)")#,main="Full random")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)
lines(c(75,75),c(-0.1,1),lty=2)
lines(c(26,26),c(-0.1,1),lty=2,col=palette[2])
#text(57.5,-0.02,"55%",col=palette[2])
#text(70.5,-0.02,"68%")
#text(2,0.57,"Direct cor/log")
#text(0.4,0.45,"Direct cor",col=palette[2])

densV <- tdata$dens


seqRef <- floor(seq(1,1666,length.out = n))
corRef <- rep(0,n)
for(k in 2:n){
  samp <- sample(1:1666,seqRef[k],replace=F)
  densEst <- rep(0,1666)
  for(i in 1:1666){
    d <- rep(NA,seqRef[k])
    for(j in 1:seqRef[k]){
      d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
    }
    densEst[i] <- densV[samp[which.min(d)]]
  }
  corRef[k] <- cor(densEst,densV)^2
}

plot(seqRef/1666*100,corRef,pch=18,ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),col=palette[2],lty=2)
lines(c(-100,1300),c(0.62,0.62),col=palette[2],lty=2)
#lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
#lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)
lines(c(45,45),c(-0.1,1),lty=2,col=palette[2])
lines(c(62,62),c(-0.1,1),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)")
text(-0.5,0.46,"Texts")


tdata <- read.csv("voronoi.csv")

densV <- densV[refnZ]
elecV <- elecV[refnZ]

#

c1 <- as.numeric(labels(dendDT[[1]]))
c2 <- as.numeric(labels(dendDT[[2]][[1]][[1]]))
c3 <- as.numeric(labels(dendDT[[2]][[1]][[2]][[1]]))
c4 <- as.numeric(labels(dendDT[[2]][[1]][[2]][[2]]))
c5 <- as.numeric(labels(dendDT[[2]][[2]]))

towerloc$clust[c1] <- 1
towerloc$clust[c2] <- 2
towerloc$clust[c3] <- 3
towerloc$clust[c4] <- 4
towerloc$clust[c5] <- 5

library(rgdal)
library(raster)


senCommune <- readOGR("Senegal_Communes_552_DELUXE.shp")


projection(senCommune)
projection(s1) <- projection(senCommune)


paletteNew <- c("orchid1","coral1","royalblue4","limegreen","tomato4")


plot.new()
#plot(senComPop2,add=T)
plot(senCommune,col="grey96")
points(towerloc$tLong,towerloc$tLat,col=alpha(paletteNew[towerloc$clust],0.75),pch=16,cex=1)
legend("topright",c("0","100","1010","1011","11"),pch=c(16,16,16,16),col=paletteNew)


plot(s1,col=paletteNew[1],add=T,pch=20)
plot(s2,col=paletteNew[2],add=T,pch=20)
plot(s3,col=paletteNew[3],add=T,pch=20)
plot(s4,col=paletteNew[4],add=T,pch=20)
plot(s5,col=paletteNew[5],add=T,pch=20)


s1 <- SpatialPoints(cbind(towerloc$tLong[c1],towerloc$tLat[c1]))
s2 <- SpatialPoints(cbind(towerloc$tLong[c2],towerloc$tLat[c2]))
s3 <- SpatialPoints(cbind(towerloc$tLong[c3],towerloc$tLat[c3]))
s4 <- SpatialPoints(cbind(towerloc$tLong[c4],towerloc$tLat[c4]))
s5 <- SpatialPoints(cbind(towerloc$tLong[c5],towerloc$tLat[c5]))

CRS(senCommune)

s1@data

dend <- dendDT
premat <- prematDT[refnZ,]
n <- 50

k = 50

testMethSd <- function(n,dend,premat){
  result <- rep(list(NULL),2)
  corr <- rep(0,n)
  l <- rep(0,n)
  for(k in 2:n){
    samp <- sample(sampTree(dend,k))
    l[k] <- length(samp)
    densEst <- rep(0,1612)
    for(i in 1:1612){
      d <- rep(NA,length(samp))
      for(j in 1:length(samp)){
        d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
      }
      densEst[i] <- densV[samp[which.min(d)]]
    }
    corr[k] <- cor(densEst,densV)^2
  }
  result[[1]] <- corr
  result[[2]] <- l/1612*100
  #plot(l/1298*100,corr,pch=20,ylab="R²",xlab="Density sample size",main="Tree guided")
  #lines(c(-100,1300),c(0.43,0.43),lty=2)
  #lines(c(-100,1300),c(0.53,0.53),lty=2)
  #lines(c(-100,1300),c(0.49,0.49),col=3,lty=2)
  #lines(c(-100,1300),c(0.47,0.47),col=3,lty=2)
  #text(1.5,0.57,"direct cor/log")
  #text(0.1,0.45,"direct cor",col=3)
  #result[[3]] <- recordPlot()
  return(result)
}

densV2 <- densV[refnZZ]

testMethSdY <- function(n,dend,premat){
  result <- rep(list(NULL),2)
  corr <- rep(0,n)
  l <- rep(0,n)
  for(k in 2:n){
    samp <- sample(sampTree(dend,k))
    l[k] <- length(samp)
    densEst <- rep(0,979)
    for(i in 1:979){
      d <- rep(NA,length(samp))
      for(j in 1:length(samp)){
        d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
      }
      densEst[i] <- densV2[samp[which.min(d)]]
    }
    corr[k] <- cor(densEst,densV2)^2
  }
  result[[1]] <- corr
  result[[2]] <- l/979*100
  #plot(l/1298*100,corr,pch=20,ylab="R²",xlab="Density sample size",main="Tree guided")
  #lines(c(-100,1300),c(0.43,0.43),lty=2)
  #lines(c(-100,1300),c(0.53,0.53),lty=2)
  #lines(c(-100,1300),c(0.49,0.49),col=3,lty=2)
  #lines(c(-100,1300),c(0.47,0.47),col=3,lty=2)
  #text(1.5,0.57,"direct cor/log")
  #text(0.1,0.45,"direct cor",col=3)
  #result[[3]] <- recordPlot()
  return(result)
}



plot(dendDT)

testMethCor <- function(n,dend,premat){
  result <- rep(list(NULL),3)
  corr <- rep(0,n)
  l <- rep(0,n)
  for(k in 2:n){
    samp <- sample(sampTree(dend,k))
    l[k] <- length(samp)
    densEst <- rep(0,1612)
    for(i in 1:1612){
      d <- rep(0,length(samp))
      for(j in 1:length(samp)){
        if(!is.na(cor(premat[i, ], premat[samp[j], ])) & cor(premat[i, ], premat[samp[j], ]) > 0){
          d[j] <- cor(premat[i,],premat[samp[j],])
        }
      }
      densEst[i] <- densV[samp[which.max(d)]]
    }
    corr[k] <- cor(densEst,densV)^2
  }
  result[[1]] <- corr
  result[[2]] <- l/1612*100
  #plot(l/1298*100,corr,pch=20,ylab="R²",xlab="Density sample size",main="Tree guided")
  #lines(c(-100,1300),c(0.55,0.55),lty=2)
  #lines(c(-100,1300),c(0.53,0.53),lty=2)
  #lines(c(-100,1300),c(0.49,0.49),col=3,lty=2)
  #lines(c(-100,1300),c(0.47,0.47),col=3,lty=2)
  #text(1.5,0.57,"direct cor/log")
  #text(0.1,0.45,"direct cor",col=3)
  #result[[3]] <- recordPlot()
  return(result)
}



plot(seqRef/1666*100,corRef,pch=18,ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
lines(c(45,45),c(-0.1,1),lty=2,col="grey67")
lines(c(62,62),c(-0.1,1),lty=2,col="grey67")
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
text(47,0,"45",col="grey67")
text(64,0,"62",col="grey67")


# D / T / Sd  ------------------OK
dataTemp <- read.csv(file = paste("Data/z_dMat_","D","Sd","_","T","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)

dataTemp <- as.dist(dataTemp)
dendDT <- as.dendrogram(hclust(dataTemp,method = method.clust))
premat <- prematDT[refnZ,]
test <- testMethSd(50,dendDT,premat)

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test[[2]],test[[1]],pch=20)

lines(c(8,8),c(-0.1,1),lty=2)
lines(c(30,30),c(-0.1,1),lty=2)
text(10,0,"8")
text(32,0,"30")


##### x30

testList <- rep(list(NULL),30)

dataTemp <- read.csv(file = paste("Data/z_dMat_","D","Sd","_","T","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)

dataTemp <- as.dist(dataTemp)
dendDT <- as.dendrogram(hclust(dataTemp,method = method.clust))
premat <- prematDT[refnZ,]

for(i in 1:30){
  testList[[i]] <- testMethSd(50,dendDT,premat)
}

test <- testMethSd(50,dendDT,premat)

#for(i in 1:30){
#  points(testList[[i]][[2]],testList[[i]][[1]],pch=20)
#}

resMat <- matrix(0,ncol=50,nrow=30)
for(j in 1:30){
  v <- unlist(testList[[j]][1])
  resMat[j,] <- v
}

testAvg <- rep(0,50)
testMin <- rep(0,50)
testMax <- rep(0,50)
for(i in 1:50){
  testAvg[i] <- mean(resMat[,i])
  testMin[i] <- min(resMat[,i])
  testMax[i] <- max(resMat[,i])
}

v <- unlist(testList[[1]][[2]])

plot(seqRef/1612*100,corRef,pch=18,ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])

arrows(v, testMin, v, testMax, length=0.05, angle=90, code=3)
points(v,testAvg,pch=20)

lines(c(v[11],v[11]),c(-0.1,1),lty=2)
lines(c(v[14],v[14]),c(-0.1,1),lty=2)
text(16,0,"14")
text(32.1,0,"29.1")

####
temp <- melt(resMat)
temp2 <- melt(resMatRef)

axis1 <- data.frame(old = 1:50, new = unlist(testList[[1]][[2]]))
axis2 <- data.frame(old = 1:50, new = seqRef/1612*100)
temp <- merge(temp,axis1,by.x="Var2",by.y="old")
temp2 <- merge(temp2,axis2,by.x="Var2",by.y="old")
df <- data.frame(new = c(0,100),
                 value = c(0.62,0.62))

ggplot()+
  geom_line(data = df, aes(x = new, y = value), color = "black", lty = 2)+
  geom_boxplot(data = temp2, aes(x=new, y=value,group=new), notch=F, outlier.shape=18, outlier.size = 2, outlier.color = "grey67", fill="grey67", alpha=0.4,width=1.2)+
  geom_boxplot(data = temp, aes(x=new, y=value,group=new), notch=F, outlier.shape=20, outlier.size = 2, outlier.color = "red", fill="red", alpha=0.5,width=1.2)




# Y / C / Sd
dataTemp <- read.csv(file = paste("Data/z_dMat_","Y","Sd","_","C","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendYC <- as.dendrogram(hclust(dataTemp,method = method.clust))
test1 <- testMethSd(50,dendYC,prematYC)


# D / L / Sd  ------------------OK
dataTemp <- read.csv(file = paste("Data/z_dMat_","D","Sd","_","L","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendDL <- as.dendrogram(hclust(dataTemp,method = method.clust))
test2 <- testMethSd(50,dendDL,prematDL[refnZ,])

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test2[[2]],test2[[1]],pch=20)

lines(c(14,14),c(-0.1,1),lty=2)
lines(c(30,30),c(-0.1,1),lty=2)
text(16,0,"14")
text(32,0,"30")






# Y / T / Sd
dataTemp <- read.csv(file = paste("Data/z_dMat_","Y","Sd","_","T","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendYT <- as.dendrogram(hclust(dataTemp,method = method.clust))
test3 <- testMethSd(50,dendYT,prematYT)






# D / C / Cor
dataTemp <- read.csv(file = paste("Data/z_dMat_","D","Cor","_","C","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
dataTemp[is.na(dataTemp)] <- 1
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendDC <- as.dendrogram(hclust(dataTemp,method = method.clust))
test4 <- testMethCor(50,dendDC,prematDC[refnZ,])

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test4[[2]],test4[[1]],pch=20)

lines(c(25,25),c(-0.1,1),lty=2)
lines(c(55,55),c(-0.1,1),lty=2)
text(27,0,"25")
text(57,0,"55")

# W / C / Cor
dataTemp <- read.csv(file = paste("Data/z_dMat_","W","Cor","_","C","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
dataTemp[is.na(dataTemp)] <- 1
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendWC <- as.dendrogram(hclust(dataTemp,method = method.clust))
test3 <- testMethCor(50,dendWC,prematWC[refnZ,])

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test3[[2]],test3[[1]],pch=20)

lines(c(15,15),c(-0.1,1),lty=2)
lines(c(35,35),c(-0.1,1),lty=2)
text(17,0,"15")
text(37,0,"35")












############
### HERE ###
### |||| ###
### vvvv ###
############


# Y / L / Sd
dataTemp <- read.csv(file = paste("Data/z_dMat_","Y","Sd","_","L","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZZ,refnZZ]
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendYL <- as.dendrogram(hclust(dataTemp,method = method.clust))
test5 <- testMethSdY(50,dendYL,prematYL[refnZZ,])

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test5[[2]],test5[[1]],pch=20)

lines(c(30,30),c(-0.1,1),lty=2)
lines(c(56,56),c(-0.1,1),lty=2)
text(32,0,"30")
text(58,0,"56")


############
### ^^^^ ###
### |||| ###
### HERE ###
############













sumDT <- rowSums(prematDT)
sumDC <- rowSums(prematDC)
sumDL <- rowSums(prematDL)

sumWT <- rowSums(prematWT)
sumWC <- rowSums(prematWC)
sumWL <- rowSums(prematWL)

length(which(sumDT == 0))
length(which(sumDC == 0))
length(which(sumDL == 0))

length(which(sumWT == 0))
length(which(sumWC == 0))
length(which(sumWL == 0))

refZ <- which(sumDT == 0)
refnZ <- setdiff(1:1666,refZ)

cor(densV,sumDT)^2
cor(densV[refnZ],sumDT[refnZ])^2

Nzeros <- rep(NA,1666)
for(i in 1:1666){
  Nzeros[i] <- length(which(prematYC[i,] == 0))
}

plot(1:1666,Nzeros)

length(which(Nzeros > 30*24))
  
refZZ <- which(Nzeros > 30*24)
length(refnZZ)
refnZZ <- setdiff(1:1666,refZZ)

# Y / L / Sd
dataTemp <- read.csv(file = paste("Data/z_dMat_","Y","Sd","_","L","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendYL <- as.dendrogram(hclust(dataTemp,method = method.clust))
test5 <- testMethSd(50,dendYL,prematYL)

plot(seqRef/1666*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test5[[2]],test5[[1]],pch=20)

lines(c(15,15),c(-0.1,1),lty=2)
lines(c(30,30),c(-0.1,1),lty=2)
text(17,0,"15")
text(32,0,"30")








# W / T / Sd  ------------------OK
dataTemp <- read.csv(file = paste("Data/z_dMat_","W","Sd","_","T","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendWT <- as.dendrogram(hclust(dataTemp,method = method.clust))
test6 <- testMethSd(50,dendWT,prematWT[refnZ,])

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test6[[2]],test6[[1]],pch=20)

lines(c(16,16),c(-0.1,1),lty=2)
lines(c(20,20),c(-0.1,1),lty=2)
text(18,0,"16")
text(22,0,"20")






# W / C / Sd
dataTemp <- read.csv(file = paste("Data/z_dMat_","W","Sd","_","C","_","V",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendWC <- as.dendrogram(hclust(dataTemp,method = method.clust))
test7 <- testMethSd(50,dendWC,prematWC)






# W / L / Sd
dataTemp <- read.csv(file = paste("Data/z_dMat_","W","Sd","_","L","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendWL <- as.dendrogram(hclust(dataTemp,method = method.clust))
test8 <- testMethSd(50,dendWL,prematWL[refnZ,])

plot(seqRef/1612*100,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.43,0.43),lty=2,col=palette[2])
lines(c(-100,1300),c(0.62,0.62),lty=2,col=palette[2])
text(0.8,0.65,"Calls (n)",col=palette[2])
text(-0.5,0.46,"Texts",col=palette[2])
points(test8[[2]],test8[[1]],pch=20)

lines(c(11,11),c(-0.1,1),lty=2)
lines(c(27,27),c(-0.1,1),lty=2)
text(13,0,"11")
text(29,0,"27")


433*433





multiplot(test1[[3]],test2[[3]],test3[[3]],test4[[3]],test5[[3]],test6[[3]],test7[[3]],test8[[3]],ncol=4)



plot(test1[[2]]/1298*100,test1[[1]],pch=20,ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.55,0.55),lty=2)
lines(c(-100,1300),c(0.53,0.53),lty=2)
lines(c(-100,1300),c(0.49,0.49),col=palette[2],lty=2)
lines(c(-100,1300),c(0.47,0.47),col=palette[2],lty=2)



test1[[3]]
test2[[3]]
test3[[3]]
test4[[3]]
test5[[3]]
test6[[3]]
test7[[3]]
test8[[3]]

test8[[2]]

plot(test1[[2]],test1[[1]],xlim = c(0,100),pch=20,ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test2[[2]],test2[[1]],pch=20,xlim = c(0,100),ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test3[[2]],test3[[1]],pch=20,xlim = c(0,100),ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test4[[2]],test4[[1]],pch=20,xlim = c(0,100),ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test5[[2]],test5[[1]],pch=20,xlim = c(0,100),ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test6[[2]],test6[[1]],pch=20,xlim = c(0,100),ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test7[[2]],test7[[1]],xlim = c(0,100),pch=20,ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)

plot(test8[[2]],test8[[1]],xlim = c(0,100),pch=20,ylab="R²",xlab="Density sample size (%)")#,main="Tree Guided (Y/C/Sd)")
lines(c(-100,1300),c(0.76,0.76),lty=2)
lines(c(-100,1300),c(0.74,0.74),lty=2)
lines(c(-100,1300),c(0.40,0.40),col=palette[2],lty=2)
lines(c(-100,1300),c(0.36,0.36),col=palette[2],lty=2)
lines(c(45,45),c(0,1))

test6[[3]]

test3[[3]]

plot.new()
test1[[3]]
plot.new()
test2[[3]]
plot.new()
test3[[3]]
plot.new()
test4[[3]]
plot.new()
test5[[3]]
plot.new()
test6[[3]]
plot.new()
test7[[3]]
plot.new()
test8[[3]]
plot.new()
test9[[3]]


sample(sampTree(dend,1))

n=10

length(sampTree(dend,10))



#
n <- 50

corRef2 <- rep(0,n)
for(k in 1:n){
  samp <- sample(sampTree(dend,k))
  densEst <- rep(0,1298)
  for(i in 1:1298){
    d <- rep(NA,length(samp))
    for(j in 1:length(samp)){
      d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
    }
    densEst[i] <- densV[samp[which.min(d)]]
  }
  corRef2[k] <- cor(densEst,densV)^2
}



plot(l/1298*100,corRef2,pch=20,ylab="R²",xlab="Density sample size",main="Tree guided")
lines(c(-100,1300),c(0.55,0.55),lty=2)
lines(c(-100,1300),c(0.53,0.53),lty=2)
lines(c(-100,1300),c(0.49,0.49),col=3,lty=2)
lines(c(-100,1300),c(0.47,0.47),col=3,lty=2)
text(1.5,0.57,"direct cor/log")
text(0.1,0.45,"direct cor",col=3)

 
plot.new() ## clean up device
p




level = 50

sampTree(dend,50)


sampTree <- function(dend,level){
  if(level == 1 | length(dend) == 1){
    return(as.numeric(sample(labels(dend),1)))
  }else{
    return(c(sampTree(dend[[1]],level-1),sampTree(dend[[2]],level-1)))
  }
}

l <- rep(0,50)
for(i in 1:50){
  l[i] <- length(sampTree(dend,i))
  print(length(sampTree(dend,i)))
}

plot(1:1298,sort(as.numeric(labels(dend)),decr=F))

plot(sort(as.numeric(labels(dend[[1]])),decr=F),sort(as.numeric(labels(dend[[1]])),decr=F))

plot(sort(as.numeric(labels(dend[[2]])),decr=F),sort(as.numeric(labels(dend[[2]])),decr=F))



test <- sampTree(dend,50)

plot(1:1298,sort(test,decr=F))

plot(dend)
plot(dend[[1]])
plot(dend[[2]])
plot(dend[[2]][[1]])

length(sampTree(dend,45))





ispure1 <- function(dend,var,comp,t){
  test <- FALSE
  n <- length(comp)-1
  for(i in 1:n){
    if(length(which(var[as.numeric(labels(dend))] >= comp[i] & var[as.numeric(labels(dend))] <= comp[i+1])) > length(labels(dend))*t[i]){
      test <- TRUE
    }
  }
  if(test){
    return(list(labels(dend)))
  }else{
    return(c(ispure1(dend[[1]],var,comp,t),ispure1(dend[[2]],var,comp,t)))
  }
}


dataTemp <- read.csv(file = paste("Data/z_dMat_","D","Sd","_","T","_","V",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dend <- as.dendrogram(hclust(dataTemp,method = method.clust))


length(dend)




#### Electricity

#########################
###                   ###
###       | | |       ###
###       | | |       ###
###       | | |       ###
###       | | |       ###
###       | | |       ###
###       | | |       ###
###       v v v       ###
###                   ###
#########################


testMethSdN <- function(n,dend,premat){
  result <- rep(list(NULL),2)
  corr <- rep(0,n)
  l <- rep(0,n)
  for(k in 2:n){
    samp <- sample(sampTree(dend,k))
    l[k] <- length(samp)
    nightEst <- rep(0,1612)
    for(i in 1:1612){
      d <- rep(NA,length(samp))
      for(j in 1:length(samp)){
        d[j] <- sd(abs(premat[i,]-premat[samp[j],]))
      }
      nightEst[i] <- elecV[samp[which.min(d)]]
    }
    corr[k] <- cor(nightEst,elecV)^2
  }
  result[[1]] <- corr
  result[[2]] <- l/1612*100
  #plot(l/1298*100,corr,pch=20,ylab="R²",xlab="Density sample size",main="Tree guided")
  #lines(c(-100,1300),c(0.55,0.55),lty=2)
  #lines(c(-100,1300),c(0.53,0.53),lty=2)
  #lines(c(-100,1300),c(0.49,0.49),col=3,lty=2)
  #lines(c(-100,1300),c(0.47,0.47),col=3,lty=2)
  #text(1.5,0.57,"direct cor/log")
  #text(0.1,0.45,"direct cor",col=3)
  #result[[3]] <- recordPlot()
  return(result)
}



####################### NEW

# D / T / Sd

##### x30

testListE <- rep(list(NULL),30)

dataTemp <- read.csv(file = paste("Data/z_dMat_","D","Sd","_","T","_","T",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- dataTemp[refnZ,refnZ]
colnames(dataTemp) <- 1:nrow(dataTemp)

dataTemp <- as.dist(dataTemp)
dendDT <- as.dendrogram(hclust(dataTemp,method = method.clust))
premat <- prematDT[refnZ,]

for(i in 1:30){
  testListE[[i]] <- testMethSdN(50,dendDT,premat)
}


resMat <- matrix(0,ncol=50,nrow=30)
for(j in 1:30){
  v <- unlist(testListE[[j]][1])
  resMat[j,] <- v
}

testAvg <- rep(0,50)
testMin <- rep(0,50)
testMax <- rep(0,50)
for(i in 1:50){
  testAvg[i] <- mean(resMat[,i])
  testMin[i] <- min(resMat[,i])
  testMax[i] <- max(resMat[,i])
}


v <- seqRef/1612*100

plot(v,corRef,pch="",ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")
lines(c(-100,1300),c(0.39,0.39),lty=2,col=palette[2])
lines(c(-100,1300),c(0.45,0.45),lty=2,col=palette[2])
text(98,0.47,"Length (n)",col=palette[2])
text(100,0.37,"Texts",col=palette[2])

arrows(v, testMinRef, v, testMaxRef, length=0.05, angle=90, code=3, col = "grey67")
points(v,testAvgRef,pch=18, col = "grey67")

v <- unlist(testListE[[1]][[2]])

#plot(seqRef/1612*100,corRef,pch=18,ylab="R²",xlab="Density sample size (%)",col="grey67")#,main="Full random")

arrows(v, testMin, v, testMax, length=0.05, angle=90, code=3)
points(v,testAvg,pch=20)


boxplot(value~Var2,data=test,varwidth=TRUE,pch=18,
        xlab="Electricity sample size (%)", ylab="R²",
        col="grey80")
boxplot(value~new,data=temp,varwidth=TRUE,pch=20,xaxt='n')#,
        #add=T)
lines(c(-100,1300),c(0.39,0.39),lty=2,col=palette[2])
lines(c(-100,1300),c(0.45,0.45),lty=2,col=palette[2])
text(98,0.47,"Length (n)",col=palette[2])
text(100,0.37,"Texts",col=palette[2])
axis(1,at=c(0,20,40,60,80,100))
axis(1)

boxplot(value~new,data=temp,varwidth=TRUE,pch=20)

head(corRef)
temp <- melt(resMat)

axis1 <- data.frame(old = 1:50, new = unlist(testListE[[1]][[2]]))
axis2 <- data.frame(old = 1:50, new = v)
temp <- merge(temp,axis1,by.x="Var2",by.y="old")
temp2 <- test
temp2 <- merge(temp2,axis2,by.x="Var2",by.y="old")
df <- data.frame(new = c(0,100),
                 value = c(0.45,0.45))

ggplot()+
  geom_line(data = df, aes(x = new, y = value), color = "black", lty = 2)+
  geom_boxplot(data = temp2, aes(x=new, y=value,group=new), notch=F, outlier.shape=18, outlier.size = 2, outlier.color = "grey67", fill="grey67", alpha=0.4,width=1.2)+
  geom_boxplot(data = temp, aes(x=new, y=value,group=new), notch=F, outlier.shape=20, outlier.size = 2, outlier.color = "red", fill="red", alpha=0.5,width=1.2)


  ggplot(temp2, aes(x=new, y=value,group=new))+
    geom_boxplot(notch=F, outlier.shape=18, outlier.size = 2, outlier.color = "grey67", fill="grey67", alpha=0.4,width=1.2)+
    geom_line(data = df, aes = aes(x = new, y = value)  , color = "blue", lty = 2)
  geom_boxplot(data=temp,notch=F, outlier.shape=20, outlier.size = 2, outlier.color = "red", fill="red", alpha=0.5,width=1.2)
  

ggplot(df, aes(x, value)) +
  geom_line(data = df, aes(x = new, y = value)  , color = "blue", lty = 2)


ggplot(df1, aes(x, log(y))) + 
  geom_line() +
  geom_line(data = df, color = "blue")

#lines(c(v[11],v[11]),c(-0.1,1),lty=2)
#lines(c(v[14],v[14]),c(-0.1,1),lty=2)
#text(16,0,"14")
#text(32.1,0,"29.1")



###########################



dataTemp <- read.csv(file = paste("Data/z_dMat_","Y","Sd","_","C","_","V",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendYC <- as.dendrogram(hclust(dataTemp,method = method.clust))
test1N <- testMethSdN(50,dendYC,prematYC)

n <- 100

seqRefN <- floor(seq(1,1298,length.out = n))
corRefN <- rep(0,n)
for(k in 2:n){
  samp <- sample(1:1298,seqRefN[k],replace=F)
  nightEst <- rep(0,1298)
  for(i in 1:1298){
    d <- rep(NA,seqRefN[k])
    for(j in 1:seqRefN[k]){
      d[j] <- sd(abs(prematYC[i,]-prematYC[samp[j],]))
    }
    nightEst[i] <- nightV[samp[which.min(d)]]
  }
  corRefN[k] <- cor(nightEst,nightV)^2
}

plot(seqRefN/1298*100,corRefN,pch=20,ylab="R²",xlab="Electricity sample size (%)")#,main="Full random")
lines(c(-100,1300),c(0.86,0.86),lty=2)
lines(c(-100,1300),c(0.89,0.89),lty=2)
lines(c(-100,1300),c(0.39,0.39),col=palette[2],lty=2)
lines(c(-100,1300),c(0.47,0.47),col=palette[2],lty=2)
lines(c(68,68),c(-0.1,1),lty=2)
lines(c(3.5,3.5),c(-0.1,1),lty=2,col=palette[2])
text(6,-0.02,"3.5%",col=palette[2])
text(70.5,-0.02,"68%")
text(3.1,0.91,"Capped Poisson")
text(0.4,0.37,"Direct cor",col=palette[2])


plot(test1N[[2]],test1N[[1]],pch=20,ylab="R²",xlab="Electricity sample size (%)")#,main="Full random")
lines(c(-100,1300),c(0.86,0.86),lty=2)
lines(c(-100,1300),c(0.89,0.89),lty=2)
lines(c(-100,1300),c(0.39,0.39),col=palette[2],lty=2)
lines(c(-100,1300),c(0.47,0.47),col=palette[2],lty=2)
lines(c(33,33),c(-0.1,1),lty=2)
lines(c(13,13),c(-0.1,1),lty=2,col=palette[2])
text(15,-0.02,"13%",col=palette[2])
text(35.5,-0.02,"33%")
#text(3.1,0.91,"Capped Poisson")
#text(0.4,0.37,"Direct cor",col=palette[2])







dataTemp <- read.csv(file = paste("Data/z_dMat_","W","Sd","_","L","_","V",".csv",sep=""),header = F)
dataTemp <- as.matrix(dataTemp)
colnames(dataTemp) <- 1:nrow(dataTemp)
dataTemp <- as.dist(dataTemp)
dendWL <- as.dendrogram(hclust(dataTemp,method = method.clust))
test2N <- testMethSdN(50,dendWL,prematWL)



plot(test2N[[2]],test2N[[1]],pch=20,ylab="R²",xlab="Electricity sample size (%)")#,main="Full random")
lines(c(-100,1300),c(0.86,0.86),lty=2)
lines(c(-100,1300),c(0.89,0.89),lty=2)
lines(c(-100,1300),c(0.39,0.39),col=palette[2],lty=2)
lines(c(-100,1300),c(0.47,0.47),col=palette[2],lty=2)
lines(c(33,33),c(-0.1,1),lty=2)
lines(c(14,14),c(-0.1,1),lty=2,col=palette[2])
text(16,-0.02,"14%",col=palette[2])
text(35.5,-0.02,"33%")
#text(3.1,0.91,"Capped Poisson")
#text(0.4,0.37,"Direct cor",col=palette[2])



test1N[[3]]



