# Most of the functions in this sheet are ad-hoc variants of usual functions created to have more control over hidden details when trying different methods.
# The actual clustering algorithm used in the article corresponds to the last section: AutoClustering


################
### Gridding ###
################


# This section contains utility functions to ready data for rasterisation.
#   <country> is a bounding box around the studied area
#   <holes> are pixels that are known to not be land officially belonging to the Country (e.g. Ocean, Gambia)
# These were later replaced by tools taken directly from the R raster package

griddingNation <- function(X,Y,Z,country,holes = NULL,fun="mean"){
  # X and Y are vectors of coordinates, Z is a vecotr of values at these coordinates
  # "country" must be a vector containing: (minLong,maxLong,minLat,maxLat,resX,resY)
  # resX/resY is expressed as the number of cells contained in the grid
  l <- length(X)
  result <- matrix(0,nrow=country[6],ncol=country[5])
  vecti <- rep(0,l)
  vectj <- rep(0,l)
  x <- seq(country[1],country[2],length.out = country[5]+1)
  y <- seq(country[3],country[4],length.out = country[6]+1)
  for(k in 1:l){
    if(result[max(which(Y[k] > y)),max(which(X[k] > x))]>0){
      result[max(which(Y[k] > y)),max(which(X[k] > x))] <- get(fun)(result[max(which(Y[k] > y)),max(which(X[k] > x))],Z[k])
    }else{
      result[max(which(Y[k] > y)),max(which(X[k] > x))] <- Z[k]
    }
  }
  result[holes] <- NA
  return(result)
}

# In principle deprecated
randtoGrid <- function(X,Y,Z,MX,MY,fun)
  # X: 1st coord, Y: 2nd coord, Z: values, 
  # MX = c(min,max,nbr of X intervals), MY=c(min,max,nbr of Y intervals)
  # fun = "mean" or "sum"
{
  SizeX <- MX[3]
  SizeY <- MY[3]
  result <- matrix(0,ncol=SizeX,nrow=SizeY)
  coordX <- MX[1]+(0:(SizeX-1))*(MX[2]-MX[1])/(SizeX-1)
  coordY <- MY[1]+(0:(SizeY-1))*(MY[2]-MY[1])/(SizeY-1)
  etiquette <- data.frame(Z=Z,coords=rep(0,length(Z)))
  for(i in 1:length(Z)){
    X0 <- max(coordX[which(coordX <= X[i])])
    Y0 <- max(coordY[which(coordY <= Y[i])])
    etiquette$coords[i] <- paste(X0,",",Y0,sep="")
  }
  values <- aggregate(etiquette$Z,by=list(etiquette$coords),FUN=get(fun))
  for(i in 1:SizeX){
    for(j in 1:SizeY){
      text0 <- paste(coordX[i],",",coordY[j],sep="")
      if(any(values$Group.1 == text0)){
        result[j,i] <- values$x[which(values$Group.1==text0)]
      }
    }
  }
  return(result)
}

# Griding the data: fixed boundaries; in principle deprecated
newrandtoGrid <- function(X,Y,Z,MX,MY,fun){
  # X: 1st coord, Y: 2nd coord, Z: values, 
  # MX = nbr of X intervals, MY = nbr of Y intervals
  # fun = "mean" or "sum" f.e.
  mx <- min(X)
  Mx <- max(X)
  my <- min(Y)
  My <- max(Y)
  intx <- seq(mx,Mx-(Mx-mx)/MX,by=(Mx-mx)/MX)
  inty <- seq(my,My-(My-my)/MY,by=(My-my)/MY)
  coords <- data.frame(Z=Z,etiquette=rep(0,length(Z)))
  result <- matrix(0,nrow=MY,ncol=MX)
  for(i in 1:length(Z)){
    X0 <- max(which(intx <= X[i]))
    Y0 <- max(which(inty <= Y[i]))
    coords$etiquette[i] <- paste(X0,".",Y0,sep="")
  }
  values <- aggregate(coords$Z,by=list(coords$etiquette),FUN=get(fun))
  for(i in 1:MX){
    for(j in 1:MY){
      text0 <- paste(i,".",j,sep="")
      if(any(values$Group.1 == text0)){
        result[j,i] <- values$x[which(values$Group.1==text0)]
      }
    }
  }
  return(result)
}

# Deprecated function to aggregate spatially.
GridingAgg <- function(InitGrid,multp,startpos=1)
  # 'multp' is a vector containing the new grid unit sizes as multiples of the initial unit size
  # 'startpos' indicates from which corner to start (counted clockwise)
{
  lm <- length(multp)
  InitRows <- nrow(InitGrid)
  InitCols <- ncol(InitGrid)
  ResultGrid <- rep(list(NULL),lm)
  for(i in 1:lm){
    locmultp <- multp[i]
    locnrow <- floor(InitRows/locmultp)
    locncol <- floor(InitCols/locmultp)
    StepMatrix <- matrix(nrow=locnrow,ncol=locncol,0)
    for(Index in 0:(locncol*locnrow-1)){
      locrow <- floor(Index/locncol) + 1
      loccol <- Index %% locncol + 1
      if(startpos == 1){
        StepMatrix[locrow,loccol] <- sum(InitGrid[((locrow-1)*locmultp+1):(locrow*locmultp),((loccol-1)*locmultp+1):(loccol*locmultp)])
      } else if(startpos == 2){
        StepMatrix[locrow,locncol-loccol+1] <- sum(InitGrid[((locrow-1)*locmultp+1):(locrow*locmultp),(InitCols-(loccol-1)*locmultp):(InitCols-loccol*locmultp+1)]) 
      } else if(startpos == 3){
        StepMatrix[locrow,loccol] <- sum(InitGrid[(InitRows-(locrow-1)*locmultp):(InitRows-locrow*locmultp+1),(InitCols-(loccol-1)*locmultp):(InitCols-loccol*locmultp+1)])
      } else{
        StepMatrix[locrow,loccol] <- sum(InitGrid[(InitRows-(locrow-1)*locmultp):(InitRows-locrow*locmultp+1),((loccol-1)*locmultp+1):(loccol*locmultp)])  
      }
    }     
    ResultGrid[[i]] <- StepMatrix
  }
  return(ResultGrid)
}


########################
### Data preparation ###
########################


# Likely deprecated functions to load and apply basic preparation to the data

monthlyText <- function(i,folderin){
  ref <- data.frame(tower=1:1666)
  temp <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)
  temp <- data.frame(orig=temp$V2,n=temp$V4)
  temp <- aggregate(temp,by=list(temp$orig),FUN=sum)
  temp <- merge(ref,temp,by.x="tower",by.y="Group.1",all=T)
  temp <- data.frame(tower=1:1666,n=temp$n)
  temp$n[which(is.na(temp$n))] <- 0
  return(temp)
}

monthlyVoice <- function(i,folderin){
  ref <- data.frame(tower=1:1666)
  temp <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
  temp <- data.frame(orig=temp$V2,n=temp$V4,t=temp$V5)
  temp <- aggregate(temp,by=list(temp$orig),FUN=sum)
  temp <- merge(ref,temp,by.x="tower",by.y="Group.1",all=T)
  temp <- data.frame(tower=1:1666,n=temp$n,t=temp$t)
  temp$n[which(is.na(temp$n))] <- 0
  temp$t[which(is.na(temp$t))] <- 0
  return(temp)
}

hourlyText <- function(i,folderin){
  ref  <- rep(NA,31*24)
  for(k in 1:31){
    ref[(k-1)*24+(1:24)] <- k*100+1:24
  }
  temp <- read.csv(paste(folderin,"SET1/SET1S_",i,".csv",sep=""),header = F)
  temp <- data.frame(time=temp$V1,n=temp$V4)
  temp <- aggregate(temp$n,by=list(temp$time),FUN=sum)
  temp$hourCode <- ref[1:nrow(temp)]
  return(temp)
}

hourlyVoice <- function(i,folderin){
  ref  <- rep(NA,31*24)
  for(k in 1:31){
    ref[(k-1)*24+(1:24)] <- k*100+1:24
  }
  temp <- read.csv(paste(folderin,"SET1/SET1V_",i,".csv",sep=""),header = F)
  temp <- data.frame(time=temp$V1,n=temp$V4)
  temp <- aggregate(temp$n,by=list(temp$time),FUN=sum)
  temp$hourCode <- ref[1:nrow(temp)]
  return(temp)
}


################
### Plotting ###
################

  
# Function to plot several ggplots side by side
multiplot <- function(...,plotlist=NULL,file,cols=1,layout=NULL) {
  library(grid)
  plots <- c(list(...),plotlist)
  numPlots = length(plots)
  if (is.null(layout)){
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Function to visualise matrices, useful to do color plots of correlation matrices
plotgrid <- function(grid,log=F){
  meltedgrid <- melt(grid)
  if(log==T){
    ggplot(meltedgrid, aes(x = Var2, y = Var1, fill = log(value))) + geom_tile() + scale_fill_gradient(low="white",high="royalblue4") + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())
  }else{
    ggplot(meltedgrid, aes(x = Var2, y = Var1, fill = value)) + geom_tile() + scale_fill_gradient(low="white",high="royalblue4") + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())
  }
}


###############
### Voronoi ###
###############


# Simple definition of voronoi neighbourhood
voronoi <- function(i,j,ref){
  d <- sqrt((ref[,1]-i)^2+(ref[,2]-j)^2)
  return(c(which.min(d),ref[which.min(d),]))
}

# [Aggregation:] voronoi of positive elements
avg_voronoi <- function(grid){
  result <- matrix(0,nrow=nrow(grid),ncol=ncol(grid))
  ref <- which(grid>0, arr.ind=T)
  refsize <- rep(0,nrow(ref))
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      temp <- voronoi(i,j,ref)
      refsize[temp[1]] <- refsize[temp[1]]+1
    }
  }
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      temp <- voronoi(i,j,ref)
      result[i,j] <- grid[temp[2],temp[3]]/refsize[temp[1]]
    }
  }
  return(result)
}

# [Aggregation:] voronoi compared to ref of centres
avg_voronoi_ref <- function(grid,ref){
  result <- matrix(0,nrow=nrow(grid),ncol=ncol(grid))
  whichm <- matrix(0,nrow=nrow(grid),ncol=ncol(grid))
  meanr <- rep(0,nrow(ref))
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      whichm[i,j] <- voronoi(i,j,ref)[1]
    }
  }
  for(i in 1:nrow(ref)){
    meanr[i] <- mean(grid[which(whichm == i)])
  }
  for(i in 1:nrow(grid)){
    for(j in 1:ncol(grid)){
      result[i,j] <- meanr[whichm[i,j]]
    }
  }
  return(list(result,meanr))
}

# [Aggregation:] voronoi compared to ref of centres that defaults to NA instead of 0 when no value is found
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


#######################
##### Correlation #####
#######################

  
 # Simple automation of Poisson fittings and R^2 computation
  
pearson <- function(D1,D2){
  a1 <- mean(D1)
  a2 <- mean(D2)
  return(sum((D1-a1)*(D2-a2))/sqrt(sum((D1-a1)^2)*sum((D2-a2)^2)))
}

rsq <- function(a,b,subset=1:length(a),l=1){
  if(l==1){
    return(cor(a[subset],b[subset])^2)
  } else{
    return(cor(log(a[subset]),log(b[subset]))^2)
  }
}

# Poisson fittings adapted to the data
rsqP <- function(a,b,c=NULL,subset=1:length(a)){
  if(!is.null(c)){
    temp <- glm(a[subset] ~ log(b[subset]) + log(c[subset]),family = poisson(link = "log"),na.action = na.exclude)
    rsq(a,exp(temp$coefficients[1])*b^temp$coefficients[2]*c^temp$coefficients[3],subset)
  }else{
    temp <- glm(a[subset] ~ log(b[subset]),family = poisson(link = "log"),na.action = na.exclude)
    rsq(a,exp(temp$coefficients[1])*b^temp$coefficients[2],subset)
  }
}

# Linear fittings adapted to the data
rsqL <- function(a,b,c=NULL,subset=1:length(a)){
  temp <- lm(log(a[subset]) ~ log(b[subset]) + log(c[subset]),na.action = na.exclude)
  rsq(a,exp(temp$coefficients[1])*b^temp$coefficients[2]*c^temp$coefficients[3])
}

adHoc <- function(aa,bb,m=100,by=0.01){
  sample <- seq(by,m,by=by)
  testcor <- rep(0,length(sample))
  for(i in 1:length(sample)){
    testcor[i] <- cor((bb)^(sample[i]),aa,method="pearson")
  }
  c <- sample[which.max(testcor)]
  temp <- bb^c
  a <- lm(aa ~ temp)$coefficients[2]
  b <- lm(aa ~ temp)$coefficients[1]
  return(list(a*bb^c+b,c(a,b,c)))
}


#########################
##### Multifractals #####
#########################


# Used only for side projects

boxCounting <- function(M,S)
{
  nr <- nrow(M)
  nc <- ncol(M)
  N <- rep(0,length(S))
  for(s in 1:length(S)){
    ss <- S[s]
    for(i in 0:(nr/ss-1)){
      for(j in 0:(nc/ss-1)){
        if(sum(M[i*ss+1:ss,j*ss+1:ss]) > 0){N[s] <- N[s]+1}
      }
    }
  }
  fit <- lm(log(N) ~ log(S))
  return(-fit$coefficients[2])
}

D0D1D2 <- function(Data,sizes)
{
  data <- GridingAgg(Data,sizes)
  vectD1 <- rep(0,length(sizes))
  vectD2 <- rep(0,length(sizes))
  for(i in 1:length(sizes)){
    data[[i]] <- data[[i]]/sum(data[[i]])
    vectD1[i] <- sum(data[[i]][which(data[[i]]>0)]*log(data[[i]][which(data[[i]]>0)])/log(10))
    vectD2[i] <- log(sum(data[[i]]^2))/log(10)
  }
  S <- log(sizes/2)/log(10)
  fit1 <- lm(vectD1 ~ S)
  fit2 <- lm(vectD2 ~ S)
  result <- c(boxCounting(Data,sizes),fit1$coefficients[2],fit2$coefficients[2])
  return(result)
}

GlidingBox <- function(InitGrid,sizes,neigh=1)
{
  InitRows <- nrow(InitGrid)
  InitCols <- ncol(InitGrid)
  Raster <- raster(InitGrid)
  msize <- max(sizes)
  ResultGrid <- rep(list(matrix(nrow=InitRows-2*msize,ncol=InitCols-2*msize,0)),length(sizes))
  for(s in sizes){
    if(s==0){
      ResultGrid[[which(sizes == 0)]] <- InitGrid[(msize+1):(InitRows-msize),(msize+1):(InitCols-msize)]
    } else {
      weightmatrix <- matrix(nrow=2*s+1,ncol=2*s+1,1)
      if(neigh==2){
        for(Index in 0:((2*s+1)*(2*s+1)-2)){
          locrow <- floor(Index/(2*s+1)) + 1
          loccol <- Index %% (2*s+1) + 1
          if((abs(s+1-locrow)+abs(s+1-loccol) > s)){
            weightmatrix[locrow,loccol] <- 0
          }
        }
      }
      result <- focal(x=Raster,w=weightmatrix,fun=sum,pad=T,na.rm=F)
      ResultGrid[[which(sizes == s)]] <- as.matrix(result)[(msize+1):(InitRows-msize),(msize+1):(InitCols-msize)]
    }
  }
  return(ResultGrid)
}

Moment_StdGrid <- function(data,sizes,q,dd)
{
  data <- GridingAgg(data,sizes)
  Zed <- matrix(rep(0,length(q)*length(sizes)),ncol=length(sizes),byrow=TRUE)
  Tau <- rep(0,length(q))
  D <- rep(0,length(q))
  alpha <- rep(0,length(q))
  falpha <- rep(0,length(q))
  for(i in 1:length(q)){
    for(j in 1:length(sizes)){
      Zed[i,j] <- sum(data[[j]][which(data[[j]] != 0)]^q[i])
    }
    fit <- lm(log(Zed[i,]) ~ log(sizes))
    Tau[i] <- fit$coefficients[[2]]
    D[i] <- Tau[i]/(q[i]-1)
  }
  for(i in 2:(length(q)-1)){
    alpha[i] <- (Tau[i+1]-Tau[i-1])/(q[i+1]-q[i-1])
    falpha[i] <- q[i]*alpha[i]-Tau[i]
  }
  alpha[1] <- (Tau[2]-Tau[1])/(q[2]-q[1])
  alpha[length(q)] <- (Tau[length(q)]-Tau[length(q)-1])/(q[length(q)]-q[length(q)-1])
  falpha[1] <- q[1]*alpha[1]-Tau[1]
  falpha[length(q)] <- q[length(q)]*alpha[length(q)]-Tau[length(q)]
  D[match(1,q)] <- (D[match(1,q)+1]+D[match(1,q)-1])/2
  Result <- data.frame(q=q,Dq=D,alpha=alpha,falpha=falpha)
  return(Result)
}

MMoment_MGBox <- function(grid,sizes,q,dd,neigh=1)
{
  data <- GlidingBox(grid,sizes,neigh)
  sizes <- sizes+0.5
  emin <- min(sizes)
  N <- nnzero(data[[1]])
  Zed <- matrix(rep(0,length(q)*(length(sizes)-1)),ncol=length(sizes)-1,byrow=TRUE)
  prealpha <- matrix(rep(0,length(q)*(length(sizes)-1)),ncol=length(sizes)-1,byrow=TRUE)
  Tau <- rep(0,length(q))
  D <- rep(0,length(q))
  alpha <- rep(0,length(q))
  falpha <- rep(0,length(q))
  for(i in 1:length(q)){
    for(j in 2:length(sizes)){
      Zed[i,j-1] <- -log(sum((data[[1]][which(data[[1]] != 0)]/data[[j]][which(data[[1]] != 0)])^q[i])/N)/log(sizes[j]/emin)
      prealpha[i,j-1] <- -sum(((data[[1]][which(data[[1]] != 0)]/data[[j]][which(data[[1]] != 0)])^q[i])*log(data[[1]][which(data[[1]] != 0)]/data[[j]][which(data[[1]] != 0)]))/(sum((data[[1]][which(data[[1]] != 0)]/data[[j]][which(data[[1]] != 0)])^q[i])*log(sizes[j]/emin))
    }
    Tau[i] <- mean(Zed[i,])
  }
  Tau1 <- Tau[which(q==1)]
  for(i in 1:length(q)){
    Tau[i] <- Tau[i]*dd/Tau1-dd
    D[i] <- Tau[i]/(q[i]-1)
    alpha[i] <- dd/Tau1*mean(prealpha[i,])
    falpha[i] <- q[i]*alpha[i]-Tau[i]
  }
  Result <- data.frame(q=q,Dq=D,tau=Tau,alpha=alpha,falpha=falpha)
  return(Result)
}


######################
##### Clustering #####
######################


# Custom functions to create distance matrices

# Based on Sd of the difference between two curves
distMatSd <- function(df){
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      if(anyNA(df[i,])|anyNA(df[j,])){
        m[i,j] <- NA
      }else{
        m[i,j] <- abs(sd(df[i,]-df[j,]))
      }
    }
  }
  return(m)
}

# Based on direct correlation between two curves (point by point)
distMatCor <- function(df){
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      if(anyNA(df[i,])|anyNA(df[j,])){
        m[i,j] <- NA
      }else{
        m[i,j] <- cor(df[i,],df[j,])    
      }
    }
  }
  return(m)
}

# Likely deprecated; for testing only  
customClust <- function(df,num){
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  temp <- matrix(0,nrow=l,ncol=l)
  m2 <- matrix(0,nrow=nrow(df),ncol=num)
  result <- rep(list(NULL),num)
  ref <- rep(0,num)
  for(i in 1:l){
    for(j in 1:l){
      m[i,j] <- abs(sd(df[i,]-df[j,]))
    }
  }
  ref[1:2] <- which(m == max(m),arr.ind = TRUE)[1,]
  m2[,1:2] <- m[,ref[1:2]]
  if(num > 2){
    for(p in 3:num){
      for(i in 1:l){
        temp[i] <- min(m[i,ref[1:(p-1)]])
      }
      indref <- setdiff(1:l,ref[1:(p-1)])
      ref[p] <- indref[which.max(temp[indref])]
      m2[,p] <- m[,ref[p]]
    }
  }
  for(r in 1:l){
    result[[which.min(m2[r,])[1]]] <- c(result[[which.min(m2[r,])[1]]],r)
  }
  return(result)
}

  
##########################################
##### Variable coefficients analysis #####
##########################################


# Only useful for side projects

bin <- function(data,n){
  vect <- rep(list(NULL),n)
  int <- (max(data)-min(data))/n
  for(i in 1:(n-1)){
    vect[[i]] <- which(data >= min(data)+(i-1)*int & data < min(data)+i*int)
  }
  vect[[n]] <- which(data >= min(data)+(n-1)*int)
  return(vect)
}

multidensAnalysis <- function(n){
  indices <- bin(dens,n)
  M <- 1:n
  mdens <- rep(0,n)
  alphaM <- rep(0,n)
  sdM <- rep(0,n)
  for(i in 1:n){
    ind <- intersect(indices[[i]],which(night>0))
    mdens[i] <- mean(dens[ind])
    alphaM[i] <- mean(log(night[ind])/log(mdens[i]),na.rm=T)
    sdM[i] <- sd(log(night[ind])/log(mdens[i]),na.rm=T)
  }
  mdens <- mdens[2:n]
  alphaM <- alphaM[2:n]
  sdM <- sdM[2:n]
  x <- 0:50000
  fit <- lm(log(alphaM) ~ log(mdens))
  estimate <- dens^(exp(fit$coefficients[1])*dens^fit$coefficients[2])
  return(cor(night,estimate)^2)
}

multidensAnalysis2 <- function(n){
  indices <- bin(dens,n)
  mdens2 <- rep(0,n)
  alphaM2 <- rep(0,n)
  sdM2 <- rep(0,n)
  for(i in 1:n){
    ind <- intersect(indices[[i]],which(night>0))
    ind <- intersect(ind,which(texts>0))
    mdens2[i] <- mean(dens[ind])
    alphaM2[i] <- mean(log(night[ind])/log(texts[ind]),na.rm=T)
    sdM2[i] <- sd(log(night[ind])/log(texts[ind]),na.rm=T)
  }
  mdens2 <- mdens2[2:n]
  alphaM2 <- alphaM2[2:n]
  sdM2 <- sdM2[2:n]
  fit2 <- lm(alphaM2 ~ mdens2)
  estimate2 <- dens^(fit2$coefficients[1]+dens*fit2$coefficients[2])
  return(cor(night,estimate2)^2)
}


######################################
##### Distance Matrix Clustering #####
######################################


# Updated functions to create distance matrices, see above

distMatSd <- function(df){
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      if(anyNA(df[i,])|anyNA(df[j,])){
        m[i,j] <- NA
      }else{
        m[i,j] <- abs(sd(df[i,]-df[j,]))
      }
    }
  }
  return(m)
}

distMatCor <- function(df){
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      if(anyNA(df[i,])|anyNA(df[j,])){
        m[i,j] <- NA
      }else{
        m[i,j] <- cor(df[i,],df[j,])    
      }
    }
  }
  return(1-m)
}

distMatSdNA <- function(df,n){
  # n is the minimal number of non-NA points for two lines to be compared
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      sample <- which(!is.na(df[i,]) & !is.na(df[j,]))
      if(length(sample)>n){
        m[i,j] <- abs(sd(df[i,sample]-df[j,sample]))
      }else{
        m[i,j] <- NA
      }
    }
  }
  return(m)
}

distMatCorNA <- function(df,n){
  # n is the minimal number of non-NA points for two lines to be compared
  l <- nrow(df)
  m <- matrix(0,nrow=l,ncol=l)
  for(i in 1:l){
    for(j in 1:l){
      sample <- which(!is.na(df[i,]) & !is.na(df[j,]))
      if(length(sample)>n){
        m[i,j] <- cor(df[i,sample],df[j,sample])
      }else{
        m[i,j] <- NA
      }
    }
  }
  return(1-m)
}

  
##############################
##### Network Clustering #####
##############################
  

# creates a feature table to apply clustering (useful for network clustering). See article to identify all the features used.
  
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
  

############################
##### Network Plotting #####
############################


radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
  
  
##########################
##### Autoclustering #####
##########################


# These functions are meant to apply autoimatically the purity-based method of clustering (see SI)
  
# Functions:
# autoClust(data,var,t,comp = NULL,method.clust = 'complete',method.purity = 'bins')
# nameClust(mpd,t,comp = NULL,dist = 'Sd',time = 'D',unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins')
# autoExpctd(mpd,t,frame,comp = NULL,dist = 'Sd',time = 'D',unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins')

# autoClust requires already loaded data
# nameClust loads the data from the variable names

# <data> is a distance matrix
# <var> is a vector of external values for each row of <data> to test clusters' purity
# <t> is the threshold(s) triggering a new division of a cluster. It should be a vector with a threshold between 0 and 1 for each bin if bins are used,
# or a number if variance is used
# <comp> defines the bins if purity is determined using bins
# <method.clust> method for hierarchical clustering. Choose "complete" (max distance) or "mean" (mean distance)
# <method.purity> method for evaluating the quality of clusters. Choose "bins" or "var" (variance)

# <mpd> Type of mobile phone data. Options are 'T', 'C', 'L'.
# <dist> Type of distance matrix: "Sd" or "Cor"
# <time> Length of considered period: 'D', 'W' or 'Y'
# <unit> Tower ('T') or Voronoi cell ('V'). Currently, only Voronoi is supported.

# Cluster purity based on bins
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

# Cluster purity based on variance
ispure2 <- function(dend,var,t){
  test <- TRUE
  if(sd(var[as.numeric(labels(dend))]) > t & length(labels(dend))>1){
      test <- FALSE
  }
  if(test){
    return(list(labels(dend)))
  }else{
    return(c(ispure2(dend[[1]],var,t),ispure2(dend[[2]],var,t)))
  }
}

# From data to clustered
autoClust <- function(data,var,t,comp = NULL,method.clust = 'complete',method.purity = 'bins'){
  data <- as.dist(data)
  dend <- as.dendrogram(hclust(data,method = method.clust))
  if(method.purity == 'bins'){
    l <- ispure1(dend,var,comp,t)
    message(paste("Number of clusters:",length(l),sep = " "))
    return(l)
  }else{
    l <- ispure2(dend,var,t)
    message(paste("Number of clusters:",length(l),sep = " "))
    return(l)
  }
}

#nameClust <- function(mpd,t,comp = NULL,dist = 'Sd',time = 'D',unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins'){
#  data <- read.csv(file = paste("Data/z_dMat_",time,dist,"_",mpd,"_",unit,".csv",sep=""),header = F)
#  data <- as.matrix(data)
#  colnames(data) <- 1:nrow(data)
#  if(var == 'dens' & unit == 'V'){
#   var <- vorData$dens
#  }else if(var == 'night' & unit == 'V'){
#    var <- vorData$nightlight
#  }else if(var == 'dens' & unit == 'T'){
#    var <- densV
#  }else if(var == 'night' & unit == 'T'){
#    var <- elecV
#  }
#  return(autoClust(data,var,t,comp,method.clust,method.purity))
}


# From names to clustered
nameClust <- function(mpd,t,comp = NULL,dist = 'Sd',time = 'D',unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins'){
  data <- read.csv(file = paste("Data/z_dMat_",time,dist,"_",mpd,"_",unit,".csv",sep=""),header = F)
  data <- as.matrix(data)
  data[is.na(data)] <- 1
  colnames(data) <- 1:nrow(data)
  if(unit == 'V'){
    print("V no longer supported")
  }else if(var == 'dens' & unit == 'T'){
    var <- densV
  }else if(var == 'night' & unit == 'T'){
    var <- elecV
  }
  return(autoClust(data,var,t,comp,method.clust,method.purity))
}

# Computation of the expected value inside each cluster
autoExpctd <- function(mpd,t,frame,comp = NULL,dist = 'Sd',time = 'D',unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins'){
  base <- nameClust(mpd,t,comp,dist,time,unit,var,method.clust,method.purity)
  expectedC <- rep(0,length(base))
  for(i in 1:length(base)){
    expectedC[i] <- mean(get(paste(var,"V",sep=""))[as.numeric(base[[i]])])
  }
  n <- ncol(frame)+1
  frame[,n] <- 0
  colnames(frame)[n] <- paste("Method",n-1,sep="")
  for(i in 1:1666){
    if(any(sapply(base, function(y) i %in% y)))
      frame[i,n] <- expectedC[which(sapply(base, function(y) i %in% y))]
  }
  return(frame)
}

# Computation of the expected value inside each cluster with purity records
autoExpctdP <- function(mpd,t,frame,comp,dist = 'Sd',time = 'D',unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins'){
  base <- nameClust(mpd,t,comp,dist,time,unit,var,method.clust,method.purity)
  b <- length(base)
  expectedC <- rep(0,b)
  p <- rep(0,b)
  for(i in 1:b){
    vect <- get(paste(var,"V",sep=""))[as.numeric(base[[i]])]
    expectedC[i] <- mean(vect)
    n <- length(comp)-1
    p2 <- rep(0,n)
    for(j in 1:n){
      p2[j] <- length(which(vect >= comp[j] & vect <= comp[j+1]))
    }
    p[i] <- max(p2)/length(vect)
  }
  n <- ncol(frame)+1
  frame[,c(n,n+1)] <- 0
  colnames(frame)[c(n,n+1)] <- c(paste("Method",n/2,sep=""),paste("Pur",n/2,sep=""))
  for(i in 1:1666){
    if(any(sapply(base, function(y) i %in% y)))
      frame[i,n] <- expectedC[which(sapply(base, function(y) i %in% y))]
      frame[i,n+1] <- p[which(sapply(base, function(y) i %in% y))]
  }
  return(frame)
}

autoExpctdFeatP <- function(mpd,t,frame,comp,unit = "V",var = 'dens',method.clust = 'complete',method.purity = 'bins'){
  data <- read.csv(file = paste("Data/fMat_",mpd,"_",unit,".csv",sep=""),header=F)
  data <- data[1:1666,]
  data <- dist(data)
  data[which(is.na(data))] <- 1
  base <- autoClust(data,get(paste(var,"V",sep="")),t,comp,method.clust,method.purity)
  b <- length(base)
  expectedC <- rep(0,b)
  p <- rep(0,b)
  for(i in 1:b){
    vect <- get(paste(var,"V",sep=""))[as.numeric(base[[i]])]
    expectedC[i] <- mean(vect)
    n <- length(comp)-1
    p2 <- rep(0,n)
    for(j in 1:n){
      p2[j] <- length(which(vect >= comp[j] & vect <= comp[j+1]))
    }
    p[i] <- max(p2)/length(vect)
  }
  n <- ncol(frame)+1
  frame[,c(n,n+1)] <- 0
  colnames(frame)[c(n,n+1)] <- c(paste("Method",n/2,sep=""),paste("Pur",n/2,sep=""))
  for(i in 1:1666){
    if(any(sapply(base, function(y) i %in% y)))
      frame[i,n] <- expectedC[which(sapply(base, function(y) i %in% y))]
    frame[i,n+1] <- p[which(sapply(base, function(y) i %in% y))]
  }
  return(frame)
}

autoExpctdFeatNetP <- function(N,t,frame,comp,unit = "T",var = 'dens',method.clust = 'complete',method.purity = 'bins'){
  data <- read.csv(file = paste("Data/featureNetSub",unit,as.character(N),".csv",sep=""),header=F)
  data <- dist(data)
  base <- autoClust(data,get(paste(var,"V",sep="")),t,comp,method.clust,method.purity)
  b <- length(base)
  expectedC <- rep(0,b)
  p <- rep(0,b)
  for(i in 1:b){
    vect <- get(paste(var,"V",sep=""))[as.numeric(base[[i]])]
    expectedC[i] <- mean(vect)
    n <- length(comp)-1
    p2 <- rep(0,n)
    for(j in 1:n){
      p2[j] <- length(which(vect >= comp[j] & vect <= comp[j+1]))
    }
    p[i] <- max(p2)/length(vect)
  }
  n <- ncol(frame)+1
  frame[,c(n,n+1)] <- 0
  colnames(frame)[c(n,n+1)] <- c(paste("Method",n/2,sep=""),paste("Pur",n/2,sep=""))
  for(i in 1:1666){
    if(any(sapply(base, function(y) i %in% y)))
      frame[i,n] <- expectedC[which(sapply(base, function(y) i %in% y))]
    frame[i,n+1] <- p[which(sapply(base, function(y) i %in% y))]
  }
  return(frame)
}
