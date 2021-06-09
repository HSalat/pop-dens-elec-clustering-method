######################################
##### Distance Matrix Clustering #####
######################################


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


############################
##### Network Plotting #####
############################


radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}


