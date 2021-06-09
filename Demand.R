replaceMax <- function(l){
  breaks <- exp(seq(0,log(50000),length.out=l+1))
  refs <- rep(list(NULL),l)
  maxs <- rep(0,l)
  for(i in 1:l){
    refs[[i]] <- vor_data$grid_id[which(vor_data$dens > breaks[i] & vor_data$dens < breaks[i+1])]
    if(length(refs[[i]])>0){
      maxs[i] <- max(vor_data$night[which(vor_data$grid_id %in% refs[[i]])],na.rm = T)
    }
    vor_dataX$night[which(vor_data$grid_id %in% refs[[i]])] <- maxs[i]
  }
  return(vor_dataX$night)
}

res_replaceMax <- rep(0,1000)
for(i in 1:1000){
  res_replaceMax[i] <- (cor(replaceMax(i),vor_data$text))^2
}
plot(1:1000,res_replaceMax,ylim=c(0,1),pch="")
lines(1:1000,res_replaceMax,lwd=2)
lines(c(0,1000),c((cor(vor_data$night,vor_data$text))^2,(cor(vor_data$night,vor_data$text))^2))

res_replaceMax2 <- rep(0,1000)
for(i in 1:1000){
  res_replaceMax2[i] <- (cor(replaceMax(i),vor_data$calls))^2
}
plot(1:1000,res_replaceMax2,ylim=c(0,1),pch="")
lines(1:1000,res_replaceMax2,lwd=2)
lines(c(0,1000),c((cor(vor_data$night,vor_data$calls))^2,(cor(vor_data$night,vor_data$calls))^2))

res_replaceMax3 <- rep(0,1000)
for(i in 1:1000){
  res_replaceMax3[i] <- (cor(replaceMax(i),vor_data$length_calls))^2
}
plot(1:1000,res_replaceMax3,ylim=c(0,1),pch="")
lines(1:1000,res_replaceMax3,lwd=2)
lines(c(0,1000),c((cor(vor_data$night,vor_data$length_calls))^2,(cor(vor_data$night,vor_data$length_calls))^2))


















