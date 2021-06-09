subset <- which(night>0)
length(subset)

subsetc <- which(night == 0)

mean(dens[subsetc])

cor(night,dens)^2

# Histograms

h <- hist(night[subset],breaks=1000,plot=F)
h <- hist(dens[subset],breaks=1000,plot=F)
h <- hist(texts[subset],breaks=1000,plot=F)
h <- hist(calls[subset],breaks=1000,plot=F)
h <- hist(length[subset],breaks=1000,plot=F)

plot(h$mids,h$counts,type='h',xlab="x",ylab="Counts",cex.lab=1.3,cex.axis=1.3)

# D and M are Poisson, N is not

subsetC <- intersect(subset,which(dens > 5000))
subsetV <- intersect(subset,which(dens <= 5000))

h <- hist(length[subsetC],breaks=1000,plot=F)
h <- hist(night[subsetC],breaks=1000,plot=F)
h <- hist(dens[subsetC],breaks=1000,plot=F)

h <- hist(length[subsetV],breaks=1000,plot=F)
h <- hist(night[subsetV],breaks=1000,plot=F)
h <- hist(dens[subsetV],breaks=1000,plot=F)


############################################################
##### Regressions: Night from Mobile phone and Density #####
############################################################

attach(vor_data)

res_cor_energy <- data.frame(type=c("All","Cities","Villages"),
                             dens2=c(rsq(night,dens2,subset),rsq(night,dens2,subsetC2),rsq(night,dens2,subsetV2)),
                             texts=c(rsq(night,texts,subset),rsq(night,texts,subsetC2),rsq(night,texts,subsetV2)),
                             calls=c(rsq(night,calls,subset),rsq(night,calls,subsetC2),rsq(night,calls,subsetV2)),
                             length=c(rsq(night,length,subset),rsq(night,length,subsetC2),rsq(night,length,subsetV2)),
                             logdens2=c(rsq(night,dens2,subset,2),rsq(night,dens2,subsetC2,2),rsq(night,dens2,subsetV2,2)),
                             logtexts=c(rsq(night,texts,subset,2),rsq(night,texts,subsetC2,2),rsq(night,texts,subsetV2,2)),
                             logcalls=c(rsq(night,calls,subset,2),rsq(night,calls,subsetC2,2),rsq(night,calls,subsetV2,2)),
                             loglength=c(rsq(night,length,subset,2),rsq(night,length,subsetC2,2),rsq(night,length,subsetV2,2)),
                             dens2ptexts=c(rsqP(night,texts,dens2,subset),rsqP(night,texts,dens2,subsetC2),rsqP(night,texts,dens2,subsetV2)),
                             dens2pcalls=c(rsqP(night,calls,dens2,subset),rsqP(night,calls,dens2,subsetC2),rsqP(night,calls,dens2,subsetV2)),
                             dens2plength=c(rsqP(night,length,dens2,subset),rsqP(night,length,dens2,subsetC2),rsqP(night,length,dens2,subsetV2))
                             )

res_cor_energy <- data.frame(type=c("All","Cities","Villages"),
                             dens=c(rsq(night,dens),rsq(night,dens,subsetC),rsq(night,dens,subsetV)),
                             texts=c(rsq(night,texts),rsq(night,texts,subsetC),rsq(night,texts,subsetV)),
                             calls=c(rsq(night,calls),rsq(night,calls,subsetC),rsq(night,calls,subsetV)),
                             length=c(rsq(night,length),rsq(night,length,subsetC),rsq(night,length,subsetV)),
                             logdens=c(rsq(night,dens,subset,),rsq(night,dens,subsetC,),rsq(night,dens,subsetV,)),
                             logtexts=c(rsq(night,texts,subset,),rsq(night,texts,subsetC,),rsq(night,texts,subsetV,)),
                             logcalls=c(rsq(night,calls,subset,),rsq(night,calls,subsetC,),rsq(night,calls,subsetV,)),
                             loglength=c(rsq(night,length,subset,),rsq(night,length,subsetC,),rsq(night,length,subsetV,)),
                             densptexts=c(rsqP(night,texts,dens,subset),rsqP(night,texts,dens,subsetC),rsqP(night,texts,dens,subsetV)),
                             denspcalls=c(rsqP(night,calls,dens,subset),rsqP(night,calls,dens,subsetC),rsqP(night,calls,dens,subsetV)),
                             densplength=c(rsqP(night,length,dens,subset),rsqP(night,length,dens,subsetC),rsqP(night,length,dens,subsetV))
)

###########################################################
##### Regressions: Elec from Mobile phone and Density #####
###########################################################


indX <- which(texts>0)
indX <- intersect(indX,which(elec > 0))
indXC <- intersect(indX,subsetC)
indXV <- intersect(indX,subsetV)

res_cor_electrification <- data.frame(type=c("All","Cities","Villages"),
                                      dens=c(rsq(elec,dens,indX),rsq(elec,dens,indXC),rsq(elec,dens,indXV)),
                                      texts=c(rsq(elec,texts,indX),rsq(elec,texts,indXC),rsq(elec,texts,indXV)),
                                      calls=c(rsq(elec,calls,indX),rsq(elec,calls,indXC),rsq(elec,calls,indXV)),
                                      length=c(rsq(elec,length,indX),rsq(elec,length,indXC),rsq(elec,length,indXV)),
                                      logdens=c(rsq(elec,dens,indX,2),rsq(elec,dens,indXC,2),rsq(elec,dens,indXV,2)),
                                      logtexts=c(rsq(elec,texts,indX,2),rsq(elec,texts,indXC,2),rsq(elec,texts,indXV,2)),
                                      logcalls=c(rsq(elec,calls,indX,2),rsq(elec,calls,indXC,2),rsq(elec,calls,indXV,2)),
                                      loglength=c(rsq(elec,length,indX,2),rsq(elec,length,indXC,2),rsq(elec,length,indXV,2)),
                                      densptexts=c(rsqP(elec,texts,dens,indX),rsqP(elec,texts,dens,indXC),rsqP(elec,texts,dens,indXV)),
                                      denspcalls=c(rsqP(elec,calls,dens,indX),rsqP(elec,calls,dens,indXC),rsqP(elec,calls,dens,indXV)),
                                      densplength=c(rsqP(elec,length,dens,indX),rsqP(elec,length,dens,indXC),rsqP(elec,length,dens,indXV))
)

res_cor_electrification <- data.frame(type=c("All","Cities","Villages"),
                             dens=c(rsq(elec,dens),rsq(elec,dens,indXC),rsq(elec,dens,indXV)),
                             texts=c(rsq(elec,texts),rsq(elec,texts,indXC),rsq(elec,texts,indXV)),
                             calls=c(rsq(elec,calls),rsq(elec,calls,indXC),rsq(elec,calls,indXV)),
                             length=c(rsq(elec,length),rsq(elec,length,indXC),rsq(elec,length,indXV)),
                             logdens=c(rsq(elec,dens,indX,2),rsq(elec,dens,indXC,2),rsq(elec,dens,indXV,2)),
                             logtexts=c(rsq(elec,texts,indX,2),rsq(elec,texts,indXC,2),rsq(elec,texts,indXV,2)),
                             logcalls=c(rsq(elec,calls,indX,2),rsq(elec,calls,indXC,2),rsq(elec,calls,indXV,2)),
                             loglength=c(rsq(elec,length,indX,2),rsq(elec,length,indXC,2),rsq(elec,length,indXV,2)),
                             densptexts=c(rsqP(elec,texts,dens,indX),rsqP(elec,texts,dens,indXC),rsqP(elec,texts,dens,indXV)),
                             denspcalls=c(rsqP(elec,calls,dens,indX),rsqP(elec,calls,dens,indXC),rsqP(elec,calls,dens,indXV)),
                             densplength=c(rsqP(elec,length,dens,indX),rsqP(elec,length,dens,indXC),rsqP(elec,length,dens,indXV))
)


############################################################
##### Regressions: Density from Mobile phone and Night #####
############################################################


# Residuals Night ~ Mobile
resTexts_y <- lm(log(night[subset]) ~ log(texts[subset]))$residuals
resCalls_y <- lm(log(night[subset]) ~ log(calls[subset]))$residuals
resLength_y <- lm(log(night[subset]) ~ log(length[subset]))$residuals

resTexts_y_C <- lm(log(night[subsetC2]) ~ log(texts[subsetC2]))$residuals
resCalls_y_C <- lm(log(night[subsetC2]) ~ log(calls[subsetC2]))$residuals
resLength_y_C <- lm(log(night[subsetC2]) ~ log(length[subsetC2]))$residuals

resTexts_y_V <- lm(log(night[subsetV2]) ~ log(texts[subsetV2]))$residuals
resCalls_y_V <- lm(log(night[subsetV2]) ~ log(calls[subsetV2]))$residuals
resLength_y_V <- lm(log(night[subsetV2]) ~ log(length[subsetV2]))$residuals

resTexts_y_pop <- lm(log(night[subset]/dens[subset]) ~ log(texts[subset]))$residuals
resCalls_y_pop <- lm(log(night[subset]/dens[subset]) ~ log(calls[subset]))$residuals
resLength_y_pop <- lm(log(night[subset]/dens[subset]) ~ log(length[subset]))$residuals

# Cor with residuals
res_cor_dens <- data.frame(type=c("All","Cities","Villages"),
                           texts=c(rsqL(dens[subset],texts[subset],exp(resTexts_y)),rsqL(dens[subsetC2],texts[subsetC2],exp(resTexts_y_C)),rsqL(dens[subsetV2],texts[subsetV2],exp(resTexts_y_V))),
                           calls=c(rsqL(dens[subset],calls[subset],exp(resCalls_y)),rsqL(dens[subsetC2],calls[subsetC2],exp(resCalls_y_C)),rsqL(dens[subsetV2],calls[subsetV2],exp(resCalls_y_V))),
                           length=c(rsqL(dens[subset],length[subset],exp(resLength_y)),rsqL(dens[subsetC2],length[subsetC2],exp(resLength_y_C)),rsqL(dens[subsetV2],length[subsetV2],exp(resLength_y_V)))
)

# Cor without residuals
res_cor_dens0 <- data.frame(type=c("All","Cities","Villages"),
                           texts=c(rsqP(dens[subset],texts[subset]),rsqP(dens[subsetC],texts[subsetC]),rsqP(dens[subsetV],texts[subsetV])),
                           calls=c(rsqP(dens[subset],calls[subset]),rsqP(dens[subsetC],calls[subsetC]),rsqP(dens[subsetV],calls[subsetV])),
                           length=c(rsqP(dens[subset],length[subset]),rsqP(dens[subsetC],length[subsetC]),rsqP(dens[subsetV],length[subsetV]))
)

corlog <- function(a,b){
  return(cor(log(a),log(b))^2)
}

res_cor_dens1 <- data.frame(type=c("All","Cities","Villages"),
                            texts=c(corlog(dens[subset],texts[subset]),corlog(dens[subsetC],texts[subsetC]),corlog(dens[subsetV],texts[subsetV])),
                            calls=c(corlog(dens[subset],calls[subset]),corlog(dens[subsetC],calls[subsetC]),corlog(dens[subsetV],calls[subsetV])),
                            length=c(corlog(dens[subset],length[subset]),corlog(dens[subsetC],length[subsetC]),corlog(dens[subsetV],length[subsetV]))
)

######################################
##### Monthly R² for call length #####
######################################


for(i in monthslist){
  assign(i,avg_voronoi_ref_NA(get(paste("grid_Sptv_t",i,sep="_")),ref2)[[2]])
}

jan[which(jan == 0)] <- 1
feb[which(feb == 0)] <- 1
mar[which(mar == 0)] <- 1
apr[which(apr == 0)] <- 1
may[which(may == 0)] <- 1
jun[which(jun == 0)] <- 1
jul[which(jul == 0)] <- 1
aug[which(aug == 0)] <- 1
sep[which(sep == 0)] <- 1
oct[which(oct == 0)] <- 1
nov[which(nov == 0)] <- 1
dec[which(dec == 0)] <- 1

res_cor_energy_month <- data.frame(type=c("All","Cities","Villages"),
                                   jan=c(rsqP(night,jan,dens,subset),rsqP(night,jan,dens,subsetC),rsqP(night,jan,dens,subsetV)),
                                   feb=c(rsqP(night,feb,dens,subset),rsqP(night,feb,dens,subsetC),rsqP(night,feb,dens,subsetV)),
                                   mar=c(rsqP(night,mar,dens,subset),rsqP(night,mar,dens,subsetC),rsqP(night,mar,dens,subsetV)),
                                   apr=c(rsqP(night,apr,dens,subset),rsqP(night,apr,dens,subsetC),rsqP(night,apr,dens,subsetV)),
                                   may=c(rsqP(night,may,dens,subset),rsqP(night,may,dens,subsetC),rsqP(night,may,dens,subsetV)),
                                   jun=c(rsqP(night,jun,dens,subset),rsqP(night,jun,dens,subsetC),rsqP(night,jun,dens,subsetV)),
                                   jul=c(rsqP(night,jul,dens,subset),rsqP(night,jul,dens,subsetC),rsqP(night,jul,dens,subsetV)),
                                   aug=c(rsqP(night,aug,dens,subset),rsqP(night,aug,dens,subsetC),rsqP(night,aug,dens,subsetV)),
                                   sep=c(rsqP(night,sep,dens,subset),rsqP(night,sep,dens,subsetC),rsqP(night,sep,dens,subsetV)),
                                   oct=c(rsqP(night,oct,dens,subset),rsqP(night,oct,dens,subsetC),rsqP(night,oct,dens,subsetV)),
                                   nov=c(rsqP(night,nov,dens,subset),rsqP(night,nov,dens,subsetC),rsqP(night,nov,dens,subsetV)),
                                   dec=c(rsqP(night,dec,dens,subset),rsqP(night,dec,dens,subsetC),rsqP(night,dec,dens,subsetV))
)

plot(1:12,res_cor_energy_month[1,2:13],ylim=c(0,1),xlab="Month",ylab="R²",pch=18,xaxt='n',yaxt='n')
lines(1:12,res_cor_energy_month[1,2:13])
points(1:12,res_cor_energy_month[2,2:13],ylim=c(0,1),col=palette[5],pch=15)
lines(1:12,res_cor_energy_month[2,2:13],col=palette[5])
points(1:12,res_cor_energy_month[1,2:13],ylim=c(0,1),xlab="Month",ylab="R²",pch=18)
lines(1:12,res_cor_energy_month[1,2:13])
points(1:12,res_cor_energy_month[3,2:13],ylim=c(0,1),col=palette[2],pch=16)
lines(1:12,res_cor_energy_month[3,2:13],col=palette[2])
legend("bottomright",c("All","Cities","Villages"),pch=c(18,15,16),col=c(1,palette[5],palette[2]))
axis(side=1,at=1:12,labels=monthslist,las=3)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=as.character(c(0,0.2,0.4,0.6,0.8,1)),las=1)

plot(1:12,res_cor_energy_month[1,2:13],ylim=c(0.6,0.85),xlab="Month",ylab="R²",pch=18,xaxt='n',yaxt='n')
lines(1:12,res_cor_energy_month[1,2:13])
points(1:12,res_cor_energy_month[2,2:13],ylim=c(0,1),col=palette[5],pch=15)
lines(1:12,res_cor_energy_month[2,2:13],col=palette[5])
points(1:12,res_cor_energy_month[1,2:13],ylim=c(0,1),xlab="Month",ylab="R²",pch=18)
lines(1:12,res_cor_energy_month[1,2:13])
points(1:12,res_cor_energy_month[3,2:13],ylim=c(0,1),col=palette[2],pch=16)
lines(1:12,res_cor_energy_month[3,2:13],col=palette[2])
legend("bottomright",c("All","Cities","Villages"),pch=c(18,15,16),col=c(1,palette[5],palette[2]))
axis(side=1,at=1:12,labels=monthslist,las=3)
axis(side=2,at=c(0.6,0.7,0.8,0.9),labels=as.character(c(0.6,0.7,0.8,0.9)),las=1)


##############################
##### Correlation tables #####
##############################


vor_data2 <- vor_data[,2:ncol(vor_data)]
vor_data3 <- vor_data[,c(6:7,13,8:12)]
vor_data4 <- vor_data[,c(6:7,13,8:12)]*vor_data[,5]

colnames(vor_data3) <- c("dens","night","nightp","texts","calls","length")
colnames(vor_data4) <- c("dens","night","nightp","texts","calls","length")

subset <- NULL
for(i in 1:nrow(vor_data3)){
  if(all(vor_data3[i,1:6]>0)){
    subset <- c(subset,i)
  }
}
vor_data_sub <- log(vor_data3[subset,])

subset2 <- intersect(which(vor_data3[,2]>0),intersect(subset,which(vor_data3[,3]>0)))

meltedcor <- melt(cor(log(vor_data3[subset2,]),method="pearson"))
ggplot(meltedcor, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Pearson/Density (log)")

meltedcorP2 <- melt(cor(vor_data2,method="pearson"))
meltedcorS2 <- melt(cor(vor_data2,method="spearman"))
meltedcorK2 <- melt(cor(vor_data2,method="kendall"))
meltedcorP3 <- melt(cor(vor_data3,method="pearson"))
meltedcorS3 <- melt(cor(vor_data3,method="spearman"))
meltedcorK3 <- melt(cor(vor_data3,method="kendall"))
meltedcorP4 <- melt(cor(vor_data4,method="pearson"))
meltedcorS4 <- melt(cor(vor_data4,method="spearman"))
meltedcorK4 <- melt(cor(vor_data4,method="kendall"))
meltedcorP5 <- melt(cor(vor_data_sub,method="pearson"))
meltedcorS5 <- melt(cor(vor_data_sub,method="spearman"))
meltedcorK5 <- melt(cor(vor_data_sub,method="kendall"))

ggg1 <- ggplot(meltedcorP2, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Pearson/Density")
ggg2 <- ggplot(meltedcorS2, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Spearman/Density")
ggg3 <- ggplot(meltedcorK2, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Kendall/Density")
ggg4 <- ggplot(meltedcorP3, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Pearson/Density")
ggg5 <- ggplot(meltedcorS3, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Spearman/Density")
ggg6 <- ggplot(meltedcorK3, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Kendall/Density")
ggg7 <- ggplot(meltedcorP4, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Pearson/Total")
ggg8 <- ggplot(meltedcorS4, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Spearman/Total")
ggg9 <- ggplot(meltedcorK4, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Kendall/Total")
ggg10 <- ggplot(meltedcorP5, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Pearson/Log(Density)")
ggg11 <- ggplot(meltedcorS5, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Spear./Log.Dens")
ggg12 <- ggplot(meltedcorK5, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))+scale_fill_gradient2(low="orange",mid="white",high="steelblue4",limits=c(-1,1))+ggtitle("Kend./Log.Dens")

multiplot(ggg1,ggg2,ggg3,cols=3)
multiplot(ggg10,ggg11,ggg12,cols=3)
multiplot(ggg4,ggg7,ggg5,ggg8,ggg6,ggg9,cols=3)


#################################
##### Plots Night vs Mobile #####
#################################

subset <- which(dens2 > 0)

temp <- glm(night[subset] ~ log(dens2[subset]) + log(length[subset]),family = poisson(link = "log"),na.action = na.exclude)
temp <- glm(night[subset] ~ log(dens[subset]) + log(texts[subset]),family = poisson(link = "log"),na.action = na.exclude)
temp <- glm(night[subset] ~ log(dens[subset]) + log(calls[subset]),family = poisson(link = "log"),na.action = na.exclude)

# Length
plot(1:length(night[subset]),
     night[subset]-exp(temp$coefficients[1])*dens2[subset]^temp$coefficients[2]*length[subset]^temp$coefficients[3],
     xlab="Tower Id",ylab="Diff Nightlight vs predicted",
     col=alpha(1, 0.3),pch=""
)
lines(c(0,1300),c(0,0),lwd=2,col=2)
points(night[subset]-exp(temp$coefficients[1])*dens2[subset]^temp$coefficients[2]*length[subset]^temp$coefficients[3],col=alpha(1, 0.3),pch=20)

plot(1:length(night),
     night-exp(temp$coefficients[1])*dens2^temp$coefficients[2]*length^temp$coefficients[3],
     xlab="Tower Id",ylab="Diff Nightlight vs predicted",
     col=alpha(1, 0.3),pch=""
)
lines(c(0,1300),c(0,0),lwd=2,col=2)
points(1:length(night),night-exp(temp$coefficients[1])*dens2^temp$coefficients[2]*length^temp$coefficients[3],col=alpha(1, 0.3),pch=20)


fit <- lm(log(exp(temp$coefficients[1])*dens[subset]^temp$coefficients[2]*length[subset]^temp$coefficients[3]) ~ log(night[subset]))
x <- seq(log(0.015),log(63),by=0.01)

plot(log(night),
     log(exp(temp$coefficients[1])*dens^temp$coefficients[2]*length^temp$coefficients[3]),
     xlab="Nightlights (log)",ylab="Predicted (log)",
     col=alpha(1, 0.3),pch=""
)
lines(x,fit$coefficients[1]+fit$coefficients[2]*x,col=2,lwd=2)
points(log(night),log(exp(temp$coefficients[1])*dens^temp$coefficients[2]*length^temp$coefficients[3]),col=alpha(1, 0.3),pch=20)

plot(night[subset],
     exp(temp$coefficients[1])*dens2[subset]^temp$coefficients[2]*length[subset]^temp$coefficients[3],
     xlab="Nighttime lights",ylab="Predicted",
     col=alpha(1, 0.3),pch=""
)
lines(0:63,0:63,col=2,lwd=2)
points(night[subset],exp(temp$coefficients[1])*dens2[subset]^temp$coefficients[2]*length[subset]^temp$coefficients[3],col=alpha(1, 0.3),pch=20)

plot(night,
     exp(temp$coefficients[1])*dens2^temp$coefficients[2]*length^temp$coefficients[3],
     xlab="Nighttime lights",ylab="Predicted",
     col=alpha(1, 0.3),pch=""
)
lines(0:63,0:63,col=2,lwd=2)
points(night,exp(temp$coefficients[1])*dens2^temp$coefficients[2]*length^temp$coefficients[3],col=alpha(1, 0.3),pch=20)



predicted <- exp(temp$coefficients[1])*dens^temp$coefficients[2]*texts^temp$coefficients[3]
predicted <- exp(temp$coefficients[1])*dens^temp$coefficients[2]*length^temp$coefficients[3]

predicted <- exp(temp$coefficients[1])*dens^temp$coefficients[2]*calls^temp$coefficients[3]
table <- data.frame(night=predicted,dens=dens,calls=calls)
table <- as.matrix(table)

predicted[which(predicted>63)] <- 63

cor(predicted,night)^2

0.860313
0.8880523

cor(predicted[subset],night[subset])^2
cor(predicted[subsetC],night[subsetC])^2
cor(predicted[subsetV],night[subsetV])^2

length(night)
length(subsetC)+length(subsetV)
length(predicted)

predictP <- function(x,y){
  return(exp(temp$coefficients[1])*x^temp$coefficients[2]*y^temp$coefficients[3])
}



write.table(table,"table.csv",sep=",",row.names = F)


table <- data.frame(night=predicted[subset],dens=dens2[subset],calls=calls[subset])
table <- as.matrix(table)
write.table(table,"table2.csv",sep=",",row.names = F)

table <- data.frame(night=night,dens=dens2,calls=calls)
table <- as.matrix(table)
write.table(table,"table3.csv",sep=",",row.names = F)


table <- data.frame(Id=1:1298,long=vor_data$l,lat,dens,nightlight,Ntexts,Ncalls,length)

SIdataset_one <- read.csv("SI-dataset_one.csv",sep=" ")
SIdataset_one$dens <- vor_data$dens2
write.csv(SIdataset_one,"SI-dataset_one.csv")

library(lattice)

n <- 100
x <- seq(0,55000,length.out = n)
y <- seq(0,45000000,length.out = n)
myGrid <- data.frame(expand.grid(x,y))
colnames(myGrid) <- c("x","y")
myGrid$z <- predictP(x,y)
myGrid$z2 <- myGrid$z

mypanel <- function(x,y,z,z2,...) {
  panel.wireframe(x,y,z,...)
  panel.cloud(x,y,z2,...)
}
wireframe(z ~ x * y, data=myGrid, xlab="X", ylab="Y", zlab="Z",
          panel=mypanel, z2=myGrid$z2)



###################################################
##### Suggested electrification rate increase #####
###################################################


cor(night,adHoc(night,length)[[1]])^2
cor(night,adHoc(night,calls)[[1]])^2
cor(night,adHoc(night,dens)[[1]])^2

cor(night[subsetC],adHoc(night[subsetC],length[subsetC])[[1]])^2
cor(night[subsetC],adHoc(night[subsetC],calls[subsetC])[[1]])^2
cor(night[subsetC],adHoc(night[subsetC],dens[subsetC])[[1]])^2

cor(night[subsetV],adHoc(night[subsetV],length[subsetV])[[1]])^2
cor(night[subsetV],adHoc(night[subsetV],calls[subsetV])[[1]])^2
cor(night[subsetV],adHoc(night[subsetV],dens[subsetV])[[1]])^2

# Night deduced from Elec x Dens
nightest <- adHoc(night,elec*dens)[[1]]
coefreg <- adHoc(night,elec*dens)[[2]]

plot(night,nightest,pch="",ylab="night(elec*dens)")
lines(c(0,63),c(0,63),lwd=2,col=2)
points(night,nightest,pch=20,col=alpha(1,0.4))
text(7,77,paste("Pears=",round(cor(nightest,night,method="pearson"),2),sep=""))
text(7,66,paste("Spear=",round(cor(nightest,night,method="spearman"),2),sep=""))

####

elecest <- adHoc(elec*dens,night)[[1]]
coefreg2 <- adHoc(elec*dens,night)[[2]]

plot(elec*dens,elecest)
lines(c(0,50000),c(0,50000),col=2)

plot(elec,elecest/dens,ylim=c(0,1))
lines(c(0,50000),c(0,50000),col=2)

elecback <- round(exp(log((nightest-coefreg[2])/coefreg[1])/coefreg[3])/dens,2)
elecback <- round(exp(log((night-coefreg[2])/coefreg[1])/coefreg[3])/dens,2)

plot(night,nightest)
lines(c(0,63),c(0,63),col=2)

plot(elec*dens,elecback,ylim=c(0,1))
lines(c(0,1),c(0,1),col=2)

# Night from Calls
temp <- adHoc(night,calls)[[1]]
temp <- adHoc(night,length)[[1]]

plot(night,temp,pch="",ylab="night(calls)")
lines(c(0,63),c(0,63),lwd=2,col=2)
points(night,temp,pch=20,col=alpha(1,0.4))
text(7,77,paste("Pears=",round(cor(temp,night,method="pearson"),2),sep=""))
text(7,66,paste("Spear=",round(cor(temp,night,method="spearman"),2),sep=""))

final <- round(exp(log((temp-coefreg[2])/coefreg[1])/coefreg[3])/dens,2)
final2 <- pmax(elec,final)
final3 <- pmin(rep(1,length(final)),final2)
diff <- final3-elec

plot(night,nightest)
plot(elec,temp2)

temp2 <- round(exp(log((nightest-coefreg[2])/coefreg[1])/coefreg[3])/dens,2)
diff2 <- final-temp2
diff2 <- pmax(0,diff2)
rdiff2 <- (final+diff2)/final

felec <- pmin(rep(1,length(final)),elec*rdiff2)

plot(1:length(diff),temp2)
plot(1:length(diff),final3)

plot(1:length(diff),diff2)
plot(1:length(diff),rdiff2)
plot(1:length(diff),felec)

plot(1:length(diff),diff)
points(1:length(diff),diff2,col=2)

plot(1:length(diff),diff,ylim=c(0,1),pch=20,col=alpha(1,0.4),xlab="Tower",ylab="Suggested elec. increase",cex=0.5,main="Calls alone")
plot(1:length(diff),final3,ylim=c(0,1),pch=20,col=alpha(1,0.4),xlab="Tower",ylab="Final elec. rate",cex=0.5,main="Calls alone")

# Night from Dens + Calls
subset <- which(night>0 & dens>0 & calls>0)
temp2 <- glm(night[subset] ~ log(dens[subset]) + log(calls[subset]),family = poisson(link = "log"),na.action = na.exclude)
temp <- exp(temp2$coefficients[1])*dens^temp2$coefficients[2]*calls^temp2$coefficients[3]

final <- round(exp(log((temp-coefreg[2])/coefreg[1])/coefreg[3])/dens,2)
final2 <- pmax(elec,final)
final3 <- pmin(rep(1,length(final)),final2)
diff <- final3-elec

plot(1:length(diff),diff,ylim=c(0,1),pch=20,col=alpha(1,0.4),xlab="Tower",ylab="Suggested elec. increase",cex=0.5,main="Dens+Calls")
plot(1:length(diff),final3,ylim=c(0,1),pch=20,col=alpha(1,0.4),xlab="Tower",ylab="Final elec. rate",cex=0.5,main="Dens+Calls")

# Villages only
nightest <- adHoc(night[subsetV],elec[subsetV]*dens[subsetV])[[1]]
coefreg <- adHoc(night[subsetV],elec[subsetV]*dens[subsetV])[[2]]

plot(night[subsetV],nightest,pch="")
lines(c(0,64),c(0,64),lwd=2,col=2)
points(night[subsetV],nightest,pch=20,col=alpha(1,0.4))
text(7,57,paste("Pears=",round(cor(nightest,night[subsetV],method="pearson"),2),sep=""))
text(7,50,paste("Spear=",round(cor(nightest,night[subsetV],method="spearman"),2),sep=""))

temp <- adHoc(night[subsetV],calls[subsetV])[[1]]

final <- round(exp(log((temp-coefreg[2])/coefreg[1])/coefreg[3])/dens[subsetV],2)
final2 <- pmax(elec[subsetV],final)
final3 <- pmin(rep(1,length(final)),final2)
diff <- final3-elec[subsetV]

plot(subsetV,diff,ylim=c(0,1),pch=20,col=alpha(1,0.4),xlab="Tower",ylab="Suggested elec. increase",cex=0.5,main="Calls alone (villages)")
plot(subsetV,elec[subsetV],ylim=c(0,1),pch=20,col=alpha(2,0.4),xlab="Tower",ylab="Final elec. rate",cex=0.5,main="Calls alone (villages)")
points(subsetV,final3,ylim=c(0,1),pch=20,col=alpha(1,0.4),xlab="Tower",ylab="Final elec. rate",cex=0.5,main="Calls alone (villages)")
for(i in 1:length(subsetV)){
  lines(c(subsetV[i],subsetV[i]),c(final3[i],elec[subsetV[i]]),lty=3,lwd=0.5)
}
legend("bottomleft",c("old","new"),col=c(2,1),pch=c(20,20),pt.cex=c(0.5,0.5))


###################################################
##### Correlations with variable coefficients #####
###################################################


n=30

# Dens by dens
indices <- bin(dens2,n)
M <- 1:n
mdens <- rep(0,n)
alphaM <- rep(0,n)
sdM <- rep(0,n)
for(i in 1:n){
  ind <- intersect(indices[[i]],which(night>0))
  mdens[i] <- mean(dens2[ind])
  alphaM[i] <- mean(log(night[ind])/log(mdens[i]),na.rm=T)
  sdM[i] <- sd(log(night[ind])/log(mdens[i]),na.rm=T)
}
mdens <- mdens[2:n]
alphaM <- alphaM[2:n]
sdM <- sdM[2:n]

x <- 0:55000
fit <- lm(log(alphaM) ~ log(mdens))
estimate <- dens^(exp(fit$coefficients[1])*dens^fit$coefficients[2])

#fit5 <- lm(alphaM ~ mdens)
#estimate5 <- fit$coefficients[1]+dens*fit$coefficients[2]

##
plot(mdens,alphaM,xlab="Density",ylab=expression(alpha),
     #main=expression(paste("Lights=Dens^",alpha,"(Dens)")),
     ylim=c(min(alphaM-sdM,alphaM+sdM,na.rm=T),max(alphaM-sdM,alphaM+sdM,na.rm=T)))
lines(x,exp(fit$coefficients[1])*x^fit$coefficients[2],col=2,lwd=1)
arrows(mdens, alphaM-sdM, mdens, alphaM+sdM, length=0.05, angle=90, code=3)
points(mdens,alphaM)
#lines(x,fit5$coefficients[1]+x*fit5$coefficients[2],col=4,lwd=1)

text(41000,0.49,paste("R²=",round(cor(night,estimate)^2,3)))

##
corrref <- rep(0,100)
for(i in 6:105){
  corrref[i-5] <- multidensAnalysis(i)
}

plot(6:105,corrref,main="R² dens by dens",pch=3,xlab="Number of bins",ylab="R²")
#lines(6:105,corrref)
#points(30,corrref[30-6],col=2)
lines(c(30,30),c(0,1),lty=3)
text(34,0.7365,"30")

plot(6:105,corrref,main="R² dens by dens",ylim=c(0,1),pch=3,xlab="Number of bins",ylab="R²")
lines(c(30,30),c(0,1),lty=3)
text(34,0.02,"30")

# Texts by dens
indices <- bin(dens2,n)
mdens2 <- rep(0,n)
alphaM2 <- rep(0,n)
sdM2 <- rep(0,n)
for(i in 1:n){
  ind <- intersect(indices[[i]],which(night>0))
  ind <- intersect(ind,which(texts>0))
  mdens2[i] <- mean(dens2[ind])
  alphaM2[i] <- mean(log(night[ind])/log(texts[ind]),na.rm=T)
  sdM2[i] <- sd(log(night[ind])/log(texts[ind]),na.rm=T)
}
mdens2 <- mdens2[2:n]
alphaM2 <- alphaM2[2:n]
sdM2 <- sdM2[2:n]

fit2 <- lm(alphaM2 ~ mdens2)
estimate2 <- dens^(fit2$coefficients[1]+dens*fit2$coefficients[2])

##
plot(mdens2,alphaM2,xlab="Density",ylab=expression(beta),
     #main=expression(paste("Lights=Texts^",alpha,"(Dens)")),
     ylim=c(min(alphaM2-sdM2,alphaM2+sdM2,na.rm=T),max(alphaM2-sdM2,alphaM2+sdM2,na.rm=T)))
lines(x,fit2$coefficients[1]+x*fit2$coefficients[2],col=2,lwd=1)
arrows(mdens2, alphaM2-sdM2, mdens2, alphaM2+sdM2, length=0.05, angle=90, code=3)
points(mdens2,alphaM2)

text(41000,0.29,paste("R²=",round(cor(night,estimate2)^2,3),sep=""))

##
corrref2 <- rep(0,100)
for(i in 6:105){
  corrref2[i-5] <- multidensAnalysis2(i)
}

plot(6:105,corrref2,main="R² Texts by dens")
lines(6:105,corrref2)

plot(6:105,corrref2,main="R² Texts by dens",ylim=c(0,1))
lines(6:105,corrref2)


# Texts by texts
texts2 <- texts[which(texts < 50000)]

length(texts)
length(texts2)

temp <- data.frame(texts=texts)
temp$bin <- texts
for(i in 1:30){
  if(length(indices2[[i]])>0){
    for(j in 1:length(indices2[[i]])){
      temp$bin[j] <- i
    }
  }
}

temp <- texts
temp <- length

indices2 <- bin(temp,n)
mdens3 <- rep(0,n)
alphaM3 <- rep(0,n)
sdM3 <- rep(0,n)
for(i in 1:n){
  ind <- intersect(indices2[[i]],which(night>0))
  ind <- intersect(ind,which(temp>0))
  mdens3[i] <- mean(temp[ind])
  alphaM3[i] <- mean(log(night[ind])/log(temp[ind]),na.rm=T)
  sdM3[i] <- sd(log(night[ind])/log(temp[ind]),na.rm=T)
}
mdens3 <- mdens3[2:n]
alphaM3 <- alphaM3[2:n]
sdM3 <- sdM3[2:n]

fit3 <- lm(alphaM3 ~ mdens3)
estimate3 <- temp^(fit3$coefficients[1]+temp*fit3$coefficients[2])
temp2 <- fit3$coefficients[1]+mdens3*fit3$coefficients[2]
temp3 <- which(temp2<1)
cor(alphaM3[temp3],temp2[temp3])^2

fit4 <- lm(log(alphaM3) ~ log(mdens3))
estimate4 <- temp^(exp(fit4$coefficients[1])*temp^fit4$coefficients[2])
temp2 <- exp(fit4$coefficients[1])*mdens3^fit4$coefficients[2]
temp3 <- which(temp2<1)
cor(alphaM3[temp3],temp2[temp3])^2

xx <- seq(0,9e+07,by=1e+06)

##
plot(mdens3,alphaM3,xlab="Texts",ylab=expression(gamma),
     #main=expression(paste("Lights=Texts^",alpha,"(Texts)")),
     ylim=c(min(alphaM3-sdM3,alphaM3+sdM3,na.rm=T),max(alphaM3-sdM3,alphaM3+sdM3,na.rm=T)))
lines(xx,fit3$coefficients[1]+xx*fit3$coefficients[2],col=2,lwd=1)
arrows(mdens3, alphaM3-sdM3, mdens3, alphaM3+sdM3, length=0.05, angle=90, code=3)
points(mdens3,alphaM3)
lines(xx,exp(fit4$coefficients[1])*xx^fit4$coefficients[2],col=4,lwd=1)

cor(night,estimate3)^2
cor(night,estimate4)^2

plot(night,estimate3)

plot(1:length(night),estimate3-night,pch="",col=alpha(1,0.4),xlab="Tower Id",ylab="Predicted-real")
lines(c(0,1300),c(0,0),lwd=2,col=2)
points(1:length(night),estimate3-night,pch=20,col=alpha(1,0.4))

###############################################################
##### Pearson/box aggregation: influence of the unit size #####
###############################################################


multp <- c(1:50)
aggGrid_Sptt_y <- GridingAgg(grid_Sptt_y,multp,startpos=1)
aggGrid_Sptv_n_y <- GridingAgg(grid_Sptv_n_y,multp,startpos=1)
aggGrid_Sptv_t_y <- GridingAgg(grid_Sptv_t_y,multp,startpos=1)
aggGrid_nightlight <- GridingAgg(nightlight,multp,startpos=1)

res_pearson_agg <- data.frame(t=rep(0,50),v_n=rep(0,50),v_t=rep(0,50))
for(i in 1:50){
  res_pearson_agg$t[i] <- pearson(aggGrid_Sptt_y[[i]],aggGrid_nightlight[[i]])
  res_pearson_agg$v_n[i] <- pearson(aggGrid_Sptv_n_y[[i]],aggGrid_nightlight[[i]])
  res_pearson_agg$v_t[i] <- pearson(aggGrid_Sptv_t_y[[i]],aggGrid_nightlight[[i]])
}

plot(1:50,res_pearson_agg$t,pch=18,xlab="Grid unit size",ylab="Pearson",ylim=c(0,1))
lines(1:50,res_pearson_agg$t)
points(1:50,res_pearson_agg$v_n,pch=15,xlab="Grid size",ylab="Pearson",main="# Call",col=palette[5])
lines(1:50,res_pearson_agg$v_n)
points(1:50,res_pearson_agg$v_t,pch=16,xlab="Grid size",ylab="Pearson",main="Call length",col=palette[2])
lines(1:50,res_pearson_agg$v_t)
legend("bottomright",c("Text messages","# Call","Total call length"),pch=c(18,15,16),col=c(1,palette[5],palette[2]))




