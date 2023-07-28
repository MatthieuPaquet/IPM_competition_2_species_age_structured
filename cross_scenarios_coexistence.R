#load the four sets of 100 simulated data and their parameter values
setwd("/home/matpaquet/Documents/IPM_competition_2_species_age_structured")
load("data/data_coexistence_model_param1AF_slice100juvmarked50nests30nyearsddpriorexp.Rdata")
list.simul_1 <- list.simul
remove(list.simul)
paramvalues_1 <- paramvalues
remove(paramvalues)
load("data/data_coexistence_model_param2AF_slice100juvmarked50nests30nyearsddpriorexp.Rdata")
remove(list.samples)
list.simul_2 <- list.simul
remove(list.simul)
paramvalues_2 <- paramvalues
remove(paramvalues)
load("data/data_coexistence_model_param3AF_slice100juvmarked50nests30nyearsddpriorexp.Rdata")
remove(list.samples)
list.simul_3 <- list.simul
remove(list.simul)
paramvalues_3 <- paramvalues
remove(paramvalues)
load("data/data_coexistence_model_param4AF_slice100juvmarked50nests30nyearsddpriorexp.Rdata")
remove(list.samples)
list.simul_4 <- list.simul
remove(list.simul)
paramvalues_4 <- paramvalues
remove(paramvalues)
nyears <- coexconstants$nyears
#plot time series and associated count data
pdf(file=paste("plots/compare_timeseries.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2))
plot(1:nyears, list.simul_1[[1]]$N1j, type='l', lwd=3, ylim=c(0,300),
     col='#7fa626', ylab='population size', xlab='years')
lines(1:nyears, list.simul_1[[1]]$N1a, type='l', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_1[[1]]$N2j, type='l', lwd=3, col='#2997af')
lines(1:nyears, list.simul_1[[1]]$N2a, type='l', lwd=3, col='#085a6b')
lines(1:nyears, list.simul_1[[1]]$N1jobs, type='p', lwd=3, col='#7fa626')
lines(1:nyears, list.simul_1[[1]]$N1aobs, type='p', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_1[[1]]$N2jobs, type='p', lwd=3, col='#2997af')
lines(1:nyears, list.simul_1[[1]]$N2aobs, type='p', lwd=3, col='#085a6b')
abline(h=0)
legend('topleft',legend="Parameter set 1",bty = "n")
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
plot(1:nyears, list.simul_2[[1]]$N1j, type='l', lwd=3, ylim=c(0,300),
     col='#7fa626', ylab='population size', xlab='years')
lines(1:nyears, list.simul_2[[1]]$N1a, type='l', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_2[[1]]$N2j, type='l', lwd=3, col='#2997af')
lines(1:nyears, list.simul_2[[1]]$N2a, type='l', lwd=3, col='#085a6b')
lines(1:nyears, list.simul_2[[1]]$N1jobs, type='p', lwd=3, col='#7fa626')
lines(1:nyears, list.simul_2[[1]]$N1aobs, type='p', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_2[[1]]$N2jobs, type='p', lwd=3, col='#2997af')
lines(1:nyears, list.simul_2[[1]]$N2aobs, type='p', lwd=3, col='#085a6b')
abline(h=0)
legend('topleft',legend="Parameter set 2",bty = "n")
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
plot(1:nyears, list.simul_3[[1]]$N1j, type='l', lwd=3, ylim=c(0,300),
     col='#7fa626', ylab='population size', xlab='years')
lines(1:nyears, list.simul_3[[1]]$N1a, type='l', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_3[[1]]$N2j, type='l', lwd=3, col='#2997af')
lines(1:nyears, list.simul_3[[1]]$N2a, type='l', lwd=3, col='#085a6b')
lines(1:nyears, list.simul_3[[1]]$N1jobs, type='p', lwd=3, col='#7fa626')
lines(1:nyears, list.simul_3[[1]]$N1aobs, type='p', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_3[[1]]$N2jobs, type='p', lwd=3, col='#2997af')
lines(1:nyears, list.simul_3[[1]]$N2aobs, type='p', lwd=3, col='#085a6b')
abline(h=0)
legend('topleft',legend="Parameter set 3",bty = "n")
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
plot(1:nyears, list.simul_4[[1]]$N1j, type='l', lwd=3, ylim=c(0,300),
     col='#7fa626', ylab='population size', xlab='years')
lines(1:nyears, list.simul_4[[1]]$N1a, type='l', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_4[[1]]$N2j, type='l', lwd=3, col='#2997af')
lines(1:nyears, list.simul_4[[1]]$N2a, type='l', lwd=3, col='#085a6b')
lines(1:nyears, list.simul_4[[1]]$N1jobs, type='p', lwd=3, col='#7fa626')
lines(1:nyears, list.simul_4[[1]]$N1aobs, type='p', lwd=3, col='#2d5f17')
lines(1:nyears, list.simul_4[[1]]$N2jobs, type='p', lwd=3, col='#2997af')
lines(1:nyears, list.simul_4[[1]]$N2aobs, type='p', lwd=3, col='#085a6b')
abline(h=0)
legend('topleft',legend="Parameter set 4",bty = "n")
legend('topright',col=c('#7fa626','#2d5f17','#2997af','#085a6b'),legend=c("juveniles sp.1","adults sp.1","juveniles sp.2","adults sp.2"),lty=1,lwd=3)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
dev.off()
###load summaries of parameter values
load("data/summary_data1ddpriorexp.Rdata")
inv1_par1_priorexp <- summary_inv1
inv2_par1_priorexp <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data2ddpriorexp.Rdata")
inv1_par2_priorexp <- summary_inv1
inv2_par2_priorexp <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data3ddpriorexp.Rdata")
inv1_par3_priorexp <- summary_inv1
inv2_par3_priorexp <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data4ddpriorexp.Rdata")
inv1_par4_priorexp <- summary_inv1
inv2_par4_priorexp <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data1ddpriorlognormlow.Rdata")
inv1_par1_priorlognormlow <- summary_inv1
inv2_par1_priorlognormlow <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data2ddpriorlognormlow.Rdata")
inv1_par2_priorlognormlow <- summary_inv1
inv2_par2_priorlognormlow <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data3ddpriorlognormlow.Rdata")
inv1_par3_priorlognormlow <- summary_inv1
inv2_par3_priorlognormlow <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data4ddpriorlognormlow.Rdata")
inv1_par4_priorlognormlow <- summary_inv1
inv2_par4_priorlognormlow <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data1ddpriorlognormhigh.Rdata")
inv1_par1_priorlognormhigh <- summary_inv1
inv2_par1_priorlognormhigh <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data2ddpriorlognormhigh.Rdata")
inv1_par2_priorlognormhigh <- summary_inv1
inv2_par2_priorlognormhigh <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data3ddpriorlognormhigh.Rdata")
inv1_par3_priorlognormhigh <- summary_inv1
inv2_par3_priorlognormhigh <- summary_inv2
remove(summary_inv1,summary_inv2)
load("data/summary_data4ddpriorlognormhigh.Rdata")
inv1_par4_priorlognormhigh <- summary_inv1
inv2_par4_priorlognormhigh <- summary_inv2
remove(summary_inv1,summary_inv2)

pdf(file=paste("plots/compare_invasion.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))

plot(x='n',ylim=c(0,10),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(inv1_par1_priorexp[4],inv1_par1_priorlognormlow[4],inv1_par1_priorlognormhigh[4]),c(1,1.5,2),c(inv1_par1_priorexp[3],inv1_par1_priorlognormlow[3],inv1_par1_priorlognormhigh[3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(inv1_par1_priorexp[2],inv1_par1_priorlognormlow[2],inv1_par1_priorlognormhigh[2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(inv1_par1_priorexp[1],inv1_par1_priorlognormlow[1],inv1_par1_priorlognormhigh[1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(inv2_par1_priorexp[4],inv2_par1_priorlognormlow[4],inv2_par1_priorlognormhigh[4]),c(3,3.5,4),c(inv2_par1_priorexp[3],inv2_par1_priorlognormlow[3],inv2_par1_priorlognormhigh[3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(inv2_par1_priorexp[2],inv2_par1_priorlognormlow[2],inv2_par1_priorlognormhigh[2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(inv2_par1_priorexp[1],inv2_par1_priorlognormlow[1],inv2_par1_priorlognormhigh[1]),col="red",cex=1)
abline(h=1)
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topleft',legend="Parameter set 1",bty = "n")
plot(x='n',ylim=c(0,10),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(inv1_par2_priorexp[4],inv1_par2_priorlognormlow[4],inv1_par2_priorlognormhigh[4]),c(1,1.5,2),c(inv1_par2_priorexp[3],inv1_par2_priorlognormlow[3],inv1_par2_priorlognormhigh[3]))
points(pch=21,x=c(1,1.5,2),c(inv1_par2_priorexp[2],inv1_par2_priorlognormlow[2],inv1_par2_priorlognormhigh[2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(inv1_par2_priorexp[1],inv1_par2_priorlognormlow[1],inv1_par2_priorlognormhigh[1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(inv2_par2_priorexp[4],inv2_par2_priorlognormlow[4],inv2_par2_priorlognormhigh[4]),c(3,3.5,4),c(inv2_par2_priorexp[3],inv2_par2_priorlognormlow[3],inv2_par2_priorlognormhigh[3]))
points(pch=22,x=c(3,3.5,4),c(inv2_par2_priorexp[2],inv2_par2_priorlognormlow[2],inv2_par2_priorlognormhigh[2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(inv2_par2_priorexp[1],inv2_par2_priorlognormlow[1],inv2_par2_priorlognormhigh[1]),col="red",cex=1)
abline(h=1)
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topleft',legend="Parameter set 2",bty = "n")
mtext("Mean estimated invation criteria", side = 3, line = 1, outer = TRUE,cex=1.2)
plot(x='n',ylim=c(0,10),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(inv1_par3_priorexp[4],inv1_par3_priorlognormlow[4],inv1_par3_priorlognormhigh[4]),c(1,1.5,2),c(inv1_par3_priorexp[3],inv1_par3_priorlognormlow[3],inv1_par3_priorlognormhigh[3]))
points(pch=21,x=c(1,1.5,2),c(inv1_par3_priorexp[2],inv1_par3_priorlognormlow[2],inv1_par3_priorlognormhigh[2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(inv1_par3_priorexp[1],inv1_par3_priorlognormlow[1],inv1_par3_priorlognormhigh[1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(inv2_par3_priorexp[4],inv2_par3_priorlognormlow[4],inv2_par3_priorlognormhigh[4]),c(3,3.5,4),c(inv2_par3_priorexp[3],inv2_par3_priorlognormlow[3],inv2_par3_priorlognormhigh[3]))
points(pch=22,x=c(3,3.5,4),c(inv2_par3_priorexp[2],inv2_par3_priorlognormlow[2],inv2_par3_priorlognormhigh[2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(inv2_par3_priorexp[1],inv2_par3_priorlognormlow[1],inv2_par3_priorlognormhigh[1]),col="red",cex=1)
abline(h=1)
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topleft',legend="Parameter set 3",bty = "n")
mtext(par(las=1),text=c("Species 1","Species 2"),
      at=c(1.5,3.5), side=1,line=1,adj=NA,cex=1,padj=NA)

plot(x='n',ylim=c(0,10),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(inv1_par4_priorexp[4],inv1_par4_priorlognormlow[4],inv1_par4_priorlognormhigh[4]),c(1,1.5,2),c(inv1_par4_priorexp[3],inv1_par4_priorlognormlow[3],inv1_par4_priorlognormhigh[3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(inv1_par4_priorexp[2],inv1_par4_priorlognormlow[2],inv1_par4_priorlognormhigh[2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(inv1_par4_priorexp[1],inv1_par4_priorlognormlow[1],inv1_par4_priorlognormhigh[1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(inv2_par4_priorexp[4],inv2_par4_priorlognormlow[4],inv2_par4_priorlognormhigh[4]),c(3,3.5,4),c(inv2_par4_priorexp[3],inv2_par4_priorlognormlow[3],inv2_par4_priorlognormhigh[3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(inv2_par4_priorexp[2],inv2_par4_priorlognormlow[2],inv2_par4_priorlognormhigh[2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(inv2_par4_priorexp[1],inv2_par4_priorlognormlow[1],inv2_par4_priorlognormhigh[1]),col="red",cex=1)
abline(h=1)
legend('topleft',legend="Parameter set 4",bty = "n")
mtext(par(las=1),text=c("Species 1","Species 2"),
      at=c(1.5,3.5),line=1)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',col=c("black","black","black",'red'),legend=c("prior type 1","prior type 2","prior type 3","true value"),pch=c(21,22,23,19))
dev.off()