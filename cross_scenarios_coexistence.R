library(latex2exp)
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
alpha_par1_priorexp <- summary_alpha
beta_par1_priorexp <- summary_beta
overlap_alpha_par1_priorexp <- alpha.overlap
overlap_beta_par1_priorexp <- beta.overlap
qt_inv1est_par1_priorexp <- qt_inv1est
qt_inv2est_par1_priorexp <- qt_inv2est
competoutcome_par1_priorexp <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data2ddpriorexp.Rdata")
inv1_par2_priorexp <- summary_inv1
inv2_par2_priorexp <- summary_inv2
alpha_par2_priorexp <- summary_alpha
beta_par2_priorexp <- summary_beta
overlap_alpha_par2_priorexp <- alpha.overlap
overlap_beta_par2_priorexp <- beta.overlap
qt_inv1est_par2_priorexp <- qt_inv1est
qt_inv2est_par2_priorexp <- qt_inv2est
competoutcome_par2_priorexp <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data3ddpriorexp.Rdata")
inv1_par3_priorexp <- summary_inv1
inv2_par3_priorexp <- summary_inv2
alpha_par3_priorexp <- summary_alpha
beta_par3_priorexp <- summary_beta
overlap_alpha_par3_priorexp <- alpha.overlap
overlap_beta_par3_priorexp <- beta.overlap
qt_inv1est_par3_priorexp <- qt_inv1est
qt_inv2est_par3_priorexp <- qt_inv2est
competoutcome_par3_priorexp <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data4ddpriorexp.Rdata")
inv1_par4_priorexp <- summary_inv1
inv2_par4_priorexp <- summary_inv2
alpha_par4_priorexp <- summary_alpha
beta_par4_priorexp <- summary_beta
overlap_alpha_par4_priorexp <- alpha.overlap
overlap_beta_par4_priorexp <- beta.overlap
qt_inv1est_par4_priorexp <- qt_inv1est
qt_inv2est_par4_priorexp <- qt_inv2est
competoutcome_par4_priorexp <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data1ddpriorlognormlow.Rdata")
inv1_par1_priorlognormlow <- summary_inv1
inv2_par1_priorlognormlow <- summary_inv2
alpha_par1_priorlognormlow <- summary_alpha
beta_par1_priorlognormlow <- summary_beta
overlap_alpha_par1_priorlognormlow <- alpha.overlap
overlap_beta_par1_priorlognormlow <- beta.overlap
qt_inv1est_par1_priorlognormlow <- qt_inv1est
qt_inv2est_par1_priorlognormlow <- qt_inv2est
competoutcome_par1_priorlognormlow <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data2ddpriorlognormlow.Rdata")
inv1_par2_priorlognormlow <- summary_inv1
inv2_par2_priorlognormlow <- summary_inv2
alpha_par2_priorlognormlow <- summary_alpha
beta_par2_priorlognormlow <- summary_beta
overlap_alpha_par2_priorlognormlow <- alpha.overlap
overlap_beta_par2_priorlognormlow <- beta.overlap
qt_inv1est_par2_priorlognormlow <- qt_inv1est
qt_inv2est_par2_priorlognormlow <- qt_inv2est
competoutcome_par2_priorlognormlow <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data3ddpriorlognormlow.Rdata")
inv1_par3_priorlognormlow <- summary_inv1
inv2_par3_priorlognormlow <- summary_inv2
alpha_par3_priorlognormlow <- summary_alpha
beta_par3_priorlognormlow <- summary_beta
overlap_alpha_par3_priorlognormlow <- alpha.overlap
overlap_beta_par3_priorlognormlow <- beta.overlap
qt_inv1est_par3_priorlognormlow <- qt_inv1est
qt_inv2est_par3_priorlognormlow <- qt_inv2est
competoutcome_par3_priorlognormlow <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data4ddpriorlognormlow.Rdata")
inv1_par4_priorlognormlow <- summary_inv1
inv2_par4_priorlognormlow <- summary_inv2
alpha_par4_priorlognormlow <- summary_alpha
beta_par4_priorlognormlow <- summary_beta
overlap_alpha_par4_priorlognormlow <- alpha.overlap
overlap_beta_par4_priorlognormlow <- beta.overlap
qt_inv1est_par4_priorlognormlow <- qt_inv1est
qt_inv2est_par4_priorlognormlow <- qt_inv2est
competoutcome_par4_priorlognormlow <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data1ddpriorlognormhigh.Rdata")
inv1_par1_priorlognormhigh <- summary_inv1
inv2_par1_priorlognormhigh <- summary_inv2
alpha_par1_priorlognormhigh <- summary_alpha
beta_par1_priorlognormhigh <- summary_beta
overlap_alpha_par1_priorlognormhigh <- alpha.overlap
overlap_beta_par1_priorlognormhigh <- beta.overlap
qt_inv1est_par1_priorlognormhigh <- qt_inv1est
qt_inv2est_par1_priorlognormhigh <- qt_inv2est
competoutcome_par1_priorlognormhigh <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data2ddpriorlognormhigh.Rdata")
inv1_par2_priorlognormhigh <- summary_inv1
inv2_par2_priorlognormhigh <- summary_inv2
alpha_par2_priorlognormhigh <- summary_alpha
beta_par2_priorlognormhigh <- summary_beta
overlap_alpha_par2_priorlognormhigh <- alpha.overlap
overlap_beta_par2_priorlognormhigh <- beta.overlap
qt_inv1est_par2_priorlognormhigh <- qt_inv1est
qt_inv2est_par2_priorlognormhigh <- qt_inv2est
competoutcome_par2_priorlognormhigh <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data3ddpriorlognormhigh.Rdata")
inv1_par3_priorlognormhigh <- summary_inv1
inv2_par3_priorlognormhigh <- summary_inv2
alpha_par3_priorlognormhigh <- summary_alpha
beta_par3_priorlognormhigh <- summary_beta
overlap_alpha_par3_priorlognormhigh <- alpha.overlap
overlap_beta_par3_priorlognormhigh <- beta.overlap
qt_inv1est_par3_priorlognormhigh <- qt_inv1est
qt_inv2est_par3_priorlognormhigh <- qt_inv2est
competoutcome_par3_priorlognormhigh <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
load("data/summary_data4ddpriorlognormhigh.Rdata")
inv1_par4_priorlognormhigh <- summary_inv1
inv2_par4_priorlognormhigh <- summary_inv2
alpha_par4_priorlognormhigh <- summary_alpha
beta_par4_priorlognormhigh <- summary_beta
overlap_alpha_par4_priorlognormhigh <- alpha.overlap
overlap_beta_par4_priorlognormhigh <- beta.overlap
qt_inv1est_par4_priorlognormhigh <- qt_inv1est
qt_inv2est_par4_priorlognormhigh <- qt_inv2est
competoutcome_par4_priorlognormhigh <-competoutcome
remove(summary_inv1,summary_inv2,
       summary_alpha,summary_beta,
       alpha.overlap,beta.overlap,
       qt_inv1est,
       qt_inv2est,
       competoutcome)
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
mtext("Mean estimated invasion criteria", side = 3, line = 1, outer = TRUE,cex=1.2)
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
legend('topright',col=c("black","black","black",'red'),legend=c("prior 1","prior 2","prior 3","true value"),pch=c(21,22,23,19))
dev.off()

####estimates alpha interactions (fecundity)
pdf(file=paste("plots/compare_alpha_estimates.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(alpha_par1_priorexp[1,1,4],alpha_par1_priorlognormlow[1,1,4],alpha_par1_priorlognormhigh[1,1,4]),c(1,1.5,2),c(alpha_par1_priorexp[1,1,3],alpha_par1_priorlognormlow[1,1,3],alpha_par1_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par1_priorexp[1,1,2],alpha_par1_priorlognormlow[1,1,2],alpha_par1_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(alpha_par1_priorexp[1,1,1],alpha_par1_priorlognormlow[1,1,1],alpha_par1_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(alpha_par1_priorexp[2,2,4],alpha_par1_priorlognormlow[2,2,4],alpha_par1_priorlognormhigh[2,2,4]),c(3,3.5,4),c(alpha_par1_priorexp[2,2,3],alpha_par1_priorlognormlow[2,2,3],alpha_par1_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par1_priorexp[2,2,2],alpha_par1_priorlognormlow[2,2,2],alpha_par1_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(alpha_par1_priorexp[2,2,1],alpha_par1_priorlognormlow[2,2,1],alpha_par1_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(alpha_par1_priorexp[1,2,4],alpha_par1_priorlognormlow[1,2,4],alpha_par1_priorlognormhigh[1,2,4]),c(5,5.5,6),c(alpha_par1_priorexp[1,2,3],alpha_par1_priorlognormlow[1,2,3],alpha_par1_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par1_priorexp[1,2,2],alpha_par1_priorlognormlow[1,2,2],alpha_par1_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(alpha_par1_priorexp[1,2,1],alpha_par1_priorlognormlow[1,2,1],alpha_par1_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(alpha_par1_priorexp[2,1,4],alpha_par1_priorlognormlow[2,1,4],alpha_par1_priorlognormhigh[2,1,4]),c(7,7.5,8),c(alpha_par1_priorexp[2,1,3],alpha_par1_priorlognormlow[2,1,3],alpha_par1_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par1_priorexp[2,1,2],alpha_par1_priorlognormlow[2,1,2],alpha_par1_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(alpha_par1_priorexp[2,1,1],alpha_par1_priorlognormlow[2,1,1],alpha_par1_priorlognormhigh[2,1,1]),col="red",cex=1)
abline(h=0)
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("Mean estimated intra and inter species interactions on fecundity (alpha)", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(alpha_par2_priorexp[1,1,4],alpha_par2_priorlognormlow[1,1,4],alpha_par2_priorlognormhigh[1,1,4]),c(1,1.5,2),c(alpha_par2_priorexp[1,1,3],alpha_par2_priorlognormlow[1,1,3],alpha_par2_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par2_priorexp[1,1,2],alpha_par2_priorlognormlow[1,1,2],alpha_par2_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(alpha_par2_priorexp[1,1,1],alpha_par2_priorlognormlow[1,1,1],alpha_par2_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(alpha_par2_priorexp[2,2,4],alpha_par2_priorlognormlow[2,2,4],alpha_par2_priorlognormhigh[2,2,4]),c(3,3.5,4),c(alpha_par2_priorexp[2,2,3],alpha_par2_priorlognormlow[2,2,3],alpha_par2_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par2_priorexp[2,2,2],alpha_par2_priorlognormlow[2,2,2],alpha_par2_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(alpha_par2_priorexp[2,2,1],alpha_par2_priorlognormlow[2,2,1],alpha_par2_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(alpha_par2_priorexp[1,2,4],alpha_par2_priorlognormlow[1,2,4],alpha_par2_priorlognormhigh[1,2,4]),c(5,5.5,6),c(alpha_par2_priorexp[1,2,3],alpha_par2_priorlognormlow[1,2,3],alpha_par2_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par2_priorexp[1,2,2],alpha_par2_priorlognormlow[1,2,2],alpha_par2_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(alpha_par2_priorexp[1,2,1],alpha_par2_priorlognormlow[1,2,1],alpha_par2_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(alpha_par2_priorexp[2,1,4],alpha_par2_priorlognormlow[2,1,4],alpha_par2_priorlognormhigh[2,1,4]),c(7,7.5,8),c(alpha_par2_priorexp[2,1,3],alpha_par2_priorlognormlow[2,1,3],alpha_par2_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par2_priorexp[2,1,2],alpha_par2_priorlognormlow[2,1,2],alpha_par2_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(alpha_par2_priorexp[2,1,1],alpha_par2_priorlognormlow[2,1,1],alpha_par2_priorlognormhigh[2,1,1]),col="red",cex=1)

abline(h=0)
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(alpha_par3_priorexp[1,1,4],alpha_par3_priorlognormlow[1,1,4],alpha_par3_priorlognormhigh[1,1,4]),c(1,1.5,2),c(alpha_par3_priorexp[1,1,3],alpha_par3_priorlognormlow[1,1,3],alpha_par3_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par3_priorexp[1,1,2],alpha_par3_priorlognormlow[1,1,2],alpha_par3_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(alpha_par3_priorexp[1,1,1],alpha_par3_priorlognormlow[1,1,1],alpha_par3_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(alpha_par3_priorexp[2,2,4],alpha_par3_priorlognormlow[2,2,4],alpha_par3_priorlognormhigh[2,2,4]),c(3,3.5,4),c(alpha_par3_priorexp[2,2,3],alpha_par3_priorlognormlow[2,2,3],alpha_par3_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par3_priorexp[2,2,2],alpha_par3_priorlognormlow[2,2,2],alpha_par3_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(alpha_par3_priorexp[2,2,1],alpha_par3_priorlognormlow[2,2,1],alpha_par3_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(alpha_par3_priorexp[1,2,4],alpha_par3_priorlognormlow[1,2,4],alpha_par3_priorlognormhigh[1,2,4]),c(5,5.5,6),c(alpha_par3_priorexp[1,2,3],alpha_par3_priorlognormlow[1,2,3],alpha_par3_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par3_priorexp[1,2,2],alpha_par3_priorlognormlow[1,2,2],alpha_par3_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(alpha_par3_priorexp[1,2,1],alpha_par3_priorlognormlow[1,2,1],alpha_par3_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(alpha_par3_priorexp[2,1,4],alpha_par3_priorlognormlow[2,1,4],alpha_par3_priorlognormhigh[2,1,4]),c(7,7.5,8),c(alpha_par3_priorexp[2,1,3],alpha_par3_priorlognormlow[2,1,3],alpha_par3_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par3_priorexp[2,1,2],alpha_par3_priorlognormlow[2,1,2],alpha_par3_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(alpha_par3_priorexp[2,1,1],alpha_par3_priorlognormlow[2,1,1],alpha_par3_priorlognormhigh[2,1,1]),col="red",cex=1)

abline(h=0)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),side=1,line=1,adj=NA,cex=1,padj=NA)

mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(alpha_par4_priorexp[1,1,4],alpha_par4_priorlognormlow[1,1,4],alpha_par4_priorlognormhigh[1,1,4]),c(1,1.5,2),c(alpha_par4_priorexp[1,1,3],alpha_par4_priorlognormlow[1,1,3],alpha_par4_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par4_priorexp[1,1,2],alpha_par4_priorlognormlow[1,1,2],alpha_par4_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(alpha_par4_priorexp[1,1,1],alpha_par4_priorlognormlow[1,1,1],alpha_par4_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(alpha_par4_priorexp[2,2,4],alpha_par4_priorlognormlow[2,2,4],alpha_par4_priorlognormhigh[2,2,4]),c(3,3.5,4),c(alpha_par4_priorexp[2,2,3],alpha_par4_priorlognormlow[2,2,3],alpha_par4_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par4_priorexp[2,2,2],alpha_par4_priorlognormlow[2,2,2],alpha_par4_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(alpha_par4_priorexp[2,2,1],alpha_par4_priorlognormlow[2,2,1],alpha_par4_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(alpha_par4_priorexp[1,2,4],alpha_par4_priorlognormlow[1,2,4],alpha_par4_priorlognormhigh[1,2,4]),c(5,5.5,6),c(alpha_par4_priorexp[1,2,3],alpha_par4_priorlognormlow[1,2,3],alpha_par4_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par4_priorexp[1,2,2],alpha_par4_priorlognormlow[1,2,2],alpha_par4_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(alpha_par4_priorexp[1,2,1],alpha_par4_priorlognormlow[1,2,1],alpha_par4_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(alpha_par4_priorexp[2,1,4],alpha_par4_priorlognormlow[2,1,4],alpha_par4_priorlognormhigh[2,1,4]),c(7,7.5,8),c(alpha_par4_priorexp[2,1,3],alpha_par4_priorlognormlow[2,1,3],alpha_par4_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par4_priorexp[2,1,2],alpha_par4_priorlognormlow[2,1,2],alpha_par4_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(alpha_par4_priorexp[2,1,1],alpha_par4_priorlognormlow[2,1,1],alpha_par4_priorlognormhigh[2,1,1]),col="red",cex=1)

abline(h=0)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),line=1)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
legend('topleft',col=c("black","black","black",'red'),legend=c("prior 1","prior 2","prior 3","true value"),pch=c(21,22,23,19))
dev.off()
####estimates beta interactions (survival)
pdf(file=paste("plots/compare_beta_estimates.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(beta_par1_priorexp[1,1,4],beta_par1_priorlognormlow[1,1,4],beta_par1_priorlognormhigh[1,1,4]),c(1,1.5,2),c(beta_par1_priorexp[1,1,3],beta_par1_priorlognormlow[1,1,3],beta_par1_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par1_priorexp[1,1,2],beta_par1_priorlognormlow[1,1,2],beta_par1_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(beta_par1_priorexp[1,1,1],beta_par1_priorlognormlow[1,1,1],beta_par1_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(beta_par1_priorexp[2,2,4],beta_par1_priorlognormlow[2,2,4],beta_par1_priorlognormhigh[2,2,4]),c(3,3.5,4),c(beta_par1_priorexp[2,2,3],beta_par1_priorlognormlow[2,2,3],beta_par1_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par1_priorexp[2,2,2],beta_par1_priorlognormlow[2,2,2],beta_par1_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(beta_par1_priorexp[2,2,1],beta_par1_priorlognormlow[2,2,1],beta_par1_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(beta_par1_priorexp[1,2,4],beta_par1_priorlognormlow[1,2,4],beta_par1_priorlognormhigh[1,2,4]),c(5,5.5,6),c(beta_par1_priorexp[1,2,3],beta_par1_priorlognormlow[1,2,3],beta_par1_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par1_priorexp[1,2,2],beta_par1_priorlognormlow[1,2,2],beta_par1_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(beta_par1_priorexp[1,2,1],beta_par1_priorlognormlow[1,2,1],beta_par1_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(beta_par1_priorexp[2,1,4],beta_par1_priorlognormlow[2,1,4],beta_par1_priorlognormhigh[2,1,4]),c(7,7.5,8),c(beta_par1_priorexp[2,1,3],beta_par1_priorlognormlow[2,1,3],beta_par1_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par1_priorexp[2,1,2],beta_par1_priorlognormlow[2,1,2],beta_par1_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(beta_par1_priorexp[2,1,1],beta_par1_priorlognormlow[2,1,1],beta_par1_priorlognormhigh[2,1,1]),col="red",cex=1)
abline(h=0)
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("Mean estimated intra and inter species interactions on survival (beta)", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(beta_par2_priorexp[1,1,4],beta_par2_priorlognormlow[1,1,4],beta_par2_priorlognormhigh[1,1,4]),c(1,1.5,2),c(beta_par2_priorexp[1,1,3],beta_par2_priorlognormlow[1,1,3],beta_par2_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par2_priorexp[1,1,2],beta_par2_priorlognormlow[1,1,2],beta_par2_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(beta_par2_priorexp[1,1,1],beta_par2_priorlognormlow[1,1,1],beta_par2_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(beta_par2_priorexp[2,2,4],beta_par2_priorlognormlow[2,2,4],beta_par2_priorlognormhigh[2,2,4]),c(3,3.5,4),c(beta_par2_priorexp[2,2,3],beta_par2_priorlognormlow[2,2,3],beta_par2_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par2_priorexp[2,2,2],beta_par2_priorlognormlow[2,2,2],beta_par2_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(beta_par2_priorexp[2,2,1],beta_par2_priorlognormlow[2,2,1],beta_par2_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(beta_par2_priorexp[1,2,4],beta_par2_priorlognormlow[1,2,4],beta_par2_priorlognormhigh[1,2,4]),c(5,5.5,6),c(beta_par2_priorexp[1,2,3],beta_par2_priorlognormlow[1,2,3],beta_par2_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par2_priorexp[1,2,2],beta_par2_priorlognormlow[1,2,2],beta_par2_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(beta_par2_priorexp[1,2,1],beta_par2_priorlognormlow[1,2,1],beta_par2_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(beta_par2_priorexp[2,1,4],beta_par2_priorlognormlow[2,1,4],beta_par2_priorlognormhigh[2,1,4]),c(7,7.5,8),c(beta_par2_priorexp[2,1,3],beta_par2_priorlognormlow[2,1,3],beta_par2_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par2_priorexp[2,1,2],beta_par2_priorlognormlow[2,1,2],beta_par2_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(beta_par2_priorexp[2,1,1],beta_par2_priorlognormlow[2,1,1],beta_par2_priorlognormhigh[2,1,1]),col="red",cex=1)

abline(h=0)
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(beta_par3_priorexp[1,1,4],beta_par3_priorlognormlow[1,1,4],beta_par3_priorlognormhigh[1,1,4]),c(1,1.5,2),c(beta_par3_priorexp[1,1,3],beta_par3_priorlognormlow[1,1,3],beta_par3_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par3_priorexp[1,1,2],beta_par3_priorlognormlow[1,1,2],beta_par3_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(beta_par3_priorexp[1,1,1],beta_par3_priorlognormlow[1,1,1],beta_par3_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(beta_par3_priorexp[2,2,4],beta_par3_priorlognormlow[2,2,4],beta_par3_priorlognormhigh[2,2,4]),c(3,3.5,4),c(beta_par3_priorexp[2,2,3],beta_par3_priorlognormlow[2,2,3],beta_par3_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par3_priorexp[2,2,2],beta_par3_priorlognormlow[2,2,2],beta_par3_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(beta_par3_priorexp[2,2,1],beta_par3_priorlognormlow[2,2,1],beta_par3_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(beta_par3_priorexp[1,2,4],beta_par3_priorlognormlow[1,2,4],beta_par3_priorlognormhigh[1,2,4]),c(5,5.5,6),c(beta_par3_priorexp[1,2,3],beta_par3_priorlognormlow[1,2,3],beta_par3_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par3_priorexp[1,2,2],beta_par3_priorlognormlow[1,2,2],beta_par3_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(beta_par3_priorexp[1,2,1],beta_par3_priorlognormlow[1,2,1],beta_par3_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(beta_par3_priorexp[2,1,4],beta_par3_priorlognormlow[2,1,4],beta_par3_priorlognormhigh[2,1,4]),c(7,7.5,8),c(beta_par3_priorexp[2,1,3],beta_par3_priorlognormlow[2,1,3],beta_par3_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par3_priorexp[2,1,2],beta_par3_priorlognormlow[2,1,2],beta_par3_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(beta_par3_priorexp[2,1,1],beta_par3_priorlognormlow[2,1,1],beta_par3_priorlognormhigh[2,1,1]),col="red",cex=1)

abline(h=0)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),side=1,line=1,adj=NA,cex=1,padj=NA)
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1.2),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(beta_par4_priorexp[1,1,4],beta_par4_priorlognormlow[1,1,4],beta_par4_priorlognormhigh[1,1,4]),c(1,1.5,2),c(beta_par4_priorexp[1,1,3],beta_par4_priorlognormlow[1,1,3],beta_par4_priorlognormhigh[1,1,3]))
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par4_priorexp[1,1,2],beta_par4_priorlognormlow[1,1,2],beta_par4_priorlognormhigh[1,1,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(1,1.5,2),c(beta_par4_priorexp[1,1,1],beta_par4_priorlognormlow[1,1,1],beta_par4_priorlognormhigh[1,1,1]),col="red",cex=1)

segments(lwd=5,c(3,3.5,4),c(beta_par4_priorexp[2,2,4],beta_par4_priorlognormlow[2,2,4],beta_par4_priorlognormhigh[2,2,4]),c(3,3.5,4),c(beta_par4_priorexp[2,2,3],beta_par4_priorlognormlow[2,2,3],beta_par4_priorlognormhigh[2,2,3]))
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par4_priorexp[2,2,2],beta_par4_priorlognormlow[2,2,2],beta_par4_priorlognormhigh[2,2,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(3,3.5,4),c(beta_par4_priorexp[2,2,1],beta_par4_priorlognormlow[2,2,1],beta_par4_priorlognormhigh[2,2,1]),col="red",cex=1)

segments(lwd=5,c(5,5.5,6),c(beta_par4_priorexp[1,2,4],beta_par4_priorlognormlow[1,2,4],beta_par4_priorlognormhigh[1,2,4]),c(5,5.5,6),c(beta_par4_priorexp[1,2,3],beta_par4_priorlognormlow[1,2,3],beta_par4_priorlognormhigh[1,2,3]))
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par4_priorexp[1,2,2],beta_par4_priorlognormlow[1,2,2],beta_par4_priorlognormhigh[1,2,2]),col="black",bg= "#7fa626",cex=2)
points(pch=19,x=c(5,5.5,6),c(beta_par4_priorexp[1,2,1],beta_par4_priorlognormlow[1,2,1],beta_par4_priorlognormhigh[1,2,1]),col="red",cex=1)

segments(lwd=5,c(7,7.5,8),c(beta_par4_priorexp[2,1,4],beta_par4_priorlognormlow[2,1,4],beta_par4_priorlognormhigh[2,1,4]),c(7,7.5,8),c(beta_par4_priorexp[2,1,3],beta_par4_priorlognormlow[2,1,3],beta_par4_priorlognormhigh[2,1,3]))
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par4_priorexp[2,1,2],beta_par4_priorlognormlow[2,1,2],beta_par4_priorlognormhigh[2,1,2]),col="black",bg="#2997af",cex=2)
points(pch=19,x=c(7,7.5,8),c(beta_par4_priorexp[2,1,1],beta_par4_priorlognormlow[2,1,1],beta_par4_priorlognormhigh[2,1,1]),col="red",cex=1)

abline(h=0)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),line=1)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
legend('topleft',col=c("black","black","black",'red'),legend=c("prior 1","prior 2","prior 3","true value"),pch=c(21,22,23,19))
dev.off()
##prior posterior overlap summaries
#### pp overlap alpha interactions (fecundity)
pdf(file=paste("plots/ppoverlap_alpha.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_alpha_par1_priorexp[1,1,],0.975),quantile(overlap_alpha_par1_priorlognormlow[1,1,],0.975),quantile(overlap_alpha_par1_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_alpha_par1_priorexp[1,1,],0.025),quantile(overlap_alpha_par1_priorlognormlow[1,1,],0.025),quantile(overlap_alpha_par1_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_alpha_par1_priorexp[1,1,]),mean(overlap_alpha_par1_priorlognormlow[1,1,]),mean(overlap_alpha_par1_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_alpha_par1_priorexp[2,2,],0.975),quantile(overlap_alpha_par1_priorlognormlow[2,2,],0.975),quantile(overlap_alpha_par1_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_alpha_par1_priorexp[2,2,],0.025),quantile(overlap_alpha_par1_priorlognormlow[2,2,],0.025),quantile(overlap_alpha_par1_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_alpha_par1_priorexp[2,2,]),mean(overlap_alpha_par1_priorlognormlow[2,2,]),mean(overlap_alpha_par1_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_alpha_par1_priorexp[1,2,],0.975),quantile(overlap_alpha_par1_priorlognormlow[1,2,],0.975),quantile(overlap_alpha_par1_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_alpha_par1_priorexp[1,2,],0.025),quantile(overlap_alpha_par1_priorlognormlow[1,2,],0.025),quantile(overlap_alpha_par1_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_alpha_par1_priorexp[1,2,]),mean(overlap_alpha_par1_priorlognormlow[1,2,]),mean(overlap_alpha_par1_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_alpha_par1_priorexp[2,1,],0.975),quantile(overlap_alpha_par1_priorlognormlow[2,1,],0.975),quantile(overlap_alpha_par1_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_alpha_par1_priorexp[2,1,],0.025),quantile(overlap_alpha_par1_priorlognormlow[2,1,],0.025),quantile(overlap_alpha_par1_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_alpha_par1_priorexp[2,1,]),mean(overlap_alpha_par1_priorlognormlow[2,1,]),mean(overlap_alpha_par1_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)

abline(h=0.35,lty=2,col="red")
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("Prior posterior overlap for intra and inter species interactions on fecundity (alpha)", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_alpha_par2_priorexp[1,1,],0.975),quantile(overlap_alpha_par2_priorlognormlow[1,1,],0.975),quantile(overlap_alpha_par2_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_alpha_par2_priorexp[1,1,],0.025),quantile(overlap_alpha_par2_priorlognormlow[1,1,],0.025),quantile(overlap_alpha_par2_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_alpha_par2_priorexp[1,1,]),mean(overlap_alpha_par2_priorlognormlow[1,1,]),mean(overlap_alpha_par2_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_alpha_par2_priorexp[2,2,],0.975),quantile(overlap_alpha_par2_priorlognormlow[2,2,],0.975),quantile(overlap_alpha_par2_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_alpha_par2_priorexp[2,2,],0.025),quantile(overlap_alpha_par2_priorlognormlow[2,2,],0.025),quantile(overlap_alpha_par2_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_alpha_par2_priorexp[2,2,]),mean(overlap_alpha_par2_priorlognormlow[2,2,]),mean(overlap_alpha_par2_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_alpha_par2_priorexp[1,2,],0.975),quantile(overlap_alpha_par2_priorlognormlow[1,2,],0.975),quantile(overlap_alpha_par2_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_alpha_par2_priorexp[1,2,],0.025),quantile(overlap_alpha_par2_priorlognormlow[1,2,],0.025),quantile(overlap_alpha_par2_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_alpha_par2_priorexp[1,2,]),mean(overlap_alpha_par2_priorlognormlow[1,2,]),mean(overlap_alpha_par2_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_alpha_par2_priorexp[2,1,],0.975),quantile(overlap_alpha_par2_priorlognormlow[2,1,],0.975),quantile(overlap_alpha_par2_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_alpha_par2_priorexp[2,1,],0.025),quantile(overlap_alpha_par2_priorlognormlow[2,1,],0.025),quantile(overlap_alpha_par2_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_alpha_par2_priorexp[2,1,]),mean(overlap_alpha_par2_priorlognormlow[2,1,]),mean(overlap_alpha_par2_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)

abline(h=0.35,lty=2,col="red")
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_alpha_par3_priorexp[1,1,],0.975),quantile(overlap_alpha_par3_priorlognormlow[1,1,],0.975),quantile(overlap_alpha_par3_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_alpha_par3_priorexp[1,1,],0.025),quantile(overlap_alpha_par3_priorlognormlow[1,1,],0.025),quantile(overlap_alpha_par3_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_alpha_par3_priorexp[1,1,]),mean(overlap_alpha_par3_priorlognormlow[1,1,]),mean(overlap_alpha_par3_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_alpha_par3_priorexp[2,2,],0.975),quantile(overlap_alpha_par3_priorlognormlow[2,2,],0.975),quantile(overlap_alpha_par3_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_alpha_par3_priorexp[2,2,],0.025),quantile(overlap_alpha_par3_priorlognormlow[2,2,],0.025),quantile(overlap_alpha_par3_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_alpha_par3_priorexp[2,2,]),mean(overlap_alpha_par3_priorlognormlow[2,2,]),mean(overlap_alpha_par3_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_alpha_par3_priorexp[1,2,],0.975),quantile(overlap_alpha_par3_priorlognormlow[1,2,],0.975),quantile(overlap_alpha_par3_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_alpha_par3_priorexp[1,2,],0.025),quantile(overlap_alpha_par3_priorlognormlow[1,2,],0.025),quantile(overlap_alpha_par3_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_alpha_par3_priorexp[1,2,]),mean(overlap_alpha_par3_priorlognormlow[1,2,]),mean(overlap_alpha_par3_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_alpha_par3_priorexp[2,1,],0.975),quantile(overlap_alpha_par3_priorlognormlow[2,1,],0.975),quantile(overlap_alpha_par3_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_alpha_par3_priorexp[2,1,],0.025),quantile(overlap_alpha_par3_priorlognormlow[2,1,],0.025),quantile(overlap_alpha_par3_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_alpha_par3_priorexp[2,1,]),mean(overlap_alpha_par3_priorlognormlow[2,1,]),mean(overlap_alpha_par3_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),side=1,line=1,adj=NA,cex=1,padj=NA)
abline(h=0.35,lty=2,col="red")
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_alpha_par4_priorexp[1,1,],0.975),quantile(overlap_alpha_par4_priorlognormlow[1,1,],0.975),quantile(overlap_alpha_par4_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_alpha_par4_priorexp[1,1,],0.025),quantile(overlap_alpha_par4_priorlognormlow[1,1,],0.025),quantile(overlap_alpha_par4_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_alpha_par4_priorexp[1,1,]),mean(overlap_alpha_par4_priorlognormlow[1,1,]),mean(overlap_alpha_par4_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_alpha_par4_priorexp[2,2,],0.975),quantile(overlap_alpha_par4_priorlognormlow[2,2,],0.975),quantile(overlap_alpha_par4_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_alpha_par4_priorexp[2,2,],0.025),quantile(overlap_alpha_par4_priorlognormlow[2,2,],0.025),quantile(overlap_alpha_par4_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_alpha_par4_priorexp[2,2,]),mean(overlap_alpha_par4_priorlognormlow[2,2,]),mean(overlap_alpha_par4_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_alpha_par4_priorexp[1,2,],0.975),quantile(overlap_alpha_par4_priorlognormlow[1,2,],0.975),quantile(overlap_alpha_par4_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_alpha_par4_priorexp[1,2,],0.025),quantile(overlap_alpha_par4_priorlognormlow[1,2,],0.025),quantile(overlap_alpha_par4_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_alpha_par4_priorexp[1,2,]),mean(overlap_alpha_par4_priorlognormlow[1,2,]),mean(overlap_alpha_par4_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_alpha_par4_priorexp[2,1,],0.975),quantile(overlap_alpha_par4_priorlognormlow[2,1,],0.975),quantile(overlap_alpha_par4_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_alpha_par4_priorexp[2,1,],0.025),quantile(overlap_alpha_par4_priorlognormlow[2,1,],0.025),quantile(overlap_alpha_par4_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_alpha_par4_priorexp[2,1,]),mean(overlap_alpha_par4_priorlognormlow[2,1,]),mean(overlap_alpha_par4_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),line=1)
abline(h=0.35,lty=2,col="red")
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
dev.off()
#### pp overlap beta interactions (survival)
pdf(file=paste("plots/ppoverlap_beta.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_beta_par1_priorexp[1,1,],0.975),quantile(overlap_beta_par1_priorlognormlow[1,1,],0.975),quantile(overlap_beta_par1_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_beta_par1_priorexp[1,1,],0.025),quantile(overlap_beta_par1_priorlognormlow[1,1,],0.025),quantile(overlap_beta_par1_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_beta_par1_priorexp[1,1,]),mean(overlap_beta_par1_priorlognormlow[1,1,]),mean(overlap_beta_par1_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_beta_par1_priorexp[2,2,],0.975),quantile(overlap_beta_par1_priorlognormlow[2,2,],0.975),quantile(overlap_beta_par1_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_beta_par1_priorexp[2,2,],0.025),quantile(overlap_beta_par1_priorlognormlow[2,2,],0.025),quantile(overlap_beta_par1_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_beta_par1_priorexp[2,2,]),mean(overlap_beta_par1_priorlognormlow[2,2,]),mean(overlap_beta_par1_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_beta_par1_priorexp[1,2,],0.975),quantile(overlap_beta_par1_priorlognormlow[1,2,],0.975),quantile(overlap_beta_par1_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_beta_par1_priorexp[1,2,],0.025),quantile(overlap_beta_par1_priorlognormlow[1,2,],0.025),quantile(overlap_beta_par1_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_beta_par1_priorexp[1,2,]),mean(overlap_beta_par1_priorlognormlow[1,2,]),mean(overlap_beta_par1_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_beta_par1_priorexp[2,1,],0.975),quantile(overlap_beta_par1_priorlognormlow[2,1,],0.975),quantile(overlap_beta_par1_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_beta_par1_priorexp[2,1,],0.025),quantile(overlap_beta_par1_priorlognormlow[2,1,],0.025),quantile(overlap_beta_par1_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_beta_par1_priorexp[2,1,]),mean(overlap_beta_par1_priorlognormlow[2,1,]),mean(overlap_beta_par1_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)
abline(h=0.35,lty=2,col="red")
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("Prior posterior overlap for intra and inter species interactions on survival (beta)", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_beta_par2_priorexp[1,1,],0.975),quantile(overlap_beta_par2_priorlognormlow[1,1,],0.975),quantile(overlap_beta_par2_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_beta_par2_priorexp[1,1,],0.025),quantile(overlap_beta_par2_priorlognormlow[1,1,],0.025),quantile(overlap_beta_par2_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_beta_par2_priorexp[1,1,]),mean(overlap_beta_par2_priorlognormlow[1,1,]),mean(overlap_beta_par2_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_beta_par2_priorexp[2,2,],0.975),quantile(overlap_beta_par2_priorlognormlow[2,2,],0.975),quantile(overlap_beta_par2_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_beta_par2_priorexp[2,2,],0.025),quantile(overlap_beta_par2_priorlognormlow[2,2,],0.025),quantile(overlap_beta_par2_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_beta_par2_priorexp[2,2,]),mean(overlap_beta_par2_priorlognormlow[2,2,]),mean(overlap_beta_par2_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_beta_par2_priorexp[1,2,],0.975),quantile(overlap_beta_par2_priorlognormlow[1,2,],0.975),quantile(overlap_beta_par2_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_beta_par2_priorexp[1,2,],0.025),quantile(overlap_beta_par2_priorlognormlow[1,2,],0.025),quantile(overlap_beta_par2_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_beta_par2_priorexp[1,2,]),mean(overlap_beta_par2_priorlognormlow[1,2,]),mean(overlap_beta_par2_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_beta_par2_priorexp[2,1,],0.975),quantile(overlap_beta_par2_priorlognormlow[2,1,],0.975),quantile(overlap_beta_par2_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_beta_par2_priorexp[2,1,],0.025),quantile(overlap_beta_par2_priorlognormlow[2,1,],0.025),quantile(overlap_beta_par2_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_beta_par2_priorexp[2,1,]),mean(overlap_beta_par2_priorlognormlow[2,1,]),mean(overlap_beta_par2_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)

abline(h=0.35,lty=2,col="red")
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_beta_par3_priorexp[1,1,],0.975),quantile(overlap_beta_par3_priorlognormlow[1,1,],0.975),quantile(overlap_beta_par3_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_beta_par3_priorexp[1,1,],0.025),quantile(overlap_beta_par3_priorlognormlow[1,1,],0.025),quantile(overlap_beta_par3_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_beta_par3_priorexp[1,1,]),mean(overlap_beta_par3_priorlognormlow[1,1,]),mean(overlap_beta_par3_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_beta_par3_priorexp[2,2,],0.975),quantile(overlap_beta_par3_priorlognormlow[2,2,],0.975),quantile(overlap_beta_par3_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_beta_par3_priorexp[2,2,],0.025),quantile(overlap_beta_par3_priorlognormlow[2,2,],0.025),quantile(overlap_beta_par3_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_beta_par3_priorexp[2,2,]),mean(overlap_beta_par3_priorlognormlow[2,2,]),mean(overlap_beta_par3_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_beta_par3_priorexp[1,2,],0.975),quantile(overlap_beta_par3_priorlognormlow[1,2,],0.975),quantile(overlap_beta_par3_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_beta_par3_priorexp[1,2,],0.025),quantile(overlap_beta_par3_priorlognormlow[1,2,],0.025),quantile(overlap_beta_par3_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_beta_par3_priorexp[1,2,]),mean(overlap_beta_par3_priorlognormlow[1,2,]),mean(overlap_beta_par3_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_beta_par3_priorexp[2,1,],0.975),quantile(overlap_beta_par3_priorlognormlow[2,1,],0.975),quantile(overlap_beta_par3_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_beta_par3_priorexp[2,1,],0.025),quantile(overlap_beta_par3_priorlognormlow[2,1,],0.025),quantile(overlap_beta_par3_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_beta_par3_priorexp[2,1,]),mean(overlap_beta_par3_priorlognormlow[2,1,]),mean(overlap_beta_par3_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),side=1,line=1,adj=NA,cex=1,padj=NA)
abline(h=0.35,lty=2,col="red")
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1,1.5,2),c(quantile(overlap_beta_par4_priorexp[1,1,],0.975),quantile(overlap_beta_par4_priorlognormlow[1,1,],0.975),quantile(overlap_beta_par4_priorlognormhigh[1,1,],0.975)),c(1,1.5,2),c(quantile(overlap_beta_par4_priorexp[1,1,],0.025),quantile(overlap_beta_par4_priorlognormlow[1,1,],0.025),quantile(overlap_beta_par4_priorlognormhigh[1,1,],0.025)))
points(pch=c(21,22,23),x=c(1,1.5,2),c(mean(overlap_beta_par4_priorexp[1,1,]),mean(overlap_beta_par4_priorlognormlow[1,1,]),mean(overlap_beta_par4_priorlognormhigh[1,1,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(3,3.5,4),c(quantile(overlap_beta_par4_priorexp[2,2,],0.975),quantile(overlap_beta_par4_priorlognormlow[2,2,],0.975),quantile(overlap_beta_par4_priorlognormhigh[2,2,],0.975)),c(3,3.5,4),c(quantile(overlap_beta_par4_priorexp[2,2,],0.025),quantile(overlap_beta_par4_priorlognormlow[2,2,],0.025),quantile(overlap_beta_par4_priorlognormhigh[2,2,],0.025)))
points(pch=c(21,22,23),x=c(3,3.5,4),c(mean(overlap_beta_par4_priorexp[2,2,]),mean(overlap_beta_par4_priorlognormlow[2,2,]),mean(overlap_beta_par4_priorlognormhigh[2,2,])),col="black",bg= "#2997af",cex=2)

segments(lwd=5,c(5,5.5,6),c(quantile(overlap_beta_par4_priorexp[1,2,],0.975),quantile(overlap_beta_par4_priorlognormlow[1,2,],0.975),quantile(overlap_beta_par4_priorlognormhigh[1,2,],0.975)),c(5,5.5,6),c(quantile(overlap_beta_par4_priorexp[1,2,],0.025),quantile(overlap_beta_par4_priorlognormlow[1,2,],0.025),quantile(overlap_beta_par4_priorlognormhigh[1,2,],0.025)))
points(pch=c(21,22,23),x=c(5,5.5,6),c(mean(overlap_beta_par4_priorexp[1,2,]),mean(overlap_beta_par4_priorlognormlow[1,2,]),mean(overlap_beta_par4_priorlognormhigh[1,2,])),col="black",bg= "#7fa626",cex=2)

segments(lwd=5,c(7,7.5,8),c(quantile(overlap_beta_par4_priorexp[2,1,],0.975),quantile(overlap_beta_par4_priorlognormlow[2,1,],0.975),quantile(overlap_beta_par4_priorlognormhigh[2,1,],0.975)),c(7,7.5,8),c(quantile(overlap_beta_par4_priorexp[2,1,],0.025),quantile(overlap_beta_par4_priorlognormlow[2,1,],0.025),quantile(overlap_beta_par4_priorlognormhigh[2,1,],0.025)))
points(pch=c(21,22,23),x=c(7,7.5,8),c(mean(overlap_beta_par4_priorexp[2,1,]),mean(overlap_beta_par4_priorlognormlow[2,1,]),mean(overlap_beta_par4_priorlognormhigh[2,1,])),col="black",bg= "#2997af",cex=2)
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),line=1)
abline(h=0.35,lty=2,col="red")
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
legend('topleft',col=c("black","black","black"),legend=c("prior 1","prior 2","prior 3"),pch=c(21,22,23))
dev.off()
##coverage
####coverage 95% alpha interactions (fecundity)
pdf(file=paste("plots/compare_alpha_coverage.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))
plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par1_priorexp[1,1,5],alpha_par1_priorlognormlow[1,1,5],alpha_par1_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par1_priorexp[2,2,5],alpha_par1_priorlognormlow[2,2,5],alpha_par1_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par1_priorexp[1,2,5],alpha_par1_priorlognormlow[1,2,5],alpha_par1_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par1_priorexp[2,1,5],alpha_par1_priorlognormlow[2,1,5],alpha_par1_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("95% Coverage intra and inter species interactions on fecundity (alpha)", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par2_priorexp[1,1,5],alpha_par2_priorlognormlow[1,1,5],alpha_par2_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par2_priorexp[2,2,5],alpha_par2_priorlognormlow[2,2,5],alpha_par2_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par2_priorexp[1,2,5],alpha_par2_priorlognormlow[1,2,5],alpha_par2_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par2_priorexp[2,1,5],alpha_par2_priorlognormlow[2,1,5],alpha_par2_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par3_priorexp[1,1,5],alpha_par3_priorlognormlow[1,1,5],alpha_par3_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par3_priorexp[2,2,5],alpha_par3_priorlognormlow[2,2,5],alpha_par3_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)

points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par3_priorexp[1,2,5],alpha_par3_priorlognormlow[1,2,5],alpha_par3_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par3_priorexp[2,1,5],alpha_par3_priorlognormlow[2,1,5],alpha_par3_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),side=1,line=1,adj=NA,cex=1,padj=NA)
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(alpha_par4_priorexp[1,1,5],alpha_par4_priorlognormlow[1,1,5],alpha_par4_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(alpha_par4_priorexp[2,2,5],alpha_par4_priorlognormlow[2,2,5],alpha_par4_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)
points(pch=c(21,22,23),x=c(5,5.5,6),c(alpha_par4_priorexp[1,2,5],alpha_par4_priorlognormlow[1,2,5],alpha_par4_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(alpha_par4_priorexp[2,1,5],alpha_par4_priorlognormlow[2,1,5],alpha_par4_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),line=1)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
dev.off()
####coverage 95% beta interactions (survival)
pdf(file=paste("plots/compare_beta_coverage.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))
plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par1_priorexp[1,1,5],beta_par1_priorlognormlow[1,1,5],beta_par1_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par1_priorexp[2,2,5],beta_par1_priorlognormlow[2,2,5],beta_par1_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par1_priorexp[1,2,5],beta_par1_priorlognormlow[1,2,5],beta_par1_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par1_priorexp[2,1,5],beta_par1_priorlognormlow[2,1,5],beta_par1_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("95% Coverage intra and inter species interactions on survival (beta)", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par2_priorexp[1,1,5],beta_par2_priorlognormlow[1,1,5],beta_par2_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par2_priorexp[2,2,5],beta_par2_priorlognormlow[2,2,5],beta_par2_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par2_priorexp[1,2,5],beta_par2_priorlognormlow[1,2,5],beta_par2_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par2_priorexp[2,1,5],beta_par2_priorlognormlow[2,1,5],beta_par2_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par3_priorexp[1,1,5],beta_par3_priorlognormlow[1,1,5],beta_par3_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par3_priorexp[2,2,5],beta_par3_priorlognormlow[2,2,5],beta_par3_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)

points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par3_priorexp[1,2,5],beta_par3_priorlognormlow[1,2,5],beta_par3_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par3_priorexp[2,1,5],beta_par3_priorlognormlow[2,1,5],beta_par3_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),side=1,line=1,adj=NA,cex=1,padj=NA)
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,8.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(beta_par4_priorexp[1,1,5],beta_par4_priorlognormlow[1,1,5],beta_par4_priorlognormhigh[1,1,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(beta_par4_priorexp[2,2,5],beta_par4_priorlognormlow[2,2,5],beta_par4_priorlognormhigh[2,2,5]),col="black",bg="#2997af",cex=2)
points(pch=c(21,22,23),x=c(5,5.5,6),c(beta_par4_priorexp[1,2,5],beta_par4_priorlognormlow[1,2,5],beta_par4_priorlognormhigh[1,2,5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(7,7.5,8),c(beta_par4_priorexp[2,1,5],beta_par4_priorlognormlow[2,1,5],beta_par4_priorlognormhigh[2,1,5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext(par(las=1),text=c("Sp.1/1","Sp.2/2","Sp.1/2","Sp.2/1"),
      at=c(1.5,3.5,5.5,7.5),line=1)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
dev.off()

#coverage 95% for invasion criteria
pdf(file=paste("plots/compare_invasion_coverage.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(2,3,2,2),omi=c(0.1,0,0.5,0))
plot(x='n',ylim=c(0,1),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(inv1_par1_priorexp[5],inv1_par1_priorlognormlow[5],inv1_par1_priorlognormhigh[5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(inv2_par1_priorexp[5],inv2_par1_priorlognormlow[5],inv2_par1_priorlognormhigh[5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")
mtext("95% Coverage invasion criteria", side = 3, line = 1, outer = TRUE,cex=1.2)

plot(x='n',ylim=c(0,1),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(inv1_par2_priorexp[5],inv1_par2_priorlognormlow[5],inv1_par2_priorlognormhigh[5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(inv2_par2_priorexp[5],inv2_par2_priorlognormlow[5],inv2_par2_priorlognormhigh[5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(inv1_par3_priorexp[5],inv1_par3_priorlognormlow[5],inv1_par3_priorlognormhigh[5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(inv2_par3_priorexp[5],inv2_par3_priorlognormlow[5],inv2_par3_priorlognormhigh[5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext(par(las=1),text=c("Species 1","Species 2"),
      at=c(1.5,3.5),side=1,line=1,adj=NA,cex=1,padj=NA)
mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

plot(x='n',ylim=c(0,1),xlim=c(0.5,4.5),pch=16,xaxt='n',xlab='',ylab='')
points(pch=c(21,22,23),x=c(1,1.5,2),c(inv1_par4_priorexp[5],inv1_par4_priorlognormlow[5],inv1_par4_priorlognormhigh[5]),col="black",bg= "#7fa626",cex=2)
points(pch=c(21,22,23),x=c(3,3.5,4),c(inv2_par4_priorexp[5],inv2_par4_priorlognormlow[5],inv2_par4_priorlognormhigh[5]),col="black",bg="#2997af",cex=2)
abline(h=0.95,lty=2,col="red")
mtext(par(las=1),text=c("Species 1","Species 2"),
      at=c(1.5,3.5),line=1)
mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
dev.off()

#2d plots invasion criteria
pdf(file=paste("plots/invasion2D_priorexp.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(5,5,2,2),omi=c(0.1,0,0.5,0))
colors <- rep("black", length(qt_inv1est_par1_priorexp[,1]))
colors[competoutcome_par1_priorexp[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par1_priorexp[,2],qt_inv2est_par1_priorexp[,1],qt_inv1est_par1_priorexp[,3],qt_inv2est_par1_priorexp[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par1_priorexp[,1],qt_inv2est_par1_priorexp[,2],qt_inv1est_par1_priorexp[,1],qt_inv2est_par1_priorexp[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par1_priorexp[,1],y=qt_inv2est_par1_priorexp[,1],col="black",bg=colors)
points(x=inv1_par1_priorexp[1],y=inv2_par1_priorexp[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)

abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)

mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")

colors <- rep("black", length(qt_inv1est_par2_priorexp[,1]))
colors[competoutcome_par2_priorexp[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par2_priorexp[,2],qt_inv2est_par2_priorexp[,1],qt_inv1est_par2_priorexp[,3],qt_inv2est_par2_priorexp[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par2_priorexp[,1],qt_inv2est_par2_priorexp[,2],qt_inv1est_par2_priorexp[,1],qt_inv2est_par2_priorexp[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par2_priorexp[,1],y=qt_inv2est_par2_priorexp[,1],col="black",bg=colors)
points(x=inv1_par2_priorexp[1],y=inv2_par2_priorexp[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)
abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")
mtext("Estimated invasion criteria using prior 1", side = 3, line = 1, outer = TRUE,cex=1.2)
colors <- rep("black", length(qt_inv1est_par3_priorexp[,1]))
colors[competoutcome_par3_priorexp[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par3_priorexp[,2],qt_inv2est_par3_priorexp[,1],qt_inv1est_par3_priorexp[,3],qt_inv2est_par3_priorexp[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par3_priorexp[,1],qt_inv2est_par3_priorexp[,2],qt_inv1est_par3_priorexp[,1],qt_inv2est_par3_priorexp[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par3_priorexp[,1],y=qt_inv2est_par3_priorexp[,1],col="black",bg=colors)
points(x=inv1_par3_priorexp[1],y=inv2_par3_priorexp[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)
abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)

mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

colors <- rep("black", length(qt_inv1est_par4_priorexp[,1]))
colors[competoutcome_par4_priorexp[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par4_priorexp[,2],qt_inv2est_par4_priorexp[,1],qt_inv1est_par4_priorexp[,3],qt_inv2est_par4_priorexp[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par4_priorexp[,1],qt_inv2est_par4_priorexp[,2],qt_inv1est_par4_priorexp[,1],qt_inv2est_par4_priorexp[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par4_priorexp[,1],y=qt_inv2est_par4_priorexp[,1],col="black",bg=colors)
points(x=inv1_par4_priorexp[1],y=inv2_par4_priorexp[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)
abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)

mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
dev.off()
pdf(file=paste("plots/invasion2D_priolognormlow.pdf",sep=""),width=8.6,height=8.6)
par(mfrow=c(2,2),mar=c(5,5,2,2),omi=c(0.1,0,0.5,0))
colors <- rep("black", length(qt_inv1est_par1_priorlognormlow[,1]))
colors[competoutcome_par1_priorlognormlow[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par1_priorlognormlow[,2],qt_inv2est_par1_priorlognormlow[,1],qt_inv1est_par1_priorlognormlow[,3],qt_inv2est_par1_priorlognormlow[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par1_priorlognormlow[,1],qt_inv2est_par1_priorlognormlow[,2],qt_inv1est_par1_priorlognormlow[,1],qt_inv2est_par1_priorlognormlow[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par1_priorlognormlow[,1],y=qt_inv2est_par1_priorlognormlow[,1],col="black",bg=colors)
points(x=inv1_par1_priorlognormlow[1],y=inv2_par1_priorlognormlow[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)

abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)

mtext("A",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 1",bty = "n")

colors <- rep("black", length(qt_inv1est_par2_priorlognormlow[,1]))
colors[competoutcome_par2_priorlognormlow[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par2_priorlognormlow[,2],qt_inv2est_par2_priorlognormlow[,1],qt_inv1est_par2_priorlognormlow[,3],qt_inv2est_par2_priorlognormlow[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par2_priorlognormlow[,1],qt_inv2est_par2_priorlognormlow[,2],qt_inv1est_par2_priorlognormlow[,1],qt_inv2est_par2_priorlognormlow[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par2_priorlognormlow[,1],y=qt_inv2est_par2_priorlognormlow[,1],col="black",bg=colors)
points(x=inv1_par2_priorlognormlow[1],y=inv2_par2_priorlognormlow[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)
abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)
mtext("B",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 2",bty = "n")
mtext("Estimated invasion criteria using prior 2", side = 3, line = 1, outer = TRUE,cex=1.2)
colors <- rep("black", length(qt_inv1est_par3_priorlognormlow[,1]))
colors[competoutcome_par3_priorlognormlow[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par3_priorlognormlow[,2],qt_inv2est_par3_priorlognormlow[,1],qt_inv1est_par3_priorlognormlow[,3],qt_inv2est_par3_priorlognormlow[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par3_priorlognormlow[,1],qt_inv2est_par3_priorlognormlow[,2],qt_inv1est_par3_priorlognormlow[,1],qt_inv2est_par3_priorlognormlow[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par3_priorlognormlow[,1],y=qt_inv2est_par3_priorlognormlow[,1],col="black",bg=colors)
points(x=inv1_par3_priorlognormlow[1],y=inv2_par3_priorlognormlow[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)
abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)

mtext("C",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 3",bty = "n")

colors <- rep("black", length(qt_inv1est_par4_priorlognormlow[,1]))
colors[competoutcome_par4_priorlognormlow[,2]=="uncertain"] <- "white"
plot(x='n',ylim=c(0,12),xlim=c(0,12),pch=16,xlab='Invasion criteria sp. 1',ylab='Invasion criteria sp. 2')
segments(lwd=1,qt_inv1est_par4_priorlognormlow[,2],qt_inv2est_par4_priorlognormlow[,1],qt_inv1est_par4_priorlognormlow[,3],qt_inv2est_par4_priorlognormlow[,1],col="darkgray")
segments(lwd=1,qt_inv1est_par4_priorlognormlow[,1],qt_inv2est_par4_priorlognormlow[,2],qt_inv1est_par4_priorlognormlow[,1],qt_inv2est_par4_priorlognormlow[,3],col="darkgray")
points(pch=21,x=qt_inv1est_par4_priorlognormlow[,1],y=qt_inv2est_par4_priorlognormlow[,1],col="black",bg=colors)
points(x=inv1_par4_priorlognormlow[1],y=inv2_par4_priorlognormlow[1],col=rgb(1,0,0,alpha=0.8),cex=1.5,pch=19)
abline(h=1,col="red",lty=2)
abline(v=1,col="red",lty=2)

mtext("D",side = 3, adj = 0.05, line = 1,cex=1.5,padj = 0.5)
legend('topright',legend="Parameter set 4",bty = "n")
dev.off()
#plot priors and true values for alpha and beta parameters
minv<-0
maxv<-2
freqv<-0.01
xx <- seq(minv,maxv,freqv)
dpriorexp <-  dexp(xx,1)
dpriorlognormlow <-  dlnorm(xx,0.5, 1)
dpriorlognormhigh <-  dlnorm(xx,log(0.8)+0.05, sdlog = sqrt(0.05))
pdf(file=paste("plots/priors.pdf",sep=""),width=8.6,height=8.6)
plot(xx,dpriorexp,ylim=c(0,2.2),xlim=c(0,2),type="l",pch=16,xlab='Parameter value',ylab='Density')
points(xx,dpriorlognormlow,lty=2,type="l")
points(xx,dpriorlognormhigh,lty=3,type="l")
abline(v=c(paramvalues_1$alphs,paramvalues_1$betas,
           paramvalues_2$alphs,paramvalues_2$betas,
           paramvalues_3$alphs,paramvalues_3$betas,
           paramvalues_4$alphs,paramvalues_4$betas
),col="red")
legend("topright", legend=c("Prior 1", "Prior 2", "Prior 3", "True values"),
       col=c("black", "black","black","red"), lty=c(1,2,3,1), cex=1)
dev.off()
#plot prior and true values for "fert1 and fert2 (\pi1 and \pi2)
minv<-0
maxv<-150
freqv<-1
xx <- seq(minv,maxv,freqv)
#note that fert1priormeanlog  ,fert2priormeanlog,
#sdprior, fert1, and fert2 are identical for all perameter sets
fert1prior <-  dlnorm(xx,paramvalues_1$fert1priormeanlog, sd=paramvalues_1$sdprior)
fert2prior <-  dlnorm(xx,paramvalues_1$fert2priormeanlog, sd=paramvalues_1$sdprior)
pdf(file=paste("plots/priors_pi.pdf",sep=""),width=8.6,height=4.3)
par(mfrow=c(1,2),mar=c(4,3,1,2),omi=c(0.1,0,0.2,0))
plot(xx,fert1prior,ylim=c(0,0.05),xlim=c(0,100),type="l",pch=16,xlab=TeX(r'($\pi_1$)'),ylab='Density')
abline(v=paramvalues_1$fert1,col="red")
legend("topright", legend=c("Prior", "True value"),
       col=c("black","red"), lty=c(1,1), cex=1)
plot(xx,fert2prior,ylim=c(0,0.05),xlim=c(0,100),type="l",pch=16,xlab=TeX(r'($\pi_2$)'),ylab='Density')
abline(v=paramvalues_1$fert2,col="red")
legend("topright", legend=c("Prior", "True value"),
       col=c("black","red"), lty=c(1,1), cex=1)
dev.off()
