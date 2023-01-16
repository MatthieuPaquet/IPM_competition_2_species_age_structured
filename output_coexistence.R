library(coda)
library(viridis)
library("scatterplot3d") 
load(file="samples_coexistence_model.Rdata")
load(file="simul_coexistence_model.Rdata")
n.simul <- length(list.samples)
n.param <- dim(list.samples[[1]]$chain1)[2]
n.years <- length(list.simul[[1]]$N1)
PLOT <- F
if (PLOT){
for (i in 1:n.simul) {
plot(list.samples[[i]]$chain1[,'alphs[1, 1]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'alphs[1, 1]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'alphs[2, 1]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'alphs[2, 1]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'alphs[1, 2]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'alphs[1, 2]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'alphs[2, 2]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'alphs[2, 2]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'betas[1, 1]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'betas[1, 1]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'betas[2, 1]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'betas[2, 1]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'betas[1, 2]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'betas[1, 2]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'betas[2, 2]'],type="l",ylim=c(-0.1,1))
points(list.samples[[i]]$chain2[,'betas[2, 2]'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'fert1'],type="l",ylim=c(0,100))
points(list.samples[[i]]$chain2[,'fert1'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'fert2'],type="l",ylim=c(0,100))
points(list.samples[[i]]$chain2[,'fert2'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'phi1'],type="l",ylim=c(0,1))
points(list.samples[[i]]$chain2[,'phi1'],type="l",col="red")
plot(list.samples[[i]]$chain1[,'phi2'],type="l",ylim=c(0,1))
points(list.samples[[i]]$chain2[,'phi2'],type="l",col="red")
}#plots
}

gelman.table <- matrix(NA,n.simul,n.param)
effective.size <- matrix(NA,n.simul,n.param)
for(s in 1:n.simul){
  for(i in 1:n.param){
    gelman.table[s,i] <- gelman.diag(list(as.mcmc(list.samples[[s]]$chain2[,i]),
                                          as.mcmc(list.samples[[s]]$chain1[,i])))$psrf[1,2]
    effective.size[s,i] <- effectiveSize(list(as.mcmc(list.samples[[s]]$chain2[,i]),
                                              as.mcmc(list.samples[[s]]$chain1[,i])))
  }#i
}#s
colnames(gelman.table) <- colnames( effective.size)<-names(list.samples[[1]]$chain1[1,])
#select subset for which "alpha" parameters (i.e., on the density dependent functions)
#converged and mix OK (based on gelman diagnostic and effective sample size)
gelman.table.alphas<-gelman.table[,c("alphs[1, 1]", "alphs[2, 1]", "alphs[1, 2]", "alphs[2, 2]",
                                     "betas[1, 1]", "betas[2, 1]", "betas[1, 2]", "betas[2, 2]",
                                     "fert1","fert2","phi1","phi2")]
effective.size<-effective.size[,c("alphs[1, 1]", "alphs[2, 1]", "alphs[1, 2]", "alphs[2, 2]",
                                  "betas[1, 1]", "betas[2, 1]", "betas[1, 2]", "betas[2, 2]",
                                  "fert1","fert2","phi1","phi2")]

max(gelman.table.alphas)
incl <- which(gelman.table.alphas[,"alphs[1, 1]"]<1.1&
                gelman.table.alphas[,"alphs[2, 1]"]<1.1&
                gelman.table.alphas[,"alphs[1, 2]"]<1.1&
                gelman.table.alphas[,"alphs[2, 2]"]<1.1&
                gelman.table.alphas[,"betas[1, 1]"]<1.1&
                gelman.table.alphas[,"betas[2, 1]"]<1.1&
                gelman.table.alphas[,"betas[1, 2]"]<1.1&
                gelman.table.alphas[,"betas[2, 2]"]<1.1&
                gelman.table.alphas[,"fert1"]<1.1&
                gelman.table.alphas[,"fert2"]<1.1&
                gelman.table.alphas[,"phi1"]<1.1&
                gelman.table.alphas[,"phi2"]<1.1&
                effective.size[,"alphs[1, 1]"]>50&
                effective.size[,"alphs[2, 1]"]>50&
                effective.size[,"alphs[1, 2]"]>50&
                effective.size[,"alphs[2, 2]"]>50&
                effective.size[,"betas[1, 1]"]>50&
                effective.size[,"betas[2, 1]"]>50&
                effective.size[,"betas[1, 2]"]>50&
                effective.size[,"betas[2, 2]"]>50&
                effective.size[,"fert1"]>50&
                effective.size[,"fert2"]>50&
                effective.size[,"phi1"]>50&
                effective.size[,"phi2"]>50)
length(incl)
x <- list()
for (i in 1:length(list.samples)) {
  x[[i]] <- rbind(list.samples[[i]]$chain1,list.samples[[i]]$chain2)
}#i
list.samples.converg <- x[incl]
n.simul.conv <- length(list.samples.converg)
n.samples <- nrow(list.samples.converg[[1]])
n.param <- length( list.samples.converg[[1]][1,])
n.n <- 100
alphs11est <- alphs12est <- alphs21est <- alphs22est <-
betas11est <- betas12est <- betas21est <- betas22est <-
fert1est <- fert2est <- phi1est <- phi2est <- s1aest <- s2aest <-
  matrix(NA,n.simul.conv,n.samples)
#these are to plot estimated yearly abundances and demographic rates 
N1jest <- array(NA,dim=c(n.simul.conv,n.years,n.samples))
N1aest <- array(NA,dim=c(n.simul.conv,n.years,n.samples))
N2jest <- array(NA,dim=c(n.simul.conv,n.years,n.samples))
N2aest <- array(NA,dim=c(n.simul.conv,n.years,n.samples))

fledg.rate.1est <- array(NA,dim=c(n.simul.conv,n.years,n.samples))
fledg.rate.2est <- array(NA,dim=c(n.simul.conv,n.years,n.samples))
svar1est <- array(NA,dim=c(n.simul.conv,n.years-1,n.samples))
svar2est <- array(NA,dim=c(n.simul.conv,n.years-1,n.samples))
for (s in 1:n.simul.conv) {
  for (i in 1:n.samples) {
    mcmc<-list.samples.converg[[s]][i,]
    alphs11est[s,i] = mcmc['alphs[1, 1]']
    alphs21est[s,i] = mcmc['alphs[2, 1]']
    alphs12est[s,i] = mcmc['alphs[1, 2]']
    alphs22est[s,i] = mcmc['alphs[2, 2]']
    betas11est[s,i] = mcmc['betas[1, 1]']
    betas21est[s,i] = mcmc['betas[2, 1]']
    betas12est[s,i] = mcmc['betas[1, 2]']
    betas22est[s,i] = mcmc['betas[2, 2]']
    fert1est[s,i] = mcmc['fert1']
    fert2est[s,i] = mcmc['fert2']
    phi1est[s,i] = mcmc['phi1']
    phi2est[s,i] = mcmc['phi2']
    s1aest[s,i] = mcmc['s1a']
    s2aest[s,i] = mcmc['s2a']
    N1jest[s,,i] = mcmc[grep('^N1j\\[', names(mcmc))]
    N1aest[s,,i] = mcmc[grep('^N1a\\[', names(mcmc))]
    N2jest[s,,i] = mcmc[grep('^N2j\\[', names(mcmc))]
    N2aest[s,,i] = mcmc[grep('^N2a\\[', names(mcmc))]
    fledg.rate.1est[s,,i] = mcmc[grep('^fledg.rate.1\\[', names(mcmc))]
    fledg.rate.2est[s,,i] = mcmc[grep('^fledg.rate.2\\[', names(mcmc))]
    svar1est[s,,i] = mcmc[grep('^svar1\\[', names(mcmc))]
    svar2est[s,,i] = mcmc[grep('^svar2\\[', names(mcmc))]
  }#i
}#s
#posterior correlations within functions
par(mfrow=c(2,2))
plot(alphs11est,alphs12est,col=rgb(0,0,0,0.1))
plot(fert1est,alphs11est,col=rgb(0,0,0,0.1))
plot(fert1est,alphs12est,col=rgb(0,0,0,0.1))
scatterplot3d(as.vector(alphs11est),as.vector(alphs12est),as.vector(fert1est),angle=55,color =rgb(0,0,0,alpha=0.1))
plot.new()
par(mfrow=c(2,2))
plot(alphs22est,alphs21est,col=rgb(0,0,0,0.1))
plot(fert2est,alphs22est,col=rgb(0,0,0,0.1))
plot(fert2est,alphs21est,col=rgb(0,0,0,0.1))
plot.new()
par(mfrow=c(2,2))
plot(betas11est,betas12est,col=rgb(0,0,0,0.1))
plot(phi1est,betas11est,col=rgb(0,0,0,0.1))
plot(phi1est,betas12est,col=rgb(0,0,0,0.1))
plot.new()
par(mfrow=c(2,2))
plot(betas22est,betas21est,col=rgb(0,0,0,0.1))
plot(phi2est,betas22est,col=rgb(0,0,0,0.1))
plot(phi2est,betas21est,col=rgb(0,0,0,0.1))
#get the true functional responses between density and vital rates
N1asimul <- matrix(NA,n.simul,n.years)
N2asimul <- matrix(NA,n.simul,n.years)
for (i in 1:n.simul) {
  for (t in 1:n.years) {
    N1asimul[i,t] <- list.simul[[i]]$N1a[t]
    N2asimul[i,t] <- list.simul[[i]]$N2a[t]
  }
}
N1a <- seq(0,max(N1asimul),length=n.n) #density index adults species1
N2a <- seq(0,max(N2asimul),length=n.n) #density index adults species2
#parameter set
parameterset <- 1
#adapted from first parameter set in Bardon&Barraquand
fert1=30
fert2=25
phi1=0.5
phi2=0.4
s1a=0.5
s2a=0.6
#recapture probabilities
p1=0.7
p2=0.7
if (parameterset == 1) {
  alphs=matrix(c(0.1, 0.05, 0.06, 0.1),ncol = 2, byrow = TRUE)
  betas=matrix(c(0.1, 0.06, 0.06, 0.1),ncol = 2, byrow = TRUE)
} else {
  if (parameterset == 2) {
    alphs=matrix(c(0.1, 0.02, 0.112, 0.1),ncol = 2, byrow = TRUE)
    betas=matrix(c(0.1, 0.125, 0.01, 0.1),ncol = 2, byrow = TRUE)
  } else {
    if (parameterset == 3) {
      alphs=matrix(c(0.1, 0.043, 0.035, 0.1),ncol = 2, byrow = TRUE)
      betas=matrix(c(0.1, 0.155, 0.165, 0.1),ncol = 2, byrow = TRUE)
    }
  }
}
##to adjust
surv1a <- surv2a <-
fec1 <- fec2 <- matrix(NA,n.n,n.n) 
for (i in 1:n.n) {
surv1a[,i] <- phi1/(1+betas[1,1]*N1a+betas[1,2]*N2a[i])
surv2a[,i] <- phi2/(1+betas[2,1] * N1a + betas[2,2] * N2a[i])
fec1[,i] <- fert1 / (1+alphs[1,1] * N1a + alphs[1,2] * N2a[i])
fec2[,i] <- fert2 / (1+ alphs[2,1] * N1a + alphs[2,2] * N2a[i])
}#i
par(mfrow=c(2,2))
persp(z = surv1a,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Survival species 1",
        zlim=c(0,1),theta = 60,ticktype="detailed")
persp(z = surv2a,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Survival species 2",
      zlim=c(0,1),theta = 60,ticktype="detailed")
persp(z = fec1,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Fecundity species 1",
      zlim=c(0,40),theta = 60,ticktype="detailed")
persp(z = fec2,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Fecundity species 2",
      zlim=c(0,40),theta = 60,ticktype="detailed")
