setwd("/home/matpaquet/Documents/IPM_competition_2_species_age_structured")
library(coda)
library(viridis)
library(scatterplot3d) 
#parameter set
parameterset <- 1
samplers <- "AF_slice"
#samplers <- "defaultsamplers"
#samplers <- "RW_block"
#Use exponential or lognormal priors for density dependent slope parameters
PRIOR <- "EXP"
#PRIOR <- "LOGLOW"
#PRIOR <- "LOGHIGH"
if (PRIOR == "EXP") {
  ddpriors <- "ddpriorexp"
} else {
  if (PRIOR == "LOGLOW") {
    ddpriors <- "ddpriorlognormlow"} 
  else {
    if (PRIOR == "LOGHIGH") {
      ddpriors <- "ddpriorlognormhigh"
    }}}
#sample sizes
nyears <- 30
nmarked <- 100
#nmarked <- 1000
#nmarked <- 10000
nnests <- 50
load(file = paste("data/data_coexistence_model_param",parameterset,samplers,nmarked,"juvmarked",nnests,"nests",nyears,"nyears",ddpriors,".Rdata",sep=""))
list2env(paramvalues,.GlobalEnv)
#If want some plots that takes time to run
PLOT <- F
n.simul <- length(list.samples)
n.param <- dim(list.samples[[1]]$chain1)[2]
n.years <- length(list.simul[[1]]$N1a)
if (PLOT){
  for (i in 1:n.simul) {
    plot(list.samples[[i]]$chain1[,'alphs[1, 1]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'alphs[1, 1]'],type="l",col="blue")
    abline(h=alphs[1,1],col="red")
    plot(list.samples[[i]]$chain1[,'alphs[2, 1]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'alphs[2, 1]'],type="l",col="blue")
    abline(h=alphs[2,1],col="red")
    plot(list.samples[[i]]$chain1[,'alphs[1, 2]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'alphs[1, 2]'],type="l",col="blue")
    abline(h=alphs[1,2],col="red")
    plot(list.samples[[i]]$chain1[,'alphs[2, 2]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'alphs[2, 2]'],type="l",col="blue")
    abline(h=alphs[2,2],col="red")
    plot(list.samples[[i]]$chain1[,'betas[1, 1]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'betas[1, 1]'],type="l",col="blue")
    abline(h=betas[1,1],col="red")
    plot(list.samples[[i]]$chain1[,'betas[2, 1]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'betas[2, 1]'],type="l",col="blue")
    abline(h=betas[2,1],col="red")
    plot(list.samples[[i]]$chain1[,'betas[1, 2]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'betas[1, 2]'],type="l",col="blue")
    abline(h=betas[1,2],col="red")
    plot(list.samples[[i]]$chain1[,'betas[2, 2]'],type="l",ylim=c(-0.1,1))
    points(list.samples[[i]]$chain2[,'betas[2, 2]'],type="l",col="blue")
    abline(h=betas[2,2],col="red")
    plot(list.samples[[i]]$chain1[,'fert1'],type="l",ylim=c(0,100))
    points(list.samples[[i]]$chain2[,'fert1'],type="l",col="blue")
    abline(h=fert1,col="red")
    plot(list.samples[[i]]$chain1[,'fert2'],type="l",ylim=c(0,100))
    points(list.samples[[i]]$chain2[,'fert2'],type="l",col="blue")
    abline(h=fert2,col="red")
    plot(list.samples[[i]]$chain1[,'phi1'],type="l",ylim=c(0,1))
    points(list.samples[[i]]$chain2[,'phi1'],type="l",col="blue")
    abline(h=phi1,col="red")
    plot(list.samples[[i]]$chain1[,'phi2'],type="l",ylim=c(0,1))
    points(list.samples[[i]]$chain2[,'phi2'],type="l",col="blue")
    abline(h=phi2,col="red")
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
if (PLOT){
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
}#plots
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
##
surv1a <- surv2a <-
  fec1 <- fec2 <- matrix(NA,n.n,n.n) 
for (i in 1:n.n) {
  surv1a[,i] <- phi1/(1+betas[1,1]*N1a+betas[1,2]*N2a[i])
  surv2a[,i] <- phi2/(1+betas[2,1] * N1a + betas[2,2] * N2a[i])
  fec1[,i] <- fert1 / (1+alphs[1,1] * N1a + alphs[1,2] * N2a[i])
  fec2[,i] <- fert2 / (1+ alphs[2,1] * N1a + alphs[2,2] * N2a[i])
}#i

surv1aestmean <- surv2aestmean <-
  fec1estmean <- fec2estmean <- matrix(NA,n.n,n.n)
surv1aest <- surv2aest <-
  fec1est <- fec2est <- array(NA,dim=c(n.simul.conv,n.n,n.n)) 
for (i in 1:n.n) {
  for(j in 1:n.n) {
    for (s in 1:n.simul.conv) {
      surv1aest[s,j,i] <- mean(phi1est[s,] / (1 + betas11est[s,] * N1a[j] + betas12est[s,] * N2a[i]))
      surv2aest[s,j,i] <- mean(phi2est[s,] / (1 + betas21est[s,] * N1a[j] + betas22est[s,] * N2a[i]))
      fec1est[s,j,i] <- mean(fert1est[s,] / (1 + alphs11est[s,] * N1a[j] + alphs12est[s,] * N2a[i]))
      fec2est[s,j,i] <- mean(fert2est[s,] / (1 + alphs21est[s,] * N1a[j] + alphs22est[s,] * N2a[i]))
    }#s
    surv1aestmean[j,i] <- mean(surv1aest[,j,i])
    surv2aestmean[j,i] <- mean(surv2aest[,j,i])
    fec1estmean[j,i] <- mean(fec1est[,j,i])
    fec2estmean[j,i] <- mean(fec2est[,j,i])
  }#j
}#i
plot.new()
par(mfrow = c(1, 1), cex.axis = 0.8, cex.lab = 1, mar = c(10, 3, 3, 3), las = 1)
pmat <- persp(z = surv1a,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Survival species 1",
              zlim=c(0,0.75),ticktype="detailed")
for (i in 1:n.n){
  for (s in 1:n.simul.conv) {
    lines(trans3d(z = surv1aest[s,,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.2)[10])
  }#s
}#i
for (i in 1:n.n){
  lines(trans3d(z = surv1aestmean[,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.5)[7])
  lines(trans3d(z = surv1a[,i],x=N1a,y=N2a[i],pmat),col="black")
}#i
pmat <- persp(z = surv2a,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Survival species 2",
              zlim=c(0,0.75),ticktype="detailed")
for (i in 1:n.n){
  for (s in 1:n.simul.conv) {
    lines(trans3d(z = surv2aest[s,,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.2)[10])
  }#s
}#i
for (i in 1:n.n){
  lines(trans3d(z = surv2aestmean[,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.5)[7])
  lines(trans3d(z = surv2a[,i],x=N1a,y=N2a[i],pmat),col="black")
}#i

pmat <- persp(z = fec1,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Fecundity species 1",
              zlim=c(0,40),ticktype="detailed")
for (i in 1:n.n){
  for (s in 1:n.simul.conv) {
    lines(trans3d(z = fec1est[s,,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.2)[10])
  }#s
}#i
for (i in 1:n.n){
  lines(trans3d(z = fec1estmean[,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.5)[7])
  lines(trans3d(z = fec1[,i],x=N1a,y=N2a[i],pmat),col="black")
}#i
pmat <- persp(z = fec2,x=N1a,y=N2a,xlab="N1a",ylab="N2a",zlab="Fecundity species 2",
              zlim=c(0,40),ticktype="detailed")
for (i in 1:n.n){
  for (s in 1:n.simul.conv) {
    lines(trans3d(z = fec2est[s,,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.2)[10])
  }#s
}#i
for (i in 1:n.n){
  lines(trans3d(z = fec2estmean[,i],x=N1a,y=N2a[i],pmat),col=viridis(11,alpha=.5)[7])
  lines(trans3d(z = fec2[,i],x=N1a,y=N2a[i],pmat),col="black")
}#i
##computation of juvenile survival and fecundity at equilibrium abundances
#Will be later used to define intercepts of "phenomenological" models with abundances centered
#adapted from code in Bardon and Barraquand
#get equilibriums numerically
#3. Creating vectors
years <- 3000
N1jdet<-rep(NA,years+1)
N2jdet<-rep(NA,years+1)
N1adet<-rep(NA,years+1)
N2adet<-rep(NA,years+1)
svar1jdet<-rep(NA,years+1)
svar2jdet<-rep(NA,years+1)
#4. INITIALIZATION
N0 <- 100
N1jdet[1] <-N0
N1adet[1] <-N0
N2jdet[1] <-N0
N2adet[1] <-N0
for (t in 1:years){
  svar1jdet[t] <- phi1/(1+betas[1,1]*N1adet[t]+betas[1,2]*N2adet[t])
  svar2jdet[t] <- phi2/(1+betas[2,1]*N1adet[t]+betas[2,2]*N2adet[t]) 
  N1jdet[t+1]<- fert1/(1+alphs[1,1]*N1adet[t]+alphs[1,2]*N2adet[t])*N1adet[t]
  N2jdet[t+1]<- fert2/(1+alphs[2,2]*N2adet[t]+alphs[2,1]*N1adet[t])*N2adet[t]
  N1adet[t+1]<-svar1jdet[t]*N1jdet[t]+s1a*N1adet[t] 
  N2adet[t+1]<-svar2jdet[t]*N2jdet[t]+s2a*N2adet[t]
}#t
N1astar <- N1adet[years+1]
N2astar <- N2adet[years+1]
surv1star <- phi1 / (1+betas[1,1] * N1astar + betas[1,2] * N2astar)
surv2star <- phi2 / (1+betas[2,1] * N1astar + betas[2,2] * N2astar)
fec1star <- fert1 / (1+alphs[1,1] * N1astar + alphs[1,2] * N2astar)
fec2star <- fert2 / (1+alphs[2,2] * N2astar + alphs[2,1] * N1astar)
###WORK IN PROGRESS
# CALCULATE THE INVASION CRITERIAS FROM THE TRUE PARAMETER VALUES
#note that since gamma (stage transition probability) is set to 1, C=0
D1 <- (fert1 * phi1) / (1 - s1a)
D2 <- (fert2 * phi2) / (1 - s2a)
n1astarsingle <- ((- alphs[1,1] - betas[1,1]) + sqrt( (alphs[1,1] + betas[1,1])^2 -
                                                        4 * alphs[1,1] * betas[1,1] *
                                                        (-D1 +1))) / (2 * alphs[1,1] * betas[1,1])
n2astarsingle <- ((- alphs[2,2] - betas[2,2]) + sqrt( (alphs[2,2] + betas[2,2])^2 -
                                                        4 * alphs[2,2] * betas[2,2] *
                                                        (-D2 +1))) / (2 * alphs[2,2] * betas[2,2])
inv1 <- D1 / ((1 + betas[1,2] * n2astarsingle) * (1 + alphs[1,2] * n2astarsingle))
inv2 <- D2 / ((1 + betas[2,1] * n1astarsingle) * (1 + alphs[2,1] * n1astarsingle))
#estimate invasion criteria
D1est <- (fert1est * phi1est) / (1 - s1aest)
D2est <- (fert2est * phi2est) / (1 - s2aest)
n1astarsingleest <- ((- alphs11est - betas11est) + sqrt( (alphs11est + betas11est)^2 -
                                                           4 * alphs11est * betas11est *
                                                           (-D1est +1))) / (2 * alphs11est * betas11est)
n2astarsingleest <- ((- alphs22est - betas22est) + sqrt( (alphs22est + betas22est)^2 -
                                                           4 * alphs22est * betas22est *
                                                           (-D2est +1))) / (2 * alphs22est * betas22est)
inv1est <- D1est / ((1 + betas12est * n2astarsingleest) * (1 + alphs12est * n2astarsingleest))
inv2est <- D2est / ((1 + betas21est * n1astarsingleest) * (1 + alphs21est * n1astarsingleest))
#traceplots of estimated invasion criteria
dev.off()
for (i in 1:n.simul.conv) {
  par(mfrow=c(2,1))
  plot(inv1est[i,c(1:(dim(inv1est)[2]/2))],type="l")
  points(inv1est[i,c(((dim(inv1est)[2]/2)+1):dim(inv1est)[2])],type="l",col="red")
  abline(h=inv1,col="green")
  plot(inv2est[i,c(1:(dim(inv2est)[2]/2))],type="l")
  points(inv2est[i,c(((dim(inv2est)[2]/2)+1):dim(inv2est)[2])],type="l",col="red")
  abline(h=inv2,col="green")
}#i
##some prior posterior overlaps

#==========================================================
#==========================================================
# R code for Book Parameter Redundancy and Identifiability
# by Diana J. Cole
# R code is for Immigration Integrated Model
# Section 9.2.2
#==========================================================
#==========================================================
overlap <- function(data,prior,minv,maxv,freqv,xlabel,truevalue) {
  # OVERLAP calculates the proportion overlap between the
  # prior and posterior using a kernel density to approximate
  # posterior
  # Also plots a graph of prior and posterior
  # 'data' contains the posterior chain
  # 'prior' contains a vector of prior values evaluated at same interval
  # as 'minv', 'maxv' and 'freqv' values given
  
  k1 <- 0.9 # Controls the smoothness of the kernel
  
  x <- seq(minv,maxv,freqv)
  nn <- length(x)
  fK <- c(rep(0,nn))
  
  overlap<-0
  for (i in 1:nn) {
    fK[i]<-kernel(x[i],data,k1)
    if (fK[i]<prior[i]){
      overlap<-overlap+fK[i]*freqv
    }
    else {
      overlap=overlap+prior[i]*freqv
    }
  }
  
  plot(x,fK,type = "l",ylab="f",xlab=xlabel)
  lines(x,prior,lty=2)
  abline(v=truevalue)#added
  return(overlap)
}


kernel <- function(y,data,k1) {
  # KERNEL Calculates a kernel density estimate for a sample in 'data'.
  #   'y' is the value at which we want the kernel estimate.
  #   'k1' can be chosen to vary the amount of smoothing;
  #   Calls the function DELTA.
  
  n <- length(data)
  h <- k1*min(sd(data),IQR(data)/1.34)/n^0.2	
  
  z <- 0
  for (i in 1:n ) {
    z<-z+delta((y-data[i])/h)
  }				            
  z<-z/(n*h);
}

delta <- function(u) {
  # DELTA calculates a normal kernel
  
  y <-(exp(-u*u/2))/sqrt(2*pi);
}
minv<-0
maxv<-2
freqv<-0.1#these can be lowered for final version
xx <- seq(minv,maxv,freqv)
plot.new()
par(mfrow=c(2,2))
beta.overlap <- array(NA,dim=c(2,2,n.simul.conv))
#change if the different beta[,] have different priors
if (PRIOR == "EXP") {
  betaprior <-  dexp(xx,1)
} else {
  if (PRIOR == "LOGLOW") {
    betaprior <-  dlnorm(xx,0.5, 1)
    } else {
    if (PRIOR == "LOGHIGH") {
      betaprior <-  dlnorm(xx,log(0.8)+0.05, sdlog = sqrt(0.05))
    }}}
for (i in 1:n.simul.conv) {
  beta.overlap[1,1,i]<-overlap(betas11est[i,],betaprior,minv,maxv,freqv,expression(beta11),betas[1,1])
  beta.overlap[1,2,i]<-overlap(betas12est[i,],betaprior,minv,maxv,freqv,expression(beta12),betas[1,2])
  beta.overlap[2,1,i]<-overlap(betas21est[i,],betaprior,minv,maxv,freqv,expression(beta21),betas[2,1])
  beta.overlap[2,2,i]<-overlap(betas22est[i,],betaprior,minv,maxv,freqv,expression(beta22),betas[2,2])
}
summary(beta.overlap)
minv<-0
maxv<-2
freqv<-0.1
xx <- seq(minv,maxv,freqv)
plot.new()
par(mfrow=c(2,2))
alpha.overlap <- array(NA,dim=c(2,2,n.simul.conv))
#change if the different alpha[,] have different priors
if (PRIOR == "EXP") {
  alphaprior <-  dexp(xx,1)
} else {
  if (PRIOR == "LOGLOW") {
    alphaprior <-  dlnorm(xx,0.5, 1)
  } else {
    if (PRIOR == "LOGHIGH") {
      alphaprior <-  dlnorm(xx,log(0.8)+0.05, sdlog = sqrt(0.05))
    }}}
for (i in 1:n.simul.conv) {
  alpha.overlap[1,1,i]<-overlap(alphs11est[i,],alphaprior,minv,maxv,freqv,expression(alpha11),alphs[1,1])
  alpha.overlap[1,2,i]<-overlap(alphs12est[i,],alphaprior,minv,maxv,freqv,expression(alpha12),alphs[1,2])
  alpha.overlap[2,1,i]<-overlap(alphs21est[i,],alphaprior,minv,maxv,freqv,expression(alpha21),alphs[2,1])
  alpha.overlap[2,2,i]<-overlap(alphs22est[i,],alphaprior,minv,maxv,freqv,expression(alpha22),alphs[2,2])
}
summary(alpha.overlap)
plot.new()
minv<-0
maxv<-150
freqv<-1
xx <- seq(minv,maxv,freqv)
plot.new()
par(mfrow=c(2,1))
fert.overlap <- matrix(NA,nrow=2,ncol=n.simul.conv)
fert1prior <-  dlnorm(xx,fert1priormode, sd=sdprior)
fert2prior <-  dlnorm(xx,fert2priormode, sd=sdprior)
for (i in 1:n.simul.conv) {
  fert.overlap[1,i]<-overlap(fert1est[i,],fert1prior,minv,maxv,freqv,expression(fert1),fert1)
  fert.overlap[2,i]<-overlap(fert2est[i,],fert2prior,minv,maxv,freqv,expression(fert2),fert2)
}
summary(fert.overlap[1,])
summary(fert.overlap[2,])
plot.new()
minv<-0
maxv<-1
freqv<-0.01
xx <- seq(minv,maxv,freqv)
plot.new()
par(mfrow=c(2,1))
phi.overlap <- matrix(NA,nrow=2,ncol=n.simul.conv)
phiprior <-  dunif(xx,0,1)
for (i in 1:n.simul.conv) {
  phi.overlap[1,i]<-overlap(phi1est[i,],phiprior,minv,maxv,freqv,expression(phi1),phi1)
  phi.overlap[2,i]<-overlap(phi2est[i,],phiprior,minv,maxv,freqv,expression(phi2),phi2)
}
summary(as.vector(phi.overlap))
getestimates <- function(param,trueval) {
  n.simul.conv <- nrow(param)
  coverage <- numeric(n.simul.conv)
  for (s in 1:n.simul.conv){
    coverage[s] <- ifelse(quantile(param[s,],0.025)<trueval&trueval<quantile(param[s,],0.975),1,0)
  }#s
  estimates <- numeric(5)
  names(estimates) <- c("simul. value","est. mean","2.5%","97.5%","coverage 95%")
  estimates[1] <- trueval
  estimates[2] <- mean(param)
  estimates[3] <- quantile(rowMeans(param),0.025)
  estimates[4] <- quantile(rowMeans(param),0.975)
  estimates[5] <- mean(coverage)
  return(estimates)
}
summary_inv1 <- getestimates(inv1est,inv1)
summary_inv2 <- getestimates(inv2est,inv2)
plot.new()
par(mfrow = c(1, 1), cex.axis = 0.8, cex.lab = 1, mar = c(10, 3, 3, 3), las = 1)
plot(main ="Mean estimated invasion criteria",x='n',ylim=c(0,max(c(summary_inv1[4],summary_inv2[4])+2)),xlim=c(0.5,2.5),pch=16,xaxt='n',xlab='',ylab='')
segments(lwd=5,c(1:2),c(summary_inv1[4],summary_inv2[4]),c(1:2),c(summary_inv1[3],summary_inv2[3]))
points(pch=21,x=c(1:2),c(summary_inv1[2],summary_inv2[2]),col="black",bg= viridis(6)[4],cex=2)
points(pch=19,x=c(1:2),c(summary_inv1[1],summary_inv2[1]),col="red",cex=1)
mtext( par(las=2),text=c("invasion criteria sp.1","invasion criteria sp.2"),
       at=c(1:2), side=1,cex=1)
abline(h=1)
text("95% Coverages:",x=0.7,y=max(c(summary_inv1[4],summary_inv2[4])+1))
text(round(summary_inv1[5],2),x=1,y=max(c(summary_inv1[4],summary_inv2[4])+1))
text(round(summary_inv2[5],2),x=2,y=max(c(summary_inv1[4],summary_inv2[4])+1))
legend('topright',col="red",legend="true value",pch=19)
save(summary_inv1,summary_inv2,
     file = paste0("data/summary_data",parameterset,ddpriors,".Rdata"))
