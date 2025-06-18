##Model to simulate and fit data for a 2 species competition model based on the theoretical model from Bardon&Barraquand 2023
#we decide to fix the maturation rate gamma=1 for both species for simplicity
#and because in practice probably few real systems would use such configuration

setwd("/home/matpaquet/Documents/IPM_competition_2_species_age_structured")
library(nimble)
library(mcmcplots)
parameterset <- 1
samplers <- "AF_slice"
#samplers <- "defaultsamplers"
#samplers <- "RW_block"
#Use exponential or lognormal priors for density dependent slope parameters
#PRIOR <- "EXP"
PRIOR <- "LOGLOW"
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
#number of simulated datasets
nsim <- 100
#sample sizes
nyears <- 30
#nyears <- 100
nmarked <- 100
#nmarked <- 1000
#nmarked <- 10000
nnests <- 50
#adapted from parameter sets in Bardon&Barraquand
fert1 <- 30
fert2 <- 25
phi1 <- 0.5
phi2 <- 0.4
s1a <- 0.5
s2a <- 0.6
#recapture probabilities
p1 <- 0.7
p2 <- 0.7
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
    } else {
      if (parameterset == 4) {
        alphs=matrix(c(0.1, 0.1, 0.1, 0.1),ncol = 2, byrow = TRUE)
        betas=matrix(c(0.1, 0.1, 0.1, 0.1),ncol = 2, byrow = TRUE)
      }
    }
  }
}
#Initial population sizes
N1jt1 <- N1jinit <- 100
N1at1 <- N1ainit <- 100
N2jt1 <- N2jinit <- 100
N2at1 <- N2ainit <- 100
#Number of nestlings marked every year
r1j <- rep(nmarked, (nyears - 1))
r2j <- rep(nmarked, (nyears - 1))
#no individuals newly marked as adults
r1a <- c(0,rep(NA,(nyears - 2)))
r2a <- c(0,rep(NA,(nyears - 2)))
#number of nests monitored
fledg.sample.1 <- rep(nnests, nyears)
fledg.sample.2 <- rep(nnests, nyears)
#Population size
N1jobs <- rep(NA, nyears)
N1aobs <- rep(NA, nyears)
N2jobs <- rep(NA, nyears)
N2aobs <- rep(NA, nyears)
#empty data
marray1j <- matrix(NA, nyears-1, nyears)
marray2j <- matrix(NA, nyears-1, nyears)
marray1a <- matrix(NA, nyears-1, nyears)
marray2a <- matrix(NA, nyears-1, nyears)
fledg1obs <- rep(NA, nyears)
fledg2obs <- rep(NA, nyears)
#write the model code
coexcode  <-  nimbleCode({
  # Likelihood for  count data (state-space model) 
  for (t in 1:nyears) {
    #Observation process
    N1jobs[t] ~ dpois(N1j[t])
    N1aobs[t] ~ dpois(N1a[t])
    N2jobs[t] ~ dpois(N2j[t])
    N2aobs[t] ~ dpois(N2a[t])
  }#t
  ###demographic stochasticity for the population level parameters
  for (t in 2:nyears) {
    #reminder: maturation rate =1 so this was simplified
    N1a[t] <- N1aold[t]+N1anew[t]
    N2a[t] <- N2aold[t]+N2anew[t]
    #old vs new adults
    N1aold[t] ~ dbin(s1a, N1a[t-1])
    N2aold[t] ~ dbin(s2a, N2a[t-1])
    N1anew[t] ~ dbin(svar1[t-1], N1j[t-1])
    N2anew[t] ~ dbin(svar2[t-1], N2j[t-1])
    #sub adults 
    N1j[t] ~ dpois(fledg.rate.1[t-1] * N1a[t-1])
    N2j[t] ~ dpois(fledg.rate.2[t-1] * N2a[t-1]) 
  }#t
  #initial population size equals observed initial population size
  N1a[1] <- round(N1at1) 
  N2a[1] <- round(N2at1)
  N1j[1] <- round(N1jt1) 
  N2j[1] <- round(N2jt1)
  N1at1 ~ T(dnorm(N1ainit, 0.01), 0, )
  N2at1 ~ T(dnorm(N2ainit, 0.01), 0, )
  N1jt1 ~ T(dnorm(N1jinit, 0.01), 0, )
  N2jt1 ~ T(dnorm(N2jinit, 0.01), 0, )
  #likelihood for productivity data
  for (t in 1:nyears) {
    fledg1obs[t] ~ dpois(fledg.sample.1[t] * fledg.rate.1[t] * 2) #*2 as both sexes
    fledg2obs[t] ~ dpois(fledg.sample.2[t] * fledg.rate.2[t] * 2)
    ##dd intra and inter on fledgling rate
    fledg.rate.1[t] <- fert1 / (1+alphs[1,1] * N1a[t] +
                                  alphs[1,2] * N2a[t] )
    fledg.rate.2[t] <- fert2 / (1+alphs[2,2] * N2a[t] + 
                                  alphs[2,1] * N1a[t])
  }#t
  #think about the priors
  fert1 ~ dlnorm(fert1priormeanlog, sd=sdprior)
  fert2 ~ dlnorm(fert2priormeanlog, sd=sdprior)
  #Likelihood for capture-recapture data: CJS model (2 age classes)
  # Multinomial likelihood
  for (t in 1:(nyears-1)) {
    marray1j[t,1:nyears] ~ dmulti(pr1j[t,1:nyears], r1j[t])
    marray2j[t,1:nyears] ~ dmulti(pr2j[t,1:nyears], r2j[t])
    marray1a[t,1:nyears] ~ dmulti(pr1a[t,1:nyears], r1a[t])
    marray2a[t,1:nyears] ~ dmulti(pr2a[t,1:nyears], r2a[t])
  }#t
  #IMPORTANT we can do this because only juveniles are newly marked
  #otherwise we would also need to provide data about the newly marked adults and add it
  for (t in 1:(nyears-2)) {
    r1a[t+1] <- sum(marray1j[1:t,t]) + sum(marray1a[1:t,t])
    r2a[t+1] <- sum(marray2j[1:t,t]) + sum(marray2a[1:t,t])
  }#t
  # m-array cell probabilities for juveniles
  for (t in 1:(nyears-1)) {
    # Main diagonal
    pr1j[t,t]  <-  svar1[t] * p1
    pr2j[t,t]  <-  svar2[t] * p2
    # Above main diagonal
    for (j in (t + 1):(nyears-1)) {
      pr1j[t,j]  <-  svar1[t] * s1a^(j-t) * (1-p1)^(j-t) * p1
      pr2j[t,j]  <-  svar2[t] * s2a^(j-t) * (1-p2)^(j-t) * p2
    } #j
    # Below main diagonal
    for (j in 1:(t-1)) {
      pr1j[t,j]  <-  0
      pr2j[t,j]  <-  0
    } #j
    # Last column
    pr1j[t,nyears]  <-  1 - sum(pr1j[t,1:(nyears-1)])
    pr2j[t,nyears]  <-  1 - sum(pr2j[t,1:(nyears-1)])
  } #t
  # m-array cell probabilities for adults
  for (t in 1:(nyears-1)){
    # Main diagonal and above
    for (j in t:(nyears-1)){
      pr1a[t,j] <- s1a^(j-t+1) * (1-p1)^(j-t) * p1
      pr2a[t,j] <- s2a^(j-t+1) * (1-p2)^(j-t) * p2
    } #j
    # Below main diagonal
    for (j in 1:(t-1)){
      pr1a[t,j] <- 0
      pr2a[t,j] <- 0
    } #j
    # Last column
    pr1a[t,nyears] <- 1-sum(pr1a[t,1:(nyears-1)])
    pr2a[t,nyears] <- 1-sum(pr2a[t,1:(nyears-1)])
  } #t
  p1 ~ dunif(0,1)
  p2 ~ dunif(0,1)
  s1a ~ dunif(0,1)
  s2a ~ dunif(0,1)
  #relationship for vital rate parameters
  for (t in 1:(nyears-1)) {
    #interspecific dd on juvenile prey survival
    svar1[t] <- phi1 / (1 + betas[1,1] * N1a[t] + betas[1,2] * N2a[t])
    svar2[t] <- phi2 / (1 + betas[2,1] * N1a[t] + betas[2,2] * N2a[t])
  }#t
  phi1 ~ dunif(0,1) #shall we change these priors too?
  phi2 ~ dunif(0,1)
 
  if (PRIOR == "EXP") {
  for (a in 1:2) {
    for (b in 1:2) {
      alphs[a,b] ~ dexp(1)
      betas[a,b] ~ dexp(1)
    }#b
  }#a
  } else { if (PRIOR == "LOGLOW") {
    for (a in 1:2) {
      for (b in 1:2) {
        alphs[a,b] ~ dlnorm(0.5, 1)
        betas[a,b] ~ dlnorm(0.5, 1)
      }#b
    }#a 
    } else {
      if (PRIOR == "LOGHIGH") {
        for (a in 1:2) {
          for (b in 1:2) {
            alphs[a,b] ~ dlnorm(log(0.8)+0.05, sdlog = sqrt(0.05))
            betas[a,b] ~ dlnorm(log(0.8)+0.05, sdlog = sqrt(0.05))
          }#b
        }#a 
      }
      }}#priors
})
#change abundances at equilibrium
coexconstants  <-  list(nyears=nyears, r1j=r1j, r2j=r2j, 
                        fledg.sample.2=fledg.sample.2, fledg.sample.1=fledg.sample.1)
#Build the model
coexmodel  <-  nimbleModel(coexcode,
                           constants = coexconstants)
#Set data and initial values
coexmodel$setData(list(marray1j=marray1j,marray1a=marray1a,
                       N1jobs=N1jobs, N1aobs=N1aobs,
                       fledg1obs=fledg1obs,r1a=r1a,r2a=r2a,
                       marray2j=marray2j, marray2a=marray2a,
                       N2jobs=N2jobs,N2aobs=N2aobs, fledg2obs=fledg2obs))
coexinitsim <- list(fert1=fert1,fert2=fert2,phi1=phi1,phi2=phi2,s1a=s1a,s2a=s2a,
                    alphs=alphs, betas=betas,
                    p1=p1, p2=p2, N1jt1=N1jt1, N2jt1=N2jt1, 
                    N1at1=N1at1, N2at1=N2at1)
nodesToSim  <-  coexmodel$getDependencies(c("alphs","betas","p1","p2","phi1",
                                            "phi2","fert1","fert2",
                                            "N1jt1","N2jt1","N1at1","N2at1"),
                                          self = F, downstream = T)
coexmodel$setInits(coexinitsim)
#Compile the model 
ccoexmodel <- compileNimble(coexmodel) 
##simulate
list.simul <- list()
###simulate 100 datasets and run IPM on each
for (i in 1:nsim) {
  set.seed(i)
  ccoexmodel$simulate(nodesToSim)
  list.simul[[i]] <- list(N1j=ccoexmodel$N1j,N1a=ccoexmodel$N1a,
                          N2j=ccoexmodel$N2j,N2a=ccoexmodel$N2a,
                          N1jobs=ccoexmodel$N1jobs, N2jobs=ccoexmodel$N2jobs,
                          N1aobs=ccoexmodel$N1aobs, N2aobs=ccoexmodel$N2aobs,
                          svar1=ccoexmodel$svar1,svar2=ccoexmodel$svar2,
                          fledg.rate.1=ccoexmodel$fledg.rate.1,
                          fledg.rate.2=ccoexmodel$fledg.rate.2,
                          fledg1obs=ccoexmodel$fledg1obs,
                          fledg2obs=ccoexmodel$fledg2obs,
                          marray1j=ccoexmodel$marray1j,
                          marray2j=ccoexmodel$marray2j,
                          marray1a=ccoexmodel$marray1a,
                          marray2a=ccoexmodel$marray2a,
                          r1a=ccoexmodel$r1a,
                          r2a=ccoexmodel$r2a)
  print(i)  
}
#this code bit is just to check that all populations persist
#until the end of the time series and check the mean pop size then
nyears.tot <- length(list.simul[[1]][[1]])
n.simul <- length(list.simul)
N.simul.a.1 <- matrix(NA,n.simul,nyears.tot)
N.simul.j.1 <- matrix(NA,n.simul,nyears.tot)
N.simul.a.2 <- matrix(NA,n.simul,nyears.tot)
N.simul.j.2 <- matrix(NA,n.simul,nyears.tot)
N.simul.obs.1<- matrix(NA,n.simul,nyears.tot)
N.simul.obs.2 <- matrix(NA,n.simul,nyears.tot)
for (i in 1:n.simul) {
    N.simul.j.1[i,] <- list.simul[[i]]$N1j[]
    N.simul.a.1[i,] <- list.simul[[i]]$N1a[]
    N.simul.j.2[i,] <- list.simul[[i]]$N2j[]
    N.simul.a.2[i,] <- list.simul[[i]]$N2a[]
    N.simul.obs.1[i,] <- list.simul[[i]]$N1aobs[] + list.simul[[i]]$N1jobs[]
    N.simul.obs.2[i,] <- list.simul[[i]]$N2aobs[]+ list.simul[[i]]$N2jobs[]
  }
N.simul.1 <- N.simul.a.1 + N.simul.j.1 
N.simul.2 <- N.simul.a.2 + N.simul.j.2 
min(N.simul.1[,nyears])
min(N.simul.2[,nyears])
mean(N.simul.1[,nyears])
mean(N.simul.2[,nyears])
mean(N.simul.1[,nyears]==0)
mean(N.simul.2[,nyears]==0)
#plot one pair of predator-prey abundance time series as illustration 
plot(1:nyears.tot, N.simul.1[nsim,], type='l', lwd=3, ylim=c(0,max(N.simul.1[nsim,],
                                                                   N.simul.2[nsim,],
                                                                   N.simul.obs.1[nsim,],
                                                                   N.simul.obs.2[nsim,])),
     col='red', ylab='population size', xlab='years')
lines(1:nyears.tot, N.simul.2[nsim,], type='l', lwd=3, col='blue')
lines(1:nyears.tot, N.simul.obs.1[nsim,], type='p', lwd=3, col='red')
lines(1:nyears.tot, N.simul.obs.2[nsim,], type='p', lwd=3, col='blue')

length(list.simul)
#####fit an IPM
list.samples <- list()
for (i in 1) {
  #set simulated data as data
  marray1j <- list.simul[[i]]$marray1j
  marray2j <- list.simul[[i]]$marray2j
  marray1a <- list.simul[[i]]$marray1a
  marray2a <- list.simul[[i]]$marray2a
  N1jobs <- list.simul[[i]]$N1jobs
  N1aobs <- list.simul[[i]]$N1aobs
  fledg1obs <- list.simul[[i]]$fledg1obs
  N2jobs <- list.simul[[i]]$N2jobs
  N2aobs <- list.simul[[i]]$N2aobs
  fledg2obs <- list.simul[[i]]$fledg2obs
}
#set values for lognormal priors to be defined as constants
sdprior <- 0.5
#supposed max observed value, used as prior mean on the log scale, is at expected fertility when N=1
fledgrate1Neq1 <- fert1 / (1+alphs[1,1])
fledgrate2Neq1 <- fert2 / (1+alphs[2,2])
#prior parameters

fert1priormeanlog <- log(fledgrate1Neq1) -(sdprior^2 / 2)
fert2priormeanlog <- log(fledgrate2Neq1) -(sdprior^2 / 2)
#set list of parameter values to save with data
paramvalues <- list(alphs = alphs, betas = betas, 
                    fert1 = fert1, fert1priormeanlog = fert1priormeanlog,
                    fert2 = fert2, fert2priormeanlog = fert2priormeanlog,
                    p1 = p1, p2 = p2, phi1 = phi1, phi2 = phi2,
                    s1a = s1a, s2a = s2a, sdprior = sdprior)
#set model constants
coexconstants <- list(nyears=nyears,
                      r1j=r1j,
                      r2j=r2j,
                      fledg.sample.2=fledg.sample.2,
                      fledg.sample.1=fledg.sample.1,
                      sdprior=sdprior,
                      fert1priormeanlog=fert1priormeanlog, fert2priormeanlog=fert2priormeanlog,
                      N1jinit=N1jinit, N1ainit=N1ainit, N2jinit=N2jinit, N2ainit=N2ainit)#define as data if initial pop size differs among simulations
coexdata <- list(marray1j=marray1j,marray1a=marray1a,r1a=r1a,
                 N1jobs=N1jobs,N1aobs=N1aobs,fledg1obs=fledg1obs,
                 marray2j=marray2j,marray2a=marray2a,r2a=r2a,
                 N2jobs=N2jobs,N2aobs=N2aobs,fledg2obs=fledg2obs)
##initial values different than true values for parameters of interest, different for each of the 2 chains but same 2 sets of values for all snim models

getinits <- function() {list(fert1=fledgrate1Neq1,
                    fert2=fledgrate2Neq1,
                    phi1=runif(1,0.3,0.9),
                    phi2=runif(1,0.3,0.9),
                    s1a=runif(1,0.4,0.9),
                    s2a=runif(1,0.4,0.9),
                    p1=runif(1,0.6,0.9),
                    p2=runif(1,0.6,0.9),
                    N1jt1=rnorm(1,N1jt1,10),
                    N2jt1=rnorm(1,N2jt1,10), 
                    N1at1=rnorm(1,N1at1,10),
                    N2at1=rnorm(1,N2at1,10),
                    alphs=matrix(runif(4,0, 0.5),2,2),
                    betas=matrix(runif(4,0, 0.5),2,2),
  N1j=rep(NA,nyears),
  N2j=rep(NA,nyears),
  N1aold=rep(NA,nyears),
  N2aold=rep(NA,nyears),
  N1anew=rep(NA,nyears),
  N2anew=rep(NA,nyears))
}
set.seed(10)#adjust if needed to avoid slice samplers getting stuck...
coexinits1 <-getinits()
coexinits2 <-getinits()
coexinits <- list(coexinits1,coexinits2)
#Build the model for the IPM
coexmodelIPM  <-  nimbleModel(coexcode,
                              constants = coexconstants,data=coexdata)
#compile model for IPM
ccoexmodelIPM  <-  compileNimble(coexmodelIPM) 
#configure the MCMC
coexmcmcConf  <-  configureMCMC(ccoexmodelIPM,monitors=c("alphs","betas","p1","p2","phi1",
                                                         "phi2","s1a","s2a",
                                                         "fert1","fert2",
                                                         "p1","p2",
                                                         "fledg.rate.1","fledg.rate.2",
                                                         "svar1","svar2",
                                                         "N1jt1","N2jt1","N1at1","N2at1","N1j","N1a","N2j","N2a"))
###block samplers
if (samplers == "AF_slice") {
  coexmcmcConf$removeSamplers(c("alphs","betas","phi1","phi2","fert1","fert2"))
  # Add RW_block samplers, modifying adaptation behavior.
  coexmcmcConf$addSampler(target = c('fert1','alphs[1,1]','alphs[1,2]'),
                          type = "AF_slice",
                          control = list(sliceAdaptFactorInterval = 20))
  coexmcmcConf$addSampler(target = c('fert2','alphs[2,2]','alphs[2,1]'),
                          type = "AF_slice",
                          control = list(sliceAdaptFactorInterval = 20))
  coexmcmcConf$addSampler(target = c('phi1','betas[1,1]','betas[1,2]'),
                          type = "AF_slice",
                          control = list(sliceAdaptFactorInterval = 20))
  coexmcmcConf$addSampler(target = c('phi2','betas[2,2]','betas[2,1]'),
                          type = "AF_slice",
                          control = list(sliceAdaptFactorInterval = 20))
}#custom samplers
if (samplers == "RW_block") {
  coexmcmcConf$removeSamplers(c("alphs","betas","phi1","phi2","fert1","fert2"))
  # Add RW_block samplers, modifying adaptation behavior.
  coexmcmcConf$addSampler(target = c('fert1','alphs[1,1]','alphs[1,2]'),
                          type = "RW_block")
  coexmcmcConf$addSampler(target = c('fert2','alphs[2,2]','alphs[2,1]'),
                          type = "RW_block")
  coexmcmcConf$addSampler(target = c('phi1','betas[1,1]','betas[1,2]'),
                          type = "RW_block")
  coexmcmcConf$addSampler(target = c('phi2','betas[2,2]','betas[2,1]'),
                          type = "RW_block")
}#custom samplers
#Build the MCMC
coexmcmc  <-  buildMCMC(coexmcmcConf)
#Compile the  MCMC
ccoexmcmc  <-  compileNimble(coexmcmc, project = ccoexmodelIPM)
#Run the MCMC
#change niter and burn in for final versions if necessary
niter <- 15100
nburnin <- 100
thin <- 10
nchains <- 2
list.samples[[1]] <- runMCMC(ccoexmcmc,niter=niter,nburnin=nburnin,thin=thin,nchains=nchains,setSeed=T,inits = coexinits)
for (i in 2:length(list.simul)) {
  #set simulated data as data
  marray1j <- list.simul[[i]]$marray1j
  marray2j <- list.simul[[i]]$marray2j
  marray1a <- list.simul[[i]]$marray1a
  marray2a <- list.simul[[i]]$marray2a
  N1jobs <- list.simul[[i]]$N1jobs
  N1aobs <- list.simul[[i]]$N1aobs
  fledg1obs <- list.simul[[i]]$fledg1obs
  N2jobs <- list.simul[[i]]$N2jobs
  N2aobs <- list.simul[[i]]$N2aobs
  fledg2obs <- list.simul[[i]]$fledg2obs
  coexdata <- list(marray1j=marray1j,marray1a=marray1a,r1a=r1a,
                   N1jobs=N1jobs,N1aobs=N1aobs,fledg1obs=fledg1obs,
                   marray2j=marray2j,marray2a=marray2a,r2a=r2a,
                   N2jobs=N2jobs,N2aobs=N2aobs,fledg2obs=fledg2obs)
  #set data
  ccoexmodelIPM$setData(coexdata)
  #Run the MCMC
  #change niter and burn in for final versions if necessary
  list.samples[[i]] <- runMCMC(ccoexmcmc,niter=niter,nburnin=nburnin,thin=thin,nchains=nchains,inits=coexinits,setSeed=T)
  ##save posterior samples together with simulated data and values
  save(paramvalues, list.simul, coexconstants,list.samples, file = paste("data/data_coexistence_model_param",parameterset,samplers,nmarked,"juvmarked",nnests,"nests",nyears,"nyears",ddpriors,".Rdata",sep=""))
  print(i)
}
sessionInfo()