##Model to simulate and fit data for a 2 species competition model based on the theoretical model from Bardon&Barraquand 2023
#we decide to fix the maturation rate gamma=1 for both species for simplicity, and because in practice probably few real systems would use such configuration
##built from Paquet&Barraquand 2023 code of a predator-prey model, so check carefully for mistakes.
#here we consider the same sample size as for the previous prey species (higher sample size).
setwd("/home/matpaquet/Documents/IPM_competition_2_species_age_structured")
library(nimble)
library(mcmcplots)
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
#Initial population sizes
N1jt1 <- N1jinit <- 100
N1at1 <- N1ainit <- 100
N2jt1 <- N2jinit <- 100
N2at1 <- N2ainit <- 100
#sample sizes
nyears <- 30
nmarked <- 100
#Number of nestlings marked every year
r1j <- rep(nmarked, (nyears - 1))
r2j <- rep(nmarked, (nyears - 1))
#number of nests monitored
fledg.sample.1 <- rep(50, nyears)
fledg.sample.2 <- rep(50, nyears)
#Population size
N1obs <- rep(NA, nyears)
N2obs <- rep(NA, nyears)
#empty data
marray1j <- matrix(NA, nyears-1, nyears)
marray2j <- matrix(NA, nyears-1, nyears)
fledg1obs <- rep(NA, nyears)
fledg2obs <- rep(NA, nyears)
#write the model code
coexcode  <-  nimbleCode({
  # Likelihood for  count data (state-space model) 
  for (t in 1:nyears) {
    #Observation process
    N1obs[t] ~ dlnorm(log(N1[t]), sd=0.1)
    N2obs[t] ~ dlnorm(log(N2[t]), sd=0.1)
    # System process
    N1[t] <- N1j[t] + N1a[t]
    N2[t] <- N2j[t] + N2a[t]
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
#see later if we need to center the abundances
  }#t
  #think about the priors
  fert1 ~ dunif(1,100) #should we put a prior on the log?
  fert2 ~ dunif(1,100)
  #Likelihood for capture-recapture data: CJS model (2 age classes)
  # Multinomial likelihood
  for (t in 1:(nyears-1)) {
    marray1j[t,1:nyears] ~ dmulti(pr1j[t,1:nyears], r1j[t])
    marray2j[t,1:nyears] ~ dmulti(pr2j[t,1:nyears], r2j[t])
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
  p1 ~ dunif(0,1)
  p2 ~ dunif(0,1)
  s1a ~ dunif(0,1)
  s2a ~ dunif(0,1)
  #relationship for vital rate parameters
  for (t in 1:(nyears-1)) {
        #interspecific dd on juvenile prey survival
        svar1[t] <- phi1/(1+betas[1,1]*N1a[t]+betas[1,2]*N2a[t])
        svar2[t] <- phi2/(1+betas[2,1]*N1a[t]+betas[2,2]*N2a[t])
  }#t
    phi1 ~ dunif(0,1)
    phi2 ~ dunif(0,1)
    #think about priors
    for (a in 1:2) {
      for (b in 1:2) {
        alphs[a,b] ~ dunif(0,1)
        betas[a,b] ~ dunif(0,1)
      }#b
    }#a
  
})
#change abundances at equilibrium
coexconstants  <-  list(nyears=nyears, r1j=r1j, r2j=r2j, 
                      fledg.sample.2=fledg.sample.2, fledg.sample.1=fledg.sample.1)
#Build the model
coexmodel  <-  nimbleModel(coexcode,
                         constants = coexconstants)
#Set data and initial values
coexmodel$setData(list(marray1j=marray1j, N1obs=N1obs, 
                     fledg1obs=fledg1obs, marray2j=marray2j,
                     N2obs=N2obs, fledg2obs=fledg2obs))
coexinits <- list(fert1=fert1,fert2=fert2,phi1=phi1,phi2=phi2,s1a=s1a,s2a=s2a,
                alphs=alphs, betas=betas,
                p1=p1, p2=p2, N1jt1=N1jt1, N2jt1=N2jt1, 
                N1at1=N1at1, N2at1=N2at1)
nodesToSim  <-  coexmodel$getDependencies(c("alphs","betas","p1","p2","phi1",
                                              "phi2","fert1","fert2",
                                              "N1jt1","N2jt1","N1at1","N2at1"),
                                            self = F, downstream = T)
coexmodel$setInits(coexinits)
#Compile the model 
ccoexmodel <- compileNimble(coexmodel) 
##simulate
list.simul <- list()
###simulate 100 datasets and run IPM on each
for (i in 1:100) {
  set.seed(i)
  ccoexmodel$simulate(nodesToSim)
  list.simul[[i]] <- list(N1j=ccoexmodel$N1j,N1a=ccoexmodel$N1a,
                          N2j=ccoexmodel$N2j,N2a=ccoexmodel$N2a,
                          N1=ccoexmodel$N1,N2=ccoexmodel$N2,
                          N1obs=ccoexmodel$N1obs, N2obs=ccoexmodel$N2obs,
                          svar1=ccoexmodel$svar1,svar2=ccoexmodel$svar2,
                          fledg.rate.1=ccoexmodel$fledg.rate.1,
                          fledg.rate.2=ccoexmodel$fledg.rate.2,
                          fledg1obs=ccoexmodel$fledg1obs,
                          fledg2obs=ccoexmodel$fledg2obs,
                          marray1j=ccoexmodel$marray1j,
                          marray2j=ccoexmodel$marray2j)
  print(i)  
}
save(list.simul, file="simul_coexistence_model.Rdata")
#this code bit is just to check that all populations persist
#until the end of the time series and check the mean pop size then
nyears.tot <- length(list.simul[[1]][[1]])
n.simul <- length(list.simul)
N.simul.1 <- matrix(NA,n.simul,nyears.tot)
N.simul.2 <- matrix(NA,n.simul,nyears.tot)
N.simul.obs.1<- matrix(NA,n.simul,nyears.tot)
N.simul.obs.2 <- matrix(NA,n.simul,nyears.tot)
for (i in 1:n.simul) {
  for (t in 1:nyears.tot) {
    N.simul.1[i,t] <- list.simul[[i]]$N1[t]
    N.simul.2[i,t] <- list.simul[[i]]$N2[t]
    N.simul.obs.1[i,t] <- list.simul[[i]]$N1obs[t]
    N.simul.obs.2[i,t] <- list.simul[[i]]$N2obs[t]
  }
}
min(N.simul.1[,nyears])
min(N.simul.2[,nyears])
mean(N.simul.1[,nyears])
mean(N.simul.2[,nyears])
mean(N.simul.1[,nyears]==0)
mean(N.simul.2[,nyears]==0)

#plot one pair of predator-prey abundance time series as illustration (100 chosen randomly)
plot(1:nyears.tot, N.simul.1[40,], type='l', lwd=3, ylim=c(0,max(N.simul.1[40,],
                                                                 N.simul.2[40,],
                                                                 N.simul.obs.1[40,],
                                                                 N.simul.obs.2[40,])),
     col='red', ylab='population size', xlab='years')
lines(1:nyears.tot, N.simul.2[40,], type='l', lwd=3, col='blue')
lines(1:nyears.tot, N.simul.obs.1[40,], type='p', lwd=3, col='red')
lines(1:nyears.tot, N.simul.obs.2[40,], type='p', lwd=3, col='blue')
#####fit an IPM
#in case we later want to fit the model on a subset of the time series
#(e.g. to reach stable population structures)
nyears.start <- 1
list.samples <- list()
for (i in 1) {
  #set simulated data as data
  #start from the 21st year
  marray1j <- matrix(NA,(nyears-1),nyears)
  marray2j <- matrix(NA,(nyears-1),nyears)
  marray1j[1:(nyears-1),1:(nyears-1)] <- list.simul[[i]]$marray1j[nyears.start:(nyears.start + nyears-2),
                                                                      nyears.start:(nyears.start + nyears-2)]
  marray1j[,nyears] <- list.simul[[i]]$marray1j[nyears.start:(nyears.start + nyears-2),
                                                    (nyears.start + nyears-1):dim(list.simul[[i]]$marray1j)[2]]
  marray2j[1:(nyears-1),1:(nyears-1)] <- list.simul[[i]]$marray2j[nyears.start:(nyears.start + nyears-2),
                                                                      nyears.start:(nyears.start + nyears-2)]
  marray2j[,nyears] <- list.simul[[i]]$marray2j[nyears.start:(nyears.start + nyears-2),
                                                    (nyears.start + nyears-1):dim(list.simul[[i]]$marray2j)[2]]
  N1obs <- list.simul[[i]]$N1obs[nyears.start:(nyears.start + nyears-1)]
  fledg1obs <- list.simul[[i]]$fledg1obs[nyears.start:(nyears.start + nyears-1)]
  N2obs <- list.simul[[i]]$N2obs[nyears.start:(nyears.start + nyears-1)]
  fledg2obs <- list.simul[[i]]$fledg2obs[nyears.start:(nyears.start + nyears-1)]
}
coexconstants <- list(nyears=nyears,
                    r1j=r1j[nyears.start:(nyears.start+nyears-2)],
                    r2j=r2j[nyears.start:(nyears.start+nyears-2)],
                    fledg.sample.2=fledg.sample.2[nyears.start:(nyears.start+nyears-1)],
                    fledg.sample.1=fledg.sample.1[nyears.start:(nyears.start+nyears-1)],
                    N1jinit=N1jinit, N1ainit=N1ainit, N2jinit=N2jinit, N2ainit=N2ainit)#define as data if initial pop size differs among simulations
coexdata <- list(marray1j=marray1j,N1obs=N1obs,fledg1obs=fledg1obs,
               marray2j=marray2j,N2obs=N2obs,fledg2obs=fledg2obs)
#Build the model for the IPM
coexmodelIPM  <-  nimbleModel(coexcode,
                            constants = coexconstants,data=coexdata,inits = coexinits)
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
  #Build the MCMC
coexmcmc  <-  buildMCMC(coexmcmcConf)
#Compile the  MCMC
ccoexmcmc  <-  compileNimble(coexmcmc, project = ccoexmodelIPM)
#Run the MCMC
#change niter and burn in for final versions if necessary
list.samples[[1]] <- runMCMC(ccoexmcmc,niter=30100,nburnin=100,thin=20,nchains=2,setSeed=T)
##save posterior samples
save(list.samples, file="samples_coexistence_model.Rdata")
for (i in 2:100) {
  #set simulated data as data
  #start from the 21st year
  marray1j <- matrix(NA,(nyears-1),nyears)
  marray2j <- matrix(NA,(nyears-1),nyears)
  marray1j[1:(nyears-1),1:(nyears-1)] <- list.simul[[i]]$marray1j[nyears.start:(nyears.start + nyears-2),
                                                                  nyears.start:(nyears.start + nyears-2)]
  marray1j[,nyears] <- list.simul[[i]]$marray1j[nyears.start:(nyears.start + nyears-2),
                                                (nyears.start + nyears-1):dim(list.simul[[i]]$marray1j)[2]]
  marray2j[1:(nyears-1),1:(nyears-1)] <- list.simul[[i]]$marray2j[nyears.start:(nyears.start + nyears-2),
                                                                  nyears.start:(nyears.start + nyears-2)]
  marray2j[,nyears] <- list.simul[[i]]$marray2j[nyears.start:(nyears.start + nyears-2),
                                                (nyears.start + nyears-1):dim(list.simul[[i]]$marray2j)[2]]
  N1obs <- list.simul[[i]]$N1obs[nyears.start:(nyears.start + nyears-1)]
  fledg1obs <- list.simul[[i]]$fledg1obs[nyears.start:(nyears.start + nyears-1)]
  N2obs <- list.simul[[i]]$N2obs[nyears.start:(nyears.start + nyears-1)]
  fledg2obs <- list.simul[[i]]$fledg2obs[nyears.start:(nyears.start + nyears-1)]
  coexdata <- list(marray1j=marray1j,N1obs=N1obs,fledg1obs=fledg1obs,
                 marray2j=marray2j,N2obs=N2obs,fledg2obs=fledg2obs)
  #set data
  ccoexmodelIPM$setData(coexdata)
  ccoexmodelIPM$setInits(coexinits)
  #Run the MCMC
  #change niter and burn in for final versions if necessary
  list.samples[[i]] <- runMCMC(ccoexmcmc,niter=30100,nburnin=100,thin=20,nchains=2,setSeed=T)
  ##save posterior samples
  save(list.samples, file="samples_coexistence_model.Rdata")
  print(i)
}
