library(nimble)
load('attract_dat.rdata')
years <- c(attract_dat$years_af,attract_dat$years_uf,attract_dat$years_am,
             attract_dat$years_um)
tmt <- rep(1:4,each=60)
newdata <- cbind(tmt,years)
newdata <- as.data.frame(newdata)
attractCode  <- nimbleCode({
  for (i in 1:(N*4)){
    years[i] ~ dpois(lambda[tmt[i]])
  }
  for (i in 1:4){
    lambda[i] ~ dgamma(1.1,.1)
  }
})
attractConsts <- list(N=60,tmt=newdata$tmt)
attractData <- list(years=newdata$years)
attract.out <- nimbleMCMC(
  code=attractCode,constants=attractConsts,data=attractData,
  niter = 5000, nburnin=1000, nchains=5,
  samplesAsCodaMCMC = TRUE,
  WAIC = TRUE,
  summary=TRUE,
  monitors = c('lambda')
)
library(coda)
gelman.diag(attract.out$samples)
allsamps <- as.matrix(attract.out$samples)
raftery.diag(allsamps)
effectiveSize(allsamps)
attract.out$summary
attract.out$WAIC
allsamps[1,]
lambdaAF <- allsamps[,1]
lambdaAM <- allsamps[,3]
lambdaUF <- allsamps[,2]
lambdaUM <- allsamps[,4]
interact <- (lambdaAF - lambdaUF - lambdaAM + lambdaUM)
mean(interact>0)
plot(density(interact,adjust=2))
plot(density(lambdaUM,adjust=2))
mean(lambdaUM>lambdaAF)