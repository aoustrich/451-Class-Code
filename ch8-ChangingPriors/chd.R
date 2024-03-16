chd <- read.table('chd.dat',header=TRUE)
library(nimble)
chdCode <- nimbleCode({
  for (i in 1:N){
    x[i] ~ dbin(p[i],nr[i])
    p[i] ~ dbeta(1,4)
  }
})
chdConsts <- list(N=8)
chdData <- list(x=chd$CHD,
                nr=chd$nRisk)
chdaov.out <- nimbleMCMC(code=chdCode,
                         constants=chdConsts,
                         data = chdData,
                         nchains=4,niter=8000,
                         nburnin=2000,thin=2,
                         samplesAsCodaMCMC=TRUE,
                         summary=TRUE,WAIC=TRUE,
                         monitors = c('p'))
chdaov.out$summary
chdaov.out$WAIC
library(coda)
gelman.diag(chdaov.out$samples)
allsamps <- as.matrix(chdaov.out$samples)
raftery.diag(allsamps)
effectiveSize(allsamps)
allsamps[1,]
p1 <- allsamps[,1]
p2 <- allsamps[,2]
p3 <- allsamps[,3]
p4 <- allsamps[,4]
p5 <- allsamps[,5]
p6 <- allsamps[,6]
p7 <- allsamps[,7]
p8 <- allsamps[,8]
ccat <- (1/4)*c(1,1,1,1,-1,-1,-1,-1)
cabE <- (1/4)*c(1,1,-1,-1,1,1,-1,-1)
cage <- (1/4)*c(1,-1,1,-1,1,-1,1,-1)
catbyabE <- 16*ccat*cabE
catbyage <- 16*ccat*cage
abEbyage <- 16*cabE*cage
mean(t(catbyage)%*%t(allsamps)>0)
mean(t(catbyabE)%*%t(allsamps)>0)
mean(t(abEbyage)%*%t(allsamps)>0)
plot(density(t(ccat)%*%t(allsamps)))
plot(density(t(cabE)%*%t(allsamps)))
plot(density(t(cage)%*%t(allsamps)))
plot(density(p1,adjust=2),xlim=c(0.01,0.37))
lines(density(p8))
mean(p8>p1)
