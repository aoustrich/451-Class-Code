library(nimble)
probitCode <- nimbleCode({
  for (i in 1:N){
    y[i] ~ dbin(p[i],1)
    p[i] <-phi(b0 + btime*time[i]) 
  }
  b0 ~ dnorm(0,sd=25)
  btime ~ dnorm(0,sd=25)
})

outside <- read.table('outside.dat',header=TRUE)
y <- outside$response
time <- outside$time
N <- length(y)
probConstants <- list(N=N)
probData <- list(y=y,time=time)
init1 <- list(b0=1,btime=-1)
init2 <- list(b0=1.5,btime=-1)
init3 <- list(b0=1.1,btime=-2)
init4 <- list(b0=-1,btime=-1)
init5 <- list(b0=0,btime=-2)
lregInitsz <- list(init2,init2,init3,init4,init5)
probit.out <- nimbleMCMC(code=probitCode,
                         constants = lregConstants,
                         data=lregData,
                         inits=lregInitsz,
                         nchains=5,niter=202000,
                         nburnin=2000,thin=100,
                         samplesAsCodaMCMC = TRUE,
                         summary = TRUE,WAIC = TRUE,
                         monitors=c('b0','btime'))
probit.out$summary
probit.out$WAIC
library(coda)
gelman.diag(probit.out$samples)
allsampsp <- as.matrix(probit.out$samples)
raftery.diag(allsampsp)
effectiveSize(allsampsp)

