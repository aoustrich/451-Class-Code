outside <- read.table('outside.dat',header=TRUE)
library(nimble)
killprobCode <- nimbleCode({
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + btime*time[i]
  }
  b0 ~ dnorm(0,sd=25)
  btime ~ dnorm(0,sd=25)
})
y <- outside$response
time <- outside$time
N <- length(y)
lregConstants <- list(N=N)
lregData <- list(y=y,time=time)
logreg.out <- nimbleMCMC(code=killprobCode,
                         constants = lregConstants,
                         data=lregData,
                         nchains=5,niter=202000,
                         nburnin=2000,thin=100,
                         samplesAsCodaMCMC = TRUE,
                         summary = TRUE,WAIC = TRUE,
                         monitors=c('b0','btime'))
logreg.out$summary
logreg.out$WAIC
library(coda)
gelman.diag(logreg.out$samples)
allsamps <- as.matrix(logreg.out$samples)
raftery.diag(allsamps)
effectiveSize(allsamps)
autocorr.diag(as.mcmc(allsamps))
####  probability plot
ttt <- seq(.3,2.1,by=.05)
allsamps[1,]
bestb0 <- mean(allsamps[,1])
bestbtime <- mean(allsamps[,2])
ppp <- exp(bestb0+bestbtime*ttt)/(1+exp(bestb0+bestbtime*ttt))
plot(ttt,ppp,main=('Probability of Kill'),
     ylab='Probability',xlab='Time in Sec.',type='l')
##plot with intervals
slptime <- outer(allsamps[,2],ttt,'*')
dim(slptime)
ptest <- sapply(seq(1:ncol(slptime)), function(a) slptime[,a]+allsamps[,1])
dim(ptest)
ptestprob <- exp(ptest)/(1+exp(ptest))
dim(ptestprob)
tptestprob <- t(ptestprob)
plot(ttt,ppp,type='n',ylab='Probability',
     xlab='Time in Sec.',
main=('All Probability Lines and 95% Post. Prob. Ints.'),
ylim=c(0,1))
for(j in 1:10000) {lines(ttt,tptestprob[,j],col='lightblue')}
lines(ttt,ppp,type='l',col='black')
intest <- apply(ptestprob,2,quantile,
                c(.025,.975))
lines(ttt,intest[1,],col='red')
lines(ttt,intest[2,],col='red')
legend('topright',legend=c('All Lines','Best Line',
                            '95% PPI'),col=c('lightblue','black','red'),
        lty=c(1,1,1))
