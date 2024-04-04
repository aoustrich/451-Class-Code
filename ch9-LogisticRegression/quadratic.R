outside <- read.table('outside.dat',header=TRUE)
library(nimble)
killquadCode <- nimbleCode({
  for (i in 1:N){
    y[i] ~ dbin(p[i],1)
    logit(p[i]) <- b0 + b1*ztime[i] + b2*ztime2[i]
  }
  b0 ~ dnorm(0,sd=25)
  b1 ~ dnorm(0,sd=25)
  b2 ~ dnorm(0,sd=25)
})
y <- outside$response
time <- outside$time
ztime <- (time - mean(time))/sd(time)
ztime2 <- ztime^2
N <- length(y)
lregConstants <- list(N=N)
lregDataq <- list(y=y,ztime=ztime,ztime2=ztime2)
init1 <- list(b0=1,b2=1,b2=-2)
init2 <- list(b0=1.5,b1=-1,b2=-1)
init3 <- list(b0=1.1,b1=2,b2=-2)
init4 <- list(b0=-1,b1=3,b2=-1.5)
init5 <- list(b0=0,b1=2,b2=-2)
lregInitsq <- list(init1,init2,init3,init4,init5)
qlogreg.out <- nimbleMCMC(code=killquadCode,
                         constants = lregConstants,
                         data=lregDataq,
                         nchains=5,niter=202000,
                         nburnin=2000,thin=100,
                         inits = lregInitsq,
                         samplesAsCodaMCMC = TRUE,
                         summary = TRUE,WAIC = TRUE,
                         monitors=c('b0','b1','b2'))
qlogreg.out$summary
qlogreg.out$WAIC
library(coda)
gelman.diag(qlogreg.out$samples)
allsampsq <- as.matrix(qlogreg.out$samples)
raftery.diag(allsampsq)
effectiveSize(allsampsq)
# covariates centered linear model only
killprobCodez <- nimbleCode({
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + b1*ztime[i]
  }
  b0 ~ dnorm(0,sd=25)
  b1 ~ dnorm(0,sd=25)
})
lregConstants <- list(N=N)
lregDataz <- list(y=y,ztime=ztime)
init1 <- list(b0=1,b1=-1)
init2 <- list(b0=1.5,b1=-1)
init3 <- list(b0=1.1,b1=-2)
init4 <- list(b0=-1,b1=-1)
init5 <- list(b0=0,b1=-2)
lregInitsz <- list(init2,init2,init3,init4,init5)
logregz.out <- nimbleMCMC(code=killprobCodez,
                         constants = lregConstants,
                         data=lregDataz,
                         inits = lregInitsz,
                         nchains=5,niter=202000,
                         nburnin=2000,thin=100,
                         samplesAsCodaMCMC = TRUE,
                         summary = TRUE,WAIC = TRUE,
                         monitors=c('b0','b1'))
logregz.out$summary
logregz.out$WAIC
library(coda)
gelman.diag(logregz.out$samples)
allsampsz <- as.matrix(logregz.out$samples)
raftery.diag(allsampsz)
effectiveSize(allsampsz)
####  probability plot
ttt <- seq(-3,3,by=.05)
bestb0 <- mean(allsampsq[,1])
bestbtime <- mean(allsampsq[,2])
bestbtime2 <- mean(allsampsq[,3])
ppp <- exp(bestb0+bestbtime*ttt+bestbtime2*ttt^2)/(1+exp(bestb0+bestbtime*ttt+bestbtime2*ttt^2))
plot(ttt,ppp,main=('Probability of Kill'),
     ylab='Probability',xlab='Time in Sec.',type='l')
##plot with intervals
slptime <- outer(allsampsq[,2],ttt,'*')
slptime2 <- outer(allsampsq[,3],ttt^2,'*')
ptest <- sapply(seq(1:ncol(slptime)), function(a) slptime[,a]+slptime2[,a]+allsampsq[,1])
ptestprob <- exp(ptest)/(1+exp(ptest))
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

