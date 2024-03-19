library(nimble)
book <- read.table('BookCost.dat',header=TRUE)
pages <- book$Pages.X.
price <- book$Price.Y.
N <- length(price)
linregCode <- nimbleCode({
  for (i in 1:N){
    price[i] ~ dnorm(mu[i],sd=s)
    mu[i] <- intercept + slope*pages[i]
  }
  intercept ~ dunif(-100,500)
  slope ~ dunif(-5,15)
  s ~ dgamma(2,.05)
})
bookConst <- list(N=N,pages=pages)
bookData <- list(price=price)
books.out <- nimbleMCMC(constants = bookConst,
                        data = bookData,
                        code = linregCode,
                        niter = 50000,
                        nburnin = 10000,
                        thin=20,
                        nchains=5,
                        samplesAsCodaMCMC = TRUE,
                        summary = TRUE,
                        WAIC = TRUE,
                        monitors = c('intercept',
                                    'slope',
                                    's'))
library(coda)
gelman.diag(books.out$samples)
allsamps <- as.matrix(books.out$samples)
allsamps[1,]
raftery.diag(allsamps)
effectiveSize(allsamps)
plot(allsamps[,1],type='l')
plot(allsamps[,2],type='l')
plot(allsamps[,3],type='l')
books.out$summary
books.out$WAIC
# posterior predictive of lines
dim(allsamps)
allsamps[1,]
plot(pages,price)
abline(-3.632,0.1476)
for (i in 1:1000){abline(allsamps[i,1],allsamps[i,3],col='lightgray')}
abline(-3.632,0.1476)
plot(pages,price,ylim=c(-50,220))
xx <- seq(0,1100,by=50)
xx
# interval estimates
plot(xx,allsamps[1,1]+allsamps[1,3]*xx,type='l',ylim=c(-50,220),col='lightgray',xlab='pages',ylab='price')
for (i in 2:1000){abline(allsamps[i,1],allsamps[i,3],col='lightgray')}
test <- matrix(0,1000,23)
for (i in 1:1000){
  for (j in 1:23){
    test[i,j] <- allsamps[i,1]+allsamps[i,3]*xx[j]
  }
}
dim(test)
test1 <- apply(test,2,quantile,c(.025,.975))
dim(test1) 
lines(xx,test1[1,],lty=2,col='blue')
lines(xx,test1[2,],lty=2,col='blue')
abline(-3.632,0.1476)
points(pages,price)
testp <- matrix(0,10000,23)
for (i in 1:10000){
  for (j in 1:23){
    testp[i,j] <- rnorm(1,allsamps[i,1]+allsamps[i,3]*xx[j],allsamps[i,2])
  }
}
test2 <- apply(testp,2,quantile,c(.025,.975))
lines(xx,test2[1,],lty=2,col='red')
lines(xx,test2[2,],lty=2,col='red')
