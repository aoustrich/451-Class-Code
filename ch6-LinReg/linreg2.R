library(nimble)
book <- read.table('BookCost.dat',header=TRUE)
pages <- book$Pages.X.
price <- book$Price.Y.
N <- length(price)
linreg2Code <- nimbleCode({
  for (i in 1:N){
    price[i] ~ dnorm(mu[i],sd=s[i])
    mu[i] <- intercept + slope*pages[i]
    s[i] <- sqrt(exp(b0 + b1*pages[i]))
  }
  intercept ~ dunif(-100,500)
  slope ~ dunif(-5,15)
  b0 ~ dnorm(0,100)
  b1 ~ dnorm(0,5)
})
bookConst <- list(N=N,pages=pages)
bookData <- list(price=price)
books2.out <- nimbleMCMC(constants = bookConst,
                        data = bookData,
                        code = linreg2Code,
                        niter = 50000,
                        nburnin = 10000,
                        thin=20,
                        nchains=5,
                        samplesAsCodaMCMC = TRUE,
                        summary = TRUE,
                        WAIC = TRUE,
                        monitors = c('intercept',
                                     'slope',
                                     'b0','b1'))
library(coda)
gelman.diag(books2.out$samples)
allsamps2 <- as.matrix(books2.out$samples)
allsamps2[1,]
raftery.diag(allsamps2)
effectiveSize(allsamps2)
plot(allsamps2[,1],type='l')
plot(allsamps2[,2],type='l')
plot(allsamps2[,3],type='l')
plot(allsamps2[,4],type='l')
books2.out$summary  
books2.out$WAIC
for(i in 1:1000){abline(allsamps2[i,3],allsamps2[i,4])}
