library(rstan)
library(loo)

options(mc.cores = parallel::detectCores())


dat <- read.table('data/map.dat',header=TRUE)
# dat <- read.csv("~/Desktop/Data Science Major/stat451/In-class Code/data/map.dat", sep="")
yctl <- dat$response[1:20]
ytmt <- dat$response[21:30]
Nctl <- 20
Ntmt <- 10
map_dat <- list(ytmt=ytmt,yctl=yctl,Ntmt=Ntmt,Nctl=Nctl)

map5.fit <- stan(file='ch3-MAP/MAP.stan',
                 data=map_dat,
                 iter=5000,
                 chains=4,
                 seed = 472649)

map5.samples <- extract(map5.fit)
log_lik1 <- map5.samples$log_lik

waic(log_lik1)
loo(log_lik1)
