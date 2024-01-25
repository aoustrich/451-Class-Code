library(rstan)
options(mc.cores = parallel::detectCores())  # a useful option
# for multi core machines

N <- 241
# f <- 59
# s <- 182
# the season long data is found in the object called y
y <- c(rep(1,182),rep(0,59))

# sleepy_dat <- list(y=y,N=N,s=s,f=f) # s and f are not needed
sleepy_dat <- list(y=y,N=N)

# this is our call to Stan
# fit (with default `thin`=1 and `warmup`=5000)
sleepy.fit <- stan(file='Aaron_sleepy.stan',
                   data=sleepy_dat,
                   seed=98293,
                   iter=10000,
                   chains=4) 


summary(sleepy.fit)
sleepy.fit #a look at our Stan fit
#  - `NUTS` (No U-Turn Sampler) is the default sampler
#  - `n_eff` (effective sample size) is the number of independent samples 
      # Fellingham wants n_eff >= 5,000 for each parameter
# - `Rhat` (Gelman-Rubin diagnostic) is the ratio of the variance of the posterior distribution to the mean of the posterior distribution
      # Should be around 1


sleepy.samples <- extract(sleepy.fit) # gets the posterior chains from the Stan run

# summary(sleepy.fit)

# names of the posterior values
names(sleepy.samples)  # theta and 'LogPosterior__'

theta <- sleepy.samples$theta         # the posterior chains for theta


dim(theta) 

plot(density(theta,adjust=2))

mean(theta)

quantile(theta,c(0.025,0.975))
