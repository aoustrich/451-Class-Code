//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  // int<lower=0> s;  // the number of successes
  // int<lower=0> f;  // the number of failures
  int<lower=0> N;  // the total number of free throws
  int y[N];     // the data
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real <lower=0,upper=1> theta;  // the probability of making a free throw
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

model {
  theta ~ beta(9,5);   // our prior distribution
  for (i in 1:N) {
     y[i] ~ bernoulli(theta);  // the likelihood
  }
}
