data {
  int<lower=0> Ntmt;
  int<lower=0> Nctl;
  int<lower=0> N;
  vector[Ntmt] ytmt;
  vector[Nctl] yctl;
}
parameters {
  real muctl;
  real mutmt;
  real<lower=0> sigma;
}

model {
  muctl ~ normal(120,20);
  mutmt ~ normal(120,20);
  sigma ~ gamma(1.1,.1);
  yctl ~ normal(muctl, sigma);
  ytmt ~ normal(mutmt, sigma);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:Nctl) {
    // calculate the log likelihood during each draw?
    log_lik[i]=normal_lpdf(yctl[i] | muctl,sigma);
  }
  for (i in 1:Ntmt) {
    log_lik[i+20]=normal_lpdf(ytmt[i] | mutmt,sigma);
  } 
}
