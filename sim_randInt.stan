//example rand int model for ranking function n = 12 cafes on internet quality

data
{
  int<lower=1> J; // number of cafes
  int<lower=1> n[J]; // number of ConnectionAttempts
  int<lower=0> count[J]; // number SuccessfulConnections
  int<lower=1,upper=J> cafe[J]; //all J cafes
}

parameters
{
  real intercept; // intercept
  vector[J] alpha; // cafe intercept effects
  real<lower=0> sigma; // standard deviation of cafe effect distribution
                       // <lower=0> sets minimum value to zero
}

transformed parameters
{
  vector[J] p;
  for ( i in 1:J ){
    p[i] = inv_logit(intercept + alpha[i]);
  }
}

model
{
  // prior distributions for parameters
  intercept ~ normal(0, 5); // intercept
  sigma ~ cauchy(0,1); //st dev of cafe effect distribution

  // likelihood model
  alpha ~ normal(0, sigma); // county intercept effects
  count ~ binomial(n, p); //attemps is count, successes is num
}
