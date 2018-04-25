//example rand int model for ranking function binomial
  
data
{
  int<lower=1> N; // N number of cafes
  int<lower=1> sizeN[N]; // SIZE number of ConnectionAttempts
  int<lower=0> count[N]; // number SuccessfulConnections
  int<lower=1,upper=N> item[N]; //all N cafes
}

parameters
{
  real intercept; // intercept
  vector[N] alpha; // cafe intercept effects
  real<lower=0> sigma; // standard deviation of cafe effect distribution
                       // <lower=0> sets minimum value to zero
}

transformed parameters
{
  vector[N] p;
  for ( i in 1:N ){
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
  count ~ binomial(sizeN, p); //sizeN is number of attempts/tries
}
