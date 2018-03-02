//example model for ranking function

data
{
  int<lower=1> J; // number of counties
  int<lower=1> n[J]; // number of births
  int<lower=0> count[J]; // number low birth weight
  int<lower=1,upper=J> county[J]; //all J counties
}

parameters
{
  real intercept; // intercept
  vector[J] alpha; // county intercept effects
  real<lower=0> sigma; // standard deviation of county effect distribution
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
  sigma ~ cauchy(0,1); //st dev of county effect distribution

  // likelihood model
  alpha ~ normal(0, sigma); // county intercept effects
  count ~ binomial(n, p); //NumLBW is count, NumBirths is num. Originally NumLBW ~ dbinom(NumBirths, p)
}
