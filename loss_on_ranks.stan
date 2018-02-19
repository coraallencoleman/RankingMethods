//loss on ranks

data
{
  int<lower=1> J; // number of counties
  int<lower=1> num[J]; // number of births
  int<lower=1> count[J]; // number low birth weight
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
  for ( i in 1:J )
  {
    p[i] = inv_logit(intercept + alpha[i]);
  }
}

model
{
  // prior distributions for parameters
  intercept ~ normal(0, 100); // intercept
  alpha[J] ~ normal(intercept, sigma); // county intercept effects
  sigma ~ cauchy(0,1); //st dev of county effect distribution

  // likelihood model
  count ~ binomial(num, p); //NumLBW is count, NumBirths is num
  alpha[J] ~ normal(alpha, sigma);
}
