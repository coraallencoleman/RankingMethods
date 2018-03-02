data
{
  int<lower=1> N; // number of students
  int<lower=1> J; // number of schools
  real y[N]; // math scores at age 11
  int<lower=1,upper=J> school[N];
}

parameters
{
  vector[1] beta; // intercept and slope
  vector[J] alpha; // school intercept effects
  real<lower=0> sigma; // standard deviation of student effect distribution
                       // <lower=0> sets minimum value to zero
  real<lower=0> sigmaA; // standard deviation of school effect distribution
}

transformed parameters
{
  vector[N] mu;
  for ( i in 1:N )
  {
    mu[i] = beta[1] + alpha[school[i]];
  }
}

model
{
  // prior distributions for parameters
  beta[1] ~ normal(0,50);
  sigma ~ cauchy(0,5);
  sigmaA ~ cauchy(0,5);
  
  // likelihood model
  alpha ~ normal(0,sigmaA);
  y ~ normal(mu,sigma);
}
