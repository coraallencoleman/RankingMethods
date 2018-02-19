### Loss Functions on the Ranking Scale ###

##Cora Allen-Coleman Feb 2018 ##

## for now, allows for only random intercept binomial multilevel bayesian model ##

library(rstan)

##Data set up ##
#Data
raw_data <- read.csv("/Users/cora/git_repos/RankingMethods/data/LBW.csv", header = TRUE)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
data = list(
  J = nrow(raw_data),
  num = with(raw_data, NumLBW),
  count = with(raw_data, NumBirths),
  county = with(raw_data,as.integer(as.factor(County)))
)

## Create Model with Random Intercepts for Each County ##
rand_int_model = stan(file="/Users/cora/git_repos/RankingMethods/loss_on_ranks.stan",data=raw_data)

#library(rethinking)
m1 <- map2stan(
  alist(
    NumLBW ~ dbinom(NumBirths, p) ,
    logit(p) <- a_county[County] ,
    a_county[County] ~ dnorm(a, sigma) ,
    a ~ dnorm(0, 10) ,
    sigma ~ dcauchy(0,1)
  ), data=raw_data, iter=4000, chains=4, WAIC=FALSE) #remove WAIC to speed up

## Get Samples From the Posterior for Each County ##
samples = 10000
post <- extract.samples(m1)
post$a_county #each of these is logit(p_i)
ranks <- apply(post$a_county, 1, rank)

## Posterior probabilities of being assigned every rank for each item ##
#summaries of that posterior distribution
apply(ranks,1,quantile)
#mean ranks for each county
apply(ranks,1,mean)
#optimal ranks over squared error loss
rank(apply(ranks,1,mean))


## Calculate Loss ##

# Squared Error Loss Rank
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
SqErrLossRnk <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    SqErrLossRnk[i,j] <- mean((ranks[i,]-j)^2)
  }
}