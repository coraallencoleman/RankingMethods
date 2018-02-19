### Loss Functions on the Ranking Scale ###

##Cora Allen-Coleman Feb 2018 ##

#Data
raw_data <- read.csv("/Users/cora/git_repos/RankingMethods/LBW.csv", header = TRUE)
m1_est <- read.csv("/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking/presentations/rankings_introduction/m1_estimates.csv", header = TRUE)


## Create Model with Random Intercepts for Each County ##
library(rstan)
library(rethinking)
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