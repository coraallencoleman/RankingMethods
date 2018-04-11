#MCMC simple rank from Berger & Deely 1988 paper on ranking methods

## Bayesian approach, with a selection of different priors: exchangable (), nonexchangeable(), informative (), noninformative ()
##observed batting averages = sample proportions from binomial distributions
##The observed batting averages are treated as sample proportions from binomial distributions with parameters:
#theta_i = true probability of a hit for the given player
##want to select the best hitter from the group (largest theta_i)

## GOAL: calculate the posterior probability that each theta_i is the largest ##

#i = # of players (# of items to rank)
i <- 12 #or 600 on p 9?
