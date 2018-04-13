#Testing for WeightedLossRanking function
#Step 0: Load
library(rstan)
library(dplyr)
#run entire ranking_function.r file. This puts WeightedLossRanking from ranking_function.r into environment and adds packages
set.seed(10)

## Possible Weights ##
unequal <- rep(1, times = 21); unequal[4:9] <- 3