#Testing for WeightedLossRanking function
#TODO ask Ron if we should/how to vary gap size without varying n

#Step 0: Load
library(rstan)
library(dplyr)
#AND run entire ranking_function.r file. This puts WeightedLossRanking from ranking_function.r into environment and adds packages
set.seed(10)

#STEP 1: Simulate Different Types of Data
## Binomial Random Intercept n = 100 ## EVEN GAPS change 
#3 dim array with 4 gap size matrices, each with 5 columns and N rows (max = 10001)
even <- array(data = NA, dim=c(4,5,10001))
gaps <- c(0.0001, 0.001, 0.01, 0.1)

for (i in 1:length(gaps)){
  N = length(seq(from = 0, to = 1, by = gaps[i]))
  even[i, 1, 1:N] <- seq(from = 1, to = N, by = 1) #ITEM
  even[i, 2, 1:N] <- seq(from = 0, to = 1, by = gaps[i]) #P
  even[i, 3, 1:N] <- rep(as.integer(100),times = N) #SIZE
  even[i, 4, 1:N] <- rbinom(n = N, size = even[i, 3, 1:N], even[i, 2, 1:N]) #SIM SUCCESSES
}

## sim data + model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sim_data = list(
  N = N, #N or numRows
  item = even[1, 1, 1:N], #ITEM ID
  sizeN = as.integer(even[1, 3, 1:N]), #same as cafes$n #SIZE
  count = as.integer(even[1, 4, 1:N]) #SIM SUCCESSES
)
sim_rand_int_model_1 <- stan(file="/Users/cora/git_repos/RankingMethods/sim_randInt.stan",data=sim_data, seed = 10)

#get posterior means from stan model
#get credible intervals from stan model
## Cafe Ranked Data Frame Output ##
sim_ranks <- WeightedLossRanking(model = sim_rand_int_model, parameter = "p", f = rank, loss = 2, lossTotal = TRUE)
rankedCafes <- as.data.frame(cafes)
rankedCafes$p <- cafes$p*100
rankedCafes$rank <- as.integer(sim_ranks) #this ranks lowest to highest
arrange(rankedCafes, desc(rank))



#uneven gap size TODO using unif

#testing over variation in N


## Binomial Random Intercept n = 100 ## RANDOM GAPS
#try with bigger gaps, random unif on a range to have arbitrary gaps
#increase gaps until trivial. decrease until broken/impossible. 
#start with equal sample sizes. (vary one thing at a time) Weights may have an impact here.
#TODO think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior.

#TODO add weights. How does this compare to not weighting at all?