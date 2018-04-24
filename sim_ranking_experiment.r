#Testing for WeightedLossRanking function
#Step 0: Load
library(rstan)
library(dplyr)
#AND run entire ranking_function.r file. This puts WeightedLossRanking from ranking_function.r into environment and adds packages
set.seed(10)

#STEP 1: Simulate Different Types of Data

## Binomial Random Intercept n = 100 ## EVEN GAPS change 
#matrix of dataframes?
my.names <- c("item", "p", "n")
replicate(4,data.frame())
gaps <- c(0.0001, 0.001, 0.01, 0.1)
for (gapsize in gaps){ #do by powers of ten
  even[i] <- data.frame(matrix((seq(from = 0, to = 1, by = gapsize))) #p
}

#TODO ask Ron if we should/how to vary gap size without varying n

even <- as.data.frame(matrix((seq(from = 1, to = 100, by = gapsize)), ncol = 1))
seq(from=1, to=100, length.out=100)
even <- as.data.frame(matrix((seq(from = 1, to = 100, by = 1)), ncol = 1))
colnames(even) <- c("item")
even$p <- seq(from = 0.01, to = 1, by = 0.01) # all unique p 
#get # correct measure
cafes$n <- c(rep(c(10, 100, 1000), times = 4)) #create lots of variation here
cafes$SuccessfulConnections <- rbinom(n = 12, size = cafes$n, cafes$p)
## sim data + model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sim_data = list(
  J = nrow(cafes), #should be 12
  n = with(cafes, n),
  count = with(cafes, SuccessfulConnections),
  item = with(cafes,as.integer(as.factor(item)))
)
#do this for all gap sizes

## Binomial Random Intercept n = 100 ## RANDOM GAPS
#try with bigger gaps, random unif on a range to have arbitrary gaps
#increase gaps until trivial. decrease until broken/impossible. 
#start with equal sample sizes. (vary one thing at a time) Weights may have an impact here.
#TODO think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior.

#add weights. How does this compare to not weighting at all?