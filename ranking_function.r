### Weighted Loss Function for Ranking by Position ###
## Cora Allen-Coleman Feb 2018 ##

## Q: for rank, must use r function sort. How can we fix this? OR is this fine?

## TODO an option for giving function a matrix of samples for each item nitems x samples

## TODO 3 outer product of the matrix to vectorize to replace double for loops (meeting) vectorizing apply outer product
## So far, only been tested with relatively simple bayesian models. allow for stan model OR matrix of parameter samples ##

## TODO loss for zero one loss

## TODO should there be autoscaling of original matrix to prevent numbers that are too small
## TODO add checks for invalid inputs

library(rstan)
library(clue)

### Ranking Function for Extracting Parameters and Ranking ### 

weight_loss_ranking <- function(model = NULL, sampleMatrix = NULL, loss = 2, parameter, f=identity, rankweights = rep(1, times = n), itemweights = rep(1, times = n)){
## DEPENDENCIES: rstan, clue
## PARAMETERS ##
  # model: a stan model
  # loss: an exponent indicating the loss function for ranking. 
    # options: 2=square, 1=absolute, zero TODO  
    # default is 2
  # parameter: parameter to rank, as created by stan model. Enter this as a string (with quotes).
  # f: scale for loss calculation. 
    # options: identity, rank (input R's sort function for now), logit TODO or any desired function
    # default is identity
  # rankweights: a vector of length equal to number of items to be ranked. Weights positions.
    # default is a vector of equal weights
  # itemweights: a vector of length equal to number of items to be ranked. Weights items.
    # default is a vector equal weights
  
  if (!is.null(sampleMatrix)){#checks for sample matrix
    i = sampleMatrix
  } else if (!is.null(model)){
    #extract samples (matrix i)
    i <- rstan::extract(model, pars=parameter)[[1]] 
  }
  #apply function/scale transformation to matrix i
  #sort transformed samples (matrix j)
  rho_i <- apply(i, 2, f)
  rho_j <- apply(rho_i, 1, sort) #Q: sort after scale transformation?
  
  #n = # items to be ranked
  n <- ncol(i)
  
  #calculate loss 
  LossRnk <- matrix(NA,n,n) #loss = mean(|rho(i) - rho(j)|^2)
  for (i in 1:n) {
    for (j in 1:n) {
      LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean(abs((rho_i[,i]-rho_j[j,]))^loss) #rankweights[j]*itemweights[i]*mean(abs((ranks[i,]-j))^loss) for rank position weighting. for both, just multiply
    }
  }
  return(solve_LSAP(LossRnk))
}

## Testing Function on Example Data ##
ranks <- weight_loss_ranking(rand_int_model, parameter = "p", loss = 2) #model case
#ranks <- weight_loss_ranking(sampleMatrix = i_samples, parameter = "p", loss = 2) #sample matrix case

## Ranked Data Frame ##
County <- raw_data0[,c(3)]
rankedDataFrame <- as.data.frame(County)
rankedDataFrame$p <- raw_data0[,4]/raw_data0[,5]*100
rankedDataFrame$rank <- as.integer(ranks)
library(dplyr); arrange(rankedDataFrame, rank)


##Example Data ##
raw_data0 <- read.csv("/Users/cora/git_repos/RankingMethods/data/LBW.csv", header = TRUE)
raw_data <- raw_data0[, c("County", "NumLBW", "NumBirths")]#subset data to only those used by stan
raw_data$County <- as.integer(as.factor(raw_data$County)) #set County column to numeric

## Stan Model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
data = list(
  J = nrow(raw_data),
  n = with(raw_data, NumBirths),
  count = with(raw_data, NumLBW),
  county = with(raw_data,as.integer(as.factor(County)))
)
## Create Model with Random Intercepts for Each County ##
rand_int_model <- stan(file="/Users/cora/git_repos/RankingMethods/randInt.stan",data=data, seed = 10)

## Weights ##
unequal <- rep(1, times = 21); unequal[4:9] <- 3

#LSAP might act weirdly with actual 0 weights (this is a problem with Louis)
#what we really want is orders of magnitude between weights
#these weights dont need to sum to one (to normalize, divide each by sum)
#c(1, e, e^2, e^3) then normalize if you want. TODO question: what should epsilon be? When do we have numerical stability problem

## Future Work Notes ##
#TODO bring in heuristic methods for LSAP look for papers by Louis' group 
# TODO question: does our 1, e, e^2 etc work better than 1, 1, 1, 0, 0, 0, (these zeros will all be tied). 
# Epsilon losses automatically breaks ties. Mostly want to show that it doesnt change answer about top 10 + ranking of bottom set will be better.
# Next question: do I really wanmt to have cliffs? Should it be smooth, gradual? How would we make this smooth? Are there various ways to try to get the good points of cliff functions.
# Can you make a smooth function that performs as well as cliffs?
# Is there a natural cliff? (page views on google?) Then is this related to threshold functions? e.g. i just care about my item, everyone else is less important
# smoothness might be desireable in situations where you see the whole list. (college rankings) 

#TODO idea for future: D3.js? this could be a good situation to use weights based on people's interests. Show different weight vectors.


##DONE: