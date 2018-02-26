### Weighted Loss Function for Ranking by Position ###
## Cora Allen-Coleman Feb 2018 ##

## TODO put in defaults for all inputs
## TODO as much flexiblity in f and loss as possible
## TODO add rank weights i.e. function(model, loss, parameter, f=functionscale, rankweights, itemweights){}
## TODO add item weights
## TODO an option for giving function a matrix of samples for each item nitems x samples
## TODO So far, only been tested with relatively simple bayesian models. allow for stan model OR matrix of parameter samples ##
## TODO Nested for loops faster by vectorizing apply outer product

## TODO should there be autoscaling of original matrix to prevent numbers that are too small

library(rstan)
library(clue)

### Ranking Function for Extracting Parameters and Ranking ### 
rank_on_weighted_loss <- function(model, loss, parameter, f=functionscale){
  # dependencies: rstan, clue
  ## parameters ##
  # model: a stan model
  # loss: a loss function for ranking. Options: square, absolute, zero. allow them to just give power for loss 
    #If input power, will have to use absolute value instead of parentheses)
  # parameter: parameter to rank, as created by stan model (as string)
  # f=functionscale: scale for loss calculation. Options: parameterscale, rank. "identity, rank, logit" set to input a function here
  # could do any monotone function for rho. For rank, super simple
  # rankweight: a vector of length equal to number of items to be ranked, ranks on items
  # itemweight: 
  # set defaults
  if (scale == "parameterscale"){
    scale <- sort 
    #1 if on parameter scale: change to loss calc. for optimal: should always be rho(i) - rho(j)
    i <- rstan::extract(model, pars=parameter)[[1]] 
    j <- apply(rstan::extract(model, pars=parameter)[[1]], 1, sort)
    
    #2 get matrices apply transformation to matrices
    rho_i <- logit(i)
    rho_j <- logit(j) 
    
  } else if (scale != "rank"){
    return("error: enter valid scaleForRanking")
  }
  ranks <- apply(rstan::extract(model, pars=parameter)[[1]], 1, scale)
  #TODO 3 outer product of hte matrix to vectorize to replace double for loops
  LossRnk <- matrix(NA,nrow(ranks),nrow(ranks))
  if (loss == "square"){
    for (i in 1:21) {
      for (j in 1:21) {
        #LossRnk[i,j] <- weight[i]*mean((ranks[i,]-j)^2)
        #i is county, j is rank position
        #for both, just multiply
        LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean((ranks[i,]-j)^2) #for rank position weighting
      }
    }
  }
  else if(loss == "absolute"){
    for (i in 1:21) {
      for (j in 1:21) {
        LossRnk[i,j] <- mean(abs(ranks[i,]-j))
      }
    }
  }
  else if(loss == "zero"){
    for (i in 1:21) {
      for (j in 1:21) { #TODO calc n
        LossRnk[i,j] <- mean(ranks[i,]!=j)
      }
    }
  } else {
    return("error: loss function not recognized")
  }
  return(solve_LSAP(LossRnk))
}


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
w_equal <- rep(1/21, times = 21)
w_unequal <- rep(1/17, times = 21); w_unequal[c(1:4)] <- .25 #LSAP might be weird with actual 0 
#(this is a problem with Louis)
#what we really want is orders of magnitude between weights
#these weights dont need to sum to one (to normalize, divide each by sum)
#c(1, e, e^2, e^3) then normalize if you want. TODO question: what should epsilon be? When do we have numerical stability problem

## Testing Function on Example Data ##
ranks <- rank_on_weighted_loss(rand_int_model, "square", "p", "rank", w_unequal);ranks

## Ranked Data Frame ##
County <- raw_data0[,c(3)]
rankedDataFrame <- as.data.frame(County)
rankedDataFrame$p <- raw_data0[,4]/raw_data0[,5]*100
rankedDataFrame$rank <- as.integer(ranks)
library(dplyr); arrange(rankedDataFrame, rank)

#TODO bring in heuristic methods for LSAP look for papers by Louis' group 
# TODO question: does our 1, e, e^2 etc work better than 1, 1, 1, 0, 0, 0, (these zeros will all be tied). 
# Epsilon losses automatically breaks ties. Mostly want to show that it doesnt change answer about top 10 + ranking of bottom set will be better.
# Next question: do I really wanmt to have cliffs? Should it be smooth, gradual? How would we make this smooth? Are there various ways to try to get the good points of cliff functions.
# Can you make a smooth function that performs as well as cliffs?
# Is there a natural cliff? (page views on google?) Then is this related to threshold functions? e.g. i just care about my item, everyone else is less important
# smoothness might be desireable in situations where you see the whole list. (college rankings) 

#TODO idea for future: D3.js? this could be a good situation to use weights based on people's interests. Show different weight vectors.