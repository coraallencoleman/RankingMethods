### Weighted Loss Function for Ranking by Position ###
## Cora Allen-Coleman Spring 2018 ##

library(rstan)
library(clue)

### Ranking Function for Extracting Parameters and Ranking ### 

WeightedLossRanking <- function(model = NULL, parameter = NULL, sampleMatrix = NULL, loss = 2,  f=identity, 
                                rankweights = rep(1, times = n), itemweights = rep(1, times = n)){
# Computes optimal ranking for a list of estimates
#   
# Args:
#   model: a stan model for the estimates
#   parameter: parameter to rank, as a string. Only necessary if inputting model rather than sampleMatrix.
#   sampleMatrix: a matrix of samples. dim = n samples by n items
#   loss: an exponent indicating the loss function for ranking. options: 2=square, 1=absolute, 0=zero
#   f: scale for loss calculation. options: identity, rank (input R's sort function for now) TODO
#   rankweights: a vector of length equal to number of items to be ranked. Weights positions.
#   itemweights: a vector of length equal to number of items to be ranked. Weights items.
#
# Returns:
#   optimal ranking for a list of estimates
  
# Dependencies: rstan, clue
  
  if (!is.null(sampleMatrix)){ #checks for sampleMatrix
    i = sampleMatrix
  } else if (!is.null(model)){ #checks for model
    i <- rstan::extract(model, pars=parameter)[[1]] #extract samples from model
  }
  rho_i <- apply(i, 2, f) #apply function/scale transformation to matrix i
  rho_j <- apply(rho_i, 1, sort) #sort transformed samples (matrix j)
  
  n <- ncol(i) #n = # items to be ranked
  
  if (loss == 0){ #zero one loss case
    LossRnk <- matrix(NA,n,n)
    for (i in 1:n) {
      for (j in 1:n) {
        LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean(rho_i[,i]!=rho_j[j,])
      }
    }
    return(solve_LSAP(LossRnk))
  } else{ #other loss cases
    LossRnk <- matrix(NA,n,n)
    for (i in 1:n) {
      for (j in 1:n) {
        LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean(abs((rho_i[,i]-rho_j[j,]))^loss)
      }
    }
    return(solve_LSAP(LossRnk))
  }
}

## Testing Function on Example Data (below) ##
ranks <- weight_loss_ranking(rand_int_model, parameter = "p", loss = 2) #model case
#ranks <- weight_loss_ranking(sampleMatrix = i_samples, parameter = "p", loss = 2) #sample matrix case

## Ranked Data Frame Output ##
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

## sampleMatrix input ##
i_samples <- rstan::extract(model, pars=parameter)[[1]] 

## Possible Weights ##
unequal <- rep(1, times = 21); unequal[4:9] <- 3

## Future Work Notes ##
## TODO 3 outer product of the matrix to vectorize to replace double for loops. vectorizing apply outer product
## TODO loss for zero one loss
## TODO Q: for rank, must use r function sort. How can we fix this? OR is this fine?
## TODO should there be autoscaling of original matrix to prevent numbers that are too small?
## TODO add checks for invalid inputs

## TODO Add heuristic methods for LSAP + compare. Look for papers by Louis' group 

## TODO Compare weighting strategies.
##  Q: what should epsilon be? When do we have numerical stability problems?
##  LSAP might act weirdly with actual 0 weights (this is a problem with Louis)
##  what we really want is orders of magnitude between weights. (weights dont need to sum to one. To normalize, divide each by sum)
##  e.g. c(1, e, e^2, e^3) then normalize if you want. TODO question: what should epsilon be? When do we have numerical stability problem
##  Q: does our 1, e, e^2 etc work better than 1, 1, 1, 0, 0, 0, (these zeros will all be tied). 
##  Epsilon losses automatically breaks ties. Mostly want to show that it doesnt change answer about top 10 + ranking of bottom set will be better.
##  Q: do I really want to have cliffs? Should it be smooth, gradual? How would we make this smooth? Are there various ways to try to get the good points of cliff functions.
##  Can you make a smooth function that performs as well as cliffs? Is there a natural cliff? (page views on google?) 
##  Then is this related to threshold functions? e.g. i just care about my item, everyone else is less important
##  Smoothness might be desireable in situations where you see the whole list. (college rankings) 

## TODO create visualizations: D3.js? this could be a good situation to use weights based on people's interests. Show different weight vectors.

