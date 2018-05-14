### Weighted Loss Function for Ranking by Position ###

## Cora Allen-Coleman Spring 2018 ##

  # Includes:
    # Weighted Loss Ranking Function
    # Weight Creation Function


library(rstan)
library(clue)

### Ranking Function for Extracting Parameters and Ranking ### 

WeightedLossRanking <- function(model = NULL, sampleMatrix = NULL, parameter = NULL, loss = 2,  f=identity, 
                                rankweights = rep(1, times = n), itemweights = rep(1, times = n), lossTotal = FALSE){
# Computes optimal ranking for a list of estimates
#   
# Args:
#   model: a stan model for the estimates
#   sampleMatrix: a matrix of samples. dim = n samples by n items. Result of PostSamples function
#   parameter: parameter to rank, as a string. Only necessary if inputting model rather than sampleMatrix.
#   loss: an exponent indicating the loss function for ranking. options: 2=square, 1=absolute, 0=zero
#   f: scale for loss calculation. options: identity, rank
#   rankweights: a vector of length equal to number of items to be ranked. Weights positions.
#   itemweights: a vector of length equal to number of items to be ranked. Weights items.
#   lossTotal: if TRUE, provides total loss
#
# Returns:
#   optimal ranking for a list of estimates
  
# Dependencies: rstan, clue
  
  if (!is.null(sampleMatrix)){ #checks for sampleMatrix
    i = sampleMatrix
    #print(dim(i)) #TODO remove
  } else if (!is.null(model)){ #checks for model
    print("model check") #TODO remove
    i <- rstan::extract(model, pars=parameter)[[1]] #extract samples from model
  }
  rho_i <- apply(i, 1, f) #apply function/scale transformation to matrix i. Should this be on cols (2)?
  rho_j <- apply(rho_i, 2, sort) #sort transformed samples (matrix j) so each column is sorted
  n <- ncol(i) #n = # items to be ranked

  if (loss == 0){
    #Q: this doesn't make sense unless we're on the rank scale, right? 
    #TODO give an error if user tries to use another scale
    LossRnk <- matrix(NA,n,n)
    for (i in 1:n) {
      for (j in 1:n) {
        LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean(m_rho_i[i,]!=m_rho_j[j,])
        }
    }
    if (lossTotal == TRUE){
      print(paste("Total Loss: ", sum(LossRnk)))
    }
    return(solve_LSAP(LossRnk))
  } else{ #all other loss cases
    LossRnk <- matrix(NA,n,n)
    for (i in 1:n) {
      for (j in 1:n) {
        LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean(abs((rho_i[i,]-rho_j[j,]))^loss)
      }
    }
    if (lossTotal == TRUE){
      print(paste("Total Loss: ", sum(LossRnk)))
    }
    return(solve_LSAP(LossRnk))
  }
}

  
RankingWeights <- function(numItems = 20, priority = "top", steepness = .9){
  # Computes optimal ranking for a list of estimates. Largest weight is always 1.
  #   
  # Args:
  #   numItems: number of items to rank
  #   priority: focus for ranking. "even" to evenly weight, "top" to prioritize top ranked items, "bottom" for bottom ranked items, "both" for both. 
  #   steepness: size of e between 0 and 1. Smaller e creates steeper weights. Larger e creates more even weights. Planning to test 3 (slow, steady, steep)
  #
  # Returns:
  #   vector of weights
  
  weights <- vector(length = numItems)
  items <- seq(1:numItems)
  
  if (priority == "even"){
    weights = rep(1, times = numItems)
  } else if (priority == "top"){
    weights = steepness^(items-1)
  } else if (priority == "bottom"){
    # reverse version (you care about last items only)
    weights = steepness^((numItems-items))
  } else if (priority == "both"){
    # weights = c(1, e, e^2, ..., e^(n+1/2) middle, ..., e^2, e, 1)
    #for even, repeats same weight at bottom. (if numItems = 20, items 10 and 11 will both have the smallest weight)
    #for odd, one item will have smallest weight. (if numItems = 21, item 11 will have the smallest weight)
    weights = steepness^(items-1) #top
    weights[(round(numItems/2 + 0.1) + 1):numItems] = steepness^(items[(round(numItems/2)):0]-1) #bottom/second half
  } else {
    return("Priority must be given as 'even', 'top', 'bottom', or 'both'.")
  }
  return(weights)
}

#RankingWeights(numItems = 21, priority = "both") #11 should be middle/lowest
