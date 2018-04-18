### Weighted Loss Function for Ranking by Position ###
## Cora Allen-Coleman Spring 2018 ##

library(rstan)
library(clue)

### Ranking Function for Extracting Parameters and Ranking ### 

WeightedLossRanking <- function(model = NULL, parameter = NULL, sampleMatrix = NULL, loss = 2,  f=identity, 
                                rankweights = rep(1, times = n), itemweights = rep(1, times = n), lossTotal = FALSE){
# Computes optimal ranking for a list of estimates
#   
# Args:
#   model: a stan model for the estimates
#   parameter: parameter to rank, as a string. Only necessary if inputting model rather than sampleMatrix.
#   sampleMatrix: a matrix of samples. dim = n samples by n items
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
  } else if (!is.null(model)){ #checks for model
    i <- rstan::extract(model, pars=parameter)[[1]] #extract samples from model
  }
  rho_i <- apply(i, 1, f) #apply function/scale transformation to matrix i
  rho_j <- apply(rho_i, 2, sort) #sort transformed samples (matrix j)
  n <- ncol(i) #n = # items to be ranked

  if (loss == 0){ #zero one loss case TODO fix this
    LossRnk <- matrix(NA,n,n)
    for (i in 1:n) {
      for (j in 1:n) {
        LossRnk[i,j] <- rankweights[j]*itemweights[i]*mean(isTRUE(all.equal(rho_i[i,],rho_j[j,])))
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

## Testing Function on Example Data (below) ##
ranks <- WeightedLossRanking(model = rand_int_model, parameter = "p", f = rank, loss = 0, lossTotal = TRUE); ranks #model case
#ranks <- WeightedLossRanking(sampleMatrix = i_samples, parameter = "p", loss = 0); ranks #sample matrix case

## Ranked Data Frame Output ##
County <- raw_data0[,c(3)]
rankedDataFrame <- as.data.frame(County)
rankedDataFrame$p <- raw_data0[,4]/raw_data0[,5]*100
rankedDataFrame$rank <- as.integer(ranks)
library(dplyr); arrange(rankedDataFrame, rank)

