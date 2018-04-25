# A testing metric for use with simulated data

library(dplyr)
library(clue)
library(rstan)

##function metric to see if rankObject's top ranked items match true top items
RankMetric <- function(rankObject = NULL, originalData = NULL, order = largest, topN = 5){
  # function metric to see if our top number matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking. Must include columns item, p, n
  #   originalDataFrame: a data frame with column of true probabilities, true N (column names must match p, n)
  #   order: largest (largest to smallest) or smallest (smallest to largest)
  #   topN: an integer number of top items to compare
  #
  # Returns:
  #   logical vector
  #
  # Dependencies: rstan, clue, dplyr
  rankedDataFrame <- as.data.frame(originalDataFrame)
  rankedDataFrame$true_p <- rankedDataFrame$p*100
  rankedDataFrame$rank <- as.integer(rankObject) #this rank orders items smallest to highest
  if (order == "largest"){
    originalDataFrame <-originalDataFrame %>% dplyr::arrange(desc(p), desc(n)) 
    rankedDataFrame <- rankedDataFrame %>% dplyr::arrange(rank)
  } else if (order == "smallest"){
    rankedDataFrame <- rankedDataFrame %>% dplyr::arrange(desc(rank))
    originalDataFrame <-originalDataFrame %>% dplyr::arrange(p, desc(n)) #TODO assume no ties in p change simulated data
  } else {
    stop("order must be input as either 'largest' or 'smallest'")
  }
  #check if each item in true top N is in ranking top N, return boolean
  return(originalDataFrame[1:topN,]$item %in% rankedDataFrame[1:topN,]$item )
}


#normal (implement this, but do simulation with binomial instead. might be relevant with survey data for counties)
#we're assuming that these tau^2 are known. If they aren't, the model could be extended to incorporate this uncertainty.
#true mean, sample means, sample size or variances
#simulate: fixed var (n), changing sample size. and use sd units. 
#sigma^2/n = tau. tau is

#TODO is rank one is in top five?



