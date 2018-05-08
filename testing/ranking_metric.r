# A testing metric for use with simulated data

library(dplyr)
library(clue)
library(rstan)

##function metric to see if rankObject's top ranked items match true top items MATRIX
RankMetric <- function(rankObject = NULL, originalData = NULL, order = "largest", topN = 5){
  # function metric to see if our top number matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking. Must include columns item, p, n
  #   originalData: a data frame with column of true probabilities, true N (column names must match p, n)
  #   order: largest (largest to smallest) or smallest (smallest to largest)
  #   topN: an integer number of top items to compare
  #
  # Returns:
  #   logical vector
  #
  # Dependencies: rstan, clue, dplyr
  rankedData <- array(data = NA, dim=c(6,length(originalData[1,])))
  rankedData[1:4,] <- originalData
  rankedData[5,] <- rankedData[2,]*100 #p*100 
  rankedData[6,] <- as.integer(rankObject) #rank orders items smallest to highest
  if (order == "largest"){
    originalData <- originalData[,order(originalData[2,])] #sort by p #TODO problem here
    rankedData <- rankedData[,order(rankedData[6,])] #sort by rank
  } else if (order == "smallest"){
    originalData <- originalData[,order(-originalData[2,])] #sort by p
    rankedData <- rankedData[,order(-rankedData[6,])] #sort by rank
  } else {
    stop("order must be input as either 'largest' or 'smallest'")
  }
  #check if each item in true top N is in ranking top N, return boolean
  return(originalData[1,1:topN] %in% rankedData[6,1:topN])
}



##function metric to see if rankObject's top ranked items match true top items
#WITH DATAFRAME not done
RankMetricDF <- function(rankObject = NULL, originalData = NULL, order = largest, topN = 5){
  # function metric to see if our top number matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking. Must include columns item, p, n
  #   originalData: a data frame with column of true probabilities, true N (column names must match p, n)
  #   order: largest (largest to smallest) or smallest (smallest to largest)
  #   topN: an integer number of top items to compare
  #
  # Returns:
  #   logical vector
  #
  # Dependencies: rstan, clue, dplyr
  rankedData <- as.data.frame(originalData)
  rankedData[5,] <- rankedData[2,]*100 #p*100
  rankedData[6,] <- as.integer(rankObject) #rank orders items smallest to highest
  if (order == "largest"){
    originalData <-originalData %>% dplyr::arrange(desc(p), desc(n)) 
    rankedData <- rankedData %>% dplyr::arrange(rank)
  } else if (order == "smallest"){
    rankedData <- rankedData %>% dplyr::arrange(desc(rank))
    originalData <-originalData %>% dplyr::arrange(p, desc(n)) #TODO assume no ties in p change simulated data
  } else {
    stop("order must be input as either 'largest' or 'smallest'")
  }
  #check if each item in true top N is in ranking top N, return boolean
  return(originalData[1:topN,]$item %in% rankedData[1:topN,]$item )
}


#normal (implement this, but do simulation with binomial instead. might be relevant with survey data for counties)
#we're assuming that these tau^2 are known. If they aren't, the model could be extended to incorporate this uncertainty.
#true mean, sample means, sample size or variances
#simulate: fixed var (n), changing sample size. and use sd units. 
#sigma^2/n = tau. tau is

#TODO is rank one is in top five?



