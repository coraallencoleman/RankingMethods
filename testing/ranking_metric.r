# for use with simulated data
library(dplyr)
library(clue)
library(rstan)
#cafes data metric

## Cafe Ranked Data Frame Output ## (from raking_testing.r)


##function metric to see if our top five matches true top five
#true top five:
cafes <-cafes %>% dplyr::arrange(desc(true_p), desc(attempts))

sim_ranks <- WeightedLossRanking(model = sim_rand_int_model, parameter = "p", f = rank, loss = 2, lossTotal = TRUE)

rankedCafes <- as.data.frame(cafes)
rankedCafes$true_p <- cafes$true_p*100
rankedCafes$rank <- as.integer(sim_ranks) #this ranks lowest to highest
arrange(rankedCafes, desc(rank))
rankedCafes <- rankedCafes %>% dplyr::arrange(desc(rank))
rankedCafes[1:5,]$cafe == cafes[1:5,]$cafe

TopRankMetric <- function(rankObject, originalDataFrame){
  # function metric to see if our top five matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking
  #   originalDataFrame: a data frame with column of true probabilities, true N (column names must match p, n)
  #
  # Returns:
  #   logical vector
  #
  # Dependencies: rstan, clue, dplyr
  
  originalDataFrame <-originalDataFrame %>% dplyr::arrange(desc(p), desc(n)) 
  
  rankedCafes <- as.data.frame(originalDataFrame)
  rankedCafes$true_p <- cafes$true_p*100
  rankedCafes$rank <- as.integer(sim_ranks) #this ranks lowest to highest
  arrange(rankedCafes, desc(rank))
  rankedCafes <- rankedCafes %>% dplyr::arrange(desc(rank))
  rankedCafes[1:5,]$cafe == cafes[1:5,]$cafe
}