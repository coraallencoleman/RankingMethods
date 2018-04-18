# A testing metric for use with simulated data

library(dplyr)
library(clue)
library(rstan)

## Cafe Ranked Data Frame Output ## (from raking_testing.r)



sim_ranks <- WeightedLossRanking(model = sim_rand_int_model, parameter = "p", f = rank, loss = 2, lossTotal = TRUE)

rankedCafes <- as.data.frame(cafes)
rankedCafes$p <- cafes$p*100
rankedCafes$rank <- as.integer(sim_ranks) #this ranks lowest to highest

cafes <-cafes %>% dplyr::arrange(desc(p), desc(n)) #true top five
rankedCafes <- rankedCafes %>% dplyr::arrange(rank)
cafes[1:5,]$item %in% rankedCafes[1:5,]$item

##function metric to see if rankObject's top ranked items match true top items
RankMetric <- function(rankObject = NULL, originalDataFrame = NULL, order = largest, topN = 5){
  # function metric to see if our top number matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking. Must include columns items, p, n
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
    originalDataFrame <-originalDataFrame %>% dplyr::arrange(p, n) 
  } else {
    stop("order must be input as either 'largest' or 'smallest'")
  }
  #check if each item in true top N is in ranking top N, return boolean
  return(originalDataFrame[1:topN,]$item %in% rankedDataFrame[1:topN,]$item )
}

RankMetric(sim_ranks, cafes, order = "largest", topN = 6)
