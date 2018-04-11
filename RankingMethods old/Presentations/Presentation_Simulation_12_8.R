#Presentation Simulation #
#goal: rank 10 items from lowest to highest

## Create a dataset ##
#Variation in sample size (variability) is more important (as long as there is some uncertainty about the rankings)

##MORE CONFLICT 
set.seed(1000)  #eg risk score
item1 <- rnorm(5, 35, 5) # n: smaller number observations, 0: mean, 3: sd high_var should be lowest ranked
item2 <-  rnorm(50, 35, 1) # n: smaller number observations, 0: mean, 1: sd low_var
item3 <- rnorm(100, 35, 1) # n: smaller number observations, 0: mean, 3: sd high_var
item4 <-  rnorm(5, 34, 5) # n: smaller number observations, 0: mean, 1: sd low_var
item5 <- rnorm(50, 34, 1) # n: smaller number observations, 0: mean, 3: sd high_var
item6 <-  rnorm(100, 34, 1) # n: smaller number observations, 0: mean, 1: sd low_var
item7 <- rnorm(5, 33, 5) # n: smaller number observations, 0: mean, 3: sd high_var
item8 <-  rnorm(50,33, 1) # n: smaller number observations, 0: mean, 1: sd low_var
item9 <- rnorm(100, 33, 1) # n: smaller number observations, 0: mean, 3: sd high_var
item10 <-  rnorm(1000, 33, 1) # n: smaller number observations, 0: mean, 1: sd low_var should be highest ranked
 
header <- c("i", "mean", "n", "var")
i <- seq(from = 1, to = 10, by = 1)
library(qpcR)
raw_data <- qpcR:::rbind.na(item1, item2, item3, item4, item5, item6, item7, item8, item9, item10)
means <- rowMeans(raw_data, na.rm = TRUE)
n <- rep(c(5, 20), times = 5)
var <- apply(raw_data,1,var,na.rm=TRUE)
data <- as.data.frame(cbind(i, means, n, var))


## Get posterior probabilities of being assigned every rank for each item ##

##Sample batting averages for each player 10,000 times.  
#a. Create an nxn matrix of sampled rank counts full of zeros for all players  
n <- length(data$i)
samples <- 20000 
#stores ranks with nrow = #samples and col = items to rank
ranks <- as.data.frame(matrix(rep(0, n*samples), nrow = samples, ncol = n)) #rows = samples, columns=rank positions
nums <- seq(1, n, by=1)
names(ranks) <- paste("Item", nums, sep="")
rankCounts <- as.data.frame(matrix(rep(0, n*n), nrow = n, ncol = n)) #rows = samples, columns=rank positions
names(rankCounts) <- paste("Rank", nums, sep="")
#rankCounts[1] is the vector for item 1

#b. sample + store the rank of the averages for each of the samples.  
set.seed(1)
for (sample in 1:samples){
  scoreAvg <- c()
  for (i in 1:n){
    scoreAvg[i] <- rnorm(1, mean = data$mean[i], sd = sqrt(data$var[i]))
  }
  #ranks of the each of the items for this vector
  ranks[sample,] <- rank(scoreAvg, na.last = NA)  #use order instead to go from largest to smallest?
}

## c. Count the number of times each item ranked at each position in the rankCount matrix.  
for (item in 1:n){
  df <- as.data.frame(count(ranks[,item]))
  for (rank in 1:nrow(df)){
    rankCounts[item, df$x[rank]] <- df$freq[rank] #TODO: this seems to have gotten reversed
  }
}

## d. Calculate the probability that each item is in each position.
rankProbs <- rankCounts/samples #posterior probabilities of each position for each item


## Add Loss Function ##
# TODO: how do I include rankProbs in calculating loss?
## a. calculate the loss matrix. 
#Each row is an item, each column is a rank position, and nxn?
#each entry is the loss associated with assigning the item in that row to that rank position. 
assignedRank <- rep(seq(from = 1, to = 10, length = 10), times = 10)
#lapply(rankProbs, MARGIN = 1, FUN =which.max())
#adply(rankProbs, 1, transform, max = which.max(rankProbs))
maxRank <- c()
for (i in 1:n){ #TODO: is this the correct way to calculate true rank?
  maxRank[i] <- which.max(rankProbs[i,])
}
trueRank <- rep(maxRank, each = n)
## Absolute Value Loss
#entries <- loss(assigning item i to rank j)
#entries <- abs(assignedRank - trueRank) 
losses <- abs(assignedRank - trueRank)
lossMatrixAV <- matrix(losses, nrow = n, ncol = n, byrow=TRUE)

## 0 1 Loss
losses <- ifelse(assignedRank == trueRank, 0, 1)
lossMatrix01 <- matrix(losses, nrow = n, ncol = n, byrow=TRUE)

## Squared Error Loss
losses <- (assignedRank - trueRank)^2
lossMatrixSE <- matrix(losses, nrow = n, ncol = n, byrow=TRUE)

## b. find optimal ranking
#using the solve_LSAP function in the clue package in R.
library(clue)
OptimalAV <- solve_LSAP(lossMatrixAV);OptimalAV #absolute value loss
Optimal01 <- solve_LSAP(lossMatrix01); Optimal01 #zero one loss
OptimalSE <- solve_LSAP(lossMatrixSE); OptimalSE #zero one loss

## Rankings Table
#all optimal rankings in one table
i <- seq(from = 1, to = 10, by = 1)
cbind(items = i, AbsoluteValueRank = OptimalAV, ZeroOneRank = Optimal01, SquaredErrorRank = OptimalSE, OriginalMeans = c(35, 35, 35, 34, 34, 34, 33, 33, 33, 33))

##graphics showing variance? confidence intervals?

