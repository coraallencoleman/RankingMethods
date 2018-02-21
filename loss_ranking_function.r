### Loss Functions on the Rank Scale ###

##Cora Allen-Coleman Feb 2018 ##

## TODO So far, only been tested with relatively simple bayesian models. Test with more complex model ##
## TODO Nested for loops faster if implemented in C ##

library(rstan)
library(clue)

##Data set up ##
#Data
raw_data <- read.csv("/Users/cora/git_repos/RankingMethods/data/LBW.csv", header = TRUE)
raw_data <- raw_data[, c("County", "NumLBW", "NumBirths")]#subset data to include only columns used by stan
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

### Ranking Function for Extracting Parameters and Ranking ### 
rank_on_loss <- function(model, loss, parameter, scale){
  # requires rstan and clue
  # model: a stan model
  # loss: a loss function for ranking. Options: square, abosolute, zero
  # parameter: parameter to rank, as created by stan model (as string)
  # scale: scale for loss calculation. Options: parameterscale, rank
  if (scale == "parameterscale"){
    scale <- sort
  } else if (scale != "rank"){
    return("error: enter valid scaleForRanking")
  }
  simple_ranks <- apply(rstan::extract(model, pars=parameter)[[1]], 1, scale)
  LossRnk <- matrix(NA,nrow(ranks),nrow(ranks))
  if (loss == "square"){
    for (i in 1:21) {
      for (j in 1:21) {
        LossRnk[i,j] <- mean((ranks[i,]-j)^2)
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
      for (j in 1:21) {
        LossRnk[i,j] <- mean(ranks[i,]!=j)
      }
    }
  } else {
    return("error: loss function not recognized")
  }
  return(solve_LSAP(LossRnk))
}

rank_on_loss(rand_int_model, "square", "p", "rank")
