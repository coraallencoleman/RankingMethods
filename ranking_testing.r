#Testing for WeightedLossRanking function

## Testing Function on Example Data ## (run this for testing after data below in environment)
ranks <- WeightedLossRanking(rand_int_model, parameter = "p", loss = 0) #model case
#ranks <- WeightedLossRanking(sampleMatrix = i_samples, parameter = "p", loss = 2) #sample matrix case

## Ranked Data Frame Output ##
County <- raw_data0[,c(3)]
rankedDataFrame <- as.data.frame(County)
rankedDataFrame$p <- raw_data0[,4]/raw_data0[,5]*100
rankedDataFrame$rank <- as.integer(ranks)
library(dplyr); arrange(rankedDataFrame, rank)


##Example Data ## (do this first)
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

## sampleMatrix input for this model ##
i_samples <- rstan::extract(rand_int_model, pars="p")[[1]] 
