#Testing for WeightedLossRanking function
#Step 0: Load
library(rstan)
library(dplyr)
#run entire ranking_function.r file. This puts WeightedLossRanking from ranking_function.r into environment and adds packages
set.seed(10)

#STEP 2
##Testing for small Normal data ##
small_normal_ranks <- WeightedLossRanking(normal_model, parameter = "alpha", loss = 2)
#estimating five means for each of the five items

##Testing for Normal Two Level Model Data ## 
norm_ranks <- WeightedLossRanking(normal_model, parameter = "alpha", loss = 2)
#expected rank? Might not be useful to do this (too long)

## Testing Function on Example County Data ##
ranks <- WeightedLossRanking(NJ_rand_int_model, parameter = "p", loss = 2) #TODO loss = 0 doesnt work
#ranks <- WeightedLossRanking(sampleMatrix = i_samples, parameter = "p", loss = 2) #sample matrix case
## County n = 21Ranked Data Frame Output ##
County <- raw_data0[,c(3)]
rankedDataFrame <- as.data.frame(County)
rankedDataFrame$p <- raw_data0$NumLBW/raw_data0$NumBirths*100
rankedDataFrame$rank <- as.integer(ranks)
library(dplyr); arrange(rankedDataFrame, rank)

## Testing Function on Illinois County Data ##
ranks <- WeightedLossRanking(IL_rand_int_model, parameter = "p", loss = 2)
#ranks <- WeightedLossRanking(sampleMatrix = i_samples, parameter = "p", loss = 2) #sample matrix case
## County n = 21Ranked Data Frame Output ##
County <- raw_data_I$County
rankedDataFrame <- as.data.frame(County)
rankedDataFrame$p <- raw_data_I$NumLBW/raw_data_I$NumBirths*100
rankedDataFrame$rank <- as.integer(ranks)
library(dplyr); arrange(rankedDataFrame, rank) #lots of disagreement here. 
#TODO compare for different types of ranking



#STEP 1: Read in data + create models 
## Binomial Random Intercept n = 10 with conflicts ##
#simulate county-like data
cafes <- as.data.frame(matrix((seq(from = 1, to = 12, by = 1)), ncol = 1))
colnames(cafes) <- c("cafe")
cafes$true_p <- rep(c(.6, .7, .8, .9), each = 3) #four levels of true p
cafes$attempts <- c(rep(c(10, 100, 1000), times = 4)) #create lots of variation here
cafes$SuccessfulConnections <- rbinom(n = 12, size = cafes$attempts, cafes$true_p)
## sim data + model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sim_data = list(
  J = nrow(cafes), #should be 12
  n = with(cafes, attempts),
  count = with(cafes, SuccessfulConnections),
  cafe = with(cafes,as.integer(as.factor(cafe)))
)

## Create Model with Random Intercepts for Each Cafe ##
sim_rand_int_model <- stan(file="/Users/cora/git_repos/RankingMethods/sim_randInt.stan",data=sim_data, seed = 10)
#get posterior means from stan model
#get credible intervals from stan model
## Cafe Ranked Data Frame Output ##
sim_ranks <- WeightedLossRanking(model = sim_rand_int_model, parameter = "p", f = rank, loss = 2, lossTotal = TRUE)
rankedCafes <- as.data.frame(cafes)
rankedCafes$true_p <- cafes$true_p*100
rankedCafes$rank <- as.integer(sim_ranks) #this ranks lowest to highest
rankedCafes<-arrange(rankedCafes, desc(rank))

## Example County Data n = 21 ## 
raw_data0 <- read.csv("/Users/cora/git_repos/RankingMethods/data/LBW.csv", header = TRUE)
raw_data <- raw_data0[, c("County", "NumLBW", "NumBirths")]#subset data to only those used by stan
raw_data$County <- as.integer(as.factor(raw_data$County)) #set County column to numeric
## Stan Data + Model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
data = list(
  J = nrow(raw_data),
  n = with(raw_data, NumBirths),
  count = with(raw_data, NumLBW),
  county = with(raw_data,as.integer(as.factor(County)))
)
## Create Model with Random Intercepts for Each County ##
NJ_rand_int_model <- stan(file="/Users/cora/git_repos/RankingMethods/randInt.stan",data=data, seed = 10)
## sampleMatrix input for County Data model ##
NJ_i_samples <- rstan::extract(rand_int_model, pars="p")[[1]] 


## Illinois County Test Data n = 102!
#http://www.countyhealthrankings.org/app/illinois/2018/measure/outcomes/1/map
raw_data_I <- read.csv("/Users/cora/git_repos/RankingMethods/data/Illinois_LBW.csv", header = TRUE)
raw_data <- raw_data_I[, c("County", "NumLBW", "NumBirths")]#subset data to only those used by stan
raw_data$County <- as.integer(as.factor(raw_data$County)) #set County column to numeric
## Stan Data + Model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
data = list(
  J = nrow(raw_data),
  n = with(raw_data, NumBirths),
  count = with(raw_data, NumLBW),
  county = with(raw_data,as.integer(as.factor(County)))
)
## Create Model with Random Intercepts for Each County ##
IL_rand_int_model <- stan(file="/Users/cora/git_repos/RankingMethods/randInt.stan",data=data, seed = 10)
## sampleMatrix input for County Data model ##
IL_i_samples <- rstan::extract(rand_int_model, pars="p")[[1]] 


##Two Level Normal Model n = 5 ##
#using subset of from Assignment 2 in MultilevelModels class
#data
math <- read.csv("/Users/cora/HardDriveDocuments/UW-Madison/Courses/Spring2018/MultilevelModels/Assignments/assignment2/hw02.csv", header = TRUE)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
math_data = list(
  N = nrow(math),
  J = with(math,length(unique(School))),
  y = with(math, Score),
  school = with(math,as.integer(as.factor(School))) #goes in numeric order, but shifts them up so that all starting at 1
)
normal_model <- stan(file="/Users/cora/git_repos/RankingMethods/normal_two_level.stan",data=math_data, seed=10)
