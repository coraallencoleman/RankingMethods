#Rankings of Illinois LBW data. Created for JSM talk Aug 2018

## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#import data
idat <- read.csv("/Users/cora/git_repos/RankingMethods/data/Illinois_LBW_item_n_y.csv")
idat$item <- as.factor(as.numeric(idat$County))
N <- nrow(idat)

#get posterior samples using glmer model
post <- PostSamples(idat)

#unweighted ranking
unweightedILResults <- WeightedLossRanking(sampleMatrix = post, loss = 2)
unweightedILRanks <- as.integer(unweightedILRank[-1])

#0-1 weighted ranking
zero_one_rankWeights <- c(rep(1, times = 10), rep(0, times = 92))
zero_one_ILResults <- WeightedLossRanking(sampleMatrix = post, loss = 2,
                                           rankWeights = zero_one_rankWeights)
zero_one_ILRanks <- as.integer(zero_one_ILResults[-1])

#gradual ranking using our function
gradWeights <- RankingWeights(numItems = N, priority = "top", steepness = .5)

grad_ILResults <- WeightedLossRanking(sampleMatrix = post, loss = 2,
                                          rankWeights = gradWeights)

grad_ILRanks <- as.integer(grad_ILResults[-1])


### Create Graphics for Slides ###
#TODO keep only 1 through 15?
library(xtable)

#unweighted ranking
idat$unweightedRanks <- unweightedILRanks #add ranks
t_unweighted_ranking <- idat[order(idat$unweightedRanks),c("County", "n", "p", "unweightedRanks")] #sort by unweighted ranks
print(xtable(t_unweighted_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#0-1 weighted ranking
idat$zero_one_rank <- zero_one_ILRanks #add ranks
t_zero_one_weighted_ranking <- idat[order(idat$zero_one_rank),c("County", "n", "p", "unweightedRanks", "zero_one_rank")] #sort by unweighted ranks
print(xtable(t_zero_one_weighted_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#gradual ranking
idat$grad_rank <- grad_ILRanks #add ranks
t_grad_ranking <- idat[order(idat$grad_rank),c("County", "n", "p", "unweightedRanks", "zero_one_rank", "grad_rank")] #sort by unweighted ranks
print(xtable(t_grad_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular


#graphical display of the posterior distributions 
#pick conflict area and show posteriors for those counties (maybe 3-5 counties?)
#Woodford Hancock De Witt Mason