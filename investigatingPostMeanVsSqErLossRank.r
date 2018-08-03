#Issue #11 in github: Posterior Mean vs Sq Error Loss Rank

#Why are these different? Shouldn't they be the same?
#The problem might be in file IllinoisRankings.r, but (more problematically) might be a problem with the ranking function itself.

##from IllinoisRankings.r
library(rstan)
library(ggplot2)

#import data
idat <- read.csv("/Users/cora/git_repos/RankingMethods/data/Illinois_LBW_item_n_y.csv")
idat$item <- as.factor(as.numeric(idat$County))
N <- nrow(idat)

#get posterior samples using glmer model
post <- PostSamples(idat) #my function in sim_ranking_experiment.r
#convert post to probabilities
p_post <- exp(-2.492716 + post)/(1 + exp(-2.492716 + post))

#1. raw rank
idat<-idat[order(idat$item),] #REORDERS we needed to reorder bc of alphabetizing differences
idat$rawRank <- as.integer(rank(idat$p)) #checked

#2. posterior mean point estimate ranking
raw_estimates_post_means <- apply(p_post, 2, mean)
idat<-idat[order(idat$item),]  #REORDERS we needed to reorder bc of alphabetizing differences
idat$postMeans <- raw_estimates_post_means
idat$pointRank <- as.integer(rank(idat$postMeans)) #checked!

#3. unweighted ranking
unweightedILResults <- WeightedLossRanking(sampleMatrix = p_post, loss = 2)
unweightedILRanks <- as.integer(unweightedILResults[[2]])

#4. 0-1 weighted ranking
zero_one_rankWeights <- c(rep(1, times = 10), rep(0, times = 92)) #weights
zero_one_ILResults <- WeightedLossRanking(sampleMatrix = p_post, loss = 2,
                                          rankWeights = zero_one_rankWeights)
zero_one_ILRanks <- as.integer(zero_one_ILResults[[2]])

zero_one_five_weights <- c(rep(1, times = 5), rep(0, times = 97)) #weights
zero_one_five <- WeightedLossRanking(sampleMatrix = p_post, loss = 2,
                                     rankWeights = zero_one_five_weights)
zero_one_five <- as.integer(zero_one_five[[2]])

#5. gradual ranking using our function
gradWeights <- RankingWeights(numItems = N, priority = "top", steepness = .95)#weights

grad_ILResults <- WeightedLossRanking(sampleMatrix = p_post, loss = 2,
                                      rankWeights = gradWeights)

grad_ILRanks <- as.integer(grad_ILResults[[2]])

idat<-idat[order(idat$item),] #we needed to reorder bc of alphabetizing differences
idat$unweightedRanks <- unweightedILRanks
idat$zero_one_rank <- zero_one_ILRanks
idat$zero_one_five <- zero_one_five
idat$grad_rank <- grad_ILRanks
repeatRanks <- apply(unweightedILResults[[1]],1,which.min) #repRanks
idat$repRanks <- repeatRanks

### Create Graphics for Slides ###
#TODO keep only 1 through 15?
library(xtable)


#raw estimate ranking
t_raw_ranking <- idat[order(idat$rawRank),c("County", "n", "p", "rawRank")] #sort by PE ranks
print(xtable(t_raw_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#point estimate ranking
t_point_ranking <- idat[order(idat$pointRank),c("County", "n", "p", "rawRank", "pointRank")] #sort by PE ranks
print(xtable(t_point_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular
