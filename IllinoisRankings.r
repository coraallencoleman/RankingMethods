#Rankings of Illinois LBW data. Created for JSM talk Aug 2018

## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

library(rstan)
#import data
idat <- read.csv("/Users/cora/git_repos/RankingMethods/data/Illinois_LBW_item_n_y.csv")
idat$item <- as.factor(as.numeric(idat$County))
N <- nrow(idat)

#get posterior samples using glmer model
post <- PostSamples(idat)
#convert post to probabilities
p_post <- exp(-2.492716 + post)/(1 + exp(-2.492716 + post))

##point estimate ranking


#unweighted ranking
unweightedILResults <- WeightedLossRanking(sampleMatrix = p_post, loss = 2)
unweightedILRanks <- as.integer(unweightedILResults[-1])

#0-1 weighted ranking
zero_one_rankWeights <- c(rep(1, times = 10), rep(0, times = 92))
zero_one_ILResults <- WeightedLossRanking(sampleMatrix = p_post, loss = 2,
                                           rankWeights = zero_one_rankWeights)
zero_one_ILRanks <- as.integer(zero_one_ILResults[-1])

#gradual ranking using our function
gradWeights <- RankingWeights(numItems = N, priority = "top", steepness = .5)

grad_ILResults <- WeightedLossRanking(sampleMatrix = p_post, loss = 2,
                                          rankWeights = gradWeights)

grad_ILRanks <- as.integer(grad_ILResults[-1])


### Create Graphics for Slides ###
#TODO keep only 1 through 15?
library(xtable)

#point estimate ranking

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
library(ggplot2);library(reshape2)
library(RColorBrewer)
#pick conflict area and show posteriors for those counties Woodford 102 Hancock 34 De Witt 19 Mason 60
conflict_subset <- as.data.frame(post[, c(102, 34, 19, 60)])
#p = exp(-1.12546)/(1+exp(-1.12546))
p_conflict_subset = exp(-2.492716 + conflict_subset)/(1 + exp(-2.492716 + conflict_subset))
#mean(p_post)
#rename
names(p_conflict_subset) <- c("Woodford", "Hancock", "De Witt", "Mason")
conflict <- melt(p_conflict_subset)
conflict$County <- conflict$variable
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#CC79A7")
conflictplot <- ggplot(conflict,aes(x=value, fill=County)) + geom_density(alpha=0.3) + 
  scale_fill_manual(values=cbPalette) + xlab("Percent Low Birth Weight") + ggtitle("Illinois County Estimates")
conflictplot
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/conflict_post.png", plot = conflictplot)

## example (sim data) ##
#Equal Variance Example with Graph

x <- data.frame(county1=rnorm(10^4, .45, 0.09), county2=rnorm(10^4,.5,0.09), county3=rnorm(10^4,.55, .09))
data<- melt(x)
cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#CC79A7")
data$County <- data$variable
data$p <- data$value
equalVar <- ggplot(data,aes(x=p, fill=County)) + geom_density(alpha=0.3) +
  scale_fill_manual(values=cbPalette) + xlim(0, 1); equalVar
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/equal_var.png", plot = equalVar)

#Unequal Variance Example
x <- data.frame(county1=rnorm(10^4, .45, 0.09), county2=rnorm(10^4,.5,0.09), county3=rnorm(10^4,0.55, 0.18))
data<- melt(x)
data$County <- data$variable
data$p <- data$value
unequalVar <- ggplot(data,aes(x=p, fill=County)) + geom_density(alpha=0.3) +
  scale_fill_manual(values=cbPalette) + xlim(0, 1);unequalVar
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/unequal_var.png", plot = unequalVar)
