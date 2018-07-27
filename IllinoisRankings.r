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

#1 raw rank
idat<-idat[order(idat$item),] #we needed to reorder bc of alphabetizing differences
idat$rawRank <- as.integer(rank(idat$p)) #checked

##2. posterior mean point estimate ranking
raw_estimates_post_means <- apply(p_post, 2, mean)
idat<-idat[order(idat$item),] #we needed to reorder bc of alphabetizing differences
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

#unweighted ranking
t_unweighted_ranking <- idat[order(idat$unweightedRanks),c("County", "n", "p", "rawRank", "pointRank", "unweightedRanks")] #sort by unweighted ranks
print(xtable(t_unweighted_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#0-1 weighted ranking
t_zero_one_weighted_ranking <- idat[order(idat$zero_one_rank),c("County", "n", "p", "unweightedRanks","zero_one_five", "zero_one_rank")] #sort by unweighted ranks
print(xtable(t_zero_one_weighted_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#gradual ranking
t_grad_ranking <- idat[order(idat$grad_rank),c("County", "n", "p", "zero_one_five", "zero_one_rank", "grad_rank")] #sort by unweighted ranks
print(xtable(t_grad_ranking[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#repeated ranking
t_repRanks <- idat[order(idat$repRanks),c("County", "n", "p", "repRanks")] #sort by repRanks
print(xtable(t_repRanks[1:12,], caption = NULL), include.rownames = FALSE) #use \scalebox{0.7}{before tabular


#graphical display of the posterior distributions 
library(ggplot2);library(reshape2)
library(RColorBrewer)
#pick conflict area and show posteriors for those counties
conflict_subset <- as.data.frame(post[, c(102, 66, 14, 47)])
#p = exp(-1.12546)/(1+exp(-1.12546))
p_conflict_subset = exp(-2.492716 + conflict_subset)/(1 + exp(-2.492716 + conflict_subset))
#mean(p_post)
#rename
names(p_conflict_subset) <- c("Woodford", "Mercer", "Clinton", "Kendall")
conflict <- melt(p_conflict_subset)
conflict$County <- conflict$variable
cbPalette <- c("#0072B2", "#009E73","#D55E00", "#F0E442",  "#CC79A7")
conflictplot <- ggplot(conflict,aes(x=value, fill=County)) + geom_density(alpha=0.5) + 
  scale_fill_manual(values=cbPalette) + xlab("Percent Low Birth Weight")
conflictplot
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/conflict_post.png", plot = conflictplot)

#graphical display of the posterior distributions 
library(ggplot2);library(reshape2)
library(RColorBrewer)
#pick conflict area and show posteriors for those counties
conflict_subset <- as.data.frame(post[, c(85, 6, 98)]) #RAW POST
#p = exp(-1.12546)/(1+exp(-1.12546))
p_conflict_subset = exp(-2.492716 + conflict_subset)/(1 + exp(-2.492716 + conflict_subset))
#mean(p_post)
#rename
names(p_conflict_subset) <- c("Scott", "Bureau", "Whiteside")
conflict <- melt(p_conflict_subset)
conflict$County <- conflict$variable
cbPalette <- c("#0072B2", "#009E73","#D55E00", "#F0E442",  "#CC79A7")
conflictplot2 <- ggplot(conflict,aes(x=value, fill=County)) + geom_density(alpha=0.5) + 
  scale_fill_manual(values=cbPalette) + xlab("Percent Low Birth Weight")
conflictplot2
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/conflict_post2.png", plot = conflictplot2)



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

#grad weights graph
idat<-idat[order(idat$item),]
idat$gradWeights <- gradWeights
idat$item <- as.numeric(idat$item)
gradWeightGraph <- ggplot(idat,aes(x=item, y=gradWeights)) + geom_point() +
  xlab("Rank Position") + ylab("Gradual Weight") + 
  annotate("text", label = "epsilon = 0.05", x = 75, y = .6, size = 8, colour = "black")
gradWeightGraph
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/gradWeightGraph.png", plot = gradWeightGraph)


#0-1 weight graph
idat<-idat[order(idat$item),]
idat$zero_one_10 <- zero_one_rankWeights
idat$item <- as.numeric(idat$item)
Top10WeightGraph <- ggplot(idat,aes(x=item, y=zero_one_10)) + geom_point() +
  xlab("Rank Position") + ylab("Top 10 Weights") + ylim(0, 1.4)
Top10WeightGraph
ggsave(filename = "/Users/cora/Dropbox/UW-Madison/CurrentResearch/Ranking (noncode)/presentations/JSM/images/Top10WeightGraph.png", plot = Top10WeightGraph)


#repeat Ranks Graphs
repeatRanks <- sort(apply(unweightedILResults[[1]],1,which.min))
unique(repeatRanks)
ranks <- seq(from=1, to = 102, by = 1)
rep <- as.data.frame(cbind(ranks = ranks, repRanks = repeatRanks))
repGraph <- g <- ggplot(rep, aes(repRanks)) + geom_bar() + xlab("Ranks")
repGraph
