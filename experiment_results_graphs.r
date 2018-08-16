#graphs to plot results from experiment

#keep parameters (use in title and organize in some way)
# could plot total loss or some other kind of metric

require(ggplot2)

## data cleaning ##
# OLD setwd("/Users/cora/git_repos/RankingMethods/results")
# 
# n100_200 <- read.csv(file = "_100_1_1_100_200_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_100_100_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_100_30_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_50_200_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_50_100_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
#load("~/gangnon/results/ranking_experiment_results.RData")
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0814.RData") #EB. df called results
results_0814 <- as.data.frame(results)

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0807.RData") #not EB. df called results
results_0807 <- as.data.frame(results)
#combine data for 33840 rows
results <- rbind(results_0814, results_0807)
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.character )
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.double )

## Run Metric ##
results$metric5 <- RankMetric(results$ranking, settings = NULL, order = "largest", topN = 5)

rankObject = NULL, settings = NULL, order = "largest", topN = 5

## PLOTS ##
setwd("/Users/cora/git_repos/RankingMethods/plots/")



## Bar Graphs ##







## TOTAL LOSS ##

# postscript("loss_nmin_nmax_plot.eps")
# n_min_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(n_max))) + geom_jitter(width = 20) + 
#   ggtitle("Total Loss by Changes in Binomial n min, max") + 
#   ylab("Total Loss"); n_min_plot
# dev.off()
# 
# postscript("loss_nmin_n_plot.eps")
# changes_in_N_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(N))) + geom_jitter(width = 20) + 
#   ggtitle("Total Loss by Changes in N") + 
#   ylab("Total Loss"); changes_in_N_plot
# dev.off()

# postscript("loss_n_loss_plot.eps")
# losses_plot <- ggplot(results, aes(x = N, y = totalLoss, colour = factor(loss))) + 
#   geom_jitter(width = 5) + 
#   ggtitle("Total Loss By Changes in N") + 
#   ylab("Total Loss"); losses_plot
# dev.off()

postscript("loss_loss_rankPriority_plot.eps")
rankPriorityLoss_lossplot <- ggplot(results, aes(x = factor(loss), y = totalLoss, colour = factor(rankPriority))) + 
  geom_jitter(size = .2, width = .15) + 
  ggtitle("Total Loss by Changes in Loss Type and Rank Priority Weighting") + 
  ylab("Total Loss") + xlab("Loss"); rankPriorityLoss_lossplot
dev.off()

postscript("loss_rankSteepness_rankPriority_plot.eps")
rankPrioritySteepness_lossplot <- ggplot(results, aes(x = rankSteepness, y = totalLoss, colour = factor(rankPriority))) + 
  geom_jitter(size = .2, width = .05) +
  ggtitle("Total Loss by Changes in Rank Steepness and Priority") +
  ylab("Total Loss") + xlab("Rank Steepness") + xlim(0, 1); rankPrioritySteepness_lossplot
dev.off()

postscript("loss_loss_rankSteepness_plot.eps")
lossrankSteepness_lossplot <- ggplot(results, aes(x = factor(loss), y = totalLoss, colour = factor(rankSteepness))) + 
  geom_jitter(size = .2, width = .15) +
  ggtitle("Total Loss by Changes in Loss and Rank Steepness") +
  ylab("Total Loss") + xlab("Loss"); lossrankSteepness_lossplot
dev.off()

## RANKING QUALITY METRIC ##

# postscript("metric_nmin_nmax_plot.eps")
# n_min_plot <- ggplot(results, aes(x = n_min, y = metric, colour = factor(n_max))) + 
#   geom_jitter(width = 10) + 
#   ggtitle("Percent Top 10 Correct Ranking by Changes in Binomial n min, max") + 
#   ylab("Percent Top 10 Correct") + xlab("n min"); n_min_plot
# dev.off()

# postscript("metric_n_nmax_plot.eps")
# changes_in_N_plot <- ggplot(results, aes(x = N, y = metric, colour = factor(n_min))) + 
#   geom_jitter(width = 10) + 
#   ggtitle("Percent Top 10 Correct Ranking By Changes in N") + 
#   ylab("Percent Top 10 Correct") + xlab("N"); changes_in_N_plot
# dev.off()

# n_losses_metricplot <- ggplot(results, aes(x = N, y = metric, colour = factor(loss))) + 
#   geom_jitter(width = 5) + 
#   ggtitle("Percent Top 10 Correct Ranking By Changes in N and loss") + 
#   ylab("Percent Top 10 Correct") + xlab("N"); n_losses_metricplot

postscript("metric_loss_rankPriority_plot.eps")
lossrankPriority_plot <- ggplot(results, aes(x = factor(loss), y = metric, colour = factor(rankPriority))) + 
  geom_jitter(size = .4, width = .15) +
  ggtitle("Percent Top 10 Correct Ranking by Changes in Loss and Rank Priority") +
  ylab("Percent Top 10 Correct") + xlab("Loss"); lossrankPriority_plot
dev.off()

postscript("metric_rankSteepness_rankPriority_plot.eps")
rankSteepRankPriority_plot <- ggplot(results, aes(x = rankSteepness, y = metric, colour = factor(rankPriority))) + 
  geom_jitter(size = .4, width = .05) +
  ggtitle("Percent Top 10 Correct Ranking by Changes in Rank Steepness and Priority") +
  ylab("Percent Top 10 Correct") + xlab("Rank Steepness") + xlim(0, 1); rankSteepRankPriority_plot
dev.off()

postscript("metric_loss_rankSteepness_plot.eps")
lossrankSteepness_plot <- ggplot(results, aes(x = factor(loss), y = metric, colour = factor(rankSteepness))) + 
  geom_jitter(size = .4, width = .15) +
  ggtitle("Percent Top 10 Correct Ranking by Changes in Loss and Rank Steepness") +
  ylab("Percent Top 10 Correct") + xlab("Loss"); lossrankSteepness_plot
dev.off()


