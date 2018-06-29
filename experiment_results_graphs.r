#graphs to plot results from experiment

#keep parameters (use in title and organize in some way)
# could plot total loss or some other kind of metric

require(ggplot2)
setwd("/Users/cora/git_repos/RankingMethods/plots/")
## data cleaning ##
#load("~/gangnon/results/ranking_experiment_results.RData")
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.RData") #called df
results <- as.data.frame(df)
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.character )
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.double )

## TOTAL LOSS ##
postscript("loss_nmin_nmax_plot.eps")
n_min_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(n_max))) + geom_jitter(width = 20) + 
  ggtitle("Avg Total Loss by Changes in Binomial n min, max") + 
  ylab("Total Loss"); n_min_plot
dev.off()

postscript("loss_nmin_n_plot.eps")
changes_in_N_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(N))) + geom_jitter(width = 20) + 
  ggtitle("Avg Total Loss by Changes in N") + 
  ylab("Total Loss"); changes_in_N_plot
dev.off()

# postscript("loss_n_loss_plot.eps")
# losses_plot <- ggplot(results, aes(x = N, y = totalLoss, colour = factor(loss))) + 
#   geom_jitter(width = 5) + 
#   ggtitle("Avg Total Loss By Changes in N") + 
#   ylab("Total Loss"); losses_plot
# dev.off()

postscript("loss_loss_rankPriority_plot.eps")
rankPriority_plot <- ggplot(results, aes(x = factor(loss), y = totalLoss, colour = factor(rankPriority))) + 
  geom_jitter(width = .15) + 
  ggtitle("Avg Total Loss by Changes in Loss Type and Rank Priority Weighting") + 
  ylab("Total Loss"); rankPriority_plot
dev.off()

postscript("loss_loss_rankPriority_plot.eps")
rankPriority_plot <- ggplot(results, aes(x = rankSteepness, y = metric, colour = factor(rankPriority))) + 
  geom_jitter(width = .005) +
  ggtitle("Avg Total Loss by \nChanges in Loss and Rank Priority") +
  ylab("Percent Correct") + xlab("Rank Steepness") + xlim(0, 1); rankPriority_plot
dev.off()

## RANKING QUALITY METRIC ##

postscript("metric_nmin_nmax_plot.eps")
n_min_plot <- ggplot(results, aes(x = n_min, y = metric, colour = factor(n_max))) + 
  geom_jitter(width = 10) + 
  ggtitle("Percent Correct Ranking by \nChanges in Binomial n min, max") + 
  ylab("Percent Correct") + xlab("n min"); n_min_plot
dev.off()

# postscript("metric_n_nmax_plot.eps")
# changes_in_N_plot <- ggplot(results, aes(x = N, y = metric, colour = factor(n_min))) + 
#   geom_jitter(width = 10) + 
#   ggtitle("Percent Correct Ranking \nBy Changes in N") + 
#   ylab("Percent Correct") + xlab("N"); changes_in_N_plot
# dev.off()

# losses_plot <- ggplot(results, aes(x = N, y = metric, colour = factor(loss))) + 
#   geom_jitter(width = 5) + 
#   ggtitle("Percent Correct Ranking \nBy Changes in N and loss") + 
#   ylab("Percent Correct") + xlab("N"); losses_plot

postscript("metric_loss_rankPriority_plot.eps")
rankPriority_plot <- ggplot(results, aes(x = factor(loss), y = metric, colour = factor(rankPriority))) + 
  geom_jitter(width = .15) +
  ggtitle("Percent Correct Ranking by \nChanges in Loss and Rank Priority") +
  ylab("Percent Correct") + xlab("Loss"); rankPriority_plot
dev.off()

postscript("metric_loss_rankPriority_plot.eps")
rankPriority_plot <- ggplot(results, aes(x = rankSteepness, y = metric, colour = factor(rankPriority))) + 
  geom_jitter(width = .005) +
  ggtitle("Percent Correct Ranking by \nChanges in Loss and Rank Priority") +
  ylab("Percent Correct") + xlab("Rank Steepness") + xlim(0, 1); rankPriority_plot
dev.off()
