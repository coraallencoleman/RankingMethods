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

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0816.RData") 
results_0816 <- as.data.frame(results)

#results <- rbind(results_0814, results_0807, results_0816)
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 13, 14)], as.character )
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 13, 14)], as.double )

## Run Metric ##
for (i in 1:nrow(results)){ #TODO this is created a list of 10 logicals NOT five.
    metric5 <- list(I(RankMetric(results$ranking[[i]], settings = results$data[i][[1]],
                              order = "largest", topN = 5)))
    results$metric5[i] <- metric5
}

# Metric 10
for (i in 1:nrow(results)){
  metric10 <- list(I(RankMetric(results$ranking[[i]], settings = results$data[i][[1]],
                               order = "largest", topN = 10)))
  results$metric10[i] <- metric10
}

# Metric 15
for (i in 1:nrow(results)){
  metric15 <- list(I(RankMetric(results$ranking[[i]], settings = results$data[i][[1]],
                                order = "largest", topN = 15)))
  results$metric15[i] <- metric15
}

save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_metric_0818.RData") #saves as an R object
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_metric_0818.RData")

## PLOTS ##

## Bar Graphs ##
results$metric5mean <- lapply(results$metric5, mean)
setwd("/Users/cora/git_repos/RankingMethods/plots/")
ps.options(fonts=c("serif"), width = 3, height = 5)
postscript("bar_metric5_e.eps")
metric5_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), mean(metric5)), stat = "summary", fun.y = "mean")
metric5_e
dev.off()

postscript("bar_metric5_loss.eps", fonts=c("serif", "Palatino"))
metric5_loss <- ggplot(results) + 
  geom_bar(aes(as.factor(loss), mean(metric5[[1]])), stat = "summary", fun.y = "mean")
metric5_loss
dev.off()

#metric 10
ps.options(fonts=c("serif"), width = 3, height = 5)
postscript("bar_metric10_e.eps")
metric10_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), mean(metric10[[1]])), stat = "summary", fun.y = "mean")
dev.off()

postscript("bar_metric10_loss.eps", fonts=c("serif", "Palatino"))
metric10_loss <- ggplot(results) + 
  geom_bar(aes(as.factor(loss), mean(metric10[[1]])), stat = "summary", fun.y = "mean")
dev.off()

#metric 15
ps.options(fonts=c("serif"), width = 3, height = 5)
postscript("bar_metric15_e.eps")
metric15_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), mean(metric15[[1]])), stat = "summary", fun.y = "mean")
dev.off()

postscript("bar_metric15_loss.eps", fonts=c("serif", "Palatino"))
metric15_loss <- ggplot(results) + 
  geom_bar(aes(as.factor(loss), mean(metric15[[1]])), stat = "summary", fun.y = "mean")
dev.off()


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


