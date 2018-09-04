#graphs to plot results from experiment

#keep parameters (use in title and organize in some way)
# could plot total loss or some other kind of metric

require(ggplot2)
setwd("/ua/allencoleman/gangnon/ranking")
source("ranking_function.r")

## data cleaning ##
# OLD setwd("/Users/cora/git_repos/RankingMethods/results")
# 
# n100_200 <- read.csv(file = "_100_1_1_100_200_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_100_100_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_100_30_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_50_200_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
# n100_200 <- read.csv(file = "_100_1_1_50_100_1_1_ascending_even_0.9__2_identity_even_0.9_1_.csv")
#load("~/gangnon/results/ranking_experiment_results.RData")
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0824.RData") #EB. df called results
results_0824 <- as.data.frame(results)

# load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0807.RData") #not EB. df called results
# results_0807 <- as.data.frame(results)
# 
# load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0816.RData") 
# results_0816 <- as.data.frame(results)

#results <- rbind(results_0814, results_0807, results_0816)
results[, c(1:8, 11, 13, 14)] <- sapply( results[,c(1:8, 11, 13, 14)], as.character )
results[, c(1:8, 11, 13, 14)] <- sapply( results[,c(1:8, 11, 13, 14)], as.double )

## Run Metric ##
results$metric5percent <- rep(0, times = nrow(results))
for (i in 1:nrow(results)){
  results$metric5[i] <- list(I(RankMetric(results$ranking[i], order = "largest", topN = 5)))
  results$metric5percent[i] <- as.double(mean(results$metric5[i][[1]]))[[1]]
}


# Metric 10
results$metric10percent <- rep(0, times = nrow(results))
for (i in 1:nrow(results)){
  results$metric10[i] <- list(I(RankMetric(results$ranking[i], order = "largest", topN = 10)))
  results$metric10percent[i] <- as.double(mean(results$metric10[i][[1]]))[[1]]
}

# Metric 15
results$metric15percent <- rep(0, times = nrow(results))
for (i in 1:nrow(results)){
  results$metric15[i] <- list(I(RankMetric(results$ranking[i], order = "largest", topN = 15)))
  results$metric15percent[i] <- as.double(mean(results$metric15[i][[1]]))[[1]]
}

# Strict Metric from 1 to 15
for (t in 1:15){
  results[[paste0("metricStrict", t)]] <- rep(0, times = nrow(results))
  for (i in 1:nrow(results)){
    results[[paste0("metricStrict", t)]][i] <- list(I(RankMetricStrict(results$ranking[i], 
                                              order = "largest", topN = t)))
    results[[paste0("metricStrictPercent", t)]][i] <- as.double(mean(results[[paste0("metricStrict", t)]][i][[1]]))[[1]]
  }
}
results[[paste0("metricStrict", 3)]] <- rep(0, times = nrow(results))
results[[paste0("metricStrict", 3)]][1] <- list(I(RankMetricStrict(results$ranking[1], 
                                                                   order = "largest", topN = 3)))
results[[paste0("metricStrictPercent", 3)]][1] <- as.double(mean(results[[paste0("metricStrict", 3)]][1][[1]])[[1]])
# for (t in 1:15){
#   for (i in 1:length(results)){
#     results[[paste0("metricStrict", t)]][i] <- list(I(RankMetricStrict(results$ranking[i], 
#                                                       order = "largest", topN = t))) #something wrong with subsetting here
#   }
#   #this might not be right
#   results[[paste0("metricStrictPercent", t)]] <- lapply(results[[paste0("metricStrict", t)]], mean)
# }
#results[[paste0("metricStrict", 5)]] <- list(I(RankMetricStrict(results$ranking[[3]], order = "largest", topN = 5)))

#need to get all percents in long format with column?

save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_metric_0824.RData") #saves as an R object
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_metric_0824.RData")

#include only priority = top (or, alternatively only bottom) to keep results coherant
results_top <- results[results$rankPriority == "top",]
results <- results_top

## PLOTS ##
#How often is #1 ranked as #1?
ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("bar_StrictMetric_e.eps")
StrictMetric1_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent1)), stat="summary", fun.y=mean) + 
  ggtitle("How Often is Item 1 Ranked First?") +
  ylab("Mean Percent Top Correct (Strict)") + xlab("Rank Weight Steepness (epsilon)")
StrictMetric1_e
dev.off()

#Strict Metric from 1 to 15 (rainbow for 1 to 15?)
ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("bar_StrictMetric_e.eps")
StrictMetric_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent5)), stat="summary", fun.y=mean, fill = "red", alpha = "0.4") + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent10)), stat="summary", fun.y=mean, fill = "orange", alpha = "0.4") +
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent15)), stat="summary", fun.y=mean, fill = "yellow", alpha = "0.4") +
  ggtitle("% of Time Rank of Each Element Correct by e (Strict)") +
  ylab("Mean Percent Top Correct (Strict)") + xlab("Rank Weight Steepness (epsilon)")
StrictMetric_e
dev.off()

ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("point_StrictMetric_e.eps")
pointStrictMetric_e <- ggplot(results) + 
  geom_point(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent5)), color = "red", stat="summary", fun.y=mean) + 
  geom_point(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent10)), color = "orange", stat="summary", fun.y=mean) +
  geom_point(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent15)), color = "yellow", stat="summary", fun.y=mean) +
  ggtitle("% of Time Rank of Each Element Correct by e (Strict)") +
  ylab("Mean Percent Top Correct (Strict)") + xlab("Rank Weight Steepness (epsilon)")
pointStrictMetric_e
dev.off() 

## Bar Graphs ##
setwd("/Users/cora/git_repos/RankingMethods/plots/")
ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("bar_metric5_e.eps")
metric5_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metric5percent)), stat="summary", fun.y=mean) + 
  ggtitle("Percent Top 5 Correct Ranking by e") +
  ylab("Percent Top 5 Correct") + xlab("Rank Weight Steepness (epsilon)")
metric5_e
dev.off()

postscript("bar_metric5_loss.eps", fonts=c("serif", "Palatino"))
metric5_loss <- ggplot(results) + 
  geom_bar(aes(as.factor(loss), as.numeric(metric5percent)), stat="summary", fun.y=mean) + 
  ggtitle("Percent Top 5 Correct Ranking by Loss") +
  ylab("Percent Top 5 Correct") + xlab("loss type")
metric5_loss
dev.off()

#metric 10
postscript("bar_metric10_e.eps")
metric10_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(results$metric10percent)), stat = "summary", fun.y = "mean") +
  ggtitle("Percent Top 10 Correct Ranking by e") +
  ylab("Percent Top 10 Correct") + xlab("Rank Weight Steepness (epsilon)")
metric10_e
dev.off()

postscript("bar_metric10_loss.eps", fonts=c("serif", "Palatino"))
metric10_loss <- ggplot(results) + 
  geom_bar(aes(as.factor(loss), as.numeric(results$metric10percent)), stat = "summary", fun.y = "mean") +
  ggtitle("Percent Top 10 Correct Ranking by Loss") +
  ylab("Percent Top 10 Correct") + xlab("loss type")
metric10_loss
dev.off()

#metric 15
postscript("bar_metric15_e.eps")
metric15_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(results$metric15percent)), stat = "summary", fun.y = "mean") + 
  ggtitle("Percent Top 15 Correct Ranking by e") +
  ylab("Percent Top 15 Correct") + xlab("Rank Weight Steepness (epsilon)")
metric15_e
dev.off()

postscript("bar_metric15_loss.eps", fonts=c("serif", "Palatino"))
metric15_loss <- ggplot(results) + 
  geom_bar(aes(as.factor(loss), as.numeric(results$metric15percent)), stat = "summary", fun.y = "mean")+ 
  ggtitle("Percent Top 15 Correct Ranking by Loss") +
  ylab("Percent Top 15 Correct") + xlab("loss type")
metric15_loss
dev.off()

# for labels 
# postscript("metric_loss_rankSteepness_plot.eps")
# lossrankSteepness_plot <- ggplot(results, aes(x = factor(loss), y = metric, colour = factor(rankSteepness))) + 
#   geom_jitter(size = .4, width = .15) +
#   ggtitle("Percent Top 10 Correct Ranking by Changes in Loss and Rank Steepness") +
#   ylab("Percent Top 10 Correct") + xlab("Loss"); lossrankSteepness_plot
# dev.off()


