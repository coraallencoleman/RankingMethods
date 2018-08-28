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

# Strict Metric 5
for (i in 1:15){
  metricStrict5 <- list(I(RankMetricStrict(results$ranking[[i]], data = results$data[i][[1]],
                                order = "largest", topN = 5)))
  results$metricStrict5[i] <- metricStrict5
}

save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_metric_0824.RData") #saves as an R object
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_metric_0824.RData")

## PLOTS ##
#reduce to include only priority = top
results_top <- results[results$rankPriority == "top",]
results <- results_top

#Stricter Metric
#Top 5
results$metric5Strictpercent <- lapply(results$metricStrict5, mean)
setwd("/Users/cora/git_repos/RankingMethods/plots/")
ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("bar_metric5_e.eps")
metric5_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metric5percent)), stat="summary", fun.y=mean) + 
  ggtitle("Percent Top 5 Correct Ranking by e") +
  ylab("Percent Top 5 Correct") + xlab("Rank Weight Steepness (epsilon)")
metric5_e
dev.off()
## Bar Graphs ##
results$metric5percent <- lapply(results$metric5, mean)
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
results$metric10percent <- lapply(results$metric10, mean)
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
results$metric15percent <- lapply(results$metric15, mean)
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


