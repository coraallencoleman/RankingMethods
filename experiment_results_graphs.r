#graphs to plot results from experiment

#keep parameters (use in title and organize in some way)
# could plot total loss or some other kind of metric

require(ggplot2)
### LOAD IN DATA ##
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.RData") #df is called results

## data cleaning ##
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.numeric )

## TOTAL LOSS ##
n_min_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(n_max))) + geom_point() + 
  geom_line() + ggtitle("Avg Total Loss By Changes in Binomial n min, max") + 
  ylab("Total Loss"); n_min_plot

changes_in_N_plot <- ggplot(results, aes(x = N, y = totalLoss, colour = factor(n_max*n_min))) + geom_point() + 
  geom_line() + ggtitle("Avg Total Loss By Changes in N") + 
  ylab("Total Loss"); changes_in_N_plot

losses_plot <- ggplot(results, aes(x = N, y = totalLoss, colour = factor(loss))) + geom_point() + 
  geom_line() + ggtitle("Avg Total Loss By Changes in N") + 
  ylab("Total Loss"); losses_plot


## RANKING QUALITY METRIC ##
RankMetric(ranks, settings = data)


