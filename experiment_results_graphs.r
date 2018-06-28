#graphs to plot results from experiment

#keep parameters (use in title and organize in some way)
# could plot total loss or some other kind of metric

require(ggplot2)

## data cleaning ##
load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.RData") #called df
results <- as.data.frame(df)
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.character )
results[, c(1:8, 11, 14)] <- sapply( results[,c(1:8, 11, 14)], as.double )

## TOTAL LOSS ##
n_min_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(n_max))) + geom_point() + 
  geom_line() + ggtitle("Avg Total Loss By Changes in Binomial n min, max") + 
  ylab("Total Loss"); n_min_plot

changes_in_N_plot <- ggplot(results, aes(x = n_min, y = totalLoss, colour = factor(N))) + geom_point() + 
  ggtitle("Avg Total Loss By Changes in N") + 
  ylab("Total Loss"); changes_in_N_plot

losses_plot <- ggplot(results, aes(x = N, y = totalLoss, colour = factor(loss))) + geom_point() + 
  ggtitle("Avg Total Loss By Changes in N") + 
  ylab("Total Loss"); losses_plot


## RANKING QUALITY METRIC ##
n_min_plot <- ggplot(results, aes(x = n_min, y = metric, colour = factor(n_min))) + geom_point() + 
  ggtitle("Percent Correct Ranking By \nChanges in Binomial n min, max") + 
  ylab("Percent Correct"); n_min_plot

changes_in_N_plot <- ggplot(results, aes(x = N, y = metric, colour = factor(n_min))) + geom_point() + 
  ggtitle("Percent Correct Ranking By \nChanges in N") + 
  ylab("Percent Correct"); changes_in_N_plot

losses_plot <- ggplot(results, aes(x = N, y = metric, colour = factor(loss))) + geom_point() + 
  ggtitle("Percent Correct Ranking By Changes in N and loss") + 
  ylab("Percent Correct"); losses_plot

