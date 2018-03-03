## Binomial Random Intercept n = 10 with conflicts ##
#simulate county-like data
cafes <- as.data.frame(matrix((seq(from = 1, to = 12, by = 1)), ncol = 1))
colnames(cafes) <- c("cafe")
cafes$true_p <- rep(c(.7, .725, .75, .775), each = 3) #four levels of true p
cafes$attempts <- c(rep(c(100, 1000, 10000), times = 4)) #create lots of variation here
cafes$SuccessfulConnections <- rbinom(n = 12, size = cafes$attempts, cafes$true_p)
## sim data + model ##
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sim_data = list(
  J = nrow(cafes), #should be 12
  n = with(cafes, attempts),
  count = with(cafes, SuccessfulConnections),
  cafe = with(cafes,as.integer(as.factor(cafe)))
)
## Create Model with Random Intercepts for Each County ##
sim_rand_int_model <- stan(file="/Users/cora/git_repos/RankingMethods/sim_randInt.stan",data=sim_data, seed = 10)

#get posterior means from stan model
p <- extract(sim_rand_int_model, pars = "p")[[1]]
post_mean_p <- apply(p, 2, mean)
#get credible intervals from stan model
credible_intervals <- t(apply(p, 2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE))

## Cafe Ranks ##
sim_ranks <- WeightedLossRanking(model = sim_rand_int_model, parameter = "p", f = rank, loss = 2, lossTotal = TRUE)

## Data Frame Output ##
rankedCafes <- as.data.frame(cafes)
rankedCafes$true_p <- cafes$true_p*100
rankedCafes$rank <- as.integer(sim_ranks) #this ranks lowest to highest
rankedCafes$post_mean_p <- post_mean_p
rankedCafes<-arrange(rankedCafes, desc(rank))
rankedCafes$Lower <- credible_intervals[,1]
rankedCafes$Upper <- credible_intervals[,2]
rankedCafes<-arrange(rankedCafes, post_mean_p) #posterior mean order

#Graphs
#Mean with CI
rankedCafes<-arrange(rankedCafes, post_mean_p)
post_means <- ggplot(rankedCafes, aes(cafe, y=post_mean_p)) + geom_point() + 
  geom_errorbar(aes(ymin=rankedCafes$Lower, ymax=rankedCafes$Upper), width=.1, color = "blue") + xlab("Cafes") + ylab("Posterior Means and CIs") + ggtitle("Posterior Means and Credible Intervals");post_means
ggsave(filename = "/Users/cora/git_repos/RankingMethods/plots/sim_posterior_meanandCIs.png", plot = post_means, device = png, width = 5, height = 5)

#Posterior Rank Plot
rankedCafes <- arrange(rankedCafes, desc(rank))
post_means <- ggplot(rankedCafes, aes(cafe, y=post_mean_p)) + geom_point() + 
  geom_errorbar(aes(ymin=rankedCafes$Lower, ymax=rankedCafes$Upper), width=.1) + xlab("Cafes") + ylab("Posterior Means and CIs") + ggtitle("Posterior Means and Credible Intervals");post_means
ggsave(filename = "/Users/cora/git_repos/RankingMethods/plots/sim_posterior_meanandCIs.png", plot = post_means, device = png, width = 5, height = 5)