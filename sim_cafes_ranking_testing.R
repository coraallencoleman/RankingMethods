## Binomial Random Intercept n = 10 with conflicts ##
library(rstan)
library(ggplot2)
#simulate county-like data
cafes <- as.data.frame(matrix((seq(from = 1, to = 12, by = 1)), ncol = 1))
colnames(cafes) <- c("cafe")
cafes$true_p <- seq(from = 0.6, to = 0.72, by = 0.01) #four levels of true p
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
post_means <- ggplot(rankedCafes, aes(cafe, y=post_mean_p)) + geom_point() + 
  geom_errorbar(aes(ymin=rankedCafes$Lower, ymax=rankedCafes$Upper), width=.1) + xlab("Cafes") + ylab("Posterior Means and CIs") + ggtitle("Posterior Means and Credible Intervals")
ggsave(filename = "/Users/cora/git_repos/RankingMethods/plots/sim_posterior_meanandCIs.png", plot = post_means, device = png, width = 5, height = 5)



#create a plot for each of the cafe's estimate p[i] distribution
p2 <- data.frame(p)
names(p2) <- seq(1:5)
p2 <- stack(p2)
p2$cafe <- p2$ind
ggplot(p2, aes(x = values, color = cafe, fill = cafe, alpha = 0.1)) + geom_density()


#Equal Variance Example with Graph
x <- data.frame(item1=rnorm(10000, 0, 1), item2=rnorm(10000,1,1), item3=rnorm(10000,2, 1))
library(ggplot2);library(reshape2)
data<- melt(x)
ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.3)

#Unequal Variance Example
x <- data.frame(item1=rnorm(10000, 0, 1), item2=rnorm(10000,2,1), item3=rnorm(10000,2, 4))
library(ggplot2);library(reshape2)
data<- melt(x)
ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.3)




ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot()

#create stochastically ordered data set
cafes <- as.data.frame(matrix((seq(from = 1, to = 5, by = 1)), ncol = 1))
colnames(cafes) <- c("cafe")
cafes$true_p <- seq(from = 0.5, to = 0.7, by = 0.05) #levels of true p
cafes$attempts <- c(rep(100000, times = nrow(cafes))) #create lots of variation here
cafes$SuccessfulConnections <- rbinom(n = 5, size = cafes$attempts, cafes$true_p)
cafes$sim_p <- cafes$SuccessfulConnections/cafes$attempts
ggplot(cafes, aes(x = sim_p, color = cafe, fill = cafe, alpha = 0.1)) + geom_density()


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

p <- extract(sim_rand_int_model, pars = "p")[[1]]
