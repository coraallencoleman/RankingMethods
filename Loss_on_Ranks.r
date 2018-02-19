### Loss Functions on the Ranking Scale ###

##Cora Allen-Coleman Feb 2018 ##

## for now, allows for only random intercept binomial multilevel bayesian model ##

library(rstan)

##Data set up ##
#Data
raw_data <- read.csv("/Users/cora/git_repos/RankingMethods/data/LBW.csv", header = TRUE)
#subset data to include only columns used by stan
raw_data <- raw_data[, c("County", "NumLBW", "NumBirths")]
raw_data$County <- as.integer(as.factor(raw_data$County)) #set County data to numeric
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
data = list(
  J = nrow(raw_data),
  n = with(raw_data, NumBirths),
  count = with(raw_data, NumLBW),
  county = with(raw_data,as.integer(as.factor(County)))
)

## Create Model with Random Intercepts for Each County ##
rand_int_model <- stan(file="/Users/cora/git_repos/RankingMethods/loss_on_ranks.stan",data=data, seed = 10)

## Get Estimates for County P from the Posterior for Each County ##
alpha = rstan::extract(rand_int_model, pars="alpha")
alpha = alpha$alpha
dim(al)
alpha_means <- apply(alpha,2,mean)
alpha_sd <- apply(alpha,2,sd)
randInt_para[3,2] <- sd(alpha)

intercept = rstan::extract(rand_int_model, pars="intercept")$intercept #overall intercept
int_mean <- mean(intercept); overall_p <- invlogit(int_mean)

### graph estimates of shrinkage ##
county_p = rstan::extract(rand_int_model, pars="p")$p
dim(county_p)
p_means <- apply(county_p,2,mean)
p_sd <- apply(county_p,2,sd)
county <- 1:length(p_original)
p_original <- raw_data$NumLBW/raw_data$NumBirths 
p_estimates <- data.frame(p_means, p_sd, p_original, county)
ggplot(p_estimates, aes(x=county, y=p_means)) + geom_errorbar(aes(ymin=p_means-2*p_sd, ymax=p_means+2*p_sd), width=.1) +
  geom_point(alpha = .5, shape = 1) + geom_point(aes(x=county, y=p_original), colour = "blue", size = .75) + 
  geom_hline(yintercept = overall_p, alpha = 1, colour = "green") + 
  xlab("NJ County") + ylab("Proportion Low Birth Weight") + ggtitle("Random Intercept Binomial Model: Low Birth Weight by County")
#######

post$a_county #each of these is logit(p_i)
ranks <- apply(post$a_county, 1, rank)

## Posterior probabilities of being assigned every rank for each item ##
#summaries of that posterior distribution
apply(ranks,1,quantile)
#mean ranks for each county
apply(ranks,1,mean)
#optimal ranks over squared error loss
rank(apply(ranks,1,mean))


## Calculate Loss ##

# Squared Error Loss Rank
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
SqErrLossRnk <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    SqErrLossRnk[i,j] <- mean((ranks[i,]-j)^2)
  }
}