#Presentation Simulation #
#goal: rank 10 items from lowest to highest

## Read in Data##
#Variation in sample size (variability) is more important (as long as there is some uncertainty about the rankings)
raw_data <- read.csv("/Users/cora/Dropbox/UW-Madison/Current Research/Gangnon/presentations/rankings_introduction/LBW.csv", header = TRUE)
m1_est <- read.csv("/Users/cora/Dropbox/UW-Madison/Current Research/Gangnon/presentations/rankings_introduction/m1_estimates.csv", header = TRUE)
## Create Model with Intercepts from Each County##
library(rstan)
library(rethinking)
m1 <- map2stan(
  alist(
    NumLBW ~ dbinom(NumBirths, p) ,
    logit(p) <- a_county[County] ,
    a_county[County] ~ dnorm(a, sigma) ,
    a ~ dnorm(0, 10) ,
    sigma ~ dcauchy(0,1)
  ), data=raw_data, iter=4000, chains=4, WAIC=FALSE) #remove WAIC to speed up

## Get Samples From the Posterior for Each County ##
samples = 10000
post <- extract.samples(m1)
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

# Absolute Value Loss Rank
AbsErrLossRnk <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    AbsErrLossRnk[i,j] <- mean(abs(ranks[i,]-j))
  }
}

# Zero One Loss Rank
ZeroOneLossRnk <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    ZeroOneLossRnk[i,j] <- mean(ranks[i,]!=j)
  }
}

LogitOrder<-apply(post$a_county,1,sort)
# Squared Error Loss Logit
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
SqErrLossLogit <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    SqErrLossLogit[i,j] <- mean((post$a_county[,i]-LogitOrder[j,])^2)
  }
}
# Abs Err Loss Logit
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
AbsErrLossLogit <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    AbsErrLossLogit[i,j] <- mean(abs(post$a_county[,i]-LogitOrder[j,]))
  }
}
# Zero One Loss Logit
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
ZeroOneLossLogit <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    ZeroOneLossLogit[i,j] <- mean((post$a_county[,i]!=LogitOrder[j,]))
  }
}

# Squared Error Loss Prob
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
SqErrLossProb <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    SqErrLossProb[i,j] <- mean((1/(1+exp(-post$a_county[,i]))-1/(1+exp(-LogitOrder[j,])))^2)
  }
}

# Absolute Error Loss Prob
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
AbsErrLossProb <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    AbsErrLossProb[i,j] <- mean(abs(1/(1+exp(-post$a_county[,i]))-1/(1+exp(-LogitOrder[j,]))))
  }
}

# Zero One Error Loss Prob
# mean((ranks[1,]-1)^2) #for county 1 ranked at position 1
# mean((ranks[1,]-3)^2)#for county 1 ranked at position 3
ZeroOneLossProb <- matrix(NA,21,21)
for (i in 1:21) {
  for (j in 1:21) {
    ZeroOneLossProb[i,j] <- mean((1/(1+exp(-post$a_county[,i]))!=1/(1+exp(-LogitOrder[j,]))))
  }
}

## b. Find optimal ranking
#using the solve_LSAP function in the clue package in R.
library(clue)
#rank scale best ranks
solve_LSAP(SqErrLossRnk)
solve_LSAP(AbsErrLossRnk)
solve_LSAP(ZeroOneLossRnk)

#logit scale best ranks
solve_LSAP(SqErrLossLogit)
solve_LSAP(AbsErrLossLogit)
solve_LSAP(ZeroOneLossLogit)

#prob scale best ranks
solve_LSAP(SqErrLossProb)
solve_LSAP(AbsErrLossProb)
solve_LSAP(ZeroOneLossProb)

## TABLES ##
library(xtable)
#slide 10 Counties by Percent Low Birth Weight
raw_data$p <- (raw_data$NumLBW/raw_data$NumBirths)
raw_data$p_round <- round(raw_data$p*100, digits=1)
#get CIs
raw_data$lowerCI <- round(100*(raw_data$p - 1.96*(sqrt((raw_data$p*(1-raw_data$p))/raw_data$NumBirths))), digits = 1)
raw_data$upperCI <- round(100*(raw_data$p + 1.96*(sqrt((raw_data$p*(1-raw_data$p))/raw_data$NumBirths))), digits = 1)
t1 <- raw_data[order(raw_data$p),c(3, 4, 5, 7, 8, 9)] #sort by observed p
xtable(t1, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#slide 11 Present Bayesian estimates/CIs next to estimates from Slide 10
#Use same order for counties used Slide 10 but don’t present data (NumLBW, NumBirths) here
#sort by observed p 
#CIs
#raw_data$post_mean <- apply(100*logistic(post$a_count), 2, mean)
raw_data$post_sd <- apply(100*logistic(post$a_count), 2, sd)
raw_data$BayesianLowerCI <- round(100*logistic(m1_est$lower), digits = 1)
raw_data$BayesianUpperCI <- round(100*logistic(m1_est$upper), digits = 1)
raw_data$BayesianEstimate <- round(100*logistic(m1_est$Mean), digits = 1)
t2 <- raw_data[order(raw_data$p),c(3, 7, 8, 9, 14, 12, 13)]
xtable(t2, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#slide 12
#Posterior mean Present posterior distributions for county-specific ranks
raw_data$post_mean_rank <- apply(ranks, 1, mean)
#raw_data$post_mean <- round(100*apply(logistic(post$a_count), 2, mean), digits = 1)
#five num summary Present posterior distributions for county-specific ranks
raw_data$min <- apply(ranks, 1, min)
raw_data$twentyfive <- apply(ranks, 1, quantile, 0.25)
raw_data$median <- apply(ranks, 1, quantile, 0.5)
raw_data$seventyfive <- apply(ranks, 1, quantile, 0.75)
raw_data$max <- apply(ranks, 1, max)
#Keep Bayesian estimates/CIs from previous slide
t3 <- raw_data[order(raw_data$p),c(3, 12,14, 13, 15, 16, 17, 18, 19, 20)] #Use same order used for Slide 10
xtable(t3, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#slide 13 REORDERED BY POSTERIOR MEAN RANKS
t4 <- raw_data[order(raw_data$post_mean_rank),c(3, 12,14, 13, 15, 16, 17, 18, 19, 20)]
xtable(t4, caption = NULL, include.rownames = FALSE)

#slide 14 Introduce rankings based on alternative loss functions (add to previous table)
#Maintain the same order as prior slide
#Note: Salem moves from rank 10 to rank 8
#raw_data$MeanSquareErrorRanking <- as.integer(solve_LSAP(SqErrLossRnk))
raw_data$AbsoluteErrorLossRanking <- as.integer(solve_LSAP(AbsErrLossRnk))
raw_data$ZeroOneLossRanking <- as.integer(solve_LSAP(ZeroOneLossRnk))
t5 <- raw_data[order(raw_data$post_mean),c(3, 14, 15, 16, 17, 18, 19, 20, 21, 22)] #Use same order used for Slide 10

options(xtable.include.rownames=F)
options(xtable.include.colnames=T)
xtable(t5, caption = NULL) #use \scalebox{0.65}{before tabular


#slide 15 histograms of ranks for 3 counties
sink(NULL)
jpeg("/Users/cora/Dropbox/UW-Madison/Current Research/Gangnon/presentations/rankings_introduction/images/hist.jpg", width = 700, height = 525,res=150)
hist(ranks[2,], xlim = c(0, 20), xlab = "", main = "", col = alpha("black",0.5)) #posterior distributions of ranks for Bergen
hist(ranks[18,], xlim = c(0, 20), xlab = "", add =TRUE, col = alpha("blue",0.5)) #posterior distributions of ranks for Som
hist(ranks[17,], xlim = c(0, 20), xlab = "", add =TRUE, col = alpha("green",0.5)) #posterior distributions of ranks for Sal
op <- par(cex = 0.6)
legend("topright", legend = c("Bergen", "Somerset", "Salem"), col = c(alpha("black",0.5), alpha("blue",0.5), alpha("green",0.5)), pch = 19)
# add legend TODO
dev.off()

## OLD Rankings Tables
#raw_data[solve_LSAP(SqErrLossRnk),] TO SORT
county_names <- raw_data$County
raw_data$p <- raw_data$NumLBW/raw_data$NumBirths
#illustrative rank table w/ raw nums
t2 <- raw_data[solve_LSAP(SqErrLossRnk),c(3, 4, 5, 6)]
#create LATEX table
xtable(t2, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#3 scales results table using square err
t3 <- raw_data[,c(3, 6)]
t3$RankScaleRanking <- as.integer(solve_LSAP(SqErrLossRnk))
t3$LogitScaleRanking <- as.integer(solve_LSAP(SqErrLossLogit))
t3$ProbScaleRanking <- as.integer(solve_LSAP(SqErrLossProb))
xtable(t3, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#sq err abs loss zero one loss for rank
t4 <- raw_data[,c(3, 6)]
t4$MeanSquareErrorRanking <- as.integer(solve_LSAP(SqErrLossRnk))
t4$AbsoluteErrorLossRanking <- as.integer(solve_LSAP(AbsErrLossRnk))
t4$ZeroOneLossRanking <- as.integer(solve_LSAP(ZeroOneLossRnk))
xtable(t4, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#losses on prob scale
t5 <- raw_data[,c(3, 6)]
t5$MeanSquareErrorRanking <- as.integer(solve_LSAP(SqErrLossProb))
t5$AbsoluteErrorLossRanking <- as.integer(solve_LSAP(AbsErrLossProb))
t5$ZeroOneLossRanking <- as.integer(solve_LSAP(ZeroOneLossProb))
xtable(t5, caption = NULL, include.rownames = FALSE) #use \scalebox{0.7}{before tabular

#all optimal rankings in one table
cbind(items = i, AbsoluteValueRank = OptimalAV, ZeroOneRank = Optimal01, SquaredErrorRank = OptimalSE, OriginalMeans = c(35, 35, 35, 34, 34, 34, 33, 33, 33, 33))
#illustrate issue by giving naive perfect ranks based on sqerror loss + rank scale (minimize for each location)
#1st: gives best rank for each county individually
apply(SqErrLossRnk,1,which.min) #2nd: loss function helps us compromise between counties. measures tolerance to moving
table(apply(SqErrLossRnk,1,which.min)) #SHOWS CONFLICTS

#last slide
#next steps: adding weight functions to prioritize particular counties or particular positions (top 10, extreme mainly)
