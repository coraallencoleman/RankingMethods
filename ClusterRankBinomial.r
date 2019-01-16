# Rank and Cluster Binomial Data WI Example

library(tidyverse)
library(reshape2)
library(coda)
library(clue)

setwd("/Users/cora/git_repos/RankingMethods/")

### ALL TOGETHER ###
rawDataFilename <- "lbw_wi.csv"
data <- cleanData(rawDataFilename)
postSamples <- createPostSamples(data)
postRanks <- posteriorRanks(postSamples)
postOrders <- posteriorOrder(postSamples)
HPDMatrices <- HPDIntervalMatrices(postSamples, postRanks, postOrders)


### Functions ###

cleanData <- function(rawDataFile){
  #TODO specify file format
  rawData <- read_csv(rawDataFile) #example data
  #remove missing, then create 2 new columns: N count normal birth weight nbw and % lbw
  data  <- rawData  %>% mutate(nbw=births-lbw,perc_lbw=lbw/births*100) %>% filter(!is.na(lbw))
  return(data)
} 

createPostSamples <- function(data){
  #sample from LBW beta distribution using lbw as shape1, nbw as shape2 Q
  #if p follows beta(a,b) and y follows bin(n,p) then p|y follows beta(a+y, b+n-y) 
  #This is an improper prior (a,b cannot be 0), but this becomes a proper posterior  as long as you have > 0 successes and > 0 failures. close to doing a bootstrap. like a null model.
  postSamples <- replicate(10000, rbeta(n = 71, shape1=lbw_wi$lbw, shape2=lbw_wi$nbw))
  return(postSamples)
}

posteriorRanks <- function(postSamples){
  postRanks <- apply(postSamples,2,rank) #when you apply by rows (2), it doesn't flip. Always outputs same. 
  return(postRanks)
}

posteriorOrder <- function(postSamples){
  postOrder <- apply(postSamples,2,sort) #when you apply by rows (2), it doesn't flip. Always outputs same. 
  return(postOrder)
}

HPDIntervalMatrices <- function(postSamples, postRanks, postOrders){
  #create 3 71 X 2 MATRIX of HPD intervals
  

  samples_HPD <- t(apply(postSamples,1,function(x) HPDinterval(mcmc(x)))) 
  postRanks_HPD <- t(apply(postRanks,1,function(x) HPDinterval(mcmc(x))))
  postOrders_HPD <- t(apply(postOrders,1,function(x) HPDinterval(mcmc(x))))
  HPD <- list(samples_HPD, postRanks_HPD, postOrders_HPD)
  return(HPD)
}

rankLoss <- function(postSamples, postRank){
#calculate loss on rank scale
  postRanks <- apply(postSamples,2,rank)
  SEL_rank <- matrix(NA,71,71) #square error loss
  for (i in 1:71) {
    for (j in 1:71) {
      SEL_rank[i,j] <- mean((postRanks[i,]-j)^2)
    }
  }
  return(rankLoss)
}

probLoss <- function(postSamples, postOrder){
#calculate loss on prob scale?
  SEL_prob <- matrix(NA,71,71) 
  for (i in 1:71) {
    for (j in 1:71) {
      SEL_prob[i,j] <- mean((postSamples[i,]-postOrder[j,])^2)
    }
  }
}

optimalRanks <- function(postSamples, SEL_rank, SEL_prob){
  postRanks <- apply(postSamples,2,rank)
  #calculates optimal ranks based on posterior, posterior stats, loss calculations
  
  postRanks_pm <- apply(postRanks,1,mean) #posterior mean 
  order_stats <- order(postRanks_pm) #TODO is this right? previous #TODO: order_stats doesnt exist yet. needs to be created. post means of rows
  
  # CREATE DF #s
  postStatsDF <- data.frame("lbw_rank_pm" = apply(postRanks,1,mean))#posterior mean)
  postStatsDF$lbw_rank_SEL_rank_ind_opt <- apply(SEL_rank,1,which.min) #individually optimal ranks
  postStatsDF$lbw_rank_SEL_rank_joint_opt <- as.numeric(solve_LSAP(SEL_rank)) #jointly optimal ranks
  postStatsDF$lbw_rank_SEL_prob_ind_opt <- apply(SEL_prob,1,which.min) #individually optimal ranks on pr scale
  postStatsDF$lbw_rank_SEL_prob_joint_opt <- as.numeric(solve_LSAP(SEL_prob)) #jointly optimal ranks on pr scale
  return(postStatsDF)
}

#Q: to get comparison probabilities to compare. What's pr that pepin has sm rate than mil
#creates 71x71 matrix Pr(county i LBW % <county j LBW %) This is comparison-wise loss. 
#a potential loss is pr of those comparisons you get right = sum(upper triangle). 
#want to min(sum lower triangle). There isnt a clear algorithm that solves this. 
#Diagonal should be .5 but it's 0. this only matters if looking for min
#TODO ARE THESE USED?
comparisonProbabilities <- function(postSamples){
  prob1 <- matrix(NA,71,71)
  for (i in 1:71) {
    for (j in 1:71) {
      prob1[i,j] <- mean(postSamples[i,]<postSamples[j,])
    }
  }
  return(prob1)
}

#TODO is this important?
#prob1a <- prob1[order(rank2a),order(rank2a)] 


## VISUALIZATIONS ##


DF_selr_selp <- function(data, postSamples, postStatsDF){
  #creates data frame for all the things we've calculated above for viz
 #lbw_results_selr
  selr <- data_frame(county=lbw_wi$county, #SEL rank scale DF
                                 LBW_pm=apply(postSamples,1,mean),
                                 LBW_LCL=lbw_HPD[,1],
                                 LBW_UCL=lbw_HPD[,2],
                                 LBW_rank_pm=lbw_rank_pm,
                                 LBW_rank_io=lbw_rank_SEL_rank_ind_opt,
                                 LBW_rank_jo=lbw_rank_SEL_rank_joint_opt,
                                 LBW_rank_LCL=lbw_rank_HPD[,1],
                                 LBW_rank_UCL=lbw_rank_HPD[,2]) %>% 
    arrange(LBW_rank_jo) #arranges df in this order
  #lbw_results_selp
  selp <- data_frame(county=lbw_wi$county, #pr scale DF
                                 LBW_pm=apply(postSamples,1,mean),
                                 LBW_LCL=lbw_HPD[,1],
                                 LBW_UCL=lbw_HPD[,2],
                                 LBW_rank_io=lbw_rank_SEL_prob_ind_opt,
                                 LBW_rank_jo=lbw_rank_SEL_prob_joint_opt,
                                 LBW_rank_LCL=lbw_rank_HPD[,1],
                                 LBW_rank_UCL=lbw_rank_HPD[,2]) %>% 
    arrange(LBW_rank_jo)
  return(selr, selp)
}



VizDataFrame <- function(postRanks, lbw_wi, lbw_rank_SEL_prob_joint_opt){
  #creates pr and scales them. Pr(given county at that rank relative to its posterior mode)
  #gives realtive prob in rank direction but not relative to other counties
  post_ranks <- t(apply(postRanks,1,function(x) table(factor(x,levels=1:71))/max(table(factor(x,levels=1:71)))))
  #post_ranks <- t(apply(postRanks,1,function(x) table(factor(x,levels=1:71))/10000)) #real posterior probabilities. issue: greys out most places
  #post_ranks <- post_ranks/max(post_ranks) #Q: 
  post_df <- melt(post_ranks) #makes it long because that's what gg plot needs
  post_df$county <- lbw_wi$county[post_df$Var1]
  post_df$rank <- post_df$Var2
  #post_df$pos <- order_stats[post_df$Var2] 
  #TODO order_stats doesnt exist yet. needs to be created. post means of rows. See q in meeting notes 1/18/19
  post_df$county <- factor(post_df$county,levels=rev(lbw_wi$county[order(lbw_rank_SEL_prob_joint_opt)]),ordered=TRUE)
  #puts them in correct order in visualization. Makes this an ordered factor. Reverse for low at top, high at bottom
}

gradientViz <- function(post_df, filename){
  #TODO order by the variable of interest (e.g. lbw_rank_SEL_prob_joint_opt)
  #grey scale rank viz #Q 
  ps.options(fonts=c("serif"), width = 7, height = 7)
  postscript("plots/wi_LBW_rank.eps")
  ggplot(post_df,aes(x=rank,y=county,color=value))+
    geom_point(pch=15,cex=2)+
    scale_y_discrete("") +
    scale_x_continuous("",breaks=seq(1,71,by=5)) +
    #  scale_x_continuous(breaks=order_stats[c(1:7,seq(10,60,by=10),67:71)],minor_breaks=order_stats,labels=c(1:7,seq(10,60,by=10),67:71))+
    scale_color_gradient(low="white",high="black",limits=c(0,1),guide=FALSE)+
    theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
    xlab("Rank") + ggtitle("County Ranks by Rank Frequency")
  dev.off()
  return("plot saved")
}
