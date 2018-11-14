#small example

library(tidyverse)
library(reshape2)
library(coda)
library(clue)
library(xtable)

setwd("/Users/cora/git_repos/RankingMethods")

lbw_ct <- read_csv("data/lbw_ct.csv")

lbw_ct <- lbw_ct %>% mutate(nbw=births-lbw,perc_lbw=lbw/births*100) %>% filter(!is.na(lbw))

#sample from LBW beta distribution using lbw as shape1, nbw as shape2 Q
lbw_samples <- replicate(10000,
                         rbeta(n = 8, shape1=lbw_ct$lbw, shape2=lbw_ct$nbw))
#if p follows beta(a,b) and y follows bin(n,p) then p|y follows beta(a+y, b+n-y) 
#This is an improper prior (a,b cannot be 0), but this becomes a proper posterior 
#as long as you have > 0 successes and > 0 failures. close to doing a bootstrap. like a null model.

lbw_ranks <- apply(lbw_samples,2,rank)
lbw_order <- apply(lbw_samples,2,sort) 
#when you do things by rows (2), it doesn't flip. Always outputs same. 

#creates 8 X 2 MATRIX of HPD intervals
lbw_HPD <- t(apply(lbw_samples,1,function(x) HPDinterval(mcmc(x)))) 
lbw_order_stats <- t(apply(lbw_ranks, 1, mean))
lbw_rank_HPD <- t(apply(lbw_ranks,1,function(x) HPDinterval(mcmc(x))))
lbw_order_HPD <- t(apply(lbw_order,1,function(x) HPDinterval(mcmc(x))))

#calculating loss on rank scale
SEL_rank <- matrix(NA,8,8) #square error loss
for (i in 1:8) {
  for (j in 1:8) {
    SEL_rank[i,j] <- mean((lbw_ranks[i,]-j)^2)
  }
}

#calculating loss on prob scale?
SEL_prob <- matrix(NA,8,8) 
for (i in 1:8) {
  for (j in 1:8) {
    SEL_prob[i,j] <- mean((lbw_samples[i,]-lbw_order[j,])^2)
  }
}

lbw_rank_pm <- apply(lbw_ranks,1,mean) #posterior mean
lbw_rank_SEL_rank_ind_opt <- apply(SEL_rank,1,which.min) #individually optimal ranks
lbw_rank_SEL_rank_joint_opt <- as.numeric(solve_LSAP(SEL_rank)) #jointly optimal ranks

lbw_rank_SEL_prob_ind_opt <- apply(SEL_prob,1,which.min) #individually optimal ranks on pr scale
lbw_rank_SEL_prob_joint_opt <- as.numeric(solve_LSAP(SEL_prob)) #jointly optimal ranks on pr scale

#Q: to get comparison probabilities to compare. What's pr that pepin has sm rate than mil
#creates 8x8 matrix Pr(county i LBW % <county j LBW %) This is comparison-wise loss. 
#a potential loss is pr of those comparisons you get right = sum(upper triangle). 
#want to min(sum lower triangle). There isnt a clear algorithm that solves this. 
#Diagonal should be .5 but its 0. this only matters if looking for min
prob1 <- matrix(NA,8,8)
for (i in 1:8) {
  for (j in 1:8) {
    prob1[i,j] <- mean(lbw_samples[i,]<lbw_samples[j,])
  }
}
#prob1a <- prob1[order(rank2a),order(rank2a)]

## VISUALIZATIONS ##

#creates data frame for all the things we've calculated above for viz
lbw_results_selr <- data_frame(county=lbw_ct$county, #SEL rank scale DF
                               LBW_pm=apply(lbw_samples,1,mean),
                               LBW_LCL=lbw_HPD[,1],
                               LBW_UCL=lbw_HPD[,2],
                               LBW_rank_pm=lbw_rank_pm,
                               LBW_rank_io=lbw_rank_SEL_rank_ind_opt,
                               LBW_rank_jo=lbw_rank_SEL_rank_joint_opt,
                               LBW_rank_LCL=lbw_rank_HPD[,1],
                               LBW_rank_UCL=lbw_rank_HPD[,2]) %>% 
  arrange(LBW_rank_jo)

lbw_results_selp <- data_frame(county=lbw_ct$county, #pr scale DF
                               LBW_pm=apply(lbw_samples,1,mean),
                               LBW_LCL=lbw_HPD[,1],
                               LBW_UCL=lbw_HPD[,2],
                               LBW_rank_io=lbw_rank_SEL_prob_ind_opt,
                               LBW_rank_jo=lbw_rank_SEL_prob_joint_opt,
                               LBW_rank_LCL=lbw_rank_HPD[,1],
                               LBW_rank_UCL=lbw_rank_HPD[,2]) %>% 
  arrange(LBW_rank_jo)

#creates pr and scales them. Pr(given county at that rank relative to its posterior mode)
#gives realtive prob in rank direction but not relative to other counties
post_ranks <- t(apply(lbw_ranks,1,function(x) table(factor(x,levels=1:8))/max(table(factor(x,levels=1:8)))))
#post_ranks <- t(apply(lbw_ranks,1,function(x) table(factor(x,levels=1:8))/10000)) #real posterior probabilities. issue: greys out most places
#post_ranks <- post_ranks/max(post_ranks) #Q: 
post_df <- melt(post_ranks) #makes it long because that's what gg plot needs
post_df$county <- lbw_ct$county[post_df$Var1]
post_df$rank <- post_df$Var2
post_df$pos <- lbw_order_stats[post_df$Var2]
post_df$county <- factor(post_df$county,levels=rev(lbw_ct$county[order(lbw_rank_SEL_prob_joint_opt)]),ordered=TRUE)
#puts them in correct order in visualization. Makes this an ordered factor. Reverse is because he wants low at top, high at bottom

##GRAPHICS 
#grey scale rank viz #Q 
ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("plots/ct_LBW_rank.eps")
ggplot(post_df,aes(x=rank,y=county,color=value))+
  geom_point(pch=15,cex=10)+
  scale_y_discrete("") +
  scale_x_continuous("",breaks=seq(1,8,by=5)) +
  #  scale_x_continuous(breaks=lbw_order_stats[c(1:7,seq(10,60,by=10),67:8)],minor_breaks=lbw_order_stats,labels=c(1:7,seq(10,60,by=10),67:8))+
  scale_color_gradient(low="white",high="black",limits=c(0,1),guide=FALSE)+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  xlab("Rank") + ggtitle("County Ranks by Rank Frequency")
dev.off()

##TABLES
xtable()