### Loss Functions on the Probability Scale ###

##Cora Allen-Coleman Feb 2018 ##

## for now, allows for only random intercept binomial multilevel bayesian model parameterized with p##

## in future, nested for loops with be much faster if implemented in C ##

library(rstan)
library(clue)

## assumes user has a stan model

## Function ### requires rstan and clue
loss_on_prob <- function(model, loss){
  #TODO include paramter option too.
  #rstan::extract(rand_int_model, pars="p")$p
  #cat('"',parameter,'"', sep ="")
  ProbOrder<-apply(rstan::extract(model, pars="p")$p,1,sort)
  Loss <- matrix(NA,nrow(ranks),nrow(ranks))
  if (loss == "square"){
    for (i in 1:21) {
      for (j in 1:21) {
        Loss[i,j] <- mean((rstan::extract(model, pars="p")$p[,i]-ProbOrder[j,])^2)
      }
    }
  }
  else if(loss == "absolute"){
    for (i in 1:21) {
      for (j in 1:21) {
        Loss[i,j] <- mean(abs(rstan::extract(model, pars="p")$p[,i]-ProbOrder[j,]))
      }
    }
  }
  else if(loss == "zero"){
    for (i in 1:21) {
      for (j in 1:21) {
        Loss[i,j] <- mean(rstan::extract(model, pars="p")$p[,i]!=ProbOrder[j,])
      }
    }
  }
  return(solve_LSAP(Loss))
}

loss_on_prob(rand_int_model, "square")
