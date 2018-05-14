##Main Questions for this Experiment:
##increase gaps in parameters until they're trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior?

### Simulations TODO
# compare different ranking options
# function to save sim results to file
# even gaps
# uneven gaps #qbeta(1:N/(N+1), 1, 1). Vary the last two parameters for variable gap size
# different losses
# weights vs no weights (part of ranking)
# variation in N
# do this simulation on rank, logit scales

#file name convention

for (i in c()){#ranking options
  RunSimulation(n_sim = 2, rankFile = "/Users/cora/git_repos/RankingMethods/results/ranks.csv")
}
