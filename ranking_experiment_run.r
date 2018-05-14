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
# variation in N #for (n in c(10, 15, 20)){#number
# do this simulation on rank, logit scales


for (n in c(20)){ #numItems
  for (l in c(1, 2)){ #loss types
  RunSimulation(N = n, a_p = 1, b_p = 1, n_min = 10, n_max = 30, a_n = 1, b_n = 1, #data
                n_assignment_method = "ascending", 
                rankPriority = "top", rankSteepness = .9, #rankWeights
                parameter = NULL, loss = l, f=identity, rankweights = "", #ranking settings
                n_sim = 1,
                fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
                metric = FALSE, metricFile = "/Users/cora/git_repos/RankingMethods/results/metricResults.csv")
  }
}
