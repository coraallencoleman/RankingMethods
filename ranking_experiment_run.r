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


for (i in c()){#ranking options
  RunSimulation(N = 10, a_p = 1, b_p = 1, n_min = 10, n_max = 30, a_n = 1, b_n = 1, #data
                n_assignment_method = "ascending", 
                rankPriority = "top", rankSteepness = .9, #rankWeights
                parameter = NULL, loss = 2, f=identity, rankweights = "", #ranking settings
                n_sim = 1,
                fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
                metric = FALSE, metricFile = "/Users/cora/git_repos/RankingMethods/results/metricResults.csv")
}
