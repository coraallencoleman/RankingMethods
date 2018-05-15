##Main Questions for this Experiment:
##increase gaps in parameters until they're trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior?

### Simulations TODO
# variation in N
# different losses
# parameter scale probability, rank, logit scale
# weights vs no weights (part of ranking)

#DATA
# even gaps
# uneven gaps #qbeta(1:N/(N+1), 1, 1). Vary the last two parameters for variable gap size
#p c()# look at beta distributions for alpha and beta. could crowd them up at one edge. will be symmetric
#n
#for below, think about how big average n should be to get p in CI
#n min: c(50) explore this to see . what differences in p1 and p2 coudl you find with this n? do a power calculation basically
#n max: se = sqrt(p*q/n). think about how small this would have to be to think about how big n needs to be
# assignment method
se = sqrt((.1*.9)/20) #graph this out to see. If its always right, then n is too big. If ranks are uniform, then n too small.

for (n in c(25, 50, 100)){ #numItems
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
