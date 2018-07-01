## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#run in ranking
setwd("/Users/cora/git_repos/RankingMethods")
source("ranking_function.r")
source("sim_ranking_experiment.r")
#creates clean returnDF
returnDF <- as.data.frame(matrix(nrow = 0, ncol = 16))
names(returnDF) <- c("sim", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                       "n_assignment_method", 
                       "rankPriority", "rankSteepness", 
                       "f", "loss", "totalLoss", "ranking", "metric")
results <- returnDF

#data characteristics
for (n in c(100)){ #numItems 
  for (n_min in c(50)){ #what really matters here in number of events 
    for (n_max in c(425)){
      #ranking settings. How do data characteristics impact performance here?
      for (l in c(1, 2)){ #loss types square and absolute
        for (rankPriority in c("even", "top", "bottom")){ #
          for (rankSteepness in c(0.25, 0.5, 0.75)){ #
            #add results to the results df
            for (sim in c(1:100)){#100 or 1000 depending on time
            results <- rbind(results, RunSimulation(N = n, a_p = 1, b_p = 1, n_min = n_min, n_max = n_max, a_n = 1, b_n = 1, #data
                                                    n_assignment_method = "ascending", 
                                                    rankPriority = rankPriority, rankSteepness = rankSteepness, #rankWeights
                                                    parameter = NULL, loss = l, 
                                                    f=identity,  #ranking settings
                                                    n_sim = 1, 
                                                    fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
                                                    metric = TRUE))
            }
          }
        }
      }
    }
  }
}

#saves results. Careful! This overwrites
save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_n100_home.RData") #saves as an R object


#load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_n100_home.RData") 
# head(results)

#### Main Questions for this Experiment ####
##increase gaps in parameters until they're trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior?
#think about orders of magnitude to start (want low, medium, high)

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
#se = sqrt((.1*.9)/20) #graph this out to see. If the ranking is always right, then n is too big. If ranks are uniform/terrible, then n too small.
# assignment method


