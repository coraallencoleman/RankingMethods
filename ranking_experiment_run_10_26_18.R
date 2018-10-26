## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#run in ranking
setwd("/Users/cora/git_repos/RankingMethods")
source("ranking_function.r")
#creates clean results
currResults <- as.data.frame(matrix(nrow = 0, ncol = 16))
names(currResults) <- c("sim", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                       "n_assignment_method", 
                       "rankPriority", "rankSteepness", 
                       "f", "loss", "totalLoss", "ranking", "data")
currResults$data <- I(list())
results <- currResults

#data characteristics
for (n in c(50)){ #numItems 
  for (n_min in c(100)){ #what really matters here in number of events 
    for (n_max in c(1000)){
      for (a_n in c(1)){
        for (b_n in c(1)){
          for (a_p in c(1)){
            for (b_p in c(1)){
        #add results to the results df
        results <- rbind(results, RunSimulation(N = n, a_p = a_p, b_p = b_p, n_min = n_min, n_max = n_max, a_n = a_n, b_n = b_n, #data
                                                 n_assignment_method = "random", 
                                                #ranking settings
                                                rankPriority = c("top"), rankSteepness = c(0, 0.0009, 0.009, 0.09, 0.9, 0.99), #rankWeights
                                                parameter = NULL, loss = c(2), 
                                                f=identity,  
                                                n_sim = 100))
            }
          }
        }
      }
    }
  }
}

#saves results. Careful! This overwrites
save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_1026.RData") #saves as an R object

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_1026.RData") 
head(results)

#for metrics and graphics see experiment_results_graphs.r