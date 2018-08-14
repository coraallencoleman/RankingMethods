## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#run in ranking
setwd("/Users/cora/git_repos/RankingMethods")
source("ranking_function.r")
#creates clean results
currResults <- as.data.frame(matrix(nrow = 0, ncol = 15))
names(currResults) <- c("sim", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                       "n_assignment_method", 
                       "rankPriority", "rankSteepness", 
                       "f", "loss", "totalLoss", "ranking")
results <- currResults

#data characteristics
for (n in c(25, 50, 100)){ #numItems 
  for (n_min in c(75, 100)){ #what really matters here in number of events 
    for (n_max in c(200, 400)){
      for (a_n in c(0.8, 1, 1.2)){
        for (b_n in c(0.8, 1, 1.2)){
          for (a_p in c(0.8, 1, 1.2)){
            for (b_p in c(0.8, 1, 1.2)){
        #add results to the results df
        results <- rbind(results, RunSimulation(N = n, a_p = a_p, b_p = b_p, n_min = n_min, n_max = n_max, a_n = a_n, b_n = b_n, #data
                                                 n_assignment_method = "ascending", 
                                                #ranking settings
                                                rankPriority = c("even", "top", "bottom"), rankSteepness = c(0.01, 0.05, 0.1), #rankWeights
                                                parameter = NULL, loss = c(1,2), 
                                                f=identity,  
                                                n_sim = 1))
            }
          }
        }
      }
    }
  }
}

#saves results. Careful! This overwrites
save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0814.RData") #saves as an R object

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0814.RData") 
head(results)
