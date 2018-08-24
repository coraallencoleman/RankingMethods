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
for (n in c(50, 200, 400)){ #numItems 
  for (n_min in c(75, 150, 300)){ #what really matters here in number of events 
    for (n_max in c(400, 600, 800)){
      for (a_n in c(0.5, 1, 3, 5)){
        for (b_n in c(0.5, 1, 3, 5)){
          for (a_p in c(0.5, 1, 3, 5)){
            for (b_p in c(0.5, 1, 3, 5)){
        #add results to the results df
        results <- rbind(results, RunSimulation(N = n, a_p = a_p, b_p = b_p, n_min = n_min, n_max = n_max, a_n = a_n, b_n = b_n, #data
                                                 n_assignment_method = "random", 
                                                #ranking settings
                                                rankPriority = c("top", "bottom", "both"), rankSteepness = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5), #rankWeights
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
save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0816.RData") #saves as an R object

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_TEST_0824.RData") 
head(results)

# test <- RunSimulation(N = 100, a_p = 2, b_p = 2, n_min = 20, n_max = 50, a_n = 1, b_n = 5, #data
#               n_assignment_method = "random", 
#               #ranking settings
#               rankPriority = c("top"), rankSteepness = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.9), #rankWeights
#               parameter = NULL, loss = c(2), 
#               f=identity,  
#               n_sim = 1)
