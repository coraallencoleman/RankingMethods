## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#run in ranking
setwd("/Users/cora/git_repos/RankingMethods")
source("ranking_function.r")
#creates clean returnDF
currResults <- as.data.frame(matrix(nrow = 0, ncol = 15))
names(currResults) <- c("sim", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                       "n_assignment_method", 
                       "rankPriority", "rankSteepness", 
                       "f", "loss", "totalLoss", "ranking")
results <- currResults

#data characteristics
for (n in c(3)){ #numItems 
  for (n_min in c(50)){ #what really matters here in number of events 
    for (n_max in c(425)){
      #add results to the results df
        results <- rbind(results, RunSimulation(N = n, a_p = 1, b_p = 1, n_min = n_min, n_max = n_max, a_n = 1, b_n = 1, #data
                                                 n_assignment_method = "ascending", 
                                                  rankPriority = c("even", "top", "bottom"), rankSteepness = rankSteepness, #rankWeights
                                                  parameter = NULL, loss = c(1,2), 
                                                  f=identity,  #ranking settings
                                                  n_sim = 2, 
                                                  fileRoot = "/Users/cora/git_repos/RankingMethods/results/"))
    }
  }
}

#saves results. Careful! This overwrites
save(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_n1_home_0807.RData") #saves as an R object

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_n1_home_0807.RData") 
head(results)


