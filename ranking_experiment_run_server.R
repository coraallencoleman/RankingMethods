## Runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#  creates an .RData file for with parameters + ranks

# run in gangnon/ranking dir 

# move script to server:
#scp /Users/cora/git_repos/RankingMethods/*.r allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/

# run IN RESULTS FOLDER
#nohup /s/pkg/linux64/R/3.4.1/bin/Rscript ranking_experiment_run_server.r > nsim0809ScreenLog.txt &

# move results back to home computer 
#scp allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_TEST_0809.RData /Users/cora/git_repos/RankingMethods/results/ 
#scp allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/nsim0809ScreenLog.txt /Users/cora/git_repos/RankingMethods/results/ 

setwd("/ua/allencoleman/gangnon/ranking")
source("ranking_function.r")
#creates clean returnDF
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
                                                      n_sim = 2, 
                                                      fileRoot = "/Users/cora/git_repos/RankingMethods/results/"))
            }
          }
        }
      }
    }
  }
}


#CAREFUL! THIS OVERWRITES
save(results, file = "/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_TEST_0809.RData")

# load("/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_TEST_0809.RData") 
# head(df)