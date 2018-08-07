## Runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#  creates an .RData file for with parameters + ranks

# run in gangnon/ranking dir 

# move script to server:
#scp /Users/cora/git_repos/RankingMethods/*.r allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/

# run IN RESULTS FOLDER
#nohup /s/pkg/linux64/R/3.4.1/bin/Rscript ranking_experiment_run_server.r > nsimTESTScreenLog.txt &

# move results back to home computer 
#scp allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_TEST_0807.RData /Users/cora/git_repos/RankingMethods/results/ 
#scp allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/nsimTESTScreenLog.txt /Users/cora/git_repos/RankingMethods/results/ 

setwd("/ua/allencoleman/gangnon/ranking")
source("ranking_function.r")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#creates clean returnDF
returnDF <- as.data.frame(matrix(nrow = 0, ncol = 15))
names(returnDF) <- c("sim", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                     "n_assignment_method", 
                     "rankPriority", "rankSteepness", 
                     "f", "loss", "totalLoss", "ranking")
results <- returnDF


#data characteristics
for (n in c(10)){ #numItems 
  for (n_min in c(50)){ #what really matters here in number of events 
    for (n_max in c(425)){
      #add results to the results df
      for (sim in c(1:1)){#100 or 1000 depending on time
        results <- rbind(results, RunSimulation(N = n, a_p = 1, b_p = 1, n_min = n_min, n_max = n_max, a_n = 1, b_n = 1, #data
                                                n_assignment_method = "ascending", 
                                                rankPriority = "top", rankSteepness = 0.05, #rankWeights
                                                parameter = NULL, loss = 2, 
                                                f=identity,  #ranking settings
                                                n_sim = 1, 
                                                fileRoot = "/ua/allencoleman/gangnon/ranking/results/"))
      }
    }
  }
}

#CAREFUL! THIS OVERWRITES
save(results, file = "/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_TEST_0807.RData")

# load("/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_TEST_0807.RData") 
# head(df)