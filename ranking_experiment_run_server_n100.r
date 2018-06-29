## Runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#  creates an .RData file for with parameters + ranks

# run in gangnon/ranking dir 

# move script to server:
#scp /Users/cora/git_repos/RankingMethods/ranking_experiment_run_server_n100.r allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/

# run:
#nohup /s/pkg/linux64/R/3.4.1/bin/Rscript ranking_experiment_run_server_n100.r > nsim100ScreenLog.txt &

# move results back to home computer 
#scp allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/results/* /Users/cora/git_repos/RankingMethods/results/ 
#scp allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/nsim1ScreenLog.txt /Users/cora/git_repos/RankingMethods/results/ 

setwd("/ua/allencoleman/gangnon/ranking")
source("ranking_function.r")
source("sim_ranking_experiment.r")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#creates clean DF
returnDF <- as.data.frame(matrix(nrow = 0, ncol = 16))
names(returnDF) <- c("run", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                       "n_assignment_method", 
                       "rankPriority", "rankSteepness", 
                       "f", "loss", "totalLoss", "ranking", "metric")
results <- returnDF
#data characteristics
system.time(
for (n in c(100)){ #numItems 
  for (n_min in c(50)){ #what really matters here in number of events 
    for (n_max in c(425)){
      #ranking settings. How do data characteristics impact performance here?
      for (l in c(1, 2)){ #loss types square and absolute
        for (rankPriority in c("even", "top", "bottom")){
          for (rankSteepness in c(0.25, 0.5, 0.75)){
          #add results to the results df
          results <- rbind(results, RunSimulation(N = n, a_p = 1, b_p = 1, n_min = n_min, n_max = n_max, a_n = 1, b_n = 1, #data
                n_assignment_method = "ascending", 
                rankPriority = rankPriority, rankSteepness = rankSteepness, #rankWeights
                parameter = NULL, loss = l, 
                f=identity,  #ranking settings
                n_sim = 100, #100 or 1000 depending on time
                #fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
                fileRoot = "/ua/allencoleman/gangnon/ranking/results/",
                metric = TRUE))
          }
         }
       }
    }
  }
}
)
#AFTER save df
#CAREFUL! THIS OVERWRITES
df <- apply(results,2,as.character)
#save(df, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.RData") #saves as an R object
save(df, file = "/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results_n100.RData")

#system(mutt -s "Master, this is an email from HAL 9000, your commands have been completed" allencoleman@wisc.edu < ~/R/output.txt)
