#move first item farther from others

#sim one dataset

## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#TODO make data by hand to see if anything behaves at all sensibly. Easy to print out loss function matrix.
#eventually they'll be similar enough to make it a hard problem. see what happens with simple problem. if not working well, 
#look at loss function matrix. is it doing what seems sensible? Is something off there?


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

#create dataframe by hand
N = 10
data <- as.data.frame(matrix(data = NA, nrow = N, ncol = 3, 
                dimnames = list(seq(1:N), c("item","n", "y"))))

data$item <- seq(1:N)
data$n <- rep(10, times = N)
data$y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
# #data characteristics
# for (n in c(3)){ #numItems 
#   for (n_min in c(5)){ #what really matters here in number of events 
#     for (n_max in c(20)){
#       for (a_n in c(0.5)){
#         for (b_n in c(0.5)){
#           for (a_p in c(0.5)){
#             for (b_p in c(0.5)){
#               #add results to the results df
#               results <- rbind(results, RunSimulation(N = n, a_p = a_p, b_p = b_p, n_min = n_min, n_max = n_max, a_n = a_n, b_n = b_n, #data
#                                                       n_assignment_method = "random", 
#                                                       #TODO n_assignment_method doesnt seem to be random
#                                                       #ranking settings
#                                                       rankPriority = c("top"), rankSteepness = c(0, 0.0001, 0.01, 0.05, 0.1, 0.2, 0.4, 0.5), #rankWeights
#                                                       parameter = NULL, loss = c(1,2), 
#                                                       f=identity,  
#                                                       n_sim = 2))
#             }
#           }
#         }
#       }
#     }
#   }
# }

debug_results <- results
#saves results. Careful! This overwrites
save(debug_results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0912.RData") #saves as an R object

load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0912.RData") 
head(debug_results)

# Metric from 1 to 15
for (t in 1:8){
  results[[paste0("metricPercent", t)]] <- rep(0, times = nrow(results))
  for (i in 1:nrow(results)){
    results[[paste0("metric", t)]][i] <- list(I(RankMetric(results$ranking[i], 
                                                           order = "largest", topN = t)))
    results[[paste0("metricPercent", t)]][i] <- as.double(mean(results[[paste0("metric", t)]][i][[1]])[[1]])
  }
}

# Strict Metric from 1 to 10
for (t in 1:10){
  results[[paste0("metricStrictPercent", t)]] <- rep(0, times = nrow(results))
  for (i in 1:nrow(results)){
    results[[paste0("metricStrict", t)]][i] <- list(I(RankMetricStrict(results$ranking[i], 
                                                                       order = "largest", topN = t)))
    results[[paste0("metricStrictPercent", t)]][i] <- as.double(mean(results[[paste0("metricStrict", t)]][i][[1]])[[1]])
  }
}

## PLOTS ##
#What % of Top 7 Ranked Correctly?
ps.options(fonts=c("serif"), width = 7, height = 7)
postscript("bar_StrictMetricTop7_e.eps")
StrictMetric7_e <- ggplot(results) + 
  geom_bar(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent7)), stat="summary", fun.y=mean) + 
  ggtitle("What % of Top 7 Ranked Correctly?") + 
  ylab("Mean Percent Top 7 Correct (Strict)") + xlab("Rank Weight Steepness (epsilon)")
StrictMetric7_e
dev.off()
