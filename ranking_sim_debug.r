#move first item farther from others

#sim one dataset

## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#make data by hand to see if anything behaves at all sensibly. DONE
#Easy to print out loss function matrix.
#eventually they'll be similar enough to make it a hard problem. 
#see what happens with simple problem. if not working well, 
#look at loss function matrix. is it doing what seems sensible? Is something off there?

# #run in ranking
setwd("/Users/cora/git_repos/RankingMethods")
source("ranking_function.r")
#creates clean results
currResults <- as.data.frame(matrix(nrow = 0, ncol = 6))
names(currResults) <- c("rankPriority", "rankSteepness",
                        "loss", "totalLoss", "ranking", "samplesizen")
results <- currResults

#create dataframe by hand
N = 10
data <- as.data.frame(matrix(data = NA, nrow = N, ncol = 3,
                dimnames = list(seq(1:N), c("item","n", "y"))))

data$item <- seq(1:N)
data$n <- seq(from = 10, to = 100, length.out = N)
data$p <- seq(from = 0, to = .5, length.out = N)
#data$y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
data$y <- rbinom(N, size = data$n, prob = data$p)

#posterior
post <- PostSamplesEB(data)

#rank weights
steepness = c(0.001, 0.01, 0.1, 0.5)
priority = c("top", "even")
rankWeights <- as.data.frame(matrix(nrow = 0, ncol = 3))
names(rankWeights) <- c("rw", "rankPriority", "rankSteepness")
for (rp in priority){
  for (rs in steepness){
    rw <- list(as.double(RankingWeights(numItems = N, priority = rp, steepness = rs)))
    rankWeights[nrow(rankWeights) + 1,] <- list(I(rw), rp, rs)
  }
}

for (l in c(2)){
  for (rp in priority){
    for (rs in steepness){
      ranks <- list()
      rankFunctionResult <- WeightedLossRanking(sampleMatrix = post, loss = l,
                              rankWeights = filter(rankWeights, rankPriority == rp, rankSteepness == rs)$rw[[1]])
      
      totalLoss <- as.numeric(sum(rankFunctionResult[[1]])) #this is an nxn rank matrix, so loss = sum(matrix)
      ranks <- list(as.integer(rankFunctionResult[[2]]))
      
      row <- c(rp, rs,"identity", l, totalLoss, "placeholder")
      currResults[nrow(currResults) + 1, ] <- row
      currResults$ranking[nrow(currResults)] <- ranks
    }
  }
}
currResults$samplesizen <- data$n ##save true data n (SimData)
results <- currResults

# 
# 
# #run in ranking
# setwd("/Users/cora/git_repos/RankingMethods")
# source("ranking_function.r")
# #creates clean results
# currResults <- as.data.frame(matrix(nrow = 0, ncol = 16))
# names(currResults) <- c("sim", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
#                         "n_assignment_method", 
#                         "rankPriority", "rankSteepness", 
#                         "f", "loss", "totalLoss", "ranking", "data")
# currResults$data <- I(list())
# results <- currResults
# 
# results <- rbind(results, RunSimulation(N = 10, a_p = 1, b_p = 1, n_min = 10, n_max = 30, a_n = 1, b_n = 1, #data
#                                               n_assignment_method = "ascending",
#                                               #ranking settings
#                                               rankPriority = c("top"), rankSteepness = c(0, 0.0001, 0.01, 0.05, 0.1, 0.2, 0.4, 0.5), #rankWeights
#                                               parameter = NULL, loss = c(1,2),
#                                               f=identity,
#                                               n_sim = 1))
# 
# debug_results <- results
# #saves results. Careful! This overwrites
# save(debug_results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0912.RData") #saves as an R object
# 
# load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_0912.RData") 
# head(debug_results)
# 
# # Metric from 1 to 15
# for (t in 1:8){
#   results[[paste0("metricPercent", t)]] <- rep(0, times = nrow(results))
#   for (i in 1:nrow(results)){
#     results[[paste0("metric", t)]][i] <- list(I(RankMetric(results$ranking[i], 
#                                                            order = "largest", topN = t)))
#     results[[paste0("metricPercent", t)]][i] <- as.double(mean(results[[paste0("metric", t)]][i][[1]])[[1]])
#   }
# }
# 
# # Strict Metric from 1 to 10
# for (t in 1:10){
#   results[[paste0("metricStrictPercent", t)]] <- rep(0, times = nrow(results))
#   for (i in 1:nrow(results)){
#     results[[paste0("metricStrict", t)]][i] <- list(I(RankMetricStrict(results$ranking[i], 
#                                                                        order = "largest", topN = t)))
#     results[[paste0("metricStrictPercent", t)]][i] <- as.double(mean(results[[paste0("metricStrict", t)]][i][[1]])[[1]])
#   }
# }
# 
# ## PLOTS ##
# #What % of Top 7 Ranked Correctly?
# ps.options(fonts=c("serif"), width = 7, height = 7)
# postscript("bar_StrictMetricTop7_e.eps")
# StrictMetric7_e <- ggplot(results) + 
#   geom_bar(aes(as.factor(rankSteepness), as.numeric(metricStrictPercent7)), stat="summary", fun.y=mean) + 
#   ggtitle("What % of Top 7 Ranked Correctly?") + 
#   ylab("Mean Percent Top 7 Correct (Strict)") + xlab("Rank Weight Steepness (epsilon)")
# StrictMetric7_e
# dev.off()
