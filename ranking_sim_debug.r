#move first item farther from others

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
#data$n <- rep(10, times = N)
#data$p <- seq(from = 0, to =1, length.out = N)
data$y <- c(1, 4, 8, 15, 20, 40, 50, 60, 80, 90)
#data$y <- rbinom(N, size = data$n, prob = data$p)

#posterior
post <- PostSamplesEB(data)

#rank weights
steepness = c(0.001, 0.01, 0.1, 0.3, 0.5)
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
      currResults$samplesizen[nrow(currResults)] <- list(data$n) ##save true data n (SimData)
    }
  }
}
results <- currResults 
#uneven_sample_size_close_p <- results
results_uneven_n_far_y <- results