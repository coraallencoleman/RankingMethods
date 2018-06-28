#Simulation/Experiments Testing for WeightedLossRanking function

#Step 0: Load

library(rstan)
library(dplyr)
library(rstanarm)
library(clue)
library(rstan)
#AND run ranking_function.r, ranking_metric.r files. 
set.seed(10)

# A testing metric for use with simulated data



##function metric to see if rankObject's top ranked items match true top items MATRIX
RankMetric <- function(rankObject = NULL, settings = NULL, order = "largest", topN = 5){
  # function metric to see if our top number matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking.
  #   originalData: a data frame with column of item IDs, n, true probabilities
  #   order: largest (largest to smallest) or smallest (smallest to largest)
  #   topN: an integer number of top items to compare
  #
  # Returns:
  #   logical vector
  #
  # Dependencies: rstan, clue, dplyr
  
  rankedData <- array(data = NA, dim=c(length(settings[,1]), 4))
  rankedData[,1:3] <- settings
  rankedData[,4] <- as.integer(rankObject) #adds rank order from WeightedLossRanking (rank orders items from smallest to highest)
  
  if (order == "largest"){
    true <- rankedData[order(rankedData[,3]),] #sort by TRUE p
    rankedData <- rankedData[order(rankedData[,4]),] #sort by calculated rank (col 4)
  } else if (order == "smallest"){
    true <- rankedData[order(-rankedData[,3]),] #sort by TRUE p #TODO need to reverse
    rankedData <- rankedData[order(-rankedData[,4]),] #sort by calculated rank (col 4)
  } else {
    stop("order must be input as either 'largest' or 'smallest'")
  }
  #check if each item in true top N is in ranking top N, return boolean
  return(true[1:10, 1] %in% rankedData[1:10, 1])
}



##function metric to see if rankObject's top ranked items match true top items
#WITH DATAFRAME not finished
RankMetricDF <- function(rankObject = NULL, originalData = NULL, order = largest, topN = 5){
  # function metric to see if our top number matches true top five for Binomial model
  #   
  # Args:
  #   rankObject: an output of WeightedLossRanking. Must include columns item, p, n
  #   originalData: a data frame with column of true probabilities, true N (column names must match p, n)
  #   order: largest (largest to smallest) or smallest (smallest to largest)
  #   topN: an integer number of top items to compare
  #
  # Returns:
  #   logical vector
  #
  # Dependencies: rstan, clue, dplyr
  rankedData <- as.data.frame(originalData)
  rankedData[5,] <- rankedData[2,]*100 #p*100
  rankedData[6,] <- as.integer(rankObject) #rank orders items smallest to highest
  if (order == "largest"){
    originalData <-originalData %>% dplyr::arrange(desc(p), desc(n)) 
    rankedData <- rankedData %>% dplyr::arrange(rank)
  } else if (order == "smallest"){
    rankedData <- rankedData %>% dplyr::arrange(desc(rank))
    originalData <-originalData %>% dplyr::arrange(p, desc(n)) #TODO assume no ties in p change simulated data
  } else {
    stop("order must be input as either 'largest' or 'smallest'")
  }
  #check if each item in true top N is in ranking top N, return boolean
  return(originalData[1:topN,]$item %in% rankedData[1:topN,]$item )
}


#normal (implement this, but do simulation with binomial instead. might be relevant with survey data for counties)
#we're assuming that these tau^2 are known. If they aren't, the model could be extended to incorporate this uncertainty.
#true mean, sample means, sample size or variances
#simulate: fixed var (n), changing sample size. and use sd units. 
#sigma^2/n = tau. tau is

#TODO is rank one is in top five?


SelectNP <- function(N = 25, a_p = 1, b_p = 1, n_min = 10, n_max = 30, a_n = 1, b_n = 1,
                     n_assignment_method = "ascending"){
  # function to simulate n, p from parameters. Deterministic.
  #   
  # Args:
  #   N: number of items to rank
  #   a_p: Shape parameter alpha for beta distribution to determine gaps in p. Allows for equal or nonequal gap size.
  #   b_p: Shape parameter beta for beta distribution to determine gaps in p. Allows for equal or nonequal gap size.
  #   n_min: minimum number of counts/tries for each binomial variable
  #   n_max: maximum number of counts/tries for each binomial variable
  #   a_n: Shape parameter alpha for beta distribution to determine gaps in n. Allows for equal or nonequal gap size.
  #   b_n: Shape parameter alpha for beta distribution to determine gaps in n. Allows for equal or nonequal gap size.
  #   n_assignment_method. Possibilities: "ascending" for assign in order, "descending" for assign in reverse order, 
  #   "random" for random assignment
  #
  # Returns:
  #   one matrix with 2 columns (n, p) and N rows
  #
  # Dependencies: 

  output <- matrix(data = NA, nrow = N, ncol = 3, 
                          dimnames = list(seq(1:N), c("item", "n", "p"))) #rows 1 to N, columns n, p
  #item (county, etc)
  output[,1] <- seq(1:N)
  #n
  output[,2] <- round(n_min + (n_max-n_min)*qbeta(1:N/(N+1), a_n, b_n), digits=0) #quantiles
  #p
  output[,3] <- qbeta((1:N)/(N+1), a_p, b_p)
    
  return(output)
}

SimData <- function(matrix){
  #simulates data from a dataframe of n, p

  # Args:
  #   matrix of deterministic n, p: A list of matrices containing N rows and 2 columns (n, p). Result of SelectNP where:
  #     n is the true attempts/tries/counts
  #     p is the true p
  #
  # Returns: 
  #   matrix of N rows and 2 columns (n, y) where n is attempts and y is successes
  #   alternative formulation (not used here): matrix of N rows and n_sim columns and make ONE deterministic n vector
  # 
  # Dependencies:
  
  N <- length(matrix[,1]) #number of items to rank (from SelectNP matrix)

  output <- matrix(data = NA, nrow = N, ncol = 3, 
                          dimnames = list(seq(1:N), c("item","n", "y")))
  #item
  output[,1] <- seq(1:N)
  #n (These are deterministic.)
  output[,2] <- matrix[,2] #
  #y counts (These vary randomly.)
  output[,3] <- rbinom(N, size = matrix[,2], prob = matrix[,3]) 
  return(output)
}

#Get Posterior Samples
PostSamples <- function(data){  
  #simulates data from a dataframe of n, p
  
  # Args:
  #   list of dataframes. Each dataframe has 3 columns named: item, n, p. Output of SimData
  #
  # Returns: 
  #   one matrix of posterior samples. The matrix has one row for each iteration, one column for each item parameter estimated
  # 
  # Dependencies: rstanarm
  
  library(rstanarm)
  options(mc.cores = parallel::detectCores())

  model1 <- stan_glmer(cbind(y, n - y) ~ (1|item), data = as.data.frame(data), iter = 3000, #default iter = 2000
                       family = binomial(link=logit), prior_intercept = normal(0, 5),
                       prior_aux = cauchy(0,1),
                       seed = 12345)
  output <- as.matrix(model1, regex_pars = "b[(Intercept) item:[0-9]+]") 
  return(output)
}

## RUN EXPERIMENT
RunSimulation <- function(N = 10, a_p = 1, b_p = 1, n_min = 10, n_max = 30, a_n = 1, b_n = 1, #data
                          n_assignment_method = "ascending", 
                          rankPriority = "top", rankSteepness = .9, #rankWeights
                          parameter = NULL, loss = 2, f=identity, #ranking settings
                          n_sim = 1,
                          fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
                          metric = FALSE){
  #combines all the above functions to run a simulation
  # Args:
  #   for SelectNP:
  #   N: number of items to rank
  #   a_p: Shape parameter alpha for beta distribution to determine gaps in p. Allows for equal or nonequal gap size.
  #   b_p: Shape parameter beta for beta distribution to determine gaps in p. Allows for equal or nonequal gap size.
  #   n_min: minimum number of counts/tries for each binomial variable
  #   n_max: maximum number of counts/tries for each binomial variable
  #   a_n: Shape parameter alpha for beta distribution to determine gaps in n. Allows for equal or nonequal gap size.
  #   b_n: Shape parameter alpha for beta distribution to determine gaps in n. Allows for equal or nonequal gap size.
  #   n_assignment_method. Possibilities: "ascending" for assign in order, "descending" for assign in reverse order, 
  #     "random" for random assignment
  #   list of dataframes. Each dataframe has 3 columns named: item, n, p. Output of SimData
  #   loss: an exponent indicating the loss function for ranking. options: 2=square, 1=absolute, 0=zero
  #   f = scale on which to rank
  #   n_sim: number of simulations. (reps)
  #   fileRoot: file path used to create file for ranking results and metric results
  #   metric: boolean indicating if metric results should be created and saved
  #   metricFile: file for RData metric results
  #
  # Returns: 
  #   list of matrices of posterior samples, one column for each item
  # Saves: RData of rank Metric (n_sim columns)
  # 
  # Dependencies: rstanarm
  settings <- SelectNP(N, a_p, b_p, n_min, n_max, a_n, b_n, n_assignment_method) #this happens once per experiment

  # create RankingWeights
  rankWeights <- RankingWeights(numItems = N, priority = rankPriority, steepness = rankSteepness)
  
  #ranks <- list() #creates list of ranks for each simulation
  rankMetricResults <- list() #create list of metric results for each simulation

  for (i in 1:n_sim){#for each simulation
    data <- SimData(settings)
    post <- PostSamples(data)
    rankFunctionResult <- WeightedLossRanking(sampleMatrix = post, parameter = parameter, loss = loss, f=f, 
                                              rankWeights = rankWeights)
    totalLoss <- as.numeric(rankFunctionResult[1])
    ranks <- as.integer(rankFunctionResult[-1])
    
    #adds parameters, total loss, and rankings to returnDF data frame as a new row of data (RETURN)
    returnDF[1, 1:14] <- c(i, N, a_p, b_p, n_min, n_max, a_n, b_n, 
        n_assignment_method, 
        rankPriority, rankSteepness, 
        "identity", loss, totalLoss)
    returnDF$ranking[i] <- list(ranks)
    
    if (metric == TRUE){ #METRIC FOR RANKING
      rankMetricResults <- RankMetric(ranks, settings = data, topN = 10) #create metric
      
      #save metric results to RData file for easy plotting
      returnDF$metric[i] <- as.numeric(sum(rankMetricResults)/10)
      print(as.numeric(sum(rankMetricResults)/10))
    }
  }
  
  return(returnDF)
}

#creates dataframe
# returnDF <- as.data.frame(matrix(nrow = 1, ncol = 15))
# names(returnDF) <- c("N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
#                            "n_assignment_method", 
#                            "rankPriority", "rankSteepness", 
#                            "parameter", "loss", "f", "totalLoss", "ranking")
results <- RunSimulation(n_sim = 1, metric = TRUE)
# write.table(results, "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.csv", sep = ",")
# #testing
# returnDF$ranking[1] <- list(c(1, 2, 3)) #works!
# returnDF[1, 1:14] <- c(i, N, a_p, b_p, n_min, n_max, a_n, b_n, n_assignment_method,
#                      rankPriority, rankSteepness, parameter, loss, "identity", totalLoss, 1)
#write.csv(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.csv")

## TESTING

# returnDF <- data.frame(run=integer(), N=integer(), a_p=double(), b_p=double(), BROKEN
#                        n_min = integer(), n_max=integer(), 
#                        a_n=double(), b_n=double(), 
#                        n_assignment_method=string(), 
#                        rankPriority=string(), rankSteepness=double(), 
#                        f=integer(), loss=integer(), totalLoss=double()) #, ranking = list(c()))
# returnDF <- as.data.frame(matrix(nrow = 0, ncol = 15))
# names(returnDF) <- c("run", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n",
#                      "n_assignment_method",
#                      "rankPriority", "rankSteepness",
#                      "f", "loss", "totalLoss", "ranking")
# results <- returnDF
# 
# for (rankPriority in c( "even")){
#   #add results to the results df
#  results <- rbind(results, RunSimulation(N = 50, a_p = 1, b_p = 1, n_min = 50, n_max = 70, a_n = 1, b_n = 1, #data
#                                                   n_assignment_method = "ascending", 
#                                                   rankPriority = rankPriority, #rankSteepness = .9, #rankWeights
#                                                   parameter = NULL, loss = 2, 
#                                                   f=identity,  #ranking settings
#                                                   n_sim = 1, #100 or 1000 depending on time
#                                                   fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
#                                                   metric = FALSE))
#   #try running burn in for longer. if that doesnt help, catch warnings
#  #https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html#markov-chains-did-not-converge
# }
# 
# #AFTER save df
# #CAREFUL! THIS OVERWRITES
# df <- apply(results,2,as.character)
# save(df, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results_test.RData") #saves as an R object
