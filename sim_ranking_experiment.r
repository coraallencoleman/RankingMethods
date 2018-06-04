#Simulation/Experiments Testing for WeightedLossRanking function

#Step 0: Load
library(rstan)
library(dplyr)
library(rstanarm)
#AND run ranking_function.r, ranking_metric.r files. 
set.seed(10)

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

  model1 <- stan_glmer(cbind(y, n - y) ~ (1|item), data = as.data.frame(data), 
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
                          metric = FALSE, metricFile = "/Users/cora/git_repos/RankingMethods/results/metricResults.csv"){
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
  #   fileRoot: file path used to create csv file for results
  #   metric: boolean indicating if metric results should be created and saved
  #   metricFile: csv file for metric results
  #
  # Returns: 
  #   list of matrices of posterior samples, one column for each item
  # Saves: csv of ranks (n_sim columns)
  # 
  # Dependencies: rstanarm
  settings <- SelectNP(N, a_p, b_p, n_min, n_max, a_n, b_n, n_assignment_method) #this happens once per experiment

  # create RankingWeights
  rankWeights <- RankingWeights(numItems = N, priority = rankPriority, steepness = rankSteepness)
  
  ranks <- list() #creates list of ranks for each simulation
  results <- list() #create list of metric results for each simulation

  for (i in 1:n_sim){
    data <- SimData(settings)
    post <- PostSamples(data)
    rankFunctionResult <- WeightedLossRanking(sampleMatrix = post, parameter = NULL, loss = loss, f=f,
                                                 rankWeights = rankWeights)
    print(rankFunctionResult)
    totalLoss <- rankFunctionResult[1]
    ranks[[i]] <- as.integer(rankFunctionResult[-1])
    print(ranks[[i]])
    if (metric == TRUE){
      results[[i]] <- RankMetric(ranks, settings = data)
    }
    #for each simulation, 
    #adds parameters, total loss, and rankings to a data frame as a new row of data
    lossDF$ranking[i] <- list(ranks[[i]])
    lossDF[1, 2:13] <- c(N, a_p, b_p, n_min, n_max, a_n, b_n, 
        n_assignment_method, 
        rankPriority, rankSteepness, 
        "identity", totalLoss)

  }
  
  #create rank file containing all info needed for experiment
  #TODO need to include function like identity or rank here. Could use as.character(quote(f)). For now, just assumes identity
  #rankFile = paste(fileRoot, N, a_p, b_p, n_min, n_max, a_n, b_n, n_assignment_method,rankPriority, 
                   #rankSteepness, parameter, loss, "identity", rankPriority, rankSteepness, n_sim, ".csv", sep = "_")
  
  #save ranks to a file (one column for each sim)
  #write.csv(ranks, file = rankFile) TODO
  
  #save metric results to a file
  if (metric == TRUE){
    #write.csv(results, file = metricFile) TODO
  }
  #return(ranks)
  return(lossDF)
}

#creates dataframe
# lossDF <- as.data.frame(matrix(nrow = 1, ncol = 15))
# names(lossDF) <- c("N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
#                            "n_assignment_method", 
#                            "rankPriority", "rankSteepness", 
#                            "parameter", "loss", "f", "totalLoss", "ranking")
results <- RunSimulation(n_sim = 1)
# write.table(results, "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.csv", sep = ",")
# #testing
# lossDF$ranking[1] <- list(c(1, 2, 3)) #works!
# lossDF[1, 1:14] <- c(i, N, a_p, b_p, n_min, n_max, a_n, b_n, n_assignment_method,
#                      rankPriority, rankSteepness, parameter, loss, "identity", totalLoss, 1)
#write.csv(results, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.csv")


