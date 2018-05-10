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
RunSimulation <- function(N = 25, a_p = 1, b_p = 1, n_min = 10, n_max = 30, a_n = 1, b_n = 1,
                          n_assignment_method = "ascending", n_sim = 1, 
                          outFile = "/Users/cora/git_repos/RankingMethods/results/sim_results.csv"){
  #combines all the above functions to run a simulation
  #TODO ranking options
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
  #   "random" for random assignment
  #   list of dataframes. Each dataframe has 3 columns named: item, n, p. Output of SimData
  #   n_sim: number of simulations. (reps)
  #   outFile: file name for results
  #
  # Returns: 
  #   list of matrices of posterior samples, one column for each item
  # 
  # Dependencies: rstanarm
  
  settings <- SelectNP(N, a_p, b_p, n_min, n_max, a_n, b_n, n_assignment_method) #this happens once per experiment

  results <- list() #create list of simulations
  for (i in 1:n_sim){
    data <- SimData(settings)
    post <- PostSamples(data)
    ranks <- WeightedLossRanking(sampleMatrix = post)
    results[[i]] <- RankMetric(ranks, settings = data)
  }
  
  #save results to a file
  write.csv(results, file = outFile)
  return(results)
}

RunSimulation(n_sim = 2)

##Main Questions for this Experiment:
##increase gaps in parameters until they're trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior?

### Simulations TODO
# compare different ranking options
# function to save sim results to file
# even gaps
# uneven gaps #qbeta(1:N/(N+1), 1, 1). Vary the last two parameters for variable gap size
# different losses
# weights vs no weights (part of ranking)
# variation in N

#TODO notes wed may 8
#write a function to generate weights

#Weights: 
# w_i = e^(i-1) where i is the rank position (vary e here. e = 1 is unweighted. then decrease e size 3 total e sizes: slow, gradual, steeply)
# reverse version (you care about last only)
# curve version (care about both ends, dont care about middle). Need to know middle rank. Then 
  # weights = c(1, e, e^2, ..., e^(n+1/2) middle, ..., e^2, e, 1)
# we should always make the top weight 1 so that things are comperable (always relative to largest)
# do this simulation on rank, logit scales

#save all ranks from each simulation. Add whether to save metrics too. Then we'll play around with metrics too.
