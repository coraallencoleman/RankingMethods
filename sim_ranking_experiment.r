#Testing for WeightedLossRanking function

##MAIN GOAL OF THIS EXPERIMENT
##increase gaps in parameters until they're trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior.

#Step 0: Load
library(rstan)
library(dplyr)
library(rstanarm)
#AND run entire ranking_function.r, ranking_metric.r files. 
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
                          n_assignment_method = "ascending", n_sim = 1){
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
  #   "random" for random assignment
  #   list of dataframes. Each dataframe has 3 columns named: item, n, p. Output of SimData
  #   n_sim: number of simulations. (reps)
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
    results[[i]] <- RankMetric(ranks, originalData = data)
  }
  return(results)
  
}

RunSimulation()

# settings <- SelectNP()
data <- SimData(settings)
# post <- PostSamples(data)
# ranks <- WeightedLossRanking(sampleMatrix = post)
results <- RankMetric(ranks, settings) #settings is item, n, p
rankedData <- array(data = NA, dim=c(length(settings[,1]), 4))
rankedData[,1:3] <- settings
rankedData[,4] <- as.integer(ranks) #adds rank order from WeightedLossRanking (rank orders items from smallest to highest)
true <- rankedData[order(rankedData[,3]),] #sort by TRUE p
rankedData <- rankedData[order(rankedData[,4]),] #sort by calculated rank (col 4)
true[1:10, 1] %in% rankedData[1:10, 1]

#RANKING EVALUATION: use RankMetric for now

#STEP 1: Simulate Different Types of Data
## Binomial Random Intercept n = 100 ## EVEN GAPS
#increase gaps until trivial. decrease until broken/impossible.
## PARAMETERS ##
gaps <- c(0.005, 0.01, 0.1) #gap sizes tested here
topN = 10

#initialize arrays and lists
even <- array(data = NA, dim=c(length(gaps),4,10001)) #3 dim array with 3 matrices, 4 col and N rows (max = 10001) each
even_model <- vector("list", length(gaps)) #list of rank objects for each of the 4 matrices
even_rank <- vector("list", length(gaps))

#do one simulation scenario at a time TODO

even_metric_results <- #rank_metric results for each of the matrices
  data.frame(metric_results = logical(length = topN))

for (i in 1:length(gaps)){ #length(gaps)
  N = length(seq(from = 0, to = 1, by = gaps[i]))
  even[i, 1, 1:N] <- seq(from = 1, to = N, by = 1) #ITEM
  even[i, 2, 1:N] <- seq(from = 0, to = 1, by = gaps[i]) #P
  even[i, 3, 1:N] <- rep(as.integer(25),times = N) #SIZE
  even[i, 4, 1:N] <- rbinom(n = N, size = even[i, 3, 1:N], even[i, 2, 1:N]) #SIM SUCCESSES 
  #TODO need to do this for n simulation (n_sim = 1) 
  #do this for each simulation sample data -> rankings one at a time 
  #so that you dont have to do keep posteriors in memory
  #TODO set num of posterior samples to smaller
  #TODO ending array FOR EACH DATA SET is 3D N, nSim nRankingMethods or a list using method as names of the list
  
  
  ## DATA FOR STAN##
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  sim_data = list(
    N = N, #N or numRows
    item = even[i, 1, 1:N], #ITEM ID
    sizeN = as.integer(even[i, 3, 1:N]), #same as cafes$n #SIZE
    count = as.integer(even[i, 4, 1:N]) #SIM SUCCESSES
  ) 
  #MODEL
  even_model[[i]] <- stan(file="/Users/cora/git_repos/RankingMethods/sim_randInt.stan",data=sim_data, seed = 10)
  
  even_rank[[i]] <- WeightedLossRanking(model = even_model[[i]], parameter = "p", loss = 2, lossTotal = TRUE)
  
  #compare using rankMetric, add to dataframe of results
  nam <- paste("gap", i, sep = "")
  even_metric_results$new <- RankMetric(even_rank[[i]], even[i, 1:4, 1:N], order = "largest", topN = topN)
  colnames(even_metric_results)[i+1] <- nam
}

#save results to a file
write.csv(even_metric_results, file = "/Users/cora/git_repos/RankingMethods/results/even_gaps_RankMetric_results.csv")

#TODO DO THIS OVER DIFFERENT LOSSES

#TODO repeat above with uneven gap size
#try with bigger gaps, random unif on a range to have arbitrary gaps
#increase gaps until trivial. decrease until broken/impossible. 

#to make this flexible for nonequal gap size
qbeta(1:N/(N+1), 1, 1) #but then vary the last two parameters for variable gap size
#then change N, a, b.

#TODO repeat with testing over variation in N

#TODO add weights. How does this compare to not weighting at all?