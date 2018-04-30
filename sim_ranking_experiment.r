#Testing for WeightedLossRanking function

##IDEA
##increase gaps until trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior.

#Step 0: Load
library(rstan)
library(dplyr)
#AND run entire ranking_function.r, ranking_metric.r files. 
set.seed(10)

selectNP <- function(N = 25, a_p = 1, b_p = 1, n_min = 10, n_max = 30, 
                     n_assignment_method = "ascending", n_sim = 1){
  # function to simulate n, p from parameters
  #   
  # Args:
  #   N: number of items to rank remove from here?
  #   a_p: Shape parameter alpha for beta distribution to determine gaps in p. Allows for nonequal gap size.
  #   b_p: Shape parameter beta for beta distribution to determine gaps in p. Allows for nonequal gap size.
  #   n_min: minimum number of counts/tries for each binomial variable (TODO evenly spaced n? ask if need a_n, b_n too?)
  #   n_max: maximum number of counts/tries for each binomial variable
  #   n_assignment_method. Possibilities: "ascending" for assign in order, "descending" for assign in reverse order, 
  #   "random" for random assignment
  #   n_sim: number of simulations needed
  #
  # Returns:
  #   n_sim matrices each with 2 columns (n, p) and N rows (N rows?)
  #
  # Dependencies: 
  output <- list()

  for (i in 1:n_sim){
    output[[i]] <- matrix(data = NA, nrow = N, ncol = 2, 
                          dimnames = list(seq(1:N), c("n", "p")))
    #n
    output[[i]][,1] <- round(runif(N, min = n_min, max = n_max), digits = 0) #qbeta(1:N/(N+1), a_n, b_n)?
    #p
    output[[i]][,2] <- qbeta((1:N)/(N+1), a_p, b_p)
  }
  return(output)
}

SimData <- function(N = 25, matrixList = , n_sim = 1){
  #simulates data from a dataframe of n, p with nrow = n_sim

  # Args:
  #   N: number of items to rank
  #   matrixList of n, p: list of matrices containing N rows and 2 columns: n, p. Result of selectNP.
  #      n is the true attempts/tries/counts
  #      p is the true p
  #   n_sim: number of simulations
  #
  # Returns: 
  #   dataframe of y (in stan format)
  # 
  # Dependencies:
  
  even[i, 1, 1:N] <- seq(from = 1, to = N, by = 1) #ITEM
  even[i, 2, 1:N] <- seq(from = 0, to = 1, by = gaps[i]) #P
  even[i, 3, 1:N] <- rep(as.integer(25),times = N) #SIZE
  even[i, 4, 1:N] <- rbinom(n = N, size = even[i, 3, 1:N], even[i, 2, 1:N]) #SIM SUCCESSES 
  
  sim_data = list(
    N = N, #N or numRows
    item = even[i, 1, 1:N], #ITEM ID
    n = as.integer(even[i, 3, 1:N]), #same as cafes$n #SIZE
    count = as.integer(even[i, 4, 1:N]) #SIM SUCCESSES
  ) 
}

DataToRanking <- function(rankingMethod = 2){
  #ranks data
  
  # Args:
  #   N: number of items to rank
  #   n: attempts as in binom(n,p)
  #   p: true p
  #   n_sim: number of simulations needed.
  #
  # Returns: 
  #   df with col: rankings, true ranking or true p  (nrow = N)
  # 
  # Dependencies: WeightedLossRanking function from ranking_function.r
  
}

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