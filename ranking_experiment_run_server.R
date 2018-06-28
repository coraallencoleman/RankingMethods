## runs experiment using function in ranking_function.r and sim_ranking_experiment.r
#create an .RData file for with parameters + ranks

#run in ranking
#to move script to server:
#scp /Users/cora/git_repos/RankingMethods/*.r allencoleman@adhara.biostat.wisc.edu:/ua/allencoleman/gangnon/ranking/
#to run:
#system.time(Rscript ranking_function.r sim_ranking_experiment.r ranking_experiment_run_server.r)



options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#creates clean DF
returnDF <- as.data.frame(matrix(nrow = 0, ncol = 15))
names(returnDF) <- c("run", "N", "a_p", "b_p", "n_min", "n_max", "a_n", "b_n", 
                       "n_assignment_method", 
                       "rankPriority", "rankSteepness", 
                       "f", "loss", "totalLoss", "ranking")
results <- returnDF
#data characteristics
#for (n in c(25, 50, 100, 200)){ #numItems
  for (n_min in c(50, 100, 200, 400)){ #what really matters here in number of events
    for (n_max in c(500, 750, 900)){
      #ranking. How do data ch. impact performance here?
      #for (l in c(1, 2)){ #loss types square and absolute
        #for (rankPriority in c( "even", "top", "bottom")){
          #add results to the results df
          results <- rbind(results, RunSimulation(N = 50, a_p = 1, b_p = 1, n_min = n_min, n_max = n_max, a_n = 1, b_n = 1, #data
                n_assignment_method = "ascending", 
                rankPriority = rankPriority, #rankSteepness = .9, #rankWeights
                parameter = NULL, loss = l, 
                f=identity,  #ranking settings
                n_sim = 1, #100 or 1000 depending on time
                #fileRoot = "/Users/cora/git_repos/RankingMethods/results/",
                fileRoot = "/ua/allencoleman/gangnon/ranking/results/",
                metric = FALSE))
          #try running burn in for longer. if that doesnt help, catch warnings
         #}
       #}
    }
  }
#}
#think about orders of magnitude to start (want low, medium, high)

#AFTER save df
#CAREFUL! THIS OVERWRITES
df <- apply(results,2,as.character)
#save(df, file = "/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.RData") #saves as an R object
save(df, file = "/ua/allencoleman/gangnon/ranking/results/ranking_experiment_results.RData")

#data frames of lists
# have an element of df be a list or matrix. We want rankings to be a matrix within a list, one for each simulation
#for every simulation, we need a matrix of ranks
# save as an R object, not csv.
#look for this discussion in tidyverse data science R book

# load("/Users/cora/git_repos/RankingMethods/results/ranking_experiment_results.RData") 
# head(df)

##Main Questions for this Experiment:
##increase gaps in parameters until they're trivial. decrease until broken/impossible. 
#think of this as an experiment. what experimental conditions do we need to run to get a sense of behavior?


### Simulations TODO
# variation in N
# different losses
# parameter scale probability, rank, logit scale
# weights vs no weights (part of ranking)

#DATA
# even gaps
# uneven gaps #qbeta(1:N/(N+1), 1, 1). Vary the last two parameters for variable gap size
#p c()# look at beta distributions for alpha and beta. could crowd them up at one edge. will be symmetric
#n
#for below, think about how big average n should be to get p in CI
#n min: c(50) explore this to see . what differences in p1 and p2 coudl you find with this n? do a power calculation basically
#n max: se = sqrt(p*q/n). think about how small this would have to be to think about how big n needs to be
#se = sqrt((.1*.9)/20) #graph this out to see. If the ranking is always right, then n is too big. If ranks are uniform/terrible, then n too small.
# assignment method


