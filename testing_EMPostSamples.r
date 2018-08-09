#testing PostSamplesEM
  #should be more efficient than stan version

setwd("/Users/cora/git_repos/RankingMethods")
source("ranking_function.r")

settings <- SelectNP(N = 5, a_p = 1, b_p = 1, n_min = 50, n_max=75, a_n = 1, b_n=1, n_assignment_method="ascending")

data <- SimData(settings)
post <- PostSamplesEB(data)
