library(readr)
library(missForest)
library(psych)
library(doParallel)

library(ggplot2)
# library(pscl)
# library(caret)


##########################################
# global variables 
##########################################
N_SPLITS = 10000
N_CORES = detectCores()
imputation = T
with_sui2 = T

source(paste(getwd(),"longitudinal/Load_DB.R", sep = "/"))
source(paste(getwd(),"longitudinal/Combined.R", sep = "/"))
source(paste(getwd(),"functions_util.R", sep = "/"))
source(paste(getwd(),"algorithm_util.R", sep = "/"))



x_total = merge(Y_bucket, combined_bucket)

y = x_total[,3]
x = x_total[,-c(1:5)]



#TODO table 1

# source(paste(getwd(),"step_1.R", sep = "/"))
source(paste(getwd(),"step_2.R", sep = "/"))
# source(paste(getwd(),"step_3.R", sep = "/"))


