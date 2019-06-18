if (!require(data.table)) install.packages('data.table')
library(data.table)

setwd("File location")

load("data.RData")

# The given dataset is named as "userLevel"
View(userLevel)

# dataset: 
# each row is a record of a user's Bing usage before or after the experiment
# group is treatmetn assignment. 1: treatment, 0: control. Randomization is on userID
# IsPreExperiment indicates whether the page view is in AA period or AB period

