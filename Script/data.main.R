# This script uses the original .txt data as downloaded from the WoS and obtains edge lists suitable
# to be converted to igraph.

# Libraries
library(igraph)
library(scales)
library(RColorBrewer)
library(ggplot2)

# Clear workspace
rm(list=ls())

# Set directors
setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

# Load functions
source("./Script/functions.R")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.rfn")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.rfn")

## 2011-2013
## =================================================================================================

# Get names of all text files with Web of Science data for relevant timespan
files <- list.files("./Data", pattern= "2013.+txt")

# Source data management script
source("./Script/data.01.R")

# Cross tab
# table(discipline[,1:2])
# There are few double assignments. Double assignments will be assigned to both disciplines: i.e. a
# paper classified as both social sciences and computer science in the WoS will be included in both
# the social sciences and the computer science field.
# Note that some papers do not fall in either category (neither Social Sciences, nor Computer Science + Physics).

# Split the data
social.13 <- social
comp_physics.13 <- comp_physics

## 2008-2010
## =================================================================================================

# Get names of all text files with Web of Science data for relevant timespan
files <- list.files("./Data", pattern= "2010.+txt")

# Source data management script
source("./Script/data.01.R")

# table(discipline[,1:2])

# Split the data
social.10 <- data[discipline$social,]
comp_physics.10 <- data[discipline$comp_physics,]


# Save all
save(list= ls(pattern="social\\.[0-9]|comp_physics\\.[0-9]"), file="./Data/records.rda")



