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

# Get names of all text files with Web of Science data for relevant timespan
files <- list.files("./Data", pattern= "2013.+txt")

# Preassign data.frame
all.data <- data.frame()

# Run the following code for each file name
for (i in 1:length(files)) {
  
  # Get file name
  file <- files[i]
  
  # Read the data in the corresponding file
  data <- read.delim(paste("./Data/", file, sep=""), row.names=NULL, stringsAsFactors=FALSE, quote = "", encoding="UTF-8")
  
  # Fix variable names
  names(data) <- names(data)[-1]
  
  # Remove last column
  data <- data[,-ncol(data)]
  
  # Rbind
  all.data <- rbind(all.data, data)
  
}

# Rename object
data <- all.data

## Split the data based on WC (Web of Science Category)
## =================================================================================================

# The data frame "discipline" classifies records into "computer science", "social science", "physics"
discipline <- as.data.frame(matrix(rep(NA, 2*nrow(data)), ncol=2))
names(discipline) <- c("social", "comp_physics")

# Records from social sciences
discipline$social[grep("Social Sciences|Sociology|Public, Environmental & Occupational Health|Anthropology|Management|Business|Economics|Education|Psychology|Gepgraphy|Political science|Behavioral sciences", data$WC, ignore.case=TRUE)] <- TRUE

# Records from computer science + physics
discipline$comp_physics[grep("computer science|informatic|physics", data$WC, ignore.case=TRUE)] <- TRUE

# NAs are FALSEs in the dataframe
for (i in 1:2) {
  discipline[[i]][is.na(discipline[[i]])] <- FALSE
}

# Cross tab
# table(discipline[,1:2])
# There are few double assignments. Double assignments will be assigned to both disciplines: i.e. a
# paper classified as both social sciences and computer science in the WoS will be included in both
# the social sciences and the computer science field.
# Note that some papers do not fall in either category (neither Social Sciences, nor Computer Science + Physics).

# Split the data
social.13 <- data[discipline$social,]
comp_physics.13 <- data[discipline$comp_physics,]

# Save
save(social.13, comp_physics.13, file="./Data/records.13.rda")



