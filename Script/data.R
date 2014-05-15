setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

source("./Script/functions.R")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.rfn")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.rfn")

library(igraph)
library(scales)
library(RColorBrewer)
library(ggplot2)


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

## Split the data based on WC
## =================================================================================================

# The data frame "discipline" classifies records into "computer science", "social science", "physics"
discipline <- as.data.frame(matrix(rep(NA, 3*nrow(data)), ncol=3))
names(discipline) <- c("computer", "social", "physics")
# Records from computer science
discipline$computer[grep("computer science|informatic", data$WC, ignore.case=TRUE)] <- TRUE
# Records from social sciences
discipline$social[grep("Social Sciences|Sociology|Public, Environmental & Occupational Health|Anthropology|Management|Business|Economics|Education|Psychology|Gepgraphy|Political science|Behavioral sciences", data$WC, ignore.case=TRUE)] <- TRUE
# Records from physics
discipline$physics[grep("physics", data$WC, ignore.case=TRUE)] <- TRUE

# NAs are FALSEs in the dataframe
for (i in 1:3) {
  discipline[[i]][is.na(discipline[[i]])] <- FALSE
}

# Cross tab
# table(discipline[,1:2])
# table(discipline[,c(1,3)])
# table(discipline[,2:3])
# There are few double assignments, primarily in social science/computer science (i.e. same paper
# classified as both computer science and social sciences).
# Double assignments will be assigned to both disciplines: i.e. a paper classified as both social
# sciences and computer science in the WoS will be included in both the social sciences and the
# computer science field.

# Split the data
social.13 <- data[discipline$social,]
computer.13 <- data[discipline$computer,]
physics.13 <- data[discipline$physics,]

# Save
save(social.13, computer.13, physics.13, file="./Data/records.13.rda")



