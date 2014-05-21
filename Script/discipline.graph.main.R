# Set directory
setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

# Libraries
library(igraph)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(Hmisc)

# Clear workspace
rm(list=ls())

# Load functions
source("./Script/functions.R")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.rfn")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.rfn")

# Load data
load("./Data/records.rda")

####################################################################################################
### 2011-2013                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "13"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong")

## =================================================================================================
## SOCIAL SCIENCES 
## =================================================================================================

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Get the graph
## -----------------------------------------------------------------------------------------------

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/discipline.graph.01.R")


# Display the graph
## -----------------------------------------------------------------------------------------------
source("./Script/plot.disc.graph.01.R")


## =================================================================================================
### COMPUTER SCIENCES + PHYSICS                                                                             ## =================================================================================================

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Get the graph
## -----------------------------------------------------------------------------------------------

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/discipline.graph.01.R")

# Display the graph
## -----------------------------------------------------------------------------------------------
source("./Script/plot.disc.graph.01.R")


####################################################################################################
### 2008-2010                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "10"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong")

## =================================================================================================
## SOCIAL SCIENCES 
## =================================================================================================
# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Get the graph
## -----------------------------------------------------------------------------------------------

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/discipline.graph.01.R")


# Display the graph
## -----------------------------------------------------------------------------------------------
source("./Script/plot.disc.graph.01.R")


## =================================================================================================
### COMPUTER SCIENCES + PHYSICS                                                                             ## =================================================================================================

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Get the graph
## -----------------------------------------------------------------------------------------------

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/discipline.graph.01.R")

# Display the graph
## -----------------------------------------------------------------------------------------------
source("./Script/plot.disc.graph.01.R")


# Save
save(list= ls(pattern="\\.one\\."), file= "./Data/graphs.rda")
