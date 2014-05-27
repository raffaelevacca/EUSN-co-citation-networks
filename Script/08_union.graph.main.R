# Load functions
source("./Script/01_functions.R")
# load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.rfn")
# load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.rfn")

# Load data
load("./Data/graphs.rda")

####################################################################################################
### 2011-2013                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "13"

# Merge the networks from social sciences and comp science + physics 
source("./Script/09_union.graph.01.R")

# Plot
source("./Script/10_union.graph.plots.R")

####################################################################################################
### 2008-2010                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "10"

# Merge the networks from social sciences and comp science + physics 
source("./Script/09_union.graph.01.R")

# Plot
source("./Script/10_union.graph.plots.R")

####################################################################################################
### 2005-2007                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "07"

# Merge the networks from social sciences and comp science + physics 
source("./Script/09_union.graph.01.R")

# Plot
source("./Script/10_union.graph.plots.R")

####################################################################################################
### 2002-2004                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "04"

# Merge the networks from social sciences and comp science + physics 
source("./Script/09_union.graph.01.R")

# Plot
source("./Script/10_union.graph.plots.R")

####################################################################################################
### 1999-2001                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "01"

# Merge the networks from social sciences and comp science + physics 
source("./Script/09_union.graph.01.R")

# Plot
source("./Script/10_union.graph.plots.R")

####################################################################################################
### 1996-1998                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "98"

# Merge the networks from social sciences and comp science + physics 
source("./Script/09_union.graph.01.R")

# Plot
source("./Script/10_union.graph.plots.R")

# Save graphs
save(list= ls(pattern= "social\\.one\\.|comp_physics\\.one\\.|union\\.gr\\."), file="./Data/graphs.rda")
