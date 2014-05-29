rm(list=ls())

# Set project path
path <- "/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/"
setwd(path)
# Set scripts path
script.path <- paste(path, "Script/", sep="")

# Libraries
library(igraph)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(Hmisc)
library(statnet)
library(networkDynamic)
library(ndtv)
library(intergraph)

# Load functions
source(paste(script.path, "01_functions.R", sep=""))

# Initialize counter
counter <- 0

# Functions
## -------------------------------------------------------------------------------------------------
# File name
file.name <- "functions.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= file.name, path= script.path)

# Import and clean data
## -------------------------------------------------------------------------------------------------
# File name
file.name <- "data.main.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.name <- file.order(i= counter, name= file.name, path= script.path)

# Accessory script
script <- "data.01.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= script, path= script.path)

# Source script
source(paste(script.path, file.name, sep=""))


# Get single discipline networks
## -------------------------------------------------------------------------------------------------
# File name
file.name <- "discipline.graph.main.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.name <- file.order(i= counter, name= file.name, path= script.path)

# Accessory scripts
## 1
script <- "discipline.graph.01.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= script, path= script.path)
## 2
script <- "discipline.graph.02.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= script, path= script.path)
## 3
script <- "plot.disc.graph.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= script, path= script.path)

# Source script
source(paste(script.path, file.name, sep=""))

# Get union network
## -------------------------------------------------------------------------------------------------
# File name
file.name <- "union.graph.main.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.name <- file.order(i= counter, name= file.name, path= script.path)

# Accessory scripts
## 1
script <- "union.graph.01.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= script, path= script.path)
## 2
script <- "union.graph.plots.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.order(i= counter, name= script, path= script.path)

# Source script
source(paste(script.path, file.name, sep=""))

# Get network animation
## -------------------------------------------------------------------------------------------------
# File name
file.name <- "network.video.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.name <- file.order(i= counter, name= file.name, path= script.path)

# Source script
source(paste(script.path, file.name, sep=""))

# Final static plots
## -------------------------------------------------------------------------------------------------
# File name
file.name <- "final.static.plots.R"
# Update counter
counter <- counter + 1
# Rename file according to counter
file.name <- file.order(i= counter, name= file.name, path= script.path)

# Source script
source(paste(script.path, file.name, sep=""))
