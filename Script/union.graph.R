setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

library(igraph)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(Hmisc)

rm(list=ls())

# Load functions
source("./Script/functions.R")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.rfn")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.rfn")

# Load data
load("./Data/graphs.13.rda")


####################################################################################################
### MERGE THE NETWORKS                                                                           ###
####################################################################################################

gr1 <- social.one
gr2 <- computer.one
gr3 <- physics.one

## Check author name consistency
## -----------------------------------------------------------------------------------------------

# In a few cases a single author is duplicated into multiple authors because of the first name. So
# let's remove the first name and only keep the last name. However we need to exclude those with
# very common last names, in which case this operation aould aggregate multiple different authors
# into a single author.

# Last names to be excluded 
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "wang", "ma", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "lu", "fu", "hu")
# Add regexp pattern
exclude <- sapply(exclude, function(x) paste("^", x, "\\>", sep=""))
# Collapse for use into grep
exclude <- paste(exclude, collapse="|")

# Create list of graph to be used for the loop
l <- list("gr1"= gr1, "gr2"= gr2, "gr3"= gr3)

for (i in 1:length(l)) {
  
  # Get the graph
  gr <- l[[i]]
  
  # Author names to be replaced (=with a blank)
  index1 <- grepl(" ", V(gr)$name)
  # Author names with last names to be exluded
  index2 <- grepl(exclude, V(gr)$name)
  
  # Get the names to be replaces (except those to be excluded), and replace them with their last name.
  V(gr)$name[index1 & !index2] <- sapply(V(gr)$name[index1 & !index2], function(y) unlist(strsplit(y, " "))[1])
  
  # Reassign the graph
  assign(names(l)[i], gr)
}


# Check that name spelling is consistent across social network and network science graphs.
data1 <- data.frame(name= V(gr1)$name, gr1=1, stringsAsFactors=FALSE)
data2 <- data.frame(name= V(gr2)$name, gr2=1, stringsAsFactors=FALSE)
data3 <- data.frame(name= V(gr3)$name, gr3=1, stringsAsFactors=FALSE)
## Merge
data <- merge(data1, data2, by="name", all=TRUE)
data <- merge(data, data3, by="name", all=TRUE)
## Reorder
data <- data[order(data$name),]

# Replace 1 with TRUE and NA with FALSE
for (i in 2:4) {
  data[[i]][is.na(data[[i]])] <- 0
  data[[i]] <- as.logical(data[[i]])
}

# # Tabulate
# ## Overlap between social sciences and computer sciences
# table(data[,2:3])
# ## Overlap between social sciences and physics
# table(data[,c(2,4)])
# ## Overlap between computer sciences and physics
# table(data[,3:4])


## Get the union graphs with edge weights
## -----------------------------------------------------------------------------------------------

# Before merging the networks, we remove edges from each network based on the *relative*
# distribution of edge weights (=number times 2 authors are co-cited) in each network. That way we
# don't have to set a single rule for the 3 networks (= 3 different disciplines) afterwards.
# We remove in each network all the edges with weight below the 0.9 percentile. In other words, we
# only keep the *most* co-cited in each network.
gr1 <- delete.edges(gr1, E(gr1)[weight < quantile(E(gr1)$weight, prob= 0.9)])
gr2 <- delete.edges(gr2, E(gr2)[weight < quantile(E(gr2)$weight, prob= 0.9)])
gr3 <- delete.edges(gr3, E(gr3)[weight < quantile(E(gr3)$weight, prob= 0.9)])


## Union between social and computer
union.gr.12 <- union.graph(gr1, gr2, w.names= c("weight", "weight"), output.w.names= c("weight.social", "weight.computer"), vert.attr.names= c("vertex.social", "vertex.computer"))

# Social+computer edge weight (number co-citations)
E(union.gr.12)$weight.12 <- E(union.gr.12)$weight.social + E(union.gr.12)$weight.computer

## Between social/computer and physics
union.gr <- union.graph(union.gr.12, gr3, w.names= c("weight.12", "weight"), output.w.names= c("weight.12", "weight.physics"), vert.attr.names= c("vertex.12", "vertex.physics"))

# Calculate total edge weight
E(union.gr)$weight <- E(union.gr)$weight.12 + E(union.gr)$weight.physics

# Clean the graph
union.gr <- remove.vertex.attribute(union.gr, "vertex.12")
union.gr <- remove.vertex.attribute(union.gr, "vertex.physics")
union.gr <- remove.edge.attribute(union.gr, "weight.12")
union.gr <- remove.edge.attribute(union.gr, "weight.physics")

# Use data to create vertex attributes that tell which network(s) the vertex comes from
union.gr <- vert.attr.multi(data, union.gr, dataID= "name", graphID= "name", attributes= c("gr1", "gr2", "gr3"), attr.names= c("v.social", "v.computer", "v.physics"))

####################################################################################################
### PLOTS                                                                             ###
####################################################################################################

gr <- union.gr

# # Only keep main component
# comp <- clusters(gr)
# gr <- delete.vertices(gr, V(gr)[comp$membership!=3])
# 
# # Remove pendants (degree==1)
# gr <- delete.vertices(gr, V(gr)[degree(gr) == 1])

# Kamada-Kawai layout
set.seed(0515)
layout <- layout.kamada.kawai(gr)

# Rotate a little
# layout <- rotate.layout(layout, -20)

# Rescale edge weigth to use it as width
width <- rescale(E(gr)$weight, to= c(0.3, 8))

# Vertex size
v.size <- rescale(degree(gr), to= c(2, 7))


# Plot with author names
## -------------------------------------------------------------------------------------------------

# Edge color
e.col <- hex.seq(width, palette= brewer.pal(9, "Blues")[5:9])
e.col <- alpha.col(e.col, 0.5)

# Vertex color
v.col <- brewer.pal(9, "Blues")[2]

# Labels: vertex names with 1st letter to upper
labels <- sapply(V(gr)$name, function(x) paste(toupper(substr(x, 1, 1)), substring(x,2), sep=""))

# Label size
l.cex <- rescale(degree(gr), to= c(0.5, 1))

# Plot
png.def("./Figures/union.names.png")
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color="darkblue", label.cex = l.cex)
dev.off()


# Plot with no names and different colors
## -------------------------------------------------------------------------------------------------

# Social Sciences ONLY vertices
v1 <- V(gr)[v.social==1 & v.computer==0 & v.physics==0]
# Computer Science ONLY vertices
v2 <- V(gr)[v.social==0 & v.computer==1 & v.physics==0]
# Physics ONLY vertices
v3 <- V(gr)[v.social==0 & v.computer==0 & v.physics==1]

# Overlapping
v12 <- V(gr)[v.social==1 & v.computer==1 & v.physics==0]
v13 <- V(gr)[v.social==1 & v.computer==0 & v.physics==1]
v23 <- V(gr)[v.social==0 & v.computer==1 & v.physics==1]
v123 <- V(gr)[v.social==1 & v.computer==1 & v.physics==1]

# All vertices who are overlapping between 2 or 3 disciplines
# only <- c(v1, v2, v3)
# v4 <- V(gr)[!(V(gr) %in% only)]

# # Edge color
# e.col <- hex.seq(width, palette= brewer.pal(9, "Blues")[5:9])
# e.col <- alpha.col(e.col, 0.5)

# Vertex colors
v.col1 <- brewer.pal(9, "Reds")[6]
v.col2 <- brewer.pal(9, "Blues")[6]
v.col3 <- brewer.pal(9, "Greens")[6]
v.col12 <- brewer.pal(9, "Purples")[6]
v.col13 <- "Yellow"
v.col23 <- "Cyan"
v.col23 <- "grey20"

# Vertex fram colors
# Darker version
v.f.col1 <- brewer.pal(9, "Blues")[9]
v.f.col2 <- brewer.pal(9, "Greens")[9]
v.f.col3 <- brewer.pal(9, "Greens")[9]
v.f.col12 <- brewer.pal(9, "Purples")[9]
v.f.col13 <- "Yellow"
v.f.col23 <- "Blue"
v.f.col23 <- "Black"


# # Transparent version
# sn.col.t <- alpha.col(sn.col, 0.5) 
# ns.col.t <- alpha.col(ns.col, 0.5)
# b.col.t <- alpha.col(b.col, 0.5)
# 
# # Edges of different types with different colors
# E(gr)[SN %--% SN]$color <- sn.col.t
# E(gr)[SN %--% B]$color <- sn.col.t
# E(gr)[SN %--% NS]$color <- b.col.t
# E(gr)[B %--% B]$color <- b.col.t
# E(gr)[NS %--% NS]$color <- ns.col.t
# E(gr)[NS %--% B]$color <- ns.col.t

# Colors
V(gr)[v1]$color <- v.col1
V(gr)[v1]$frame.color <- v.f.col1
V(gr)[v2]$color <- v.col2
V(gr)[v2]$frame.color <- v.f.col2
V(gr)[v3]$color <- v.col3
V(gr)[v3]$frame.color <- v.f.col3
V(gr)[v4]$color <- v.col4
V(gr)[v4]$frame.color <- v.f.col4


# Plot
png.def("./Figures/temp.png")
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, vert.col= V(gr)$color, vert.frame.col= V(gr)$frame.color)
dev.off()
