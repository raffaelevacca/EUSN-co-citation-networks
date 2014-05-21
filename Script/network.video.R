# Set working directory
setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

library(statnet)
library(networkDynamic)
library(ndtv)
library(igraph)
library(intergraph)

rm(list=ls())

# # Load data
# load("/Users/work/Documents/Dropbox/_Lavoro/2013-02_CTSI_network_data/Data/CTSI_networks.rda")
# load("/Users/work/Documents/Dropbox/_Lavoro/2013-02_CTSI_network_data/Data/CTSI_graphs.rda")
# load("./Data/layout.matrixes.rda")

# Load functions
source("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.R")
source("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.R")
source("./Script/functions.R")

# Load data
load("./Data/graphs.rda")


# Convert graphs to networks
union.net.13 <- asNetwork(union.gr.13)
union.net.10 <- asNetwork(union.gr.10)

# Check attributes
network::list.vertex.attributes(union.net.13)
# union.net.13 %v% "na"
network::list.edge.attributes(union.net.13)

# Into list
net.list <- list(union.net.10, union.net.13)

# union.net.13 %v% "vertex.names"

# Create networkDynamic object
net.dyn <- networkDynamic(network.list= net.list, vertex.pid= "vertex.names")


# Compute layouts for animation (Kamada-Kawai)
net.dyn <- compute.animation(net.dyn, slice.par=list(start=0, end=2, interval= 1, aggregate.dur= 1, rule="latest"))



### Compare network as extracted from networkDynamic to original network
####################################################################################################

# Compare networks
network.edgecount(network.collapse(net.dyn, at=0)) == network.edgecount(union.net.10)
network.size(network.collapse(net.dyn, at=0)) == network.size(union.net.10)
network.edgecount(network.collapse(net.dyn, at=1)) == network.edgecount(union.net.13)
network.size(network.collapse(net.dyn, at=1)) == network.size(union.net.13)

# 
# # Check vertex attributes
# network::list.vertex.attributes(net)
# 
# # Get its CTSI vertex attribute
# ctsi09 <- !is.na(net %v% "ctsi09") & net %v% "ctsi09"
# 
# # Generate vertex color vector for CTSI vs non-CTSI
# color <- rep("grey70", network.size(net))
# color[ctsi09] <- "red"
# net %v% "color" <- color
# 
# # Generate vertex size vector for CTSI vs non-CTSI
# size <- rep(0.35, network.size(net))
# size[ctsi09] <- 0.7
# net %v% "size" <- size
# 
# # Save kamada-kawai layout for this network
# net.kk <- plot(net, mode="kamadakawai")
# 
# # Plot
# png.def("temp1.png")
# plot(net, coord= net.kk, vertex.col= "color", vertex.cex= "size", vertex.border= "grey40", edge.col= "grey", edge.lwd= 0.01)
# dev.off()
# 
# rownames(net.kk) <- network.vertex.names(net)
# 
# # Do the same as above, but with the original network
# net <- net.uf.grant.main2009
# 
# ctsi09 <- !is.na(net %v% "ctsi09") & net %v% "ctsi09"
# 
# color <- rep("grey70", network.size(net))
# color[ctsi09] <- "red"
# net %v% "color" <- color
# 
# size <- rep(0.35, network.size(net))
# size[ctsi09] <- 0.7
# net %v% "size" <- size
# 
# # Order net.kk according to vertices in the new network
# df1 <- data.frame(v= rownames(net.kk), x= net.kk[,1], y= net.kk[,2])
# df2 <- data.frame(v= network.vertex.names(net), order= 1:network.size(net))
# df <- merge(df1, df2)
# df <- df[order(df$order),]
# coord <- as.matrix(df[,c("x", "y")])
# 
# # Plot
# png.def("temp2.png")
# plot(net, coord= coord, vertex.col= "color", vertex.cex= "size", vertex.border= "grey40", edge.col= "grey", edge.lwd= 0.01)
# dev.off()

## Vertex discipline TEA
## -------------------------------------------------------------------------------------------------


# After the conversion to networkDynamic object, vertex attributes ctsi08-ctsi12 are vectors of 
# length 1660, one value for each vertex *of the networkDynamic* network.
# However, it looks like these vertex attributes are messed up in the networkDynamic object, i.e.
# they are not put in the right order.
# So I get the ctsi09-ctsi12 attributes from the original networks.

# Data.frame with vertex.names from dynamic network
names <- network.vertex.names(net.dyn)
# Save the order of vertexes in the dynamic network
df <- data.frame(v= names, order= 1:length(names))

# Get CTSI affiliation in each year, from ctsi vertex attributes in the networks
for (i in c("10", "13")) {
  
  # Get network
  net <- get(paste("union.net.", i, sep=""))
  
  # Get discipline attribute in network
  disc <- net %v% "discipline"
  
  # Put vertex names and discipline into data.frame
  year <- data.frame(v= net %v% "vertex.names", disc= disc)
  
  # Rename discipline variable
  names(year)[2] <-  paste("disc", i, sep="")
  
  # Merge with vertex.names from net.dyn
  df <- merge(df, year, all.x=TRUE)
}

# # lapply function to replace NAs with FALSE in the df variables
# df <- lapply(df, function(x) {x[is.na(x)] <- FALSE; x})
# df <- as.data.frame(df)
# Put records back in the order of vertices in net.dyn
df <- df[order(df$order),]

# Put relevant attributes in a list
disc.list <- as.list(df[grep("disc", names(df))])

# Use list to create a single TEA disc.active in the networkDynamic object, in each year
for (i in 1:length(disc.list)) {
  activate.vertex.attribute(net.dyn, "discipline", as.list(disc.list[[i]]), at= i-1)
}

# get.vertex.attribute.active(net.dyn, "discipline", at=0)

# Vertex color TEA
## -------------------------------------------------------------------------------------------------

# Create color list that selects color based on discipline value

# RColorBrewer palettes
pal.1 <- brewer.pal(9, "Blues")
pal.2 <- brewer.pal(9, "Greys")
pal.3 <- brewer.pal(9, "Reds")

# Pre-assign color vectors
v.col <- rep(NA, 3)
v.f.col <- rep(NA, 3)
e.col <- rep(NA, 3)

# Vertex colors
v.col[1] <- pal.1[7]
v.col[2] <- pal.2[4]
v.col[3] <- pal.3[7]
# Vertex frame colors
v.f.col[1] <- pal.1[9]
v.f.col[2] <- pal.2[9]
v.f.col[3] <- pal.3[9]
# Edge colors
e.col[1] <- pal.1[6] 
e.col[2] <- pal.2[6]
e.col[3] <- pal.3[6]


# Replicate disc.list
color <- disc.list

# Populate color list
for (i in 1:length(color)) {
color[[i]][!is.na(disc.list[[i]]) & disc.list[[i]]==1] <- v.col[1]
color[[i]][!is.na(disc.list[[i]]) & disc.list[[i]]==2] <- v.col[2]
color[[i]][!is.na(disc.list[[i]]) & disc.list[[i]]==3] <- v.col[3]
}

# Rename
names(color) <- c("col10", "col13")

## Use color list to assign color.active to the networkDyanmic object
for (i in 1:length(color)) {
  activate.vertex.attribute(net.dyn, "color", as.list(color[[i]]), at= i-1)
}

# # Do the same as above with size: 0.7 for ctsi, 0.35 outside
# size <- rep(rep(0.35, network.size(net.dyn)), 5)
# size <- split(size, f= rep(1:5, each= network.size(net.dyn)))
# ## Replace with 0.7 when ctsi==TRUE in each list element
# for (i in 1:length(size)) {
#   size[[i]][ctsi[[i]]] <- 0.7
# }
# 
# ## Use size to assign size.active to the networkDyanmic object
# for (i in 1:length(size)) {
#   activate.vertex.attribute(net.dyn, "size", as.list(size[[i]]), at= i-1)
# }


x11()
render.animation(net.dyn, vertex.col="color", vertex.cex= 0.5, vertex.border= "grey40", edge.col= "grey", edge.lwd= 0.01, displaylabels= FALSE, plot.par= list(bg='white', mai= rep(0, 4)), render.par= list(tween.frames= 40), ani.options=list(interval=0.1))

saveVideo(ani.replay(), video.name= "prova.mp4", other.opts= "-b 10000k -s 1920x1080", clean= TRUE)



















