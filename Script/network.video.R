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
# network::list.vertex.attributes(union.net.13)
# union.net.13 %v% "na"
# network::list.edge.attributes(union.net.13)

# Check consistency
## -------------------------------------------------------------------------------------------------

# Check consistency of names across networks, and correctedness of conversion into dynamic network

# Vertex names in 10 vs 13
names.10 <- data.frame(name= union.net.10 %v% "vertex.names", y10=1, stringsAsFactors=FALSE)
names.13 <- data.frame(name= union.net.13 %v% "vertex.names", y13=1, stringsAsFactors=FALSE)
## Merge
names <- merge(names.10, names.13, by="name", all=TRUE)
## Reorder
names <- names[order(names$name),]

# Replace 1 with TRUE and NA with FALSE
for (i in 2:3) {
  names[[i]][is.na(names[[i]])] <- 0
  names[[i]] <- as.logical(names[[i]])
}

# # Tabulate
# ## Author overlap between social sciences and computer sciences + physics
# table(names[,c("y10", "y13")])

# Create networkDynamic object
net.dyn <- networkDynamic(network.list= list(union.net.10, union.net.13), vertex.pid= "vertex.names")

# Check that networkDynamic() results are consistent with previous data.frame
## Dataframe of vertex activity
data.act <- get.vertex.activity(net.dyn, as.spellList= TRUE)
## Dataframe of vertex IDs and names
v.names <- data.frame(vertex.id= 1:length(net.dyn %v% "vertex.names"), name= net.dyn %v% "vertex.names")
## Bring names into data.act
data.act <- merge(data.act, v.names, by= "vertex.id")
## Get y10 and y13
data.act$y10 <- data.act$y13 <- NA
data.act$y10[data.act$onset==0] <- TRUE
data.act$y10[data.act$onset>0] <- FALSE
data.act$y13[data.act$terminus==2] <- TRUE
data.act$y13[data.act$terminus<2] <- FALSE
## Results are consistent
# table(data.act[,c("y10", "y13")])

# Compare network as extracted from networkDynamic to original network

# Compare networks
network.edgecount(network.collapse(net.dyn, at=0)) == network.edgecount(union.net.10)
network.size(network.collapse(net.dyn, at=0)) == network.size(union.net.10)
network.edgecount(network.collapse(net.dyn, at=1)) == network.edgecount(union.net.13)
network.size(network.collapse(net.dyn, at=1)) == network.size(union.net.13)

# Compute layouts for animation (Kamada-Kawai)
set.seed(0522)
net.dyn <- compute.animation(net.dyn, slice.par=list(start=0, end=1, interval= 1, aggregate.dur= 1, rule="latest"))


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
attributes <- data.frame(v= names, order= 1:length(names))

# Get discipline in each year from static network vertex attributes
for (i in c("10", "13")) {
  
  # Get network
  net <- get(paste("union.net.", i, sep=""))
  
  # Get discipline attribute in network
  disc <- net %v% "discipline"
  
  # Put vertex names and discipline into data.frame
  disc <- data.frame(v= net %v% "vertex.names", disc= disc)
  
  # Rename discipline variable
  names(disc)[2] <-  paste("y", i, sep="")
  
  # Merge with vertex.names from net.dyn
  attributes <- merge(attributes, disc, all.x=TRUE)
}

# Put records back in the order of vertices in net.dyn
attributes <- attributes[order(attributes$order),]

# Get relevant attributes
disciplines <- attributes[grep("y", names(attributes))]

# Use list to create a single TEA disc.active in the networkDynamic object, in each year
for (i in 1:length(disciplines)) {
  activate.vertex.attribute(net.dyn, "discipline", as.list(disciplines[[i]]), at= i-1)
}

# get.vertex.attribute.active(net.dyn, "discipline", at=0)

# Set graphical vertex Time Extended Attributes
## -------------------------------------------------------------------------------------------------

# ---------- Vertex color TEA

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

# Apply transparency
e.col <- alpha.col(e.col, 0.5)
v.col <- alpha.col(v.col, 0.8)
v.f.col <- alpha.col(v.f.col, 0.8)

# Replicate disciplines
v.color <- disciplines

# Populate v.color list
for (i in 1:length(v.color)) {
# Social sciences v.color
v.color[[i]][!is.na(disciplines[[i]]) & disciplines[[i]]==1] <- v.col[1]
# Comp science + physics v.color
v.color[[i]][!is.na(disciplines[[i]]) & disciplines[[i]]==2] <- v.col[2]
# Both v.color
v.color[[i]][!is.na(disciplines[[i]]) & disciplines[[i]]==3] <- v.col[3]
}


## Use v.color list to assign v.color.active to the networkDyanmic object
for (i in 1:length(v.color)) {
  activate.vertex.attribute(net.dyn, "v.color", as.list(v.color[[i]]), at= i-1)
}

# ---------- Vertex frame color TEA

# Replicate disciplines
v.f.color <- disciplines

# Populate v.color list
for (i in 1:length(v.f.color)) {
  # Social sciences v.color
  v.f.color[[i]][!is.na(disciplines[[i]]) & disciplines[[i]]==1] <- v.f.col[1]
  # Comp science + physics v.f.color
  v.f.color[[i]][!is.na(disciplines[[i]]) & disciplines[[i]]==2] <- v.f.col[2]
  # Both v.f.color
  v.f.color[[i]][!is.na(disciplines[[i]]) & disciplines[[i]]==3] <- v.f.col[3]
}


## Use v.color list to assign v.color.active to the networkDyanmic object
for (i in 1:length(v.f.color)) {
  activate.vertex.attribute(net.dyn, "v.f.color", as.list(v.f.color[[i]]), at= i-1)
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

# Set graphical edge Time Extended Attributes 
## -------------------------------------------------------------------------------------------------

# Edge list from dynamic network
elist.dyn <- as.matrix.network(net.dyn, matrix.type="edgelist")
# Get vertex names in the order used in edge list
vnames <- attr(elist.dyn, "vnames")
# Get edge list with vertex names instead of vertex ids
elist.dyn[,1] <- vnames[as.numeric(elist.dyn[,1])]
elist.dyn[,2] <- vnames[as.numeric(elist.dyn[,2])]
elist.dyn <- as.data.frame(elist.dyn)

# Activity spells for each edge
edge.act <- get.edge.activity(net.dyn)

# Add "on" and "term" variables to edge list
elist.dyn$on <- sapply(edge.act, function(x) x[1])
elist.dyn$term <- sapply(edge.act, function(x) x[2])

# Edge list with edge weight from a static network
elist <- as.matrix.network(union.net.13, matrix.type="edgelist", attrname= "weight")
# Get vertex names in the order used in edge list
vnames <- attr(elist, "vnames")
# Get edge list with vertex names instead of vertex ids
elist[,1] <- vnames[as.numeric(elist[,1])]
elist[,2] <- vnames[as.numeric(elist[,2])]
elist <- as.data.frame(elist)




# Get discipline in each year from static network vertex attributes
for (i in c("10", "13")) {
  
  # Get network
  net <- get(paste("union.net.", i, sep=""))
  
  # Get discipline attribute in network
  disc <- net %v% "discipline"
  
  # Put vertex names and discipline into data.frame
  disc <- data.frame(v= net %v% "vertex.names", disc= disc)
  
  # Rename discipline variable
  names(disc)[2] <-  paste("y", i, sep="")
  
  # Merge with vertex.names from net.dyn
  attributes <- merge(attributes, disc, all.x=TRUE)
}

# Put records back in the order of vertices in net.dyn
attributes <- attributes[order(attributes$order),]

# Get relevant attributes
disciplines <- attributes[grep("y", names(attributes))]

# Use list to create a single TEA disc.active in the networkDynamic object, in each year
for (i in 1:length(disciplines)) {
  activate.vertex.attribute(net.dyn, "discipline", as.list(disciplines[[i]]), at= i-1)
}

# Get animation
## -------------------------------------------------------------------------------------------------

# x11()
render.animation(net.dyn, vertex.col="v.color", vertex.cex= 0.8, vertex.border= "v.f.color", edge.col= alpha.col("grey", 0.4), edge.lwd= 0.01, displaylabels= FALSE, plot.par= list(bg='white', mai= rep(0, 4)), render.par= list(tween.frames= 50), ani.options=list(interval=0.1, outdir= paste(getwd(), "/Figures", sep=""), ani.width= 2560, ani.height= 1440))

# saveVideo(ani.replay(), video.name= "prova.mp4", other.opts= "-b 10000k -s 1280x720", clean= TRUE)

saveHTML(ani.replay(), interval=0.1, outdir= paste(getwd(), "/Figures", sep=""), ani.width= 960, ani.height= 540)

















