# Set working directory
setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

library(statnet)
library(networkDynamic)
library(ndtv)
library(igraph)
library(intergraph)
library(scales)

rm(list=ls())

# Load functions
source("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.R")
source("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.R")
source("./Script/functions.R")


## =================================================================================================
### Get the data                                                                                 ###
## =================================================================================================

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


## =================================================================================================
### Get the networkDynamic object                                                                ###
## =================================================================================================

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

## =================================================================================================
### Get the discipline vertex attribute as a TEA attribute                                       ###
## =================================================================================================

# Data.frame with vertex.names from dynamic network
dyn.names <- network.vertex.names(net.dyn)
# Save the order of vertexes in the dynamic network
dyn.names <- data.frame(names= dyn.names, order= 1:length(dyn.names), stringsAsFactors=FALSE)

# Preassign disciplines data frame
disciplines <- dyn.names

# Get discipline in each year from static network vertex attributes
for (i in c("10", "13")) {
  
  # Get network
  net <- get(paste("union.net.", i, sep=""))
  
  # Get discipline attribute in network
  disc <- net %v% "discipline"
  
  # Put vertex names and discipline into data.frame
  disc <- data.frame(names= net %v% "vertex.names", disc= disc)
  
  # Rename discipline variable
  names(disc)[2] <-  paste("y", i, sep="")
  
  # Merge with vertex.names from net.dyn
  disciplines <- merge(disciplines, disc, by= "names", all.x=TRUE)
}

# Put records back in the order of vertices in net.dyn
disciplines <- disciplines[order(disciplines$order),]

# Use list to create a single TEA disc.active in the networkDynamic object, in each year
## Attribute name in each year
y <- c("y10", "y13")
## For each year
for (i in 1:length(y)) {
  activate.vertex.attribute(net.dyn, "discipline", disciplines[[y[i]]], at= i-1)
}

# get.vertex.attribute.active(net.dyn, "discipline", at=0)

## =================================================================================================
### Get graphical attributes as TEAs                                                             ###
## =================================================================================================

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
e.col <- alpha.col(e.col, 0.3)
v.col <- alpha.col(v.col, 0.65)
v.f.col <- alpha.col(v.f.col, 0.9)

# Vertex color
## -------------------------------------------------------------------------------------------------

# Replicate disciplines
v.color <- disciplines

# Populate v.color 
variables <- c("y10", "y13")
for (v in variables) {
# Social sciences v.color
v.color[[v]][!is.na(disciplines[[v]]) & disciplines[[v]]==1] <- v.col[1]
# Comp science + physics v.color
v.color[[v]][!is.na(disciplines[[v]]) & disciplines[[v]]==2] <- v.col[2]
# Both v.color
v.color[[v]][!is.na(disciplines[[v]]) & disciplines[[v]]==3] <- v.col[3]
}


## Use v.color to assign v.color.active to the networkDyanmic object
v.color <- v.color[,variables]
for (i in 1:length(v.color)) {
  activate.vertex.attribute(net.dyn, "v.color", v.color[[i]], at= i-1)
}

# Vertex frame color
## -------------------------------------------------------------------------------------------------

# Replicate disciplines
v.f.color <- disciplines

# Populate v.f.color 
variables <- c("y10", "y13")
for (v in variables) {
  # Social sciences v.f.color
  v.f.color[[v]][!is.na(disciplines[[v]]) & disciplines[[v]]==1] <- v.f.col[1]
  # Comp science + physics v.f.color
  v.f.color[[v]][!is.na(disciplines[[v]]) & disciplines[[v]]==2] <- v.f.col[2]
  # Both v.f.color
  v.f.color[[v]][!is.na(disciplines[[v]]) & disciplines[[v]]==3] <- v.f.col[3]
}

## Use v.f.color to assign v.f.color.active to the networkDyanmic object
v.f.color <- v.f.color[,variables]
for (i in 1:length(v.f.color)) {
  activate.vertex.attribute(net.dyn, "v.f.color", v.f.color[[i]], at= i-1)
}

# Vertex size
## -------------------------------------------------------------------------------------------------

# Pre-assign degree data frame for dynamic network
dyn.degree <- dyn.names

# Get degree in each year 
for (i in c("10", "13")) {
  
  # Get network
  gr <- get(paste("union.gr.", i, sep=""))
  
  # Get degree
  degree <- igraph::degree(gr)
  
  # As data.frame
  degree <- data.frame(names= names(degree), degree= degree, stringsAsFactors=FALSE)
  
  # Rename
  names(degree)[2] <- i
  
  # Merge with vertex.names from net.dyn
  dyn.degree <- merge(dyn.degree, degree, by= "names", all.x=TRUE)
}

# Put records back in the order of vertices in net.dyn
dyn.degree <- dyn.degree[order(dyn.degree$order),]

# Only keep relevant variables
v <- c("10", "13")
dyn.degree <- dyn.degree[v]
# Rescale
for (i in 1:length(dyn.degree)) {
  dyn.degree[[i]] <- rescale(dyn.degree[[i]], c(0.3, 1.5))
}


# Use list to create a single TEA disc.active in the networkDynamic object, in each year
for (i in 1:length(v)) {
  activate.vertex.attribute(net.dyn, "size", dyn.degree[[v[i]]], at= i-1)
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

# Edge weight
## -------------------------------------------------------------------------------------------------

# Edge list from dynamic network
elist.dyn <- as.matrix.network(net.dyn, matrix.type="edgelist")

# Get vertex names in the edge list
vnames <- attr(elist.dyn, "vnames")
# Get edge list with vertex names instead of vertex ids
elist.dyn[,1] <- vnames[as.numeric(elist.dyn[,1])]
elist.dyn[,2] <- vnames[as.numeric(elist.dyn[,2])]
elist.dyn <- as.data.frame(elist.dyn, stringsAsFactors=FALSE)
# Rename
names(elist.dyn) <- c("from", "to")

# Activity spells for each edge
edge.act <- get.edge.activity(net.dyn)

# Add "on" and "term" variables to edge list
elist.dyn$on <- sapply(edge.act, function(x) x[1])
elist.dyn$term <- sapply(edge.act, function(x) x[2])

# Save original order of records (=order of edges) in dynamic network
elist.dyn$order <- 1:nrow(elist.dyn)

# Sort "from" and "to" so as to avoid duplicated edges when merging with edge list from static networks.
## Which data.frame record has "from" larger than "to"
swap <- which(elist.dyn[,"from"] > elist.dyn[,"to"])

# If there are any of these records...
if (length(swap) > 0) { 
  # Swap second with first column in those.
  elist.dyn[swap, 1:2] <- cbind(elist.dyn[swap, 2], elist.dyn[swap, 1]) 
}

# Edge lists with edge weights from static networks.
## Relevant networks in a list
l <- list("10"= union.net.10, "13"= union.net.13)
## For each network
for (i in 1:length(l)) {
  
  # Get network
  net <- l[[i]]
  
  # Get ordered edge list with weight
  elist <- ord.el(net, data.names = c("from", "to", paste("w", names(l)[i], sep="")))
  
  # Merge with dyn network edge list
  elist.dyn <- merge(elist.dyn, elist, by= c("from", "to"), all=TRUE)
}

# Reorder edge list according to original order of edges in dynamic network
elist.dyn <- elist.dyn[order(elist.dyn[["order"]]),]

# Crop out weights
weights <- elist.dyn[,c("w10", "w13")]

# Rescale
for (i in 1:length(weights)) {
  weights[[i]] <- rescale(weights[[i]], to= c(0.01, 5))
}

# Use elist.dyn columns to set TEAs of edge weight in each network
## For each year's weight
for (i in 1:length(weights)) {
  activate.edge.attribute(net.dyn, "weight", weights[[i]], at= i-1)
}

# describe(get.edge.attribute.active(net.dyn, "weight", at=0))

# Edge color
## -------------------------------------------------------------------------------------------------

# Relevant graphs in a list
l <- list("10"= union.gr.10, "13"= union.gr.13)

# Edges of different types with different colors
## For each graph
for (i in 1:length(l)) {
  
  # Get the graph
  gr <- l[[i]]
  
  # Social Sciences ONLY vertices
  v1 <- V(gr)[vertex.1==1 & vertex.2==0]
  # Comp+Physics Science ONLY vertices
  v2 <- V(gr)[vertex.1==0 & vertex.2==1]
  # Overlapping vertices
  v3 <- V(gr)[vertex.1==1 & vertex.2==1]
  
  ## Within Social Sciences
  E(gr)[v1 %--% v1]$color <- e.col[1]
  ## Between Social Sciences and Overlap
  E(gr)[v1 %--% v3]$color <- e.col[1]
  ## Within Comp+Physics
  E(gr)[v2 %--% v2]$color <- e.col[2]
  ## Between Comp+Physics and Overlap
  E(gr)[v2 %--% v3]$color <- e.col[2]
  ## Within Overlap
  E(gr)[v3 %--% v3]$color <- e.col[3]
  ## Notice that by construction there are no edges between Social Sciences and Comp+Physics
  # E(gr)[v1 %--% v2]
  
  # Reassign the graph
  assign(paste("union.gr.", names(l)[i], sep=""), gr)
}

# Edge lists with edge colors from static networks.
# Relevant graphs in a list
l <- list("10"= union.gr.10, "13"= union.gr.13)
## For each graph
for (i in 1:length(l)) {
  
  # Get network
  gr <- l[[i]]
  
  # Get ordered edge list with weight
  elist <- ord.el(gr, w.name= "color", data.names = c("from", "to", paste("col", names(l)[i], sep="")))
  
  # Merge with dyn network edge list
  elist.dyn <- merge(elist.dyn, elist, by= c("from", "to"), all=TRUE)
}

# Reorder edge list according to original order of edges in dynamic network
elist.dyn <- elist.dyn[order(elist.dyn[["order"]]),]

# Crop out colors
e.colors <- elist.dyn[,c("col10", "col13")]

# Use elist.dyn columns to set TEAs of edge color in each network
## For each year's weight
for (i in 1:length(e.colors)) {
  activate.edge.attribute(net.dyn, "e.color", e.colors[[i]], at= i-1)
}

# describe(get.edge.attribute.active(net.dyn, "e.color", at=0))

## =================================================================================================
### Get the animation                                                                            ###
## =================================================================================================

# Legend for animation
legend <- expression(legend(2.3, -1.5, legend= c("Social Sciences", "Computer Sciences/Physics", "Both"), pch= c(21, 21, 21), col= v.f.col, pt.bg= v.col, bty="n", cex= 2.5, pt.cex=4, x.intersp= 0.5))

# Render parameters 
render.par <- list(tween.frames= 1, extraPlotCmds= legend)

# x11()
render.animation(net.dyn, vertex.col="v.color", vertex.cex= "size", vertex.border= "v.f.color", edge.col= "e.color", edge.lwd= "weight", displaylabels= FALSE, plot.par= list(bg='white', mai= rep(0, 4)), render.par= render.par, ani.options=list(interval=0.1, outdir= paste(getwd(), "/Figures", sep=""), ani.width= 3840, ani.height= 2160))

# saveVideo(ani.replay(), video.name= "prova.mp4", other.opts= "-b 10000k -s 1280x720", clean= TRUE)

saveHTML(ani.replay(), interval=0.1, outdir= paste(getwd(), "/Figures", sep=""), ani.width= 1920, ani.height= 1080, autobrowse=FALSE)
