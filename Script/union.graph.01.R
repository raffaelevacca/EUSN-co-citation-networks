## =================================================================================================
### Merge the networks from social sciences and comp science + physics                           ###
## =================================================================================================

gr1 <- get(paste("social.one.", year, sep=""))
gr2 <- get(paste("comp_physics.one.", year, sep="")) 

## Check author name consistency
## -----------------------------------------------------------------------------------------------

# Check that name spelling is consistent across social network and network science graphs.
data1 <- data.frame(name= V(gr1)$name, gr1=1, stringsAsFactors=FALSE)
data2 <- data.frame(name= V(gr2)$name, gr2=1, stringsAsFactors=FALSE)
## Merge
data <- merge(data1, data2, by="name", all=TRUE)
## Reorder
data <- data[order(data$name),]

# Replace 1 with TRUE and NA with FALSE
for (i in 2:3) {
  data[[i]][is.na(data[[i]])] <- 0
  data[[i]] <- as.logical(data[[i]])
}

# # Tabulate
# ## Author overlap between social sciences and computer sciences + physics
# table(data[,2:3])


## Get the union graphs with edge weights
## -----------------------------------------------------------------------------------------------

# Before merging the networks, we remove edges from each network based on the *relative*
# distribution of edge weights (=number times 2 authors are co-cited) in each network. This way we
# don't have to set a single threshold for the 2 networks (= 2 different disciplines) afterwards.
# We remove in each network all the edges with weight below the 0.9 percentile. In other words, we
# only keep the *most* co-cited in each network.
gr1 <- delete.edges(gr1, E(gr1)[weight < quantile(E(gr1)$weight, prob= 0.9)])
gr2 <- delete.edges(gr2, E(gr2)[weight < quantile(E(gr2)$weight, prob= 0.9)])

## Union graph
union.gr <- union.graph(gr1, gr2, w.names= c("weight", "weight"), output.w.names= c("weight.1", "weight.2"), vert.attr.names= c("vertex.1", "vertex.2"))

# Calculate total edge weight
E(union.gr)$weight <- E(union.gr)$weight.1 + E(union.gr)$weight.2

# Create vertex attribute that gives discipline: 1= social sciences, 2= comp sciences or physics, 3= both
V(union.gr)$discipline <- NA
V(union.gr)[vertex.1 & !vertex.2]$discipline <- 1
V(union.gr)[!vertex.1 & vertex.2]$discipline <- 2
V(union.gr)[vertex.1 & vertex.2]$discipline <- 3

# Only keep the main component
## Find components
comp <- clusters(union.gr)
## Vertex sequence in the main component
main <- V(union.gr)[comp$membership==which(comp$csize==max(comp$csize))[1]]
## Size of main comp as a proportion of network size: save as network attribute
union.gr$prop.main <- length(main)/vcount(union.gr)
## Only keep main component in graph
union.gr <- delete.vertices(union.gr, V(union.gr)[comp$membership!=which(comp$csize==max(comp$csize))[1]])

# Save
assign(paste("union.gr.", year, sep=""), union.gr)