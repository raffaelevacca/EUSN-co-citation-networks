## =================================================================================================
### Plots for the union graph                                                                    ###
## =================================================================================================

# Set the year
year <- "10"

# Get the graph
gr <- get(paste("union.gr.", year, sep=""))

# # Remove pendants (degree==1)
# gr <- delete.vertices(gr, V(gr)[degree(gr) == 1])

# Kamada-Kawai layout
set.seed(0515)
layout <- layout.kamada.kawai(gr)

# Rotate
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
l.cex <- 0.5 # rescale(degree(gr), to= c(0.5, 1))

# Plot
png.def(paste("./Figures/union.names.", year, ".png", sep=""))
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color="darkblue", label.cex = l.cex)
dev.off()


# Plot with no names and different colors
## -------------------------------------------------------------------------------------------------

# Rescale edge weigth to use it as width
width <- rescale(E(gr)$weight, to= c(0.5, 8))

# Vertex size
v.size <- rescale(degree(gr), to= c(1.5, 6))

# Social Sciences ONLY vertices
v1 <- V(gr)[vertex.1==1 & vertex.2==0]
# Comp+Physics Science ONLY vertices
v2 <- V(gr)[vertex.1==0 & vertex.2==1]
# Overlapping vertices
v3 <- V(gr)[vertex.1==1 & vertex.2==1]

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

# Edges of different types with different colors
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

# Color for Social sciences
V(gr)[v1]$color <- v.col[1]
V(gr)[v1]$frame.color <- v.f.col[1]
# Color for Comp+physics
V(gr)[v2]$color <- v.col[2]
V(gr)[v2]$frame.color <- v.f.col[2]
# Color for Overlap
V(gr)[v3]$color <- v.col[3]
V(gr)[v3]$frame.color <- v.f.col[3]


# Plot
png.def(paste("./Figures/union.colors.", year, ".png", sep=""))
plot.gr(gr, layout= layout, vertex.size=v.size, vert.col= V(gr)$color, vert.frame.col= V(gr)$frame.color, edge.width= width, edge.color= E(gr)$color)
dev.off()