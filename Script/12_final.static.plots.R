# Load graphs
# load("./Data/graphs.rda")
# Load static layout from dynamic network
# load("./Data/dyn.layout.rda")

# Relevant time period
year <- "13"

# Get the graph
gr <- get(paste("union.gr.", year, sep=""))

# Kamada-Kawai layout
set.seed(0515)
layout <- igraph::layout.kamada.kawai(gr)

# # Static layout from dynamic network
# layout <- dyn.layout
# # Reorder according to vertex names in gr
# layout <- layout.order(layout, gr)


# Plot with author names
## -------------------------------------------------------------------------------------------------

# Rescale edge weigth to use it as width
width <- rescale(E(gr)$weight, to= c(0.3, 8))

# Vertex size
v.size <- rescale(igraph::degree(gr), to= c(2, 7))

# Edge color
e.col <- hex.seq(width, palette= brewer.pal(9, "Blues")[5:9])
e.col <- alpha.col(e.col, 0.4)

# Vertex color
v.col <- brewer.pal(9, "Blues")[2]

# Labels: vertex names with 1st letter to upper
labels <- sapply(V(gr)$name, function(x) paste(toupper(substr(x, 1, 1)), substring(x,2), sep=""))

# Label size
l.cex <- 0.5

# Plot
png.def(paste("./Figures/union.names.", year, ".final.png", sep=""), width= 2700, height= 2000, margins= c(0,0,0,2))
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color=alpha.col("darkblue", 0.7), label.cex = l.cex, label.family= "sans")
dev.off()

# Plot with no names and different colors
## -------------------------------------------------------------------------------------------------

# Rescale edge weigth to use it as width
E(gr)$width <- rescale(E(gr)$weight, to= c(0.5, 8))

# Vertex size
v.size <- rescale(igraph::degree(gr), to= c(1.5, 6))

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

# Vertex colors
v.col[1] <- pal.1[7]
v.col[2] <- pal.2[4]
v.col[3] <- pal.3[7]
# Vertex frame colors
v.f.col[1] <- pal.1[9]
v.f.col[2] <- pal.2[9]
v.f.col[3] <- pal.3[9]

# Color for Social sciences
V(gr)[v1]$color <- v.col[1]
V(gr)[v1]$frame.color <- v.f.col[1]
# Color for Comp+physics
V(gr)[v2]$color <- v.col[2]
V(gr)[v2]$frame.color <- v.f.col[2]
# Color for Overlap
V(gr)[v3]$color <- v.col[3]
V(gr)[v3]$frame.color <- v.f.col[3]

# Colors for edges
## Within Social Sciences
E(gr)[v1 %--% v1]$color <- hex.seq(E(gr)[v1 %--% v1]$width, palette= pal.1[5:9])
## Between Social Sciences and Overlap
E(gr)[v1 %--% v3]$color <- hex.seq(E(gr)[v1 %--% v3]$width, palette= pal.1[5:9])
## Within Comp+Physics
E(gr)[v2 %--% v2]$color <- hex.seq(E(gr)[v2 %--% v2]$width, palette= pal.2[5:9])
## Between Comp+Physics and Overlap
E(gr)[v2 %--% v3]$color <- hex.seq(E(gr)[v2 %--% v3]$width, palette= pal.2[5:9])
## Within Overlap
E(gr)[v3 %--% v3]$color <- hex.seq(E(gr)[v3 %--% v3]$width, palette= pal.3[5:9])

# Apply transparency to all colors
E(gr)$color <- alpha.col(E(gr)$color, 0.4)
V(gr)$color <- alpha.col(V(gr)$color, 0.65)
V(gr)$frame.color <- alpha.col(V(gr)$frame.color, 0.9)

# Plot
png.def(paste("./Figures/union.colors.", year, ".final.png", sep=""), width= 2700, height= 2000, margins= c(0,0,0,2))
plot.gr(gr, layout= layout, vertex.size=v.size, vert.col= V(gr)$color, vert.frame.col= V(gr)$frame.color, edge.width= E(gr)$width, edge.color= E(gr)$color)

# Legend
legend(1, 0.1, legend= c(expression(bold("Author cited in:")), "Social Sciences", "Computer Science/Physics", "Both"), pch= c(NA, 21, 21, 21), col= c(NA, v.f.col), pt.bg= c(NA, v.col), bty="n", cex= 1.5, pt.cex=2,  y.intersp= 1.5)
dev.off()
