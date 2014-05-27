gr <- author.gr

# Describe number of cocitations
# describe(E(gr)$weight)
# png.def("temp.png")
# qplot(E(gr)$weight, geom="histogram", binwidth=1)
# dev.off()

# 75 percentile of number times 2 authors are co-cited
perc <- quantile(E(gr)$weight, prob= 0.9)

# Plot the network

# Only keep edges with weigth>=threshold
gr <- delete.edges(gr, E(gr)[weight< perc])

# Only keep main component
# comp <- clusters(gr)
# gr <- delete.vertices(gr, V(gr)[comp$membership!=which(comp$csize==max(comp$csize))])

# Remove isolates
comp <- clusters(gr)
gr <- delete.vertices(gr, V(gr)[comp$membership %in% which(comp$csize==1)])

# Network layout
set.seed(0514)
layout <- layout.kamada.kawai(gr)

# Rescale edge weigth to use as width
width <- rescale(E(gr)$weight, to= c(0.3, 8))

# Edge color
e.col <- hex.seq(width, palette= brewer.pal(9, "Blues")[5:9])
e.col <- alpha.col(e.col, 0.5)

# Vertex color
v.col <- brewer.pal(9, "Blues")[2]

# Vertex size
v.size <- rescale(degree(gr), to= c(2, 7))

# Labels: vertex names with 1st letter to upper
labels <- sapply(V(gr)$name, function(x) paste(toupper(substr(x, 1, 1)), substring(x,2), sep=""))

# Label size
l.cex <- rescale(degree(gr), to= c(0.4, 0.7))

# Girvan-Newman subgroups
# ebc <- edge.betweenness.community(gr, directed=FALSE)

# png.def("temp.png")
# plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= "grey40")
# dev.off()

# Plot
png.def(paste("./Figures/", discipline, ".", year, ".png", sep=""), width= 1000, height= 1000)
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color="darkblue", label.cex = l.cex)
dev.off()