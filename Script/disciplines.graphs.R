# Set directory
setwd("/Users/work/Documents/Dropbox/_Lavoro/2014-05-14_EUSN_Vizaward/")

# Libraries
library(igraph)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(Hmisc)

# Clear workspace
rm(list=ls())

# Load functions
source("./Script/functions.R")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graph.plots.rfn")
load("/Volumes/WorkDrive/Lavoro/_Lavori/R_functions/graphics.funs.rfn")

# Load data
load("./Data/records.13.rda")

####################################################################################################
### SOCIAL SCIENCES                                                                              ###
####################################################################################################


# Get relevant data.frame
data <- social.13

# Save column with article cited references
all.cr <- data$CR

# Remove elements of all.cr with no cited references
all.cr <- all.cr[sapply(all.cr, nchar)>0]

# Convert co-cited references to co-cited author names: lapply au.names() to co-cited references
# from all papers
all.cr <- lapply(all.cr, au.names)

# For each element of all.cr, turn into data.frame with author names and ID for the citing paper
all.cr <- lapply(seq_along(all.cr), function(i) data.frame(author= all.cr[[i]], paper= i, stringsAsFactors=FALSE))

# Rbind all data.frames in list into single data.frame
all.cr <- do.call(rbind, all.cr)

# Save as a backup
all.cr.first <- all.cr

# Get number of citations and only keep the top cited authors in edge list
## -----------------------------------------------------------------------------------------------

# Number of top authors to be kept in the final network
n.top <- 150

# Remove empty strings and "anonymous" from all.cr
all.cr <- all.cr[!(all.cr$author %in% c("", "anonymous")),]
# Also correct "granovetter"
all.cr$author[grep("granovetms", all.cr$author)] <- "granovetter" 

# Get number of citations by author
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

# Names of top 
top <- citations$Var1[1:n.top]

# Some authors in the top appear as multiple authors because of their first names which are spelled in different ways. So in the
# original data I replace all strings containing those authors' last names with only the author's
# last name, so that the author is correctly counted only once.
# citations[grep("\\<wasserman\\>", citations[[1]]),]
# citations[grep("\\<granovetter\\>", citations[[1]]),]
# citations[grep("\\<burt\\>", citations[[1]]),]

# Get only the last names of top 
last.names <- as.data.frame(sapply(top, function(y) unlist(strsplit(y, " "))[1]), stringsAsFactors=FALSE)
names(last.names) <- "last.name"

# Select the last names to keep. Only full names with a blank space are relevant (they include first
# names that create the mismatch between different spellings of same author).
names <-  last.names[[1]][grep(" ", rownames(last.names))]

# Select the last names to keep. We are going to use last.names as the actual author names in the
# diagram. Therefore some last names need to be excluded because they are actually shared by
# different people, or are not last names but first word of other kind of string.
names <- names[!(names %in% c("world", "centers", "lin", "cohen", "van"))]
# Remove NA
names <- names[!is.na(names)]

# Go back to all.cr. Take each (most cited) last name in names. For each, replace long author name with single last name in "names"
for (name in names) {
  all.cr$author[grep(paste("^", name, "\\>", sep=""), all.cr$author)] <- name  
}

# Get number of citations again
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

# Now the top does not have single authors spelled in differnt ways.

# Variable names
names(citations) <- c("author", "n.citations")

# Top cited authors
top <- citations$author[1:n.top]

# Keep only top authors in all.cr
top.cr <- all.cr[all.cr$author %in% top,]

# Because top.cr will be used to create an edge list, duplicates can now be removed. This means that
# when calculating co-citations between A and B by third papers, only being cited together by a
# unique paper matters for A and B; if A or B have been cited multiple times by that paper, that
# doesn't matter.
top.cr <- top.cr[!duplicated(top.cr),]

# How many papers and authors are there?
# length(unique(top.cr$paper))
# length(unique(top.cr$author))


# Get the graphs
## -------------------------------------------------------------------------------------------------

# Get the two-mode graph author->citing paper from 
bi.author.gr <- graph.data.frame(top.cr, directed=FALSE)

# This will give the "type" vertex attribute for the bi.gr graph: vertex names with letters are authors, otherwise they are papers.
V(bi.author.gr)$type <- grepl("[a-z]", V(bi.author.gr)$name)

# Get the one mode projection of authors by authors
author.gr <- bipartite.projection(bi.author.gr)[[1]]

# Save
social.bi <- bi.author.gr
social.one <- author.gr
save(list=ls(pattern=".bi|.one"), file= "./Data/graphs.13.rda")

# Display the graph
## -----------------------------------------------------------------------------------------------

gr <- social.one

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

png.def("./Figures/social.png", width= 1000, height= 1000)
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color="darkblue", label.cex = l.cex)
dev.off()


####################################################################################################
### COMPUTER SCIENCES + PHYSICS                                                                             ###
####################################################################################################


# Get relevant data.frame
data <- comp_physics.13

# Save column with article cited references
all.cr <- data$CR

# Remove elements of all.cr with no cited references
all.cr <- all.cr[sapply(all.cr, nchar)>0]

# Convert co-cited references to co-cited author names: lapply au.names() to co-cited references
# from all papers
all.cr <- lapply(all.cr, au.names)

# For each element of all.cr, turn into data.frame with author names and ID for the citing paper
all.cr <- lapply(seq_along(all.cr), function(i) data.frame(author= all.cr[[i]], paper= i, stringsAsFactors=FALSE))

# Rbind all data.frames in list into single data.frame
all.cr <- do.call(rbind, all.cr)

# Save as a backup
all.cr.first <- all.cr

# Get number of citations and only keep the top cited authors in edge list
## -----------------------------------------------------------------------------------------------

all.cr <- all.cr.first
# Number of top authors to be kept in the final network
n.top <- 150

# Remove empty strings and "anonymous" from all.cr
all.cr <- all.cr[!(all.cr$author %in% c("", "anonymous")),]
# Also correct "granovetter" and other mispelled names
all.cr$author[grep("granovetms", all.cr$author)] <- "granovetter" 
all.cr$author[grep("de meo p", all.cr$author)] <- "demeo p" 
all.cr$author[grep("agrawal\\>", all.cr$author)] <- "agarwal"

# Get number of citations by author
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

# Names of top 
top <- citations$Var1[1:n.top]

# Some authors in the top appear as multiple authors because of their first names which are spelled in different ways. So in the
# original data I replace all strings containing those authors' last names with only the author's
# last name, so that the author is correctly counted only once.
# citations[grep("\\<ma\\>", citations[[1]]),]

# Get only the last names of top 
last.names <- as.data.frame(sapply(top, function(y) unlist(strsplit(y, " "))[1]), stringsAsFactors=FALSE)
names(last.names) <- "last.name"

# Select the last names to keep. Only full names with a blank space are relevant (they include first
# names that create the mismatch between different spellings of same author).
names <-  last.names[[1]][grep(" ", rownames(last.names))]

# Select the last names to keep. We are going to use last.names as the actual author names in the
# diagram. Therefore some last names need to be excluded because they are actually shared by
# different people, or are not last names but first word of other kind of string.
## Examples:
# citations[grep("^li\\>", citations[[1]]),]
# citations[grep("^yu\\>", citations[[1]]),]
# citations[grep("^wang\\>", citations[[1]]),]
# citations[grep("^ma\\>", citations[[1]]),]
# citations[grep("^van\\>", citations[[1]]),]
# citations[grep("^lin\\>", citations[[1]]),]
# citations[grep("^liu\\>", citations[[1]]),]
# citations[grep("^zhang\\>", citations[[1]]),]
# citations[grep("^chen\\>", citations[[1]]),]
# citations[grep("^zhou\\>", citations[[1]]),]
# citations[grep("^tang\\>", citations[[1]]),]
# citations[grep("^huang\\>", citations[[1]]),]
# citations[grep("^jung\\>", citations[[1]]),]
## Remove very common last names
names <- names[!(names %in% c("li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen"))]
# Other last names may seem very common but in fact are not
# Example:
# citations[grep("^hui\\>", citations[[1]]),]
# citations[grep("^shi\\>", citations[[1]]),]
# citations[grep("^cha\\>", citations[[1]]),]
# citations[grep("^chin\\>", citations[[1]]),]
# Remove NA
names <- names[!is.na(names)]

# Go back to all.cr. Take each (most cited) last name in names. For each, replace long author name with single last name in "names"
for (name in names) {
  all.cr$author[grep(paste("^", name, "\\>", sep=""), all.cr$author)] <- name  
}

# Get number of citations again
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

# Now the top does not have single authors spelled in differnt ways.

# Variable names
names(citations) <- c("author", "n.citations")

# Top cited authors
top <- citations$author[1:n.top]

# Keep only top authors in all.cr
top.cr <- all.cr[all.cr$author %in% top,]

# Because top.cr will be used to create an edge list, duplicates can now be removed. This means that
# when calculating co-citations between A and B by third papers, only being cited together by a
# unique paper matters for A and B; if A or B have been cited multiple times by that paper, that
# doesn't matter.
top.cr <- top.cr[!duplicated(top.cr),]

# How many papers and authors are there?
# length(unique(top.cr$paper))
# length(unique(top.cr$author))


# Get the graphs
## -------------------------------------------------------------------------------------------------

# Get the two-mode graph author->citing paper from 
bi.author.gr <- graph.data.frame(top.cr, directed=FALSE)

# This will give the "type" vertex attribute for the bi.gr graph: vertex names with letters are authors, otherwise they are papers.
V(bi.author.gr)$type <- grepl("[a-z]", V(bi.author.gr)$name)

# Get the one mode projection of authors by authors
author.gr <- bipartite.projection(bi.author.gr)[[1]]

# Save
comp_physics.bi <- bi.author.gr
comp_physics.one <- author.gr
save(list=ls(pattern=".bi|.one"), file= "./Data/graphs.13.rda")

# Display the graph
## -----------------------------------------------------------------------------------------------

gr <- comp_physics.one

# Describe number of cocitations
# describe(E(gr)$weight)
# png.def("temp.png")
# qplot(E(gr)$weight, geom="histogram", binwidth=1)
# dev.off()

# 75 percentile of number times 2 authors are co-cited
perc <- quantile(E(gr)$weight, prob= 0.9)

# Plot the network

# Only keep edges with weigth>=threshold
gr <- delete.edges(gr, E(gr)[weight<perc])

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

png.def("./Figures/comp_physics.png", width= 1000, height= 1000)
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color="darkblue", label.cex = l.cex)
dev.off()

