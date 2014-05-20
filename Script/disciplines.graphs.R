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
### AUTHORS IN SOCIAL SCIENCES                                                                              ###
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

# Some authors have a blank (" ") as first character, this creates problems with the following
# string manipulations. Correct this.
all.cr$author[grep("^ ", all.cr$author)] <- sapply(all.cr$author[grep("^ ", all.cr$author)], function(y) gsub("^ ", "", y)) 

# Remove empty strings and "anonymous" from all.cr
all.cr <- all.cr[!(all.cr$author %in% c("", "anonymous")),]
# Also correct "granovetter" and other mispelled names
all.cr$author[grep("granovetms", all.cr$author)] <- "granovetter" 

# Save as a backup
all.cr.first <- all.cr

# Some authors appear as multiple authors because of their first names which are spelled in different ways. So in the
# original data I replace all strings containing those authors' last names with only the author's
# last name, so that the author is correctly counted only once.
# all.cr[grep("\\<wasserman\\>", all.cr[[1]]),]
# all.cr[grep("\\<granovetter\\>", all.cr[[1]]),]
# all.cr[grep("\\<burt\\>", all.cr[[1]]),]
# Get authors' last names from full names
all.cr$last.name <- sapply(all.cr$author, function(y) unlist(strsplit(y, " "))[1])

# Select the last names to keep. We are going to use last.names as the actual author names in the
# diagram. Therefore some last names need to be excluded because they are actually shared by
# different people, or are not last names but first word of other kind of string.
## Examples:
# all.cr[grep("^li\\>", all.cr[[1]]), 1]; all.cr[grep("^yu\\>", all.cr[[1]]), 1]; all.cr[grep("^wang\\>", all.cr[[1]]), 1]; all.cr[grep("^ma\\>", all.cr[[1]]), 1]; all.cr[grep("^van\\>", all.cr[[1]]), 1]; all.cr[grep("^lin\\>", all.cr[[1]]), 1]; all.cr[grep("^liu\\>", all.cr[[1]]), 1]; all.cr[grep("^zhang\\>", all.cr[[1]]), 1]; all.cr[grep("^chen\\>", all.cr[[1]]), 1]; all.cr[grep("^zhou\\>", all.cr[[1]]), 1]; all.cr[grep("^tang\\>", all.cr[[1]]), 1]; all.cr[grep("^huang\\>", all.cr[[1]]), 1]; all.cr[grep("^jung\\>", all.cr[[1]]), 1]; all.cr[grep("^de\\>", all.cr[[1]]), 1]; all.cr[grep("^jones\\>", all.cr[[1]]), 1]; all.cr[grep("^smith\\>", all.cr[[1]]), 1]; all.cr[grep("^lee\\>", all.cr[[1]]), 1]; all.cr[grep("^us\\>", all.cr[[1]]), 1]; all.cr[grep("^kim\\>", all.cr[[1]]), 1]; all.cr[grep("^national\\>", all.cr[[1]]), 1]; all.cr[grep("^yang\\>", all.cr[[1]]), 1]; all.cr[grep("^green\\>", all.cr[[1]]), 1]; all.cr[grep("^wu\\>", all.cr[[1]]), 1]; all.cr[grep("^chan\\>", all.cr[[1]]), 1]; all.cr[grep("^luo\\>", all.cr[[1]]), 1]; all.cr[grep("^chang\\>", all.cr[[1]]), 1]; all.cr[grep("^zhao\\>", all.cr[[1]]), 1]; all.cr[grep("^xu\\>", all.cr[[1]]), 1]; all.cr[grep("^cho\\>", all.cr[[1]]), 1]; all.cr[grep("^chung\\>", all.cr[[1]]), 1]
# Other last names may seem very common but in fact are not (it's the same person).
# Example:
# all.cr[grep("^oh\\>", all.cr[[1]]), 1]
# all.cr[grep("^peng\\>", all.cr[[1]]), 1]
# all.cr[grep("^yin\\>", all.cr[[1]]), 1]
# Set of last names to exclude from replacement "last name -> full name"
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong")
# Convert to grep pattern
exclude <- paste("^", exclude, "\\>", sep="", collapse="|")

# Use "last.name" as author
all.cr$author.2 <- all.cr$last.name
# ... except for very common last names
all.cr$author.2[grep(exclude, all.cr$author)] <- all.cr$author[grep(exclude, all.cr$author)]

# Remove and rename variables
all.cr$author <- all.cr$author.2
all.cr$author.2 <- NULL
all.cr$last.name <- NULL

# Get number of citations
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

# Add discipline identifier
citations$discipline <- 1

# Save social science all.cr and citations
all.cr_soc.sc <- all.cr
citations_soc.sc <- citations

####################################################################################################
### AUTHORS IN COMPUTER SCIENCES + PHYSICS                                                                             ###
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

# Some authors have a blank (" ") as first characters, this creates problems with the following
# string manipulations. Correct this.
all.cr$author[grep("^ ", all.cr$author)] <- sapply(all.cr$author[grep("^ ", all.cr$author)], function(y) gsub("^ ", "", y)) 

# Remove empty strings and "anonymous" from all.cr
all.cr <- all.cr[!(all.cr$author %in% c("", "anonymous")),]
# Also correct "granovetter" and other mispelled names
all.cr$author[grep("granovetms", all.cr$author)] <- "granovetter" 
all.cr$author[grep("de meo p", all.cr$author)] <- "demeo p" 
all.cr$author[grep("agrawal\\>", all.cr$author)] <- "agarwal"

# Save as a backup
all.cr.first <- all.cr

# Some authors appear as multiple authors because of their first names which are spelled in different ways. So in the
# original data I replace all strings containing those authors' last names with only the author's
# last name, so that the author is correctly counted only once.
# Get authors' last names from full names
all.cr$last.name <- sapply(all.cr$author, function(y) unlist(strsplit(y, " "))[1])

# Select the last names to keep. We are going to use last.names as the actual author names in the
# diagram. Therefore some last names need to be excluded because they are actually shared by
# different people, or are not last names but first word of other kind of string.
# Examples:
# all.cr[grep("^park\\>", all.cr[[1]]), 1]; all.cr[grep("^sun\\>", all.cr[[1]]), 1]; all.cr[grep("^zhu\\>", all.cr[[1]]), 1]; all.cr[grep("^yan\\>", all.cr[[1]]), 1]; all.cr[grep("^gao\\>", all.cr[[1]]), 1]; all.cr[grep("^jiang\\>", all.cr[[1]]), 1]; all.cr[grep("^jin\\>", all.cr[[1]]), 1]; all.cr[grep("^white\\>", all.cr[[1]]), 1]; all.cr[grep("^cheng\\>", all.cr[[1]]), 1]; all.cr[grep("^shi\\>", all.cr[[1]]), 1]; all.cr[grep("^han\\>", all.cr[[1]]), 1]; all.cr[grep("^shen\\>", all.cr[[1]]), 1]; all.cr[grep("^zeng\\>", all.cr[[1]]), 1]; all.cr[grep("^he\\>", all.cr[[1]]), 1]; all.cr[grep("^ding\\>", all.cr[[1]]), 1]; all.cr[grep("^hsu\\>", all.cr[[1]]), 1]; all.cr[grep("^guo\\>", all.cr[[1]]), 1]; all.cr[grep("^choi\\>", all.cr[[1]]), 1]; all.cr[grep("^tsai\\>", all.cr[[1]]), 1]; all.cr[grep("^xie\\>", all.cr[[1]]), 1]; all.cr[grep("^yuan\\>", all.cr[[1]]), 1]; all.cr[grep("^pan\\>", all.cr[[1]]), 1]; all.cr[grep("^tan\\>", all.cr[[1]]), 1]; all.cr[grep("^cai\\>", all.cr[[1]]), 1]; all.cr[grep("^fan\\>", all.cr[[1]]), 1]; all.cr[grep("^kang\\>", all.cr[[1]]), 1]; all.cr[grep("^lim\\>", all.cr[[1]]), 1]; all.cr[grep("^zou\\>", all.cr[[1]]), 1]; all.cr[grep("^su\\>", all.cr[[1]]), 1]; all.cr[grep("^du\\>", all.cr[[1]]), 1]; all.cr[grep("^wei\\>", all.cr[[1]]), 1]; all.cr[grep("^chi\\>", all.cr[[1]]), 1]; all.cr[grep("^das\\>", all.cr[[1]]), 1]; all.cr[grep("^hong\\>", all.cr[[1]]), 1]; all.cr[grep("^xiao\\>", all.cr[[1]]), 1]; all.cr[grep("^chu\\>", all.cr[[1]]), 1]; all.cr[grep("^ye\\>", all.cr[[1]]), 1]; all.cr[grep("^ng\\>", all.cr[[1]]), 1]; all.cr[grep("^wong\\>", all.cr[[1]]), 1]; all.cr[grep("^chiu\\>", all.cr[[1]]), 1]; all.cr[grep("^peng\\>", all.cr[[1]]), 1]; all.cr[grep("^ren\\>", all.cr[[1]]), 1]; all.cr[grep("^xiang\\>", all.cr[[1]]), 1]; all.cr[grep("^zhong\\>", all.cr[[1]]), 1]
# Set of last names to exclude from replacement "last name -> full name"
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong")
# Convert to grep pattern
exclude <- paste("^", exclude, "\\>", sep="", collapse="|")

# Use "last.name" as author
all.cr$author.2 <- all.cr$last.name
# ... except for very common last names
all.cr$author.2[grep(exclude, all.cr$author)] <- all.cr$author[grep(exclude, all.cr$author)]

# Remove and rename variables
all.cr$author <- all.cr$author.2
all.cr$author.2 <- NULL
all.cr$last.name <- NULL

# Get number of citations
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

# Discipline identifier
citations$discipline <- 2

# Save phys.comp all.cr
all.cr_phys.comp <- all.cr
citations_phys.comp <- citations


####################################################################################################
### ALL DISCIPLINES                                                                             ###
####################################################################################################

## Find top cited authors in both disciplines
## -----------------------------------------------------------------------------------------------

# Number of top authors
top.n <- 300

# Top cited authors in social sciences
citations_soc.sc <- citations_soc.sc[order(citations_soc.sc$Freq, decreasing=TRUE),]
top.1 <- citations_soc.sc[1:top.n,]

# Top cited authors in physics+comp science
citations_phys.comp <- citations_phys.comp[order(citations_phys.comp$Freq, decreasing=TRUE),]
top.2 <- citations_phys.comp[1:top.n,]

# Merge the 2 top authors
top <- merge(top.1, top.2, by= "Var1", all=TRUE)

# If Freq.x or Freq.y is NA that means 0 (author not cited)
top$Freq.x[is.na(top$Freq.x)] <- 0
top$Freq.y[is.na(top$Freq.y)] <- 0

# Overall number of citations in soc science + phys/comp science
top$Freq <- top$Freq.x + top$Freq.y

# Overallp top cited authors
top <-  top[order(top$Freq, decreasing=TRUE),]
top <- top[1:top.n,]

# Recode discipline.x and discipline.y
top$discipline.x[is.na(top$discipline.x)] <- 0
top$discipline.y[is.na(top$discipline.y)] <- 0
# Create variable indicating author discipline
top$discipline <- NA
## Overlapping (both disciplines)
top$discipline[top$discipline.x==1 & top$discipline.y==2] <- 3
## Soc sciences
top$discipline[top$discipline.x==1 & top$discipline.y==0] <- 1
## Physics/comp science
top$discipline[top$discipline.x==0 & top$discipline.y==2] <- 2

# table(top$discipline)


## Only keep top cited authors in two-mode edge list
## -----------------------------------------------------------------------------------------------

# Rbind the two two-mode edge lists
all.cr <- rbind(all.cr_soc.sc, all.cr_phys.comp)


# Keep only top authors in all.cr
top.cr <- all.cr[all.cr$author %in% top$Var1,]

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

# Display the graph
## -----------------------------------------------------------------------------------------------

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
# comp <- clusters(gr)
# gr <- delete.vertices(gr, V(gr)[comp$membership %in% which(comp$csize==1)])

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

png.def("./Figures/no.split.png", width= 1000, height= 1000)
plot.gr(gr, layout= layout, vertex.size=v.size, edge.width= width, edge.color= e.col, vert.col= v.col, vert.frame.col= NA, labels= labels, label.color="darkblue", label.cex = l.cex)
dev.off()




