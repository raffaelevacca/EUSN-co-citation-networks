# Find proportion of authors citedin the social sciences vs comp science/physics.
## Set discipline identifier
citations.social$social <- TRUE
citations.phys$phys <- TRUE
## Merge
citations.all <- merge(citations.social, citations.phys, by="Var1", all=TRUE)
## The identifier are FALSE if NAs
citations.all$social[is.na(citations.all$social)] <- FALSE
citations.all$phys[is.na(citations.all$phys)] <- FALSE
## Table
tab <- table(citations.all$social, citations.all$phys)
## Set number of top authors to keep in each discipline as proportion of authors cited in that discipline
n.top.phys <- round(300*(sum(tab[,2])/sum(tab)))
n.top.social <- round(300*(sum(tab[2,])/sum(tab)))

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Names of top 
top <- citations.social$Var1[1:n.top.social]

# Keep only top authors in all.cr
top.cr <- all.cr.social[all.cr.social$author %in% top,]

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
assign(paste("social.bi.", year, sep=""), bi.author.gr)
assign(paste("social.one.", year, sep=""), author.gr)


## COMPUTER SCIENCES + PHYSICS 
## -----------------------------------------------------------------------------------------------

# Names of top  
top <- citations.phys$Var1[1:n.top.phys]

# Keep only top authors in all.cr
top.cr <- all.cr.phys[all.cr.phys$author %in% top,]

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
assign(paste("comp_physics.bi.", year, sep=""), bi.author.gr)
assign(paste("comp_physics.one.", year, sep=""), author.gr)


