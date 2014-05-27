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
## Only if there are names with a leading blank...
if (length(all.cr$author[grep("^ ", all.cr$author)])>0) {
  ## ...remove leading blank
  all.cr$author[grep("^ ", all.cr$author)] <- sapply(all.cr$author[grep("^ ", all.cr$author)], function(y) gsub("^ ", "", y)) 
}

# Save as a backup
all.cr.first <- all.cr

# Get number of citations and only keep the top cited authors in edge list
## -----------------------------------------------------------------------------------------------

# Remove empty strings and "anonymous" from all.cr
all.cr <- all.cr[!(all.cr$author %in% c("", "anonymous")),]
# Correct mispelled names
for (i in 1:length(original)) {
  all.cr$author[grep(original[i], all.cr$author)] <- replacement[i]
}

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
# exclude <- ... # exclude is assigned in .main.R
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

# Get number of citations by author
citations <- as.data.frame(table(all.cr$author), stringsAsFactors=FALSE)

# Order by number of citations
citations <- citations[order(citations$Freq, decreasing=TRUE),]

