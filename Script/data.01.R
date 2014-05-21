# Preassign data.frame
all.data <- data.frame()

# Run the following code for each file name
for (i in 1:length(files)) {
  
  # Get file name
  file <- files[i]
  
  # Read the data in the corresponding file
  data <- read.delim(paste("./Data/", file, sep=""), row.names=NULL, stringsAsFactors=FALSE, quote = "", encoding="UTF-8")
  
  # Fix variable names
  names(data) <- names(data)[-1]
  
  # Remove last column
  data <- data[,-ncol(data)]
  
  # Rbind
  all.data <- rbind(all.data, data)
  
}

# Rename object
data <- all.data

## Split the data based on WC (Web of Science Category)
## -------------------------------------------------------------------------------------------------

# The data frame "discipline" classifies records into "computer science", "social science", "physics"
discipline <- as.data.frame(matrix(rep(NA, 2*nrow(data)), ncol=2))
names(discipline) <- c("social", "comp_physics")

# Records from social sciences
discipline$social[grep("Social Sciences|Sociology|Public, Environmental & Occupational Health|Anthropology|Management|Business|Economics|Education|Psychology|Gepgraphy|Political science|Behavioral sciences", data$WC, ignore.case=TRUE)] <- TRUE

# Records from computer science + physics
discipline$comp_physics[grep("computer science|informatic|physics", data$WC, ignore.case=TRUE)] <- TRUE

# NAs are FALSEs in the dataframe
for (i in 1:2) {
  discipline[[i]][is.na(discipline[[i]])] <- FALSE
}

# Split the data
social <- data[discipline$social,]
comp_physics <- data[discipline$comp_physics,]

