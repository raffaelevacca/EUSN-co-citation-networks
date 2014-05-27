# Load functions
source("./Script/01_functions.R")

# Load data
load("./Data/records.rda")

####################################################################################################
### 2011-2013                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "13"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong", "anderson")


## =================================================================================================
## Get authors and number of citations
## =================================================================================================

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.social <- all.cr

# Save data frame of cited authors and number of citations
citations.social <- citations


### COMPUTER SCIENCES + PHYSICS                                                                             ## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.phys <- all.cr

# Save data frame of cited authors and number of citations
citations.phys <- citations

## =================================================================================================
## Get graphs for single disciplines
## =================================================================================================

# Source script
source("./Script/06_discipline.graph.02.R")

## =================================================================================================
## Display graphs for single disciplines
## =================================================================================================

# source("./Script/07_plot.disc.graph.R")

####################################################################################################
### 2008-2010                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "10"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong", "anderson")

## =================================================================================================
## Get authors and number of citations
## =================================================================================================

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.social <- all.cr

# Save data frame of cited authors and number of citations
citations.social <- citations


### COMPUTER SCIENCES + PHYSICS                                                                             ## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.phys <- all.cr

# Save data frame of cited authors and number of citations
citations.phys <- citations

## =================================================================================================
## Get graphs for single disciplines
## =================================================================================================

# Source script
source("./Script/06_discipline.graph.02.R")

####################################################################################################
### 2005-2007                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "07"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong", "anderson")

## =================================================================================================
## Get authors and number of citations
## =================================================================================================

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.social <- all.cr

# Save data frame of cited authors and number of citations
citations.social <- citations


### COMPUTER SCIENCES + PHYSICS                                                                             ## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.phys <- all.cr

# Save data frame of cited authors and number of citations
citations.phys <- citations

## =================================================================================================
## Get graphs for single disciplines
## =================================================================================================

# Source script
source("./Script/06_discipline.graph.02.R")

####################################################################################################
### 2002-2004                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "04"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong", "anderson")

## =================================================================================================
## Get authors and number of citations
## =================================================================================================

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.social <- all.cr

# Save data frame of cited authors and number of citations
citations.social <- citations


### COMPUTER SCIENCES + PHYSICS                                                                             ## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.phys <- all.cr

# Save data frame of cited authors and number of citations
citations.phys <- citations

## =================================================================================================
## Get graphs for single disciplines
## =================================================================================================

# Source script
source("./Script/06_discipline.graph.02.R")


####################################################################################################
### 1999-2001                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "01"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong", "anderson")

## =================================================================================================
## Get authors and number of citations
## =================================================================================================

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.social <- all.cr

# Save data frame of cited authors and number of citations
citations.social <- citations


### COMPUTER SCIENCES + PHYSICS                                                                             ## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.phys <- all.cr

# Save data frame of cited authors and number of citations
citations.phys <- citations

## =================================================================================================
## Get graphs for single disciplines
## =================================================================================================

# Source script
source("./Script/06_discipline.graph.02.R")


####################################################################################################
### 1996-1998                                                                                    ###
####################################################################################################

# Set the timespan identifier
year <- "98"

# Set the strings to be excluded from last name/full name replacement
exclude <- c("world", "centers", "lin", "cohen", "van", "li", "yu", "lu", "wang", "ma", "van", "lin", "liu", "zhang", "chen", "zhou", "tang", "huang", "jung", "fu", "hu", "cohen", "de", "smith", "lee", "jones", "us", "kim", "national", "yang", "green", "wu", "von", "american", "chan", "luo", "department", "chang", "zhao", "xu", "united", "cho", "chung", "park", "sun", "zhu", "yan", "gao", "jiang", "jin", "white", "cheng", "shi", "han", "shen", "zeng", "ding", "he", "hsu", "guo", "choi", "tsai", "cao", "xie", "yuan", "pan", "tan", "cai", "fan", "kang", "lim", "zeng", "zou", "du", "su", "wei", "chi", "das", "hong", "xiao", "chu", "ye", "ng", "wong", "chiu", "peng", "ren", "xiang", "zhong", "anderson")

## =================================================================================================
## Get authors and number of citations
## =================================================================================================

## SOCIAL SCIENCES 
## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("social.", year, sep=""))

# Set discipline identifier
discipline <- "social"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms")
replacement <- c("granovetter")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.social <- all.cr

# Save data frame of cited authors and number of citations
citations.social <- citations


### COMPUTER SCIENCES + PHYSICS                                                                             ## -----------------------------------------------------------------------------------------------

# Get relevant data.frame
data <- get(paste("comp_physics.", year, sep=""))

# Set discipline identifier
discipline <- "comp_physics"

# Set string replacements to be done to correct mispelled author names
original <- c("granovetms", "agrawal\\>")
replacement <- c("granovetter", "agarwal")

# Source script
source("./Script/05_discipline.graph.01.R")

# Save data frame of cited authors by citing paper (2-mode edge list)
all.cr.phys <- all.cr

# Save data frame of cited authors and number of citations
citations.phys <- citations

## =================================================================================================
## Get graphs for single disciplines
## =================================================================================================

# Source script
source("./Script/06_discipline.graph.02.R")

# Save
save(list= ls(pattern="\\.one\\."), file= "./Data/graphs.rda")
