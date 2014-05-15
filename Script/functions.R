# Convert co-cited references to co-cited author names
au.names <- function(x) {
  # This function takes as argument a character vector where all co-cited references are together,
  # separated by ";". It returns a character vector where each element is the (first) author name of
  # a co-cited reference.
  
  # Split x on character ";", and unlist (strsplit() returns a list by default). This results in a character vector: each element is a reference.
  x <- unlist(strsplit(x, ";"))
  
  # Sometimes strings like "3M", "2-M" or "2-I" appear as a citation. Remove those.
  x <- x[!grepl("[0-9]-*[MI]", x)]
  
  # From each element of the character vector (each reference), extract the author name (i.e. the part of the string before ",")
  x <- sapply(x, function(y) unlist(strsplit(y, ","))[1])
  # Delete rownames (they just keep the original vector element)
  names(x) <- NULL
  
  # Keep just letters and blank spaces in the strings 
  x <- sapply(x, function(y) gsub("[^a-zA-Z ]", "", y))
  
  # Also remove the FIRST blank space in the strings
  x <- sapply(x, function(y) gsub("^ ", "", y)) 
  
  # All to lower letters
  x <- tolower(x)
  
  # Only keep the first string (last name)
  # x <- sapply(x, function(y) unlist(strsplit(y, " "))[1])
  
  # After the cleaning above, some names are the same (in fact they should be the same author): so just keep uniques
  # x <- unique(x)
  
  #   # Mixed Case Capitalizing
  #   x <- sapply(x, capwords)
  #   
  #   # Delete vector names
  #   names(x) <- NULL
  
  # Sort in alphabetic order
  x <- sort(x)
  
  # Return
  return(x)
}

# Gets a "reordered" edge list with edge weights from a graph. 
ord.el <- function(gr, w.name= "weight", data.names= c("from", "to", "weight")) { 
  # "ordered" means that in the edge list, the "from" vertex name is always lower than the "to"
  # vertex name. This is useful to find duplicated edges in undirected graphs.
  #
  # Arguments:
  # -- gr: (igraph) the graph
  # -- w.name: (character) the name of the edge weight attribute
  # -- data.names: (character) the name of columns in the resulting data.frame
  #
  # Return value:
  # A data.frame with the edge list plus edge weights, and ordered (vertex names in the 1st column
  # always lower than vertex names in the second column).
  
  # Argument validity
  stopifnot(is.igraph(gr))
  
  # Get edge list using vertex names V(gr)$name as data.frame.
  el <- cbind(as.data.frame(get.edgelist(gr), stringsAsFactors=FALSE), get.edge.attribute(gr, w.name))
  
  # Rename variables
  names(el) <- data.names
  
  # Which data.frame record has "from" larger than "to"
  swap <- which(el[,"from"] > el[,"to"])
  
  # If there are any of these records...
  if (length(swap) > 0) { 
    # Swap second with first column in those.
    el[swap, 1:2] <- cbind(el[swap,2], el[swap,1]) 
  }
  
  # Return edge list
  return(el)
}


# Generates the union of gr1 and gr2 preserving edge weights from both graphs.
union.graph <- function(gr1, gr2, w.names= c("weight", "weight"), output.w.names= c("weight.sn", "weight.ns"), vert.attr.names= c("sn.vertex", "ns.vertex")) {
  # This function generates the union of gr1 and gr2 and preserves edge weights from both graphs.
  # Arguments:
  # -- gr1, gr2: (igraph) the 2 graphs to join
  # -- w.names: (character) the names of the edge attributes with edge weights in gr1 and gr2 respectively
  # -- output.w.names: (character) the names to be given in the output graph to edge attributes with
  # the weights from gr1 and gr2 respectively
  # -- vert.attr.names: (character) the names to be given to logical veretx attribute indicating
  # whether a veretx was in gr1 and gr2 respectively.
  #
  # Requires:
  # ord.el()
  # vert.attr.wrap()
  # 
  #
  # Rerturn value:
  # a graph
  
  stopifnot(is.igraph(gr1), is.igraph(gr2), !is.directed(gr1), !is.directed(gr2), is.named(gr1), is.named(gr2), w.names[1] %in% list.edge.attributes(gr1), w.names[2] %in% list.edge.attributes(gr2), is.named(gr1), is.named(gr2))
  
  # Get gr1's and gr2's edge lists, including edge weights, using ord.el()
  el1 <- ord.el(gr1, w.name= w.names[1], data.names= c("from", "to", output.w.names[1]))
  el2 <- ord.el(gr2, w.name= w.names[2], data.names= c("from", "to", output.w.names[2]))
  
  # By merging the 2 edge lists with all=TRUE we get the graph union edge list.
  el3 <- merge(el1, el2, all=TRUE)
  
  # From the united edge list, we get the union graph
  gr <- graph.data.frame(el3, directed=FALSE)
  
  # Set grant and pub edge weights as 0 if they are NA (NA means no edge exists, so edge weight==0)
  ## Get the index of NAs for the 2 edge weights
  index1 <- which(is.na(get.edge.attribute(gr, output.w.names[1])))
  index2 <- which(is.na(get.edge.attribute(gr, output.w.names[2])))
  # Set NAs to 0s
  gr <- set.edge.attribute(gr, name= output.w.names[1], index= index1, value= 0)
  gr <- set.edge.attribute(gr, name= output.w.names[2], index= index2, value= 0)
  
  # Get vertex attributes indicating where the vertex is from between gr1 and gr2 (may be both)
  ## Data frame of nodes from gr1
  gr1.v <- data.frame(name= V(gr1)$name, gr1= TRUE)
  ## Data frame of nodes from gr2
  gr2.v <- data.frame(name= V(gr2)$name, gr2= TRUE)
  ## Merge
  all <- merge(gr1.v, gr2.v, all=TRUE)
  ## When gr1 and gr2 are NA, that means they are FALSE
  all$gr1[is.na(all$gr1)] <- FALSE
  all$gr2[is.na(all$gr2)] <- FALSE
  ## Bring $gr1 and $gr2 into gr as vertex attributes, with names vert.attr.names
  gr <- vert.attr.multi(all, gr, dataID= "name", graphID= "name", attributes= c("gr1", "gr2"), attr.names= vert.attr.names)
  
  # Return
  return(gr)
  
}


## -----------------------------------------------------------------------------------------------
# Function: vert.attr()
## -----------------------------------------------------------------------------------------------

# Extracts a node variable from a data.frame and returns it as a vector ready to be set as vertex
# attribute in a graph. May aggregate the variable by node before returning.
vert.attr <- function(data, attribute, graph, dataID, graphID= "name") {
  # This function extracts variables at the author level from 'data' and set them as 
  # vertex.attributes in 'graph', a network of authors. The function aggregates the variable by
  # author using the aggregating function FUN. The function needs a 'graph' igraph object of authors
  # in which the UFIDs are recorded as a  attribute, and a 'data' data.frame in which the
  # same UFIDs are associated to a variable (attribute).
  #
  # Arguments: 
  # -- data: the data.frame containing the author attribute variable to be imported in the graph (it 
  # must also include a authorID variable).
  # -- graph: researchers graph where to import the attribute.
  # -- dataID: (character) the variable in data giving the ID of nodes in the graph (authors)
  # -- graphID: (character) the vertex attribute of graph that gives node IDs (i.e. IDs of authors)
  # which are the same as dataID.
  # -- attribute: a variable name in 'data' (as character)
  # -- FUN: a function name given as character. The argument is ignored if "data" has one record for 
  # each dataID value, i.e. does not need aggregation by dataID. If NA or missing, attribute is
  # aggregated by just taking its 1st non-NA value for each value of dataID.
  #
  # The function takes data and aggregates FUN(attribute) by dataID in 'data'. It returns the result 
  # of the aggregation in the order of dataID given by V(graph)$name. 
  # NOTE that if attribute is categorical (e.g. College), the argument FUN is ignored
  # and vert.attr() is just going to take the category of the variable for authorID (if there are more 
  # than 1 category, e.g. more than 1 Academic Units, for the same authorID, vert.attr just takes the 
  # 1st category associated to authorID in 'data').
  #
  # Return value
  # A vector which contains the attribute 'attribute' from data, in the order given by the order of authorID in the graph 'graph'.
  
  # Argument validity
  stopifnot(is.data.frame(data), is.character(attribute), is.igraph(graph), dataID %in% names(data), attribute %in% names(data), graphID %in% list.vertex.attributes(graph))
  
  # Crop only relevant variables: dataID and attribute
  data <- data[,c(dataID, attribute)]
  
  # Clean data from duplicates on c(dataID, attribute), because the function needs to evaluate
  # whether attribute needs aggregation or not.
  data <- data[!duplicated(data),]
  
  # Get the relevant objects:
  ## authorID from 'data'
  dataID <- data[[dataID]] 
  ## 'attribute' from 'data'
  attr <- data[[attribute]]
  
  # Put into data.frame
  data <- data.frame(dataID= dataID, attr= attr)
  
  ## authorID in the order of V(graph)$name, from 'graph'
  vertex.name <- get.vertex.attribute(graph, name= graphID)
  
  # Put vertex.name in a data.frame with an additional variable giving the record order
  vertex.name <- data.frame(vertex.name= vertex.name, order= 1:length(vertex.name))
  
  # Merge data and vertex names
  # all.y=TRUE means that vertexes are kept in the resulting data.frame ONLY if they are in the
  # graph (vertex.name).
  data <- merge(data, vertex.name,
                by.x= 'dataID', by.y= 'vertex.name',
                all.y=TRUE)
  
  # data contains ONLY and ALL the vertexes in the graph. Order them by the variable "order" (which 
  # gives the original order of ufids in vertex.name)
  data <- data[order(data$order),]
  
  # Get only the attribute vector from the data.frame
  attr <- data[, 'attr']
  
  # Return attr
  return(attr)
}


# Uses vert.attr() to get multiple variables from a data.frame and set them as vertex attributes
# in a graph.
vert.attr.multi <- function(data, graph, dataID, graphID= "name", attributes, attr.names= attributes) {
  # This function is a wrapper for vert.attr(): it repeatedly applies vert.attr() to a set of
  # attributes.
  # The problem that this function solves is that authors (nodes) in the graph and authors (records)
  # in the data.frame data are not necessarily in the same sort order.
  #
  # Arguments: 
  # -- data: the data.frame containing the vertex attribute variables to be imported in the graph (it 
  # must also include a authorID variable)
  # -- graph: researchers graph where to import the attribute
  # -- authorID: (character) the variable in data giving the ID of vertexes.
  # -- attributes: (character) variable names in 'data'.
  # -- FUN: (character) function names given as character. The FUN argument to vert.attr(). If non
  # specified, it means FUN=NA for all attributes.
  # -- attr.names: (character) names to use for the attributes in the graph.
  #
  # Requires:
  # vert.attr()
  #
  # Return value
  # an igraph object.
  
  # Argument validity.
  stopifnot(is.data.frame(data), is.character(attributes), is.igraph(graph))
  
  # For each element of attributes
  for (i in 1:length(attributes)) {
    # Take the corresponding element of attributes, and apply vert.attr() on that element of attributes and that 
    # element of FUN. Set the result as vertex.attribute in 'graph', whose name is the same as 
    # the element of attributes.
    graph <- set.vertex.attribute(graph= graph,
                                  name= attr.names[i],
                                  value= vert.attr(graph= graph, data= data, dataID= dataID, graphID= graphID, attribute= attributes[i]))
  }
  
  # Return the graph
  return(graph)
  
}



# Save functions
# save.image("./Data/functions.rfn")