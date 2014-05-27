## String manipulation
## =================================================================================================

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
ord.el <- function(gr, w.name= "weight", data.names= c("from", "to", "weight"), numeric.attribute= TRUE) {
  # This function is to be used by union.graph.weight(). 
  # "ordered" means that in the edge list, the "from" vertex name is always lower than the "to"
  # vertex name. This is useful to find duplicated edges in undirected graphs.
  #
  # Arguments:
  # -- gr: (igraph or network) the graph
  # -- w.name: (character) the name of the edge weight attribute
  # -- data.names: (character) the name of columns in the resulting data.frame
  # -- numeric.attribute: (logical) is the edge attribute numeric (TRUE) or not?
  #
  # Return value:
  # A data.frame with the edge list plus edge weights, and ordered (vertex names in the 1st column
  # always lower than vertex names in the second column).
  
  # Argument validity
  stopifnot(is.igraph(gr) | is.network(gr))
  
  if (is.igraph(gr)) {
    # Check that gr has relevant edge attribute
    stopifnot(w.name %in% igraph::list.edge.attributes(gr))
    
    # Get edge list using vertex names V(gr)$name as data.frame.
    el <- as.data.frame(cbind(igraph::get.edgelist(gr), igraph::get.edge.attribute(gr, w.name)), stringsAsFactors=FALSE)
    
  } else {
    # Check that gr has relevant edge attribute
    stopifnot(w.name %in% network::list.edge.attributes(gr))
    
    # Get edgelist
    el <- network::as.matrix.network(net, matrix.type="edgelist", attrname= w.name)
    
    # Get vertex names in the order used in edge list
    vnames <- attr(el, "vnames")
    
    # Get edge list with vertex names instead of vertex ids
    el[, 1] <- vnames[as.numeric(el[, 1])]
    el[, 2] <- vnames[as.numeric(el[, 2])]
    
    # To data.frame
    el <- as.data.frame(el, stringsAsFactors=FALSE)
    
    # Convert weight to numeric
    el[[3]] <- as.numeric(el[[3]])
  }
  
  # Rename variables
  names(el) <- data.names
  
  # If the edge attribute is numeric, convert to numeric
  if(numeric.attribute) {
    el[[data.names[3]]] <- as.numeric(el[[data.names[3]]])
  }
  
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


## Network manipulation
## =================================================================================================

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
  
  stopifnot(is.igraph(gr1), is.igraph(gr2), !igraph::is.directed(gr1), !igraph::is.directed(gr2), is.named(gr1), is.named(gr2), w.names[1] %in% igraph::list.edge.attributes(gr1), w.names[2] %in% igraph::list.edge.attributes(gr2), is.named(gr1), is.named(gr2))
  
  # Get gr1's and gr2's edge lists, including edge weights, using ord.el()
  el1 <- ord.el(gr1, w.name= w.names[1], data.names= c("from", "to", output.w.names[1]))
  el2 <- ord.el(gr2, w.name= w.names[2], data.names= c("from", "to", output.w.names[2]))
  
  # By merging the 2 edge lists with all=TRUE we get the graph union edge list.
  el3 <- merge(el1, el2, all=TRUE)
  
  # From the united edge list, we get the union graph
  gr <- graph.data.frame(el3, directed=FALSE)
  
  # Set grant and pub edge weights as 0 if they are NA (NA means no edge exists, so edge weight==0)
  ## Get the index of NAs for the 2 edge weights
  index1 <- which(is.na(igraph::get.edge.attribute(gr, output.w.names[1])))
  index2 <- which(is.na(igraph::get.edge.attribute(gr, output.w.names[2])))
  # Set NAs to 0s
  gr <- igraph::set.edge.attribute(gr, name= output.w.names[1], index= index1, value= 0)
  gr <- igraph::set.edge.attribute(gr, name= output.w.names[2], index= index2, value= 0)
  
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
  stopifnot(is.data.frame(data), is.character(attribute), is.igraph(graph), dataID %in% names(data), attribute %in% names(data), graphID %in% igraph::list.vertex.attributes(graph))
  
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
  vertex.name <- igraph::get.vertex.attribute(graph, name= graphID)
  
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
    graph <- igraph::set.vertex.attribute(graph= graph,
                                  name= attr.names[i],
                                  value= vert.attr(graph= graph, data= data, dataID= dataID, graphID= graphID, attribute= attributes[i]))
  }
  
  # Return the graph
  return(graph)
  
}

# Plot igraph object
plot.gr <- function(gr, layout= "kk", vert.col, col.attr, cat.cols, palette= "Set1", NAcol= def.col, def.col= 'grey40', vert.frame.col= 'black', vert.shape= "circle", vert.size= 1.3, scale.vert.size= TRUE, vert.size.range= c(1,6), edge.width= 0.3, edge.col= 'grey60', labels= NA, label.cex= 0.5, label.family= 'sans', label.font= 2, label.dist= 0, label.degree= -pi/2, label.color= 'blue', set.par= FALSE, ...) {
  # Arguments:
  # -- gr (igraph): the graph to be plotted
  # -- layout:"layout" argument in set.layout()
  # -- vert.col, col.attr, cat.cols, palette, NAcol, def.col: arguments for set.vert.col()
  # -- labels: argument for set.vert.label()
  # -- vert.size, scale.vert.size, vert.size.range: arguments for set.vert.size().
  # -- vert.shape, vert.frame.col: vectors to be set as shapes and frame colors for V(gr) (recycled
  # if necessary)
  # -- edge.col, edge.width: vectors to be set as colors and widths for E(gr) (recycled if necessary)
  # -- set.par: (logical) if TRUE, function arguments are used to set graph parameters rather than to plot the graph.
  # -- ... further arguments to be passed to plot.igraph()
  #
  # Requires:
  # library plotrix.
  # set.vert.col()
  # set.layout()
  # set.vert.size()
  # set.vert.lab()
  #
  # Return value:
  # If set.par== FALSE, a plot.
  # If set.par==TRUE, a graph.
  ## -----------------------------------------------------------------------------------------------
  
  ## Arguments validity
  ## -----------------------------------------------------------------------------------------------
  ## Graph
  stopifnot(is.igraph(gr))
  
  # Save the function call to get the call arguments
  m <- match.call()
  
  ## Set graph layout using set.layout()
  ## -----------------------------------------------------------------------------------------------
  
  # If layout argument is SPECIFIED, OR if graph$layout does not exist, use set.layout() to set graph layout
  if (!missing(layout) | !("layout" %in% list.graph.attributes(gr))) {
    gr <- set.layout(gr, layout)
  }
  # The code above is NOT executed if layout is NOT specified (missing(layout)) AND gr$layout exists:
  # in this case the existing gr$layout is used.
  
  
  ## Set vertex color using set.vert.col()
  ## -----------------------------------------------------------------------------------------------
  gr <- set.vert.col(gr, vert.col, col.attr, cat.cols, palette, NAcol, def.col)
  
  
  ## Set node size using set.vert.size()
  ## -----------------------------------------------------------------------------------------------
  gr <- set.vert.size(gr, vert.size, scale.vert.size, vert.size.range)
  
  
  # Set vertex labels using set.vert.lab()
  ## -----------------------------------------------------------------------------------------------
  gr <- set.vert.lab(gr, labels, label.cex, label.family, label.font, label.dist, label.degree, label.color)
  
  
  # Other graphical parameters
  ## -----------------------------------------------------------------------------------------------
  
  # Set $shape
  V(gr)$shape <- vert.shape
  
  # Set $frame.color
  V(gr)$frame.color <- vert.frame.col
  
  # Set edge.width and edge.color from arguments
  E(gr)$width <- edge.width
  E(gr)$color <- edge.col
  
  # Return value
  # If set.par==TRUE, return the graph
  if (set.par) {  
    # Return the graph
    return(gr)
  } else {
    # Else, if set.par==FALSE
    # Use return the plot, with possible further arguments
    plot.igraph(gr, ...)
  }
}


# Function to set graph layout according to one single parameter "layout"
set.layout <- function(gr, layout) {
  # Arguments:
  # -- gr (igraph): the graph to be plotted
  # -- layout: it can be character, a string indicating the layout function to be used for the plot
  # (see possible values); function, a function to be used to compute layout; numeric matrix, a
  # matrix containing layout coordinates.
  #
  # Return value
  # a graph
  
  # Argument validity
  stopifnot(any(is.character(layout), is.function(layout), is.matrix(layout)))
  
  ## If argument 'layout' is character...
  ## (NOTE the sequence of IF, instead of & operator, because if 'layout' is NOT character, the 2nd
  ## condition must NOT be evaluated)
  if (is.character(layout)) {
    if (!(layout %in% c('se', 'fr', 'kk', 'lgl', 'gopt', 'drl'))) stop("If 'layout' is not a coordinate matrix nor a layout function, then it must be a character in 'se', fr', 'kk', 'lgl', 'gopt', 'drl', to choose a layout function")
  }  
  
  # If 'layout' is a character referring to a layout.function name, switch() sets the value of 
  # layout to the proper function, according to which of the following strings m$layout is equal to.
  # NOTE that switch() also converts 'layout' from a character string (as in the
  # argument) to an actual function object.
  # The following switch() is applied only if argument layout is character. If it's already a
  # function, it stays unchanged and is passed on to set graph layout.
  if (is.character(layout)) {
    layout <- switch(layout,
                     se= layout.spring,
                     fr= layout.fruchterman.reingold,
                     kk= layout.kamada.kawai,
                     lgl= layout.lgl,
                     gopt= layout.graphopt,
                     drl= layout.drl)
  }
  # At this point layout is a function (if it was character or function in the call), or a matrix 
  # (if it was matrix in the call).
  # If it's a function, calculate and set nodes' layout according to that function
  if (is.function(layout))  {
    gr$layout <- layout(gr)
  }
  # If it's a matrix, just use that matrix as gr$layout
  if (is.matrix(layout)) {
    gr$layout <- layout
  }
  
  # Return
  return(gr)
}


# Function to set vertex colors 
set.vert.col <- function(gr, vert.col, col.attr, cat.cols, palette= "Set1", NAcol= def.col, def.col= 'grey40') {
  # Arguments:
  # -- vert.col: (character) a vector of colors to be used as is for vertex colors.
  # -- col.attr: (character) name of the categorical vertex attribute to be used for coloring nodes.
  # -- cat.cols: a list of type list(category=color), used to color nodes according to categories of
  # col.attr.
  # -- palette: (character) a palette of colors (color names or hex codes) for unique categories of col.attr. OR, the name of the RColorBrewer palette to be used to color categories of col.attr.
  # -- NAcol: color to be used for vertexes with NA value on col.attr (this is ignored if col.attr
  # is NOT specified).
  # -- def.col: (character) default color to be used for nodes with col.attr==NA or == values not
  # included in 'cat.cols' argument.
  #
  # Return value
  # a graph.  
  
  # If vertex.color is specified, use it as vector of colors
  if (!missing(vert.col)) {
    # It must be character
    stopifnot(is.character(vert.col))
    # Warning if it's not the right length
    if (length(vert.col)>1 & length(vert.col)<vcount(gr)) {
      warning('"vertex.color" is a vector of colors but it is shorter than vcount(gr)')
    }
    # Set it
    V(gr)$color <- vert.col
    
  } else {
    # Otherwise (vertex.color NOT specified)
    
    # Set default (residual) color from argument 'def.col'
    V(gr)$color <- rep(def.col, vcount(gr))
    
    # The following code ONLY applies if 'col.attr' is non-missing
    if (!missing(col.attr)) {
      
      # Check that col.attr is specified as character naming a vertex attribute.
      stopifnot(is.character(col.attr), col.attr %in% list.vertex.attributes(gr))
      
      # If "cat.cols" is NOT specified: create an appropriate cat.cols list
      if (missing(cat.cols)) {
        
        # Get the unique values in col.attr
        uni.vals <- unique(get.vertex.attribute(gr, col.attr))
        # ... except NA
        uni.vals <- uni.vals[!is.na(uni.vals)]
        
        # Get the number of those unique values
        n.col <- length(uni.vals)
        
        # If "palette" has length==1, it must be the name of a RColorBrewer palette
        if (length(palette==1)) {
          if (!(palette %in% rownames(brewer.pal.info))) {
            stop('If "palette" has length 1 it must be the name of a RColorBrewer palette.')
          }
          # Get that many colors from RColowBrewer.
          ## Number of colors in the RColorBrewer palette
          nc <- brewer.pal.info[palette,"maxcolors"]
          ## Get as many colors as n.col from the palette
          cat.cols <- brewer.pal(nc, palette)[1:n.col]
        } else {
          # If palette has length > 1, use it as color palette
          cat.cols <- palette
        }
        
        # If n.col is higher than the maximum number of colors in the palette, colors will have 
        # shorter length than n.col. In this case, recycle with a warning.
        if (length(cat.cols) < n.col) {
          cat.cols <- rep(cat.cols, length.out= n.col)
          warning("Colors are less than unique values in col.attr: they were recycled.")
        }
        
        # Turn cat.cols into a list
        cat.cols <- as.list(cat.cols)
        # Use the unique values as list names
        names(cat.cols) <- uni.vals
      }
      
      # For each element of list 'cat.cols'
      for (i in 1:length(cat.cols)) {
        # Find category to be colored in names(cat.cols)
        category <- names(cat.cols)[i]
        # Subset positions of vertices in that category
        index <- !is.na(get.vertex.attribute(gr, col.attr)) & get.vertex.attribute(gr, col.attr)==category
        # Set color for those vertexes to corresponding element of 'cat.cols'
        V(gr)[index]$color <- cat.cols[[i]]
        
        # But if index has length 0 (so no color has been set), it means that no value equal to 'category' exists for col.attr in data: WARNING.
        if (length(index)==0) warning(paste('While setting colors: Value "', category, '" does not exist for variable "', col.attr, '" in data', sep=''))
        # Also, if the cat.cols list contains a non-character vector, that's not a valid color: ERROR.
        if (!is.character(cat.cols[[i]])) stop(paste("While setting colors: Value '", cat.cols[[i]], "' in 'cat.cols' argument is not a valid color", sep=''))
      } 
      
      # For vertexes with NA value on col.attr, set color to NAcol
      V(gr)[is.na(get.vertex.attribute(gr, col.attr))]$color <- NAcol
    }
  }
  
  # Return
  return(gr)
}

# Function to set vertex size
set.vert.size <- function(gr, vert.size= 1.3, scale.vert.size= TRUE, vert.size.range= c(1,6)) {
  # Arguments:
  # -- gr: the graph
  # -- vert.size: It can be numeric, character, ot a function. If numeric, it's either a fixed node size
  # (default 2), or a numeric vector (possibly to be recycled) with node sizes. If character, it
  # must give a continuous vertex attribute in graph to map node size from (NOTE: this vector is
  # then rescaled to the range c(min.size, max.size). If a function, it must take 'gr' as an
  # argument and return a vector of length=vcount(gr); in this case, the function is used to
  # calculate vertex sizes.
  # -- scale.vert.size (logical) should the size be rescaled
  # -- vert.size.range: (numeric of length 2) the range to rescale node size to.
  # 
  # Return value
  # a graph
  
  # Argument validity
  stopifnot(is.igraph(gr), (is.character(vert.size) | is.numeric(vert.size)), is.numeric(vert.size.range))
  
  # If argument size is missing AND there is no size vertex attribute already existing, then apply the default size 
  if (missing(vert.size)) {
    if (is.null(V(gr)$size)) {
      V(gr)$size <- vert.size
    }
  } else {
    # Otherwise, if argument size is specified, then apply the following code (which overwrites existing V(gr)$size).
    # If size is FUNCTION
    if (is.function(vert.size)) {
      # it is used to calculate the vector of vertex sizes and set it as size.
      # In this case, size() must be a function that takes the graph 'gr' as its argument, and returns a
      # vector of sizes with length=vcount(gr).
      
      # Use function to calculate the size.
      newsize<- vert.size(gr)
      
      # Verify that the function has returned a vector of the same length as the number of vertexes.
      if (length(newsize)!=vcount(gr)) stop("Argument 'vert.size' is a function but its output has not the same length as the number of vertexes in 'gr'.")
      
      # Set the output of size(gr) as vertex size, after rescaling the size if scale.size is TRUE
      if (scale.vert.size) {
        V(gr)$size <- scales::rescale(newsize, c(size.range[1], size.range[2]))
      } else {
        V(gr)$size <- newsize
      }
      
    }
    
    # If size is CHARACTER
    if (is.character(vert.size)) {
      if (!(vert.size %in% list.vertex.attributes(gr))) stop('If "vert.size" is character, it must be the name of a continuous vertex attribute in "gr" to be mapped to node size.')
      # ... and it's the name of a vertex.attribute (so the stop() above has been avoided), then
      # node size is that vertex.attribute rescaled from min.size to max.size.
      # (NOTE that in get.vertex.attribute() the 'name' argument is 'm$size' and not just 'size', 
      # because 'm$size' is the argument in the function call, while 'size' is whatever vertex attribute 
      # in graph is called size)
      if (scale.size) {
        V(gr)$size <- rescale(get.vertex.attribute(gr, name= size), c(vert.size.range[1], vert.size.range[2]))
      } else {
        V(gr)$size <- get.vertex.attribute(gr, name= size)
      }
    } 
    
    # If size is NUMERIC
    if (is.numeric(vert.size)) {
      # it's set as node size: it may be a single value resulting in fixed node size, or a numeric
      # vector giving different node sizes.
      # NOTE That if 'size' is a numeric vector >1 but with length!=length(V(gr)) it's going to be
      # recycled with a warning.
      # If numeric size as length > 1...
      if (length(vert.size)>1) {
        # Give a warning if it's not the same as the number of vertexes.
        if(length(vert.size)!=length(V(gr))) { 
          stop('"vert.size" has different length than vcount(gr)')
        }
        # Rescale it if scale.size is TRUE
        if (scale.vert.size) {
          V(gr)$size <- scales::rescale(vert.size, c(vert.size.range[1], vert.size.range[2]))
        } else {
          # Otherwise set directly as size
          V(gr)$size <- vert.size
        }
      } else { 
        # Otherwise (length==1), set as size without rescaling.
        V(gr)$size <- vert.size
      }
    }
  }
  # Return
  return(gr)
}


# Function to set vertex labels
set.vert.lab <- function(gr, labels= NA, label.cex= 0.5, label.family= 'sans', label.font= 2, label.dist= 0, label.degree= -pi/2, label.color= 'blue') {
  # Arguments:
  # -- labels: either (as character of length=1) name of vertex attribute to use as labels, or (as
  # character or numeric of length= length(V(gr))) vector of values to be used as node labels. If 'labels'
  # is NA, nodes will have no labels.
  #
  # Return value:
  # a graph
  
  # If function argument 'labels' is missing (all elements are NA) AND graph doesn't already have vertex labels, node 
  # labels will be NA.
  if (all(is.na(labels))) {
    if (is.null(V(gr)$label)) {
      V(gr)$label <- NA 
    } 
  } else {
    # Else, if function argument "labels" is NOT missing 
    # NOTE that the following code will overwrite existing vertex labels existing in the graph.
    # If 'labels' has length==1... 
    if (length(labels)==1) {
      # ...and it's the name of a vertex attribute...
      if (labels %in% list.vertex.attributes(gr)) {
        # ... that attribute is used as labels.
        V(gr)$label <- get.graph.attribute(graph= gr, name= labels)
      } else {
        # If 'labels' has length==1 and it's NOT the name of a vertex attribute, no labels with a
        # warning.
        V(gr)$label <- NA
        warning("'labels' has length 1, but it's not the name of a vertex attribute: labels are NA.")
      }
      ## While if 'labels' has length>1...
    } else {
      ## ... it must be the same length as the number of vertexes...
      if (length(labels)==vcount(gr)) {
        ## ...and if that's the case, the 'labels' vector is used as labels...
        V(gr)$label <- labels
      } else {
        ## ... while if not, no labels with a warning
        V(gr)$label <- NA
        warning("'labels' has length>1, but it's not the same length as V(gr): labels are NA.")
      }
    }
  }
  
  # If at least one label is not NA, then set the vertex label parameters using corresponding
  # function arguments.
  if (any(!is.na(V(gr)$label))) {
    # Label size
    V(gr)$label.cex <- label.cex
    # Font family
    V(gr)$label.family <- label.family
    # Font face
    V(gr)$label.font <- label.font
    # Distance from vertex
    V(gr)$label.dist <- label.dist
    # Angle from veretex
    V(gr)$label.degree <- label.degree
    # Color
    V(gr)$label.color <- label.color
  }
  
  # Return
  return(gr)
  
}


## Graphics
## =================================================================================================

# Convert a numeric vector into sequential colors from a given RColorBrewer palette.
hex.seq <- function(vector, palette="Reds", NAcol= "grey", space= "Lab", reverse=FALSE) {
  # Arguments:
  # -- vector: (numeric) a numeric vector 
  # -- palette: (character) a color palette (i.e. vector of colors), or the name of an RColorBrewer *sequential* palette
  # -- NAcol: (character) color for NA values in the vector
  # -- space: (character) the "space" argument to colorRamp(), one of "Lab" or "rgb".
  # -- reverse: (logical) if TRUE the color scale is reversed.
  #
  # Return value
  # A vector of colors (hex codes), the same length as "vector".
  
  # Argument validity
  stopifnot(is.numeric(vector), is.character(palette))
  
  # Load required packages
  library(RColorBrewer)
  library(scales)
  
  # If vector is not already scaled to c(0,1), rescale it
  if (!(range(vector, na.rm=TRUE)[1]>=0 & range(vector, na.rm=TRUE)[2]<=1)) {
    vector <- rescale(vector, c(0,1))
  }
  
  # Get the color palette
  ## If palette is character of length 1
  if (length(palette)==1) {
    # Palette must be an RColorBrewer sequential palette
    stopifnot(palette %in% rownames(brewer.pal.info[brewer.pal.info$category %in% c("seq", "div"),]))
    # Get the color from RColorBrewer
    colors <- brewer.pal(9,name= palette)
  } else {
    # Otherwise palette is used as a palette itself
    colors <- palette
  }
  
  # Reverse the palette, if reverse==TRUE
  if (reverse) colors <- colors[length(colors):1]
  
  # Interpolate the palette to obtain rgb colors for vector
  rgb.mat <- colorRamp(colors, space=space)(vector)
  
  # Rename columns of rgb.mat so it can be fed to rgb()
  colnames(rgb.mat) <- c("red", "green", "blue")
  
  # Index rows with ALL NA entries.
  na.ind <- apply(rgb.mat, 1, function(x) all(is.na(x)))
  # Replace those rows with rgb codes for NAcol
  rgb.mat[na.ind,] <- as.numeric(col2rgb(NAcol))
  
  # For each row of rgb.mat, appply rgb() to convert that row to hex color.
  rgb(rgb.mat, maxColorValue=255)  
}

# Make a given color transparent with a given alpha value
alpha.col <- function(col, alpha=0.5) {
  # Arguments:
  # -- col: (character) a color as color names (e.g. one in colors()) or hex code string.
  # -- alpha: (numeric) value in (0,1) giving the transparency
  #
  # Return value:
  # Character, an hex code with the alpha specification added.
  
  # Argument validity
  stopifnot(is.character(col), is.numeric(alpha), alpha>=0, alpha<=1)
  
  # Convert the 0-1 alpha to the 0-255 range
  alpha <- 255*alpha
  
  # Turn the color to rgb values
  rgb.col <- t(col2rgb(col))
  
  # Convert back to hex code, but adding the alpha specification
  rgb(rgb.col, maxColorValue=255, alpha= alpha)
  
}

# Open an appropriate "default" png device
png.def <- function(filename, wide= TRUE, type= 'cairo-png', width= 2500, height= 2000, pointsize= 10, res= 200, margins= rep(0,4), ...) {
  # Arguments:
  # -- wide: (logical) if TRUE the png file has given width and height, if FALSE has other width and
  # heigth, if missing width and height must be provided as arguments
  # -- filename, type, width, height, pointsize, res: arguments to be passed to png()
  
  # IF wide==TRUE (default) 
  if (wide==TRUE) {
    w <- 2500
    h <- 2000
  }
  # IF wide==FALSE
  if (wide==FALSE) {
    w <- 2500
    h <- 1400
  }
  
  # But IF any of width or height is NOT missing, its value overwrites the defaults
  if (!missing(width)) w <- width
  if (!missing(height)) h <- height
  
  # The resulting w and h are used as width and height
  png(filename= filename, type= type, width= w, height= h, pointsize= pointsize, res= res, ...)
  
  # Set plot margins
  par(mai= margins)
}

## File names
## =================================================================================================

# Rewrites script file names based on script order
file.order <- function(i, name, path) {
  # Arguments:
  # -- i: (numeric) the number to attach (will be attached in formatC(width=2, flag='0') format)
  # -- name: (character) the name of file without order number
  # -- path: (character) path of file
  
  # Get the actual file name from 'name' (a number different from i may already be attached to the file name)
  file.name <- list.files(path)[grep(pattern= name, list.files(path))]
  
  # Set the new file name based on i
  newfile.name <- paste(formatC(i, width=2, flag='0'), name, sep='_')
  
  # IF newfile.name is different from file.name, rename file
  if (file.name != newfile.name) {
    file.rename(from= file.name, to= newfile.name)
  }
  
  # Return new file name
  return(newfile.name)
}
