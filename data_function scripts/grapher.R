##### function to create graphs #####
#Wiedemann, Gregor; Niekler, Andreas (2017): Hands-on: A five day text mining course for humanists and social scientists in R. Proceedings of the 1st Workshop on Teaching NLP for Digital Humanities (Teach4DH@GSCL 2017), Berlin.

grapher <- function(coocTerm, numberOfCoocs, toks, measure = "LOGLIK"){
  minimumFrequency = 10
  binDTM <- toks %>% 
    dfm() %>% 
    dfm_trim(min_docfreq = minimumFrequency) %>% 
    dfm_weight("boolean")
  
  coocs <- calculateCoocStatistics(coocTerm, binDTM, measure)

  # Display the numberOfCoocs main terms
  imm.coocs <- names(coocs[1:numberOfCoocs])
  
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # The structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:numberOfCoocs){
    
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
    
    #print the co-occurrences
    coocs2[1:10]
    
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  # Sample of some examples from resultGraph
  resultGraph[sample(nrow(resultGraph), 6), ]
  
  # set seed for graph plot
  set.seed(42)
  
  # Create the graph object as undirected graph
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  
  # Identification of all nodes with less than 2 edges
  verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
  # These edges are removed from the graph
  graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 
  
  # Assign colors to nodes (search term blue, others orange)
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', ifelse(V(graphNetwork)$name %in% imm.coocs, 'darkolivegreen', 'orange')) 
  
  # Set edge colors
  E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
  # scale significance between 1 and 10 for edge width
  E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))
  
  # Set edges with radius
  E(graphNetwork)$curved <- 0.15 
  # Size the nodes by their degree of networking (scaled between 5 and 15)
  V(graphNetwork)$size <- scales::rescale(degree(graphNetwork), to = c(10, 25))
  
  # Define the frame and spacing for the plot
  par(mai=c(0,0,1,0)) 
  
  visIgraph(graphNetwork) 
  
  log_df <- data.frame(names = names(coocs), loglik = coocs)
  rownames(log_df) <- 1:nrow(log_df)
  
  graph_list <- list()
  graph_list[[1]] <- graphNetwork #network object
  graph_list[[2]] <- imm.coocs #names of co-occs (redundant)
  graph_list[[3]] <- log_df #data frame of co-occs and significance
  return(graph_list)
}