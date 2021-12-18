##### function to create graphs #####
#Wiedemann, Gregor; Niekler, Andreas (2017): Hands-on: A five day text mining course for humanists and social scientists in R. Proceedings of the 1st Workshop on Teaching NLP for Digital Humanities (Teach4DH@GSCL 2017), Berlin.

  numberOfCoocs = 10
  toks = token_filter3("noun", 1940, 2020, token.all)
  measure = 'LOGLIK'

grapherdemo <- function(numberOfCoocs, toks, measure = "LOGLIK"){
  #oppositeg = ifelse(coocTerm == 'male/characters', 'female/characters', 'male/characters')
  #coocTerm = 'male/characters'
  #### graph df function
  graph_df <- function(coocTerm){
  minimumFrequency = 10
  binDTM <- toks %>% 
    dfm() %>% 
    dfm_trim(min_docfreq = minimumFrequency) %>% 
    dfm_weight("boolean")
  
  coocs <- calculateCoocStatistics(coocTerm, binDTM, measure)

  # Display the numberOfCoocs main terms
  imm.coocs <- names(coocs[1:numberOfCoocs])
  logs <- coocs[1:numberOfCoocs]
  
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
  #resultGraph[sample(nrow(resultGraph), 6), ]
  list_graph = list()
  list_graph[[1]] = imm.coocs
  list_graph[[2]] = resultGraph
  list_graph[[3]] = logs
  return(list_graph)
  }
  male_list = graph_df('male/characters')
  female_list = graph_df('female/characters')
  male = male_list[[2]]
  male_coocs = male_list[[1]]
  male_logs = male_list[[3]]
  female = female_list[[2]]
  female_coocs = female_list[[1]]
  female_logs = female_list[[3]]
  complete = rbind(male, female)
  tail(complete)
  complete <- distinct(complete)
  nrow(complete)
  # set seed for graph plot
  set.seed(42)
  resultGraph = complete
  names(complete)[3] = 'weight'
  head(complete)
  # Create the graph object as undirected graph
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  E(graphNetwork)$weight = complete$weight
  is_weighted(graphNetwork)
  
  complete
  
  # Identification of all nodes with less than 2 edges
  verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
  # These edges are removed from the graph
  #graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 
  #imm.coocs
  
  #for vertices #####
  #male to female - not needed
  #ftm = rowSums(ends(graphNetwork, es = E(graphNetwork), names = T) == c('female/characters', 'male/characters'))
  #female primary nodes
  fto = ends(graphNetwork, es = E(graphNetwork), names = T)[,1] == 'female/characters'
  fto2 = ends(graphNetwork, es = E(graphNetwork), names = T)[,2] == 'female/characters'
  #male primary nodes
  mto = ends(graphNetwork, es = E(graphNetwork), names = T)[,1] == 'male/characters'
  mto2 = ends(graphNetwork, es = E(graphNetwork), names = T)[,2] == 'male/characters'
  
   #female connections
  fc = ends(graphNetwork, es = E(graphNetwork), names = T)[,2][as.logical(fto)]
  fc2 = ends(graphNetwork, es = E(graphNetwork), names = T)[,1][as.logical(fto2)]
  #male connections
  mc = ends(graphNetwork, es = E(graphNetwork), names = T)[,2][as.logical(mto)]
  mc2 = ends(graphNetwork, es = E(graphNetwork), names = T)[,1][as.logical(mto2)]
  
  #imm.coocs
  #male and female
  main_cm = c(mc, mc2)
  main_cf = c(fc, fc2)
  maf = intersect(main_cm, main_cf)
  #fam = intersect(mc2, female_coocs)
  intersect = intersect(male_coocs, female_coocs)
  #for edges #####
  # fem <- rep(oppositeg, length(maf))
  # m.maf = c(fem, maf)
  # dim(m.maf) <- c(length(maf),2)
  # 
  # all_edges = ends(graphNetwork, es = E(graphNetwork), names = T)
  # all_edges = as.data.frame(all_edges)
  # m.maf = as.data.frame(m.maf)
  # all_edges$x = paste(all_edges$V1, all_edges$V2)
  # m.maf$x = paste(m.maf$V2, m.maf$V1)
  # #common nodes between males and females?
  # #male primary nodes
  # mpn = ends(graphNetwork, es = E(graphNetwork), names = T)[,2] %in% male_coocs
  # #male female intersection
  # mfi = all_edges$x %in% m.maf$x
  # sum(mfi)
  
  # Assign colors to nodes (search term blue, primary green, others orange)
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == c('male/characters'), adjustcolor('cornflowerblue', alpha = 0.9),
                                  ifelse(V(graphNetwork)$name %in% c('female/characters'), adjustcolor('orange', alpha = 0.9),
                                         ifelse(V(graphNetwork)$name %in% c(intersect), adjustcolor('purple', alpha = 0.8),
                                  ifelse(V(graphNetwork)$name %in% male_coocs, adjustcolor('cornflowerblue', alpha = 0.8),
                                         ifelse(V(graphNetwork)$name %in% female_coocs, adjustcolor('orange', alpha = 0.9), adjustcolor('grey', alpha = 0.4))))))
  
  #V(graphNetwork)$color <- ifelse(V(graphNetwork)$name %in% fc, 'orange', V(graphNetwork)$color)
  # Set edge colors
  #E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
  # scale significance between 1 and 10 for edge width
  E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))
  
  E(graphNetwork)$color <- adjustcolor('DarkGray', alpha.f = 0.4)
   
   # E(graphNetwork)$color <- ifelse(mtf == 2, adjustcolor('cornflowerblue', alpha.f = 0.4),
   #                                 ifelse(mfi == TRUE, adjustcolor('orange', alpha.f = 0.4),
   #                                 ifelse(fto == TRUE, adjustcolor('orange', alpha.f = 0.4),
   #                                 ifelse(mpn == TRUE, adjustcolor('cornflowerblue', alpha.f = 0.4), adjustcolor('DarkGray', alpha.f = .4)))))
   #E(graphNetwork)$width <- ifelse(mtf == 2, 6, ifelse(fto == 1, 6, 2))
   
   #E(graphNetwork)$color <- ifelse(mfi == TRUE, adjustcolor('purple', alpha.f = 0.4), adjustcolor('DarkGray', alpha.f = .4))
   
   # also color edges according to their starting node
  #edge.start <- ends(graphNetwork, es = E(graphNetwork), names = T)[,1]
  #E(graphNetwork)$color <- V(graphNetwork)$color[edge.start]
  #E(graphNetwork)$arrow.mode <- 0
  
  # Set edges with radius
  E(graphNetwork)$curved <- 0.15 
  # Size the nodes by their degree of networking (scaled between 5 and 15)
  V(graphNetwork)$size <- scales::rescale(degree(graphNetwork), to = c(10, 25))
  
  # Define the frame and spacing for the plot
  par(mai=c(0,0,1,0)) 
  
  visIgraph(graphNetwork) 
  #visSave(graphNetwork, "male_graph.html", selfcontained = TRUE, background = "white")
  
  #log_df <- data.frame(names = names(coocs), loglik = coocs)
  #rownames(log_df) <- 1:nrow(log_df)
  
  graph_list <- list()
  graph_list[[1]] <- graphNetwork #network object
  graph_list[[2]] <- female_logs #names of co-occs (redundant)
  graph_list[[3]] <- male_logs #data frame of co-occs and significance
  return(graph_list)
}
