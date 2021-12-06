#community detection from overall network
#remember to set working directory to 'data_function scripts' folder
library(tidyverse)
library(quanteda)
library(igraph) #for creating graphs
library(visNetwork) #for visualizing graphs

#load tokens, get it ready for analysis
load("token.all.RData")
#convert tokens to all lower
token.all <- tokens_tolower(token.all) #convert all tokens to lower

#select window of words around males and female characters
#males
toks.male <- token.all %>% 
  tokens_select(pattern = 'male/characters', selection = 'remove', padding = TRUE, window = 5)

#females
toks.female <- token.all %>% 
  tokens_select(pattern = 'female/characters', selection = 'remove', padding = TRUE, window = 5)


#DETECTING COMMUNITIES IN MALE NETWORKS
#filter to keep only words that occur at least 10 times
dfm_male <-  toks.male %>% dfm() %>% dfm_trim(min_termfreq = 10)
male_filtered = colnames(dfm_male)
toks.male <- token.all %>% 
  tokens_select(pattern = male_filtered, selection = 'keep', padding = TRUE)

#feature co-occurrence matrix for males
male_fcmat = fcm(toks.male, context = c("window"),
                 count = c("weighted"), #words are weighted within the window
                 window = 5)

male_fcmat[1:2,1:2] #a small portion of the feature co-occurrence matrix

male_graph = graph_from_adjacency_matrix(male_fcmat, weighted = TRUE) #create graph from matrix
edgelist_male <- get.data.frame(male_graph)
edgelist_mm <- as.matrix(edgelist_male[ ,c("from", "to")])

male_graph <- graph_from_edgelist(edgelist_mm, directed = FALSE) 
male_graph <- set.edge.attribute(male_graph, "weight", value = edgelist_male$weight)
graph_m = simplify(male_graph, remove.loops = TRUE) #remove self-looping edges

#louvian communities
louvain_male <- cluster_louvain(male_graph, weights = E(male_graph)$weights)#detect communities
male_graph$community <- louvain_male$membership
unique(male_graph$community)

#most important word in each community
communities <- data.frame()

for (i in unique(male_graph$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(male_graph, v = which(male_graph$community == i))
  # get size of each subgraph
  size <- igraph::gorder(subgraph)
  # get betweenness centrality
  btwn <-  igraph::betweenness(subgraph)
  communities <- communities %>% 
    dplyr::bind_rows(
      data.frame(community = i,
                 n_characters = size,
                 most_important = names(which(btwn == max(btwn)))
      )
    )
}

knitr::kable(communities %>% 
               dplyr::select(community, n_characters, most_important))

#top five in each community
top_twenty <- data.frame()

for (i in unique(male_graph$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(male_graph, v = which(male_graph$community == i))
  
  # for larger communities
  if (igraph::gorder(subgraph) > 1000) {
    # get degree
    degree <-  igraph::degree(subgraph)
    # get top ten degrees
    top <- names(head(sort(degree, decreasing = TRUE), 10))
    result <- data.frame(community = i, rank = 1:10, character = top)
  } else {
    result <- data.frame(community = NULL, rank = NULL, character = NULL)
  }
  
  top_twenty <- top_twenty %>% 
    dplyr::bind_rows(result)
}

top_twenty

knitr::kable(
  top_five %>% 
    tidyr::pivot_wider(names_from = rank, values_from = character)
)

#Visualising the communities
male_subgraph <- induced_subgraph(male_graph, v = top_twenty$character)
male_subgraph <- simplify(male_subgraph)
male_subgraph$community <- top_twenty$community
unique(male_subgraph$community)

# give our nodes some properties, incl scaling them by degree and coloring them by community
V(male_subgraph)$size <- 3
V(male_subgraph)$frame.color <- "white"
V(male_subgraph)$color <- male_subgraph$community
V(male_subgraph)$label <- V(male_subgraph)$name
V(male_subgraph)$label.cex <- 1.2

# also color edges according to their starting node
edge.start <- ends(male_subgraph, es = E(male_subgraph), names = F)[,1]
E(male_subgraph)$color <- V(male_subgraph)$color[edge.start]
E(male_subgraph)$arrow.mode <- 0

# only label central characters
#v_labels <- which(V(friends_graph)$name %in% friends)

#for (i in 1:length(V(friends_graph))) {
#  if (!(i %in% v_labels)) {
#    V(friends_graph)$label[i] <- ""
#  }
#}

l2 <- layout_with_mds(male_subgraph)
plot(male_subgraph, rescale = T, layout = l2, main = "Male Graph")
length(V(male_subgraph))

male_subgraph$community
V(male_subgraph)
#match results

##### old ######

#nodes with highest degrees in each of the major communities
#community 1 - relationships?
head(sort(degree(graph_m)[graph_m.comm[[1]]], decreasing = TRUE), 20)
c1 = names(head(sort(degree(graph_m)[graph_m.comm[[1]]], decreasing = TRUE), 20))
#community 2 - action?
head(sort(degree(graph_m)[graph_m.comm[[2]]], decreasing = TRUE), 20)
c2 = names(head(sort(degree(graph_m)[graph_m.comm[[2]]], decreasing = TRUE), 20))
#community 3 - violence?
head(sort(degree(graph_m)[graph_m.comm[[3]]], decreasing = TRUE), 20)
c3 = names(head(sort(degree(graph_m)[graph_m.comm[[3]]], decreasing = TRUE), 20))

male.c = c(c1, c2, c3)
subgraph.m = induced_subgraph(graph_m, male.c)
member.m = rep(1:3, each = 20)
clusters.m = make_clusters(subgraph.m, membership = member.m, modularity = TRUE)
#layout <-layout.fruchterman.reingold(graph_m)
plot(induced_subgraph(graph_m, male.c), vertex.colors = clusters.m$membership, vertex.size = 4, vertex.label = NA)


visIgraph(male_subgraph)



##### old ########



#DETECTING COMMUNITIES IN FEMALE NETWORKS
#filter to keep only words that occur at least 10 times
dfm_female <-  toks.female %>% dfm() %>% dfm_trim(min_termfreq = 10)
female_filtered = colnames(dfm_female)
toks.female <- token.all %>% 
  tokens_select(pattern = female_filtered, selection = 'keep', padding = TRUE)
#feature co-occurrence matrix for females
female_fcmat = fcm(toks.female, context = c("window"),
                 count = c("weighted"), #words are weighted within the window
                 window = 5)

female_fcmat[1:2,1:2] #a small portion of the feature co-occurrence matrix

graph_f = graph_from_adjacency_matrix(female_fcmat, mode = "undirected") #create graph from matrix
graph_f = simplify(graph_m, remove.loops = TRUE) #remove self-looping edges
graph_f.comm <- cluster_fast_greedy(graph_m) #detect communities
#membership(graph_m.comm)
length(graph_f.comm) #number of communities - note that there are many small communities but only few major ones
sizes(graph_f.comm) #sizes of communities

#nodes with highest degrees in each of the major communities
#community 1 - violence?
head(sort(degree(graph_f)[graph_f.comm[[1]]], decreasing = TRUE), 20)
#community 2 - action?
head(sort(degree(graph_f)[graph_f.comm[[2]]], decreasing = TRUE), 20)
#community 3 - relationships?
head(sort(degree(graph_f)[graph_f.comm[[3]]], decreasing = TRUE), 20)

#note the presence of female characters in the relationship community for males
#but male characters in the violence community for females

#ignore

# minimumFrequency = 50
# binDTM <- toks.male %>% 
#   dfm() %>% 
#   dfm_trim(min_docfreq = minimumFrequency) %>% 
#   dfm_weight("count")
# comat <- t(binDTM) %*% binDTM
# film_graph <- graph_from_adjacency_matrix(comat, mode = "undirected")
# #plot.igraph(film_graph, vertex.label = NA, vertex.size = 8)
# film_graph <- simplify(film_graph, remove.loops = TRUE)
# c1 = cluster_fast_greedy(film_graph)
# modularity(c1)
# vertex.label = ifelse(degree(film_graph) > 1000, film_graph$label, NA)
# V(film_graph)
# # modularity matrix
# plot.igraph(film_graph, vertex.color=membership(c1), vertex.size = 2, vertex.label = ifelse(degree(film_graph) > 2500, V(film_graph), NA))
# length(c1)
# degree(film_graph)
# sizes(c1)
# c1
# head((c1[[3]]))


     