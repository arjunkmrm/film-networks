#community detection from overall network
#remember to set working directory to 'data_function scripts' folder
library(tidyverse)
library(quanteda)
library(igraph) #for creating graphs
library(visNetwork) #for visualizing graphs

source("token_filter.R") #filter tokens
#load tokens, get it ready for analysis
load("token.all.RData")
#convert tokens to all lower
token.all <- tokens_tolower(token.all) #convert all tokens to lower
token.all = tokens_sample(token.all, size = 22638, replace = FALSE, prob = NULL, by = decade)
#token.all <- token_filter2('noun', 1940, 2020, token.all)
#select window of words around males and female characters
#males
# toks.male <- token.all %>% 
#   tokens_select(pattern = 'male/characters', selection = 'remove', padding = TRUE, window = 5)
# 
# #females
# toks.female <- token.all %>% 
#   tokens_select(pattern = 'female/characters', selection = 'remove', padding = TRUE, window = 5)

#gender = 'male'
#DETECTING COMMUNITIES
#toks.all = token.all
#gender = 'female'
detect_communities <- function(toks.all, gender = 'male', nn = 10){
  toks <- toks.all %>% 
     tokens_select(pattern = paste(gender, '/characters', sep = ''), selection = 'remove', padding = TRUE, window = 5)

  #filter to keep only words that occur at least 10 times
dfm <-  toks %>% dfm() %>% dfm_trim(min_termfreq = 10)
filtered = colnames(dfm)
toks <- token.all %>% 
  tokens_select(pattern = filtered, selection = 'keep', padding = TRUE)

#feature co-occurrence matrix for males
fcmat = fcm(toks, context = c("window"),
                 count = c("weighted"), #words are weighted within the window
                 window = 5)

#fcmat[1:2,1:2] #a small portion of the feature co-occurrence matrix

graph = graph_from_adjacency_matrix(fcmat, weighted = TRUE) #create graph from matrix
edgelist <- get.data.frame(graph)
edgelist_m <- as.matrix(edgelist[ ,c("from", "to")])

graph <- graph_from_edgelist(edgelist_m, directed = FALSE) 
graph <- set.edge.attribute(graph, "weight", value = edgelist$weight)
graph = simplify(graph, remove.loops = TRUE) #remove self-looping edges

#louvian communities
louvain <- cluster_louvain(graph, weights = E(graph)$weights)#detect communities 
graph$community <- louvain$membership
#unique(male_graph$community)
paste('modularity =', modularity(louvain))

#most important word in each community
communities <- data.frame()

for (i in unique(graph$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(graph, v = which(graph$community == i))
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

communities = arrange(communities, desc(n_characters))
top_comm <- communities$community[1:5]

#knitr::kable(communities %>%
#               dplyr::select(community, n_characters, most_important))

#top five in each community
top_ten <- data.frame()
n = 0
for (i in top_comm) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(graph, v = which(graph$community == i))
  
  # for larger communities
#  if (igraph::gorder(subgraph) > 1055) {
    n = n + 1
    # get degree
    degree <-  igraph::degree(subgraph)
    # get top ten degrees
    top <- names(head(sort(degree, decreasing = TRUE), nn))
    result <- data.frame(community = i, rank = 1:nn, word = top)
 # } else {
 #   result <- data.frame(community = NULL, rank = NULL, character = NULL)
  #}
  
  top_ten <- top_ten %>% 
    dplyr::bind_rows(result)
}

print(top_ten)
write.csv(top_ten, paste(gender, '.csv', sep = ''))
n
# knitr::kable(
#   top_five %>% 
#     tidyr::pivot_wider(names_from = rank, values_from = character)
# )

#Visualising the communities
subgraph <- induced_subgraph(graph, v = top_ten$word)
subgraph <- simplify(subgraph)
subgraph$community
nodes = data.frame(word = names(V(subgraph)))
group = rep(1:n, each = nn)
top_ten$group = group
clusters = inner_join(nodes, top_ten)
subgraph$community <- clusters$group
#unique(subgraph$community)

# give our nodes some properties, incl scaling them by degree and coloring them by community
V(subgraph)$size <- 5
V(subgraph)$frame.color <- "white"
V(subgraph)$color <- subgraph$community
#V(male_subgraph)$label <- V(male_subgraph)$name
V(subgraph)$label.cex <- 1.5

# also color edges according to their starting node
edge.start <- ends(subgraph, es = E(subgraph), names = F)[,1]
E(subgraph)$color <- V(subgraph)$color[edge.start]
E(subgraph)$arrow.mode <- 0

# only label central characters
#v_labels <- which(V(friends_graph)$name %in% friends)

#for (i in 1:length(V(friends_graph))) {
#  if (!(i %in% v_labels)) {
#    V(friends_graph)$label[i] <- ""
#  }
#}



#l2 <- layout_with_mds(male_subgraph)
#layout <- layout_with_kk(male_subgraph, weights=weights)
#plot(graph, layout=layout)
#plot(male_subgraph, rescale = T, layout = l2, main = "Male Graph")
#length(V(male_subgraph))

#visIgraph(male_subgraph) %>% visIgraphLayout(layout = "layout_with_mds") %>% visNodes(size = 12)
#layout_in_circle
#"layout_with_mds"

#plot by groups
#make clusters first
clust_obj = make_clusters(subgraph, membership = clusters$group)

# weights <- ifelse(crossing(male_clust, male_subgraph), 1, 100)
# layout <- layout_with_kk(male_subgraph, weights=weights)
# plot(male_subgraph, layout=layout)

prettyColors <- c("turquoise4", "azure4", "olivedrab","deeppink4", "blue")
communityColors <- prettyColors[membership(clust_obj)]


edge.weights <- function(community, network, weight.within = 100, weight.between = 1) {
  bridges <- crossing(communities = community, graph = network)
  weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
  return(weights) 
}
E(subgraph)$weight <- edge.weights(clust_obj, subgraph)
layout <- layout_with_fr(subgraph, weights=E(subgraph)$weight)
plot(subgraph, layout=layout, col = communityColors)
}

detect_communities(token.all, 'female', 10)

#visIgraph(male_subgraph) %>% visIgraphLayout(layout = "layout_with_fr") %>% visNodes(size = 12)

##### ignore ########
# #nodes with highest degrees in each of the major communities
# #community 1 - relationships?
# head(sort(degree(graph_m)[graph_m.comm[[1]]], decreasing = TRUE), 20)
# c1 = names(head(sort(degree(graph_m)[graph_m.comm[[1]]], decreasing = TRUE), 20))
# #community 2 - action?
# head(sort(degree(graph_m)[graph_m.comm[[2]]], decreasing = TRUE), 20)
# c2 = names(head(sort(degree(graph_m)[graph_m.comm[[2]]], decreasing = TRUE), 20))
# #community 3 - violence?
# head(sort(degree(graph_m)[graph_m.comm[[3]]], decreasing = TRUE), 20)
# c3 = names(head(sort(degree(graph_m)[graph_m.comm[[3]]], decreasing = TRUE), 20))
# 
# male.c = c(c1, c2, c3)
# subgraph.m = induced_subgraph(graph_m, male.c)
# member.m = rep(1:3, each = 20)
# clusters.m = make_clusters(subgraph.m, membership = member.m, modularity = TRUE)
# #layout <-layout.fruchterman.reingold(graph_m)
# plot(induced_subgraph(graph_m, male.c), vertex.colors = clusters.m$membership, vertex.size = 4, vertex.label = NA)
# 
# #DETECTING COMMUNITIES IN FEMALE NETWORKS
# #filter to keep only words that occur at least 10 times
# dfm_female <-  toks.female %>% dfm() %>% dfm_trim(min_termfreq = 10)
# female_filtered = colnames(dfm_female)
# toks.female <- token.all %>% 
#   tokens_select(pattern = female_filtered, selection = 'keep', padding = TRUE)
# #feature co-occurrence matrix for females
# female_fcmat = fcm(toks.female, context = c("window"),
#                  count = c("weighted"), #words are weighted within the window
#                  window = 5)
# 
# female_fcmat[1:2,1:2] #a small portion of the feature co-occurrence matrix
# 
# graph_f = graph_from_adjacency_matrix(female_fcmat, mode = "undirected") #create graph from matrix
# graph_f = simplify(graph_m, remove.loops = TRUE) #remove self-looping edges
# graph_f.comm <- cluster_fast_greedy(graph_m) #detect communities
# #membership(graph_m.comm)
# length(graph_f.comm) #number of communities - note that there are many small communities but only few major ones
# sizes(graph_f.comm) #sizes of communities
# 
# #nodes with highest degrees in each of the major communities
# #community 1 - violence?
# head(sort(degree(graph_f)[graph_f.comm[[1]]], decreasing = TRUE), 20)
# #community 2 - action?
# head(sort(degree(graph_f)[graph_f.comm[[2]]], decreasing = TRUE), 20)
# #community 3 - relationships?
# head(sort(degree(graph_f)[graph_f.comm[[3]]], decreasing = TRUE), 20)

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


     