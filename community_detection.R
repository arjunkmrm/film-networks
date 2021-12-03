#community detection from overall network
#remember to set working directory to 'data_function scripts' folder
library(tidyverse)
library(quanteda)

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

graph_m = graph_from_adjacency_matrix(male_fcmat, mode = "undirected") #create graph from matrix
graph_m = simplify(graph_m, remove.loops = TRUE) #remove self-looping edges
graph_m.comm <- cluster_fast_greedy(graph_m) #detect communities
#membership(graph_m.comm)
length(graph_m.comm) #number of communities - note that there are many small communities but only few major ones
sizes(graph_m.comm) #sizes of communities

#nodes with highest degrees in each of the major communities
#community 1 - relationships?
head(sort(degree(graph_m)[graph_m.comm[[1]]], decreasing = TRUE), 20)
#community 2 - action?
head(sort(degree(graph_m)[graph_m.comm[[2]]], decreasing = TRUE), 20)
#community 3 - violence?
head(sort(degree(graph_m)[graph_m.comm[[3]]], decreasing = TRUE), 20)

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


     