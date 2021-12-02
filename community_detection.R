#community detection from overall network

#create tokens for males
toks.male <- token.all %>% 
  tokens_select(pattern = 'male/characters', padding = FALSE, window = 5)

#create tokens for females
toks.female <- toks.spacy %>% 
  tokens_select(pattern = c("*/NOUN", "*/VERB", "*/ENTITY", "*/ADJ")) %>% 
  tokens_select(pattern = paste(entity.female$name, "/ENTITY", sep = ""), padding = FALSE, window = 5) %>% 
  tokens_remove(pattern = "*/ENTITY", padding = FALSE) %>% 
  tokens_remove("")

male_fcmat = fcm(toks.male, context = c("window"),
                 count = c("weighted"),
                 window = 5)
male_fcmat
graph_m = graph_from_adjacency_matrix(male_fcmat, mode = "undirected")
graph_m = simplify(graph_m)
graph_m.comm <- cluster_fast_greedy(graph_m)
membership(graph_m.comm)
#plot(graph_m, vertex.size = 2, vertex.label = NA)

minimumFrequency = 50 
binDTM <- toks.male %>% 
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency) %>% 
  dfm_weight("count")

comat <- t(binDTM) %*% binDTM
film_graph <- graph_from_adjacency_matrix(comat, mode = "undirected")
#plot.igraph(film_graph, vertex.label = NA, vertex.size = 8)
film_graph <- simplify(film_graph, remove.loops = TRUE)
c1 = cluster_fast_greedy(film_graph)
modularity(c1)
# modularity matrix
plot.igraph(film_graph, vertex.color=membership(c1), vertex.label = NA, vertex.size = 10)
length(c1)
sizes(c1)
c1
head((c1[[4]]))

     