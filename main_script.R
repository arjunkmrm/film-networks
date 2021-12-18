#everything related to statistical calculations of networks
#and plots

#remember to set directory to 'data_function scripts' folder

#load libraries
library(tidyverse) 
library(quanteda) #for text cleaning
library(igraph) #for creating graphs
library(visNetwork) #for visualizing graphs
library(wordcloud) #for creating wordclouds

#load_functions
source("calculatecoocstats.R") #calculate co-occurrence statistics
source("grapher.R") #create graph
source("graphervf.R")
#Wiedemann, Gregor; Niekler, Andreas (2017): Hands-on: A five day text mining course for humanists and social scientists in R. Proceedings of the 1st Workshop on Teaching NLP for Digital Humanities (Teach4DH@GSCL 2017), Berlin.

source("rawcounts.R") #find raw counts of co-occurrences
source("token_filter.R") #filter tokens

#load tokens, get it ready for analysis
load("token.all.RData")
#convert tokens to all lower
token.all <- tokens_tolower(token.all) #convert all tokens to lower

#sample based on min in a decade
token.all = tokens_sample(token.all, size = 22638, replace = FALSE, prob = NULL, by = decade)

token.all = token.all %>% tokens_remove(c('ex/adj', 'ex/noun'))

#create a token set with only generalized pos info
pos_replace <- function(toks.replace){
  toks.replace <- toks.replace %>% 
    tokens_replace(pattern = c("*/NOUN", "*/VERB", "*/ADJ"), replacement = c("NOUN", "VERB", "ADJ"))
  return(toks.replace)
}
token.pos <- pos_replace(token.all) 
#convert tokens to all lower
token.pos <- tokens_tolower(token.pos)

#1. Find Probability of Verbs/Adj/Noun given male/female across decades

p_decdat <- data.frame() #initialize data frame
pos = c('verb', 'adj', 'noun') #pos to be analysed

for(j in 0:7){ #for loop to run for each decade
  year = 1940 + j*10 #create decade variable
  pos_counts <- rawcounts(token_filter("all", year, token.pos)) #find raw co-occurrence counts
  male_pos <- pos_counts["male/characters", pos] #filter pos
  male_p <- male_pos / sum(male_pos) #find empirical probability
  male_pdat <- data.frame(pos = names(male_pos), p = male_p) #organise data frame
  male_pdat$gender = "male" #assign gender
  
  #do the same for females
  female_pos <- pos_counts["female/characters", pos]
  female_p <- female_pos / sum(female_pos)
  female_pdat <- data.frame(pos = names(female_pos), p = female_p)
  female_pdat$gender = "female"
  
  p_decdat.temp <- rbind(male_pdat, female_pdat) #bind gender data
  p_decdat.temp$year <- year #assign year
  p_decdat <- rbind(p_decdat, p_decdat.temp) #bind ind. decade with overall
}
  
  #plot for adjectives
  ggplot(p_decdat[pos == "adj",], aes(x = year, y = p, color = gender)) +
    geom_point(size = 1.3) + geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(fill = gender), alpha = 0.1) +
    geom_line(size = 0.8) + theme_minimal() + ggtitle("Probability of co-occurence with Adjectives") +
    theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
          legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
  ggsave("adj.png")
  
  #find significance
  ancova.adj <- aov(p ~ year*gender, data = p_decdat[pos == "adj",])
  summary(ancova.adj)
  
  #plot for verbs
  ggplot(p_decdat[pos == "verb",], aes(x = year, y = p, color = gender)) +
    geom_point(size = 1.2) + geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(fill = gender), alpha = 0.1) +
    geom_line(size = 0.8) + theme_minimal() + ggtitle("Probability of co-occurence with Verbs") + theme_minimal() +
    theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
          legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
  ggsave("verb.png")
  
  #find significance
  ancova.verb <- aov(p ~ year*gender, data = p_decdat[pos == "verb",])
  summary(ancova.verb)
  
  #plot for nouns
  ggplot(p_decdat[pos == "noun",], aes(x = year, y = p, color = gender)) +
    geom_point(size = 1.2) +
    geom_line(size = 0.8) + theme_minimal() + ggtitle("Probability of co-occurence with Nouns") + 
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(fill = gender), alpha = 0.1) +
    theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
          legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
  ggsave("noun.png")
  
  #find significance
  ancova.noun <- aov(p ~ year*gender, data = p_decdat[pos == "noun",])
  summary(ancova.noun)
  
#2. PPMI verbs, nouns, adjs
  all_ind <- data.frame() 
  plot_class <- function(term){
    all_ind <- data.frame() #initialise
    #term <- "best/adj" #term to find PPMI for
    #pos <- "adj" #pos of word 
    #i = 0
    for(i in 0 : 7){ #for loop to run across decades
      j = 1940 + 10*i
      male_ind = grapher("male/characters", 4 , token_filter('all', j, token.pos), "MI")[[3]][] #get PPMI data for given decade
      male_ind$rank = 1 : nrow(male_ind) #rank words - redundant
      male_ind <- male_ind %>% filter(names == term) #filter term given
      male_ind$year = j #attach year info
      male_ind$gender = "male" #assign gender
      
      #same for females
      j = 1940 + 10*i
      female_ind = grapher("female/characters", 4 ,token_filter('all', j, token.pos), "MI")[[3]][]
      female_ind$rank = 1 : nrow(female_ind)
      female_ind <- female_ind %>% filter(names == term)
      female_ind$year = j
      female_ind$gender = "female"
      
      #bind to overall data
      all_ind <- rbind(all_ind, male_ind, female_ind)
    }
    print(all_ind)
    #plot 
    ggplot(all_ind, aes(x = year, y = loglik, color = gender)) +
      geom_point(color = "black") + 
      geom_line(size = 1) +
      geom_smooth(method = "lm", se = TRUE, size = 1, aes(fill = gender), alpha = 0.1) + theme_minimal() +
      ylab("Pointwise Mutual Information") + ggtitle(term) +
      theme(axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14),
            legend.text = element_text(color = "black", size = 12), legend.title = element_text(color = "black", size = 14),
            panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
  }
  
  plot_class('adj')
  
  
#2. Individual words - PPMI across decades 

plot_word <- function(term, pos){
all_ind <- data.frame() #initialise
#term <- "best/adj" #term to find PPMI for
#pos <- "adj" #pos of word 
for(i in 0 : 7){ #for loop to run across decades
  j = 1940 + 10*i
  male_ind = grapher("male/characters", 10 ,token_filter(pos, j, token.all), "LOGLIK")[[3]][] #get PPMI data for given decade
  male_ind$rank = 1 : nrow(male_ind) #rank words - redundant
  male_ind <- male_ind %>% filter(names == term) #filter term given
  male_ind$year = j #attach year info
  male_ind$gender = "male" #assign gender
  
  #same for females
  j = 1940 + 10*i
  female_ind = grapher("female/characters", 10 ,token_filter(pos, j, token.all), "LOGLIK")[[3]][]
  female_ind$rank = 1 : nrow(female_ind)
  female_ind <- female_ind %>% filter(names == term)
  female_ind$year = j
  female_ind$gender = "female"

  #bind to overall data
  all_ind <- rbind(all_ind, male_ind)
}

#plot 
ggplot(all_ind, aes(x = year, y = loglik)) +
  geom_point(color = "black") + 
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = TRUE, size = 1, alpha = 0.1) + theme_minimal() +
  ylab("Loglikelihood") + ggtitle(term) +
  theme(axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12), legend.title = element_text(color = "black", size = 14),
        panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3)) 
  #facet_wrap(~ gender)
}

plot_word('boss/noun', 'noun')
ggsave("wedding_noun_female.png", width = 6, height = 4)

#check significance
ancova.word <- lm(loglik~year*gender, data = all_ind)
summary(ancova.word)
anova(ancova.word)

#check significance - females
all_ind_fem <- all_ind %>% filter(gender == "female")
ancova.word <- lm(loglik~year, data = all_ind_fem)
summary(ancova.word)
anova(ancova.word) 

#3. bar plot, networks, word cloud - log likelihoods
library(wordcloud)
#male
male.perm <- data.frame() #initialize
  #graph.m = grapher("male/characters", 20, token_filter2("noun", 1940, 2020, token.all)) #extract graph info
  graph.m = grapher("male/characters", 20, token.all) 
  gr.m <- graph.m[[3]] #pass graph object
  gr.m <- gr.m[gr.m$names != "female/characters",] #filter out female characters
  gr.m <- gr.m[1:22,] #filter 20 
  gr.m$rank = 1 : nrow(gr.m) #rank
  gr.m$gender = "male" #assign gender
  
  #bar plot
  ggplot(gr.m[1:25,], aes(reorder(names, -loglik), loglik)) +
    geom_bar(stat = "identity", fill = "black", color = "white", alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("co-occurring terms")
  ggsave("male_verb.png", width = 18, height = 10)
  
  #word cloud
  wordcloud(gr.m$names, as.integer(gr.m$loglik), rot.per = 0, colors = brewer.pal(4, "RdYlBu"))
  
  gr.m
  #network
  visIgraph(graph.m[[1]]) %>% visNodes(font = list(size = 28))
  #graphm_df = as_data_frame(graph.m[[1]])
  #head(graphm_df)
  
  #community structure
  #male
  graphm = graph.m[[1]] #store graph object
  vism <- toVisNetworkData(graphm) #create visnetwork object
  nodes <- vism$nodes #store nodes
  nodes <- nodes %>% select(-color) #remove color nodes
  edges <- vism$edges #store edges
  graphm = simplify(graphm, remove.loops = TRUE) #remove loops
  vism_comm <- walktrap.community(graphm) #find clusters using igraph
  modularity(vism_comm) #modularity
  #vism_comm
  length(vism_comm)
   plot(vism_comm, graphm, vertex.size = 4, vertex.label = NA)
  nodes$group <- membership(vism_comm) #store group info
  #no physics
  vism_graph <- visNetwork(nodes, edges, width = 1600, height = 900) %>% visPhysics(enabled = FALSE) %>% 
    visNodes(size = 8, font = c(size = 8)) %>% visEdges(color = c(opacity = 0.4))
  vism_graph
  visSave(vism_graph, "male_graph.html", selfcontained = TRUE, background = "white")
  vism_comm[[1]]
  degree(graphm)['help/noun']
  
  #female
  female.perm <- data.frame()
  #graph.f = grapher("female/characters", 20, token_filter2("noun", 1940, 2020, token.all))
  graph.f = grapher("female/characters", 20, token.all) 
  gr.f <- graph.f[[3]]
  gr.f <- gr.f[gr.f$names != "male/characters",]
  gr.f <- gr.f[1:21,]
  gr.f$rank = 1 : nrow(gr.f)
  gr.f$gender = "female"
  
  #bar plot
  ggplot(gr.f[1:20,], aes(reorder(names, -loglik), loglik)) +
    geom_bar(stat = "identity", fill = "black", color = "white", alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("co-occurring terms")
  ggsave("fv_wc.png")
  
  #merge male female and remove duples?
  gr.f
  #word cloud
  wordcloud(gr.f$names, as.integer(gr.f$loglik), rot.per = 0, colors = brewer.pal(4, "RdYlBu"))
 
  visIgraph(graph.f[[1]]) %>% visNodes(font = list(size = 28))
  
  #community structure
  graphf = graph.f[[1]] #store graph object
  visf <- toVisNetworkData(graphf) #create visnetwork object
  nodes <- visf$nodes #store nodes
  nodes <- nodes %>% select(-color) #remove color nodes
  edges <- visf$edges #store edges
  graphf = simplify(graphf, remove.loops = TRUE) #remove loops
  visf_comm <- cluster_fast_greedy(graphf) #find clusters using igraph
  modularity(visf_comm) #modularity
  visf_comm
  length(visf_comm)
  nodes$group <- membership(visf_comm) #store group info
  #no physics
  visf_graph <- visNetwork(nodes, edges, width = 1600, height = 900) %>% visPhysics(enabled = FALSE) %>% 
    visNodes(size = 8, font = c(size = 8)) %>% visEdges(color = c(opacity = 0.4))
  visf_graph
  visSave(visf_graph, "female_graph.html", selfcontained = TRUE, background = "white")
  
  graphf
  source("graphervf.R")
  source('grapherdemo.R')       
  #token filter 2 is all except
 graph = graphervf(15, token_filter2("noun", 1940, 2020, token.all))
 graph_plot = visIgraph(graph[[1]]) %>% visNodes(font = list(size = 28))
 graph_plot
 visSave(graph_plot, 'complete_plot.html')
 visIgraph(graph[[1]])
 
 
 source('grapherdemo.R')
 graph = grapherdemo(20, token_filter3('adj', 1940, 1950, token.all))
 #graph_plot = visIgraph(graph[[1]]) %>% visNodes(font = list(size = 28))
 #graph_plot
 female_primary = graph[[2]] #20 female primary nodes
 male_primary = graph[[3]] #20 male primary nodes
 g = graph[[1]] #graph
 is_weighted(graph[[1]])
 visIgraph(g) %>% visNodes(font = list(size = 24))
 
 #male
 dmat = distances(graph[[1]], v=V(graph[[1]]), to='male/characters')
 male_c = dmat[, 'male/characters'] 
 male_c = sort(male_c, decreasing = T)[1:20]
 
 #female
 fmat = distances(graph[[1]], v=V(graph[[1]]), to='female/characters')
 female_c = fmat[, 'female/characters'] 
 female_c = sort(female_c, decreasing = T)[1:20]
 allc = c(male_c, female_c)
 allc = sort(allc, decreasing = T)
 names(allc)
 #random
 mandf = intersect(names(female_c), names(male_c))
 allc[names(allc) %in% mandf]
 male_only = names(male_c[!names(male_c) %in% mandf])
 female_only = names(female_c[!names(female_c) %in% mandf])
 
 #network
 visIgraph(g)
 keep_nodes = names(c(allc, male_primary, female_primary))
 keep_nodes = c(keep_nodes, 'male/characters')
 remove_nodes = names(V(g))[!names(V(g)) %in% keep_nodes]
 #g <- g - remove_nodes
 
 #V(g)$color <- ifelse(V(g)$name == 'childhood/noun', 'red', 'grey')
 visIgraph(g)
 
 #edge colors
 all_edges = ends(g, es = E(g), names = T)
 all_edges = as.data.frame(all_edges)
 male_c = male_c[names(male_c != 'beach/noun')]
 malet_bool <- all_edges$V2 %in% names(male_c)  
 all_edges$V2[all_edges$V2 %in% names(male_c)  ]
 femalet_bool <- all_edges$V2 %in% names(female_c) 
 #mft_bool <- all_edges$V2 %in% names(mandf) 
 E(g)$color <- ifelse(malet_bool == TRUE, adjustcolor('cornflowerblue'),
                      ifelse(femalet_bool == TRUE, adjustcolor('orange'),
                             'grey'))
 
 edge.start <- ends(g, es = E(g), names = F)[,1]
 E(g)$color <-  ifelse(malet_bool == TRUE, V(g)$color[edge.start], adjustcolor('grey', alpha=0.4))
 
 mprimary_tropes = c('is/verb', 'friend/noun', 'takes/verb', 'tells/verb',
                     'kill/verb', 'agent/noun', 'help/noun', 
                     'brother/noun', 'former/adj')
 m_pcolor = paste('male/characters', mprimary_tropes)
 all_edges$V3 = paste(all_edges$V1, all_edges$V2)
 mp_bool = all_edges$V3 %in% m_pcolor
 
 fprimary_tropes = c('love/noun', 'marriage/noun', 'relationship/noun',
                     'tells/verb')
 f_pcolor = paste('female/characters', fprimary_tropes)
 all_edges$V3 = paste(all_edges$V1, all_edges$V2)
 all_edges$V1[all_edges$V2 == 'tells/verb']
 fp_bool = all_edges$V3 %in% f_pcolor
 
 E(g)$color <-  ifelse(mp_bool == TRUE, V(g)$color[edge.start], 
                          ifelse(fp_bool == TRUE, V(g)$color[edge.start],
                            ifelse(malet_bool == TRUE, V(g)$color[edge.start],
                              ifelse(femalet_bool == TRUE, V(g)$color[edge.start],       
                       adjustcolor('grey', alpha=0.4)))))

 all_edges$V3[malet_bool]
 
V(g)$color <- ifelse(V(g)$name == c('male/characters'), adjustcolor('cornflowerblue', alpha = 0.9),
                                 ifelse(V(g)$name %in% c('female/characters'), adjustcolor('orange', alpha = 0.9),
                                        ifelse(V(g)$name %in% c(intersect(mprimary_tropes, fprimary_tropes)), adjustcolor('purple', alpha = 0.9),
                                               ifelse(V(g)$name %in% mprimary_tropes, adjustcolor('cornflowerblue', alpha = 0.9),
                                                      ifelse(V(g)$name %in% fprimary_tropes, adjustcolor('orange', alpha = 0.9),
                                                             ifelse(V(g)$name %in% c(names(male_c), names(female_c)), adjustcolor('darkgrey', alpha = 0.9),
                                                             adjustcolor('grey', alpha = 0.2)))))))
 
 #V(g)$color <- when(V(g)$name %in% 'male/character', adjustcolor('red', alpha = 0.8))
 visIgraph(g)
 
 

 keep_nodes = names(c(allc, male_primary, female_primary))
 keep_nodes = c(keep_nodes, 'male/characters', 'female/characters')
 remove_nodes = names(V(g))[!names(V(g)) %in% keep_nodes]
 
 
 
 g_trim <- g - remove_nodes
 visIgraph(g_trim) %>% visNodes(font = list(size = 26))
 
 
 
 
 #### General + 1940 - 1970 #################################################
 
 source('grapherdemo.R')
 graph = grapherdemo(20, token_filter3('all', 2010, 2020, token.all)) #create graph
 female_primary = graph[[2]] #20 female primary nodes
 male_primary = graph[[3]] #20 male primary nodes
 g = graph[[1]] #save graph as g
 visIgraph(g) #display
 
 #Top secondary co-occurences
 #male
 dmat = distances(graph[[1]], v=V(graph[[1]]), to='male/characters') #compute path weights
 male_c = dmat[, 'male/characters'] #secondary to male
 male_c = sort(male_c, decreasing = T)[1:20] #sort top 20
 
 #female
 fmat = distances(graph[[1]], v=V(graph[[1]]), to='female/characters') #compute path weights
 female_c = fmat[, 'female/characters'] #secondary to male
 female_c = sort(female_c, decreasing = T)[1:20] #sort top 20
 
 #store all secondary
 allc = c(male_c, female_c) 
 allc = sort(allc, decreasing = T) #sort decreasing

 #why?
 mandf = intersect(names(female_c), names(male_c))
 allc[names(allc) %in% mandf]
 male_only = names(male_c[!names(male_c) %in% mandf])
 female_only = names(female_c[!names(female_c) %in% mandf])
 
 #edge colors
 all_edges = ends(g, es = E(g), names = T) #store all edges
 all_edges = as.data.frame(all_edges) #convert to dataframe
 
 #check 
 all_edges$V2[all_edges$V1 == 'female/characters']
 #male_c = male_c[names(male_c != 'beach/noun')]
 male.sec_bool <- all_edges$V2 %in% names(male_c)  #create bool of all male secondary co-oocs
 female.sec_bool <- all_edges$V2 %in% names(female_c)  #create bool of all female secondary co-oocs
  
 # E(g)$color <- ifelse(malet_bool == TRUE, adjustcolor('cornflowerblue'),
 #                      ifelse(femalet_bool == TRUE, adjustcolor('orange'),
 #                             'grey'))
 visIgraph(g) #display
 
 #color only male.sec and female.sec edges based on the start edge color
 edge.start <- ends(g, es = E(g), names = F)[,1]
 # E(g)$color <-  ifelse(male.sec_bool == TRUE, V(g)$color[edge.start], 
 #                       ifelse(female.sec_bool == TRUE, V(g)$color[edge.start],
 #                       adjustcolor('grey', alpha=0.4)))
 
 male_ps = intersect(all_edges$V1[male.sec_bool], names(male_primary))
 female_ps = intersect(all_edges$V1[female.sec_bool], names(female_primary))
 all_edges$V1[female.sec_bool]
 
 #color only primary tropes that have a path
 #mprimary_tropes = c('is/verb', 'friend/noun', 'takes/verb', 'tells/verb',
                     'kill/verb', 'agent/noun', 'help/noun', 
                     'brother/noun', 'former/adj')
 mprimary_tropes = male_ps
 mprimary_tropes = mprimary_tropes[mprimary_tropes != 'female/characters']
 m_pcolor = paste('male/characters', mprimary_tropes)
 all_edges$V3 = paste(all_edges$V1, all_edges$V2)
 mp_bool = all_edges$V3 %in% m_pcolor
 
 #fprimary_tropes = c('love/noun', 'marriage/noun', 'relationship/noun',
                     'tells/verb')
fprimary_tropes = female_ps
 f_pcolor = paste('female/characters', fprimary_tropes)
 all_edges$V3 = paste(all_edges$V1, all_edges$V2)
 all_edges$V1[all_edges$V2 == 'tells/verb']
 fp_bool = all_edges$V3 %in% f_pcolor
 
 E(g)$color <-  adjustcolor('grey', alpha=0.9)
 
 E(g)$color <-  ifelse(mp_bool == TRUE, V(g)$color[edge.start], 
                       ifelse(fp_bool == TRUE, V(g)$color[edge.start],
                              
                              ifelse(male.sec_bool == TRUE, V(g)$color[edge.start],
                                     ifelse(female.sec_bool == TRUE, V(g)$color[edge.start],       
                                            adjustcolor('grey', alpha=0.4)))))
 
 visIgraph(g)
 all_edges$V3[malet_bool]
 
 V(g)$color <- ifelse(V(g)$name == c('male/characters'), adjustcolor('cornflowerblue', alpha = 0.9),
                      ifelse(V(g)$name %in% c('female/characters'), adjustcolor('orange', alpha = 0.9),
                             ifelse(V(g)$name %in% c(intersect(mprimary_tropes, fprimary_tropes)), adjustcolor('purple', alpha = 0.9),
                                    ifelse(V(g)$name %in% mprimary_tropes, adjustcolor('cornflowerblue', alpha = 0.9),
                                           ifelse(V(g)$name %in% fprimary_tropes, adjustcolor('orange', alpha = 0.9),
                                                  ifelse(V(g)$name %in% c(names(male_c), names(female_c)), adjustcolor('darkgrey', alpha = 0.9),
                                                         adjustcolor('grey', alpha = 0.2)))))))
 
 #V(g)$color <- when(V(g)$name %in% 'male/character', adjustcolor('red', alpha = 0.8))
 visIgraph(g)
 
 
 
 keep_nodes = names(c(allc, male_primary, female_primary))
 keep_nodes = c(keep_nodes, 'male/characters', 'female/characters')
 remove_nodes = names(V(g))[!names(V(g)) %in% keep_nodes]
 
 
 
 g_trim <- g - remove_nodes
 visIgraph(g_trim) %>% visNodes(font = list(size = 26))
 
 
 male_c
 
 
 
 
 #ignore
 ### Uni
 source('grapher.r')
 token.test = token.all %>% tokens_remove('female/characters')
 graph = grapher('male/characters', 20, token_filter3("all", 1940, 1950, token.test))
 graph_plot = visIgraph(graph[[1]]) %>% visNodes(font = list(size = 28))
 g = graph[[1]]
 
 fem_rem = names(ego(g, order = 2, 'female/characters')[[1]])
 mem = names(ego(g, order = 2, 'male/characters')[[1]])
 mem = mem[mem != 'female/characters']
 fem_rem = fem_rem[!fem_rem %in% mem]
 g = g - fem_rem
 visIgraph(g)
 V(g)$color <- ifelse(V(g)$name == 'twin/adj', 'red', 'grey')
# 
 