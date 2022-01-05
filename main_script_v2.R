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
token.all = token.all %>% tokens_remove(c('ex/adj', 'ex/noun'))

#sample based on min in a decade
set.seed(42)
token.all = tokens_sample(token.all, size = 22638, replace = FALSE, prob = NULL, by = decade)



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
  
  
#2. double words - PPMI across decades 
i = 0
gender = 'female'
term1 = 'marriage/noun'
term2 = 'proposal/noun'
plot_word <- function(term1, term2, gender){
male = data.frame()
for(i in 0 : 7){ #for loop to run across decades
  male_temp <- data.frame()
  j = 1940 + 10*i

  male_temp = grapher(paste(gender,'/characters', sep=''), 10 , token_filter('all', j, token.all), "LOGLIK")[[3]][] #get PPMI data for given decade
  #male_ind$rank = 1 : nrow(male_ind) #rank words - redundant
  male_temp <- male_temp %>% filter(names == term1) #filter term given
  male_temp$year = j #attach year info
  male_temp$gender = "male" #assign gender
  names(male_temp)[2] = 'll1'
  male_ind = male_temp
  
  male_temp = grapher(term1, 10, token_filter('all', j, token.all), "LOGLIK")[[3]][] #get PPMI data for given decade
  male_temp <- male_temp %>% filter(names == term2) #filter term given
  names(male_temp)[2] = 'll2'
  male_temp$names = ifelse(is.na(male_temp$names), 'term2', male_temp$names)
  male_ind = cbind(male_temp, male_ind)
  male = rbind(male, male_ind)
  # #same for females
  # j = 1940 + 10*i
  # female_ind = grapher("female/characters", 10 ,token_filter(pos, j, token.all), "LOGLIK")[[3]][]
  # female_ind$rank = 1 : nrow(female_ind)
  # female_ind <- female_ind %>% filter(names == term)
  # female_ind$year = j
  # female_ind$gender = "female"

  #bind to overall data
  #all_ind <- ifelse(gender == 'male', rbind(all_ind, male_ind), 
             #       rbind(all_ind, male_ind))
}
male$ll = male$ll1 + male$ll2
male = male %>% select(year, ll)

#check significance
ancova.word <- lm(ll~year, data = male)
R2 = round(summary(ancova.word)[[8]], 2)
p = round(anova(ancova.word)[[5]][1], 2)
label1 = paste('R^2 == ', R2, sep = '')
label2 = paste('p == ', p, sep = '')
label1
label2
#plot 
ggplot(male, aes(x = year, y = ll)) +
  geom_point(color = "black") + 
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = TRUE, size = 1, alpha = 0.1) + theme_minimal() +
  ylab("Loglikelihood Ratio") + ggtitle(paste(gender, term1, term2, sep = '-')) +
  theme(axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12), legend.title = element_text(color = "black", size = 14),
        ) +
        annotate('text', label = label1, x = 1950, y = 190, parse = TRUE) +
        annotate('text', label = label2, x = 1950, y = 175, parse = TRUE)
  #facet_wrap(~ gender)

#panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3)
}

plot_word('relationship/noun', 'sexual/adj', 'female')
ggsave("wife_children.png", width = 6, height = 4)

#check significance
ancova.word <- lm(loglik~year*gender, data = all_ind)
summary(ancova.word)
anova(ancova.word)

#check significance - females
all_ind_fem <- all_ind %>% filter(gender == "female")
ancova.word <- lm(loglik~year, data = all_ind_fem)
summary(ancova.word)
anova(ancova.word) 

######### Individual words ############

plot_word_single <- function(term, gender){
  male = data.frame()
  for(i in 0 : 7){ #for loop to run across decades
    male_temp <- data.frame()
    j = 1940 + 10*i
    male_temp = grapher(paste(gender,'/characters', sep=''), 10 , token_filter('all', j, token.all), "LOGLIK")[[3]][] #get PPMI data for given decade
    #male_ind$rank = 1 : nrow(male_ind) #rank words - redundant
    male_temp <- male_temp %>% filter(names == term) #filter term given
    male_temp$year = j #attach year info
    male_temp$gender = "male" #assign gender
    names(male_temp)[2] = 'll'
    male_ind = male_temp
    male = rbind(male, male_ind)
  }
  male = male %>% select(year, ll)
  
  #check significance
  ancova.word <- lm(ll~year, data = male)
  R2 = round(summary(ancova.word)[[8]], 2)
  p = round(anova(ancova.word)[[5]][1], 2)
  label1 = paste('R^2 == ', R2, sep = '')
  label2 = paste('p == ', p, sep = '')
  print(term)
  print(label1)
  print(label2)
  #plot 
  ggplot(male, aes(x = year, y = ll)) +
    geom_point(color = "black") + 
    geom_line(size = 1) +
    geom_smooth(method = "lm", se = TRUE, size = 1, alpha = 0.1) + theme_minimal() +
    ylab("Loglikelihood Ratio") + ggtitle(term) +
    theme(axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14),
          legend.text = element_text(color = "black", size = 12), legend.title = element_text(color = "black", size = 14),
    ) #+
    #annotate('text', label = label1, x = 1950, y = 75, parse = TRUE) +
    #annotate('text', label = label2, x = 1950, y = 65, parse = TRUE)
}

#write a loop for saving multiple plots
male_noun <- c('agent/noun', 'boss/noun', 'murder/noun', 'leader/noun', 'office/noun', 
               'attorney/noun', 'fight/noun', 'owner/noun', 'assistant/noun')
female_noun <- c('love/noun', 'girlfriend/noun', 'wife/noun', 'relationship/noun', 'affair/noun', 
                 'marriage/noun', 'wedding/noun', 'date/noun')


male_verb <- c('kill/verb', 'led/verb', 'confronts/verb')
female_verb <- c('marry/verb', 'dating/verb', 'attracted/verb', 'loves/verb')

male_adj <- c('wealthy/adj', 'best/adj', 'corrupt/adj', 'suspicious/adj', 'criminal/adj')
female_adj <- c('pregnant/adj', 'married/adj', 'beautiful/adj', 'romantic/adj', 'attractive/adj')

for(i in female_adj){
plot_word_single(i, 'female')
ggsave(paste(str_split(i, '/')[[1]][1], '.png', sep=''), width = 6, height = 4)
}

############################################################

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
 
 #### Path Analysis #################################################
 
 source('grapherdemo.R')
 graph = grapherdemo(11, token_filter3('noun', 1940, 2020, token.all)) #create graph
 #graph = grapher('male/characters', 21, token.all)
 female_primary = graph[[2]] #20 female primary nodes
 male_primary = graph[[3]] #20 male primary nodes
 g = graph[[1]] #save graph as g
 visIgraph(g) %>% visNodes(font = list(size = 26))  #display
 #g = simplify(g)
 
 #Top secondary co-occurences
 #male
 dmat = distances(g, v='male/characters', to='beach/noun', weights = NA) #compute path weights
 male_c = dmat['male/characters', ] #secondary to male
 male_c = sort(male_c, decreasing = T)[1:20] #sort top 20
 
 #female
 fmat = distances(graph[[1]], v=V(graph[[1]]), to='female/characters') #compute path weights
 female_c = fmat[, 'female/characters'] #secondary to male
 female_c = sort(female_c, decreasing = T)[1:20] #sort top 20
 
 
 
#######function to return top most significant tropes################################
 #find shortest paths in unweighted graph to all grey nodes
 #find grey nodes
 top_tropes <- function(gender = 'male/characters'){
 all_secondary = V(g)$name[!V(g)$name %in% c(names(male_primary), names(female_primary))]
 all_secondary = all_secondary[!all_secondary %in% c('male/characters', 'female/characters')]
 filter =  ifelse(gender == 'male/characters', 'female/characters', 'male/characters')
 #find all shortest paths
 a = shortest_paths(
   g,
   from = gender,
   to = all_secondary,
   weights = NA
 )
 
 l = data.frame()
 
 #filter paths that go through females
 for(i in 1:length(a$vpath)){
 l_temp = data.frame(start = (a$vpath[[i]]$name)[1], mid = (a$vpath[[i]]$name)[2], end = (a$vpath[[i]]$name)[3])
 l = rbind(l, l_temp)
 }
 
 l = l %>% filter(mid != filter)
 
 #find weights of all these paths
 
 for(i in 1:nrow(l)){
 l[i, 4] = sum(E(g, path = c(l[i,1], l[i,2], l[i,3]))$weight)
 }
 #head(l)
 l = arrange(l, desc(V4))
 l <- distinct(l)
 return(l)
 }
 
 ## clean up text
 
 top_male = top_tropes('male/characters')
 top_female = top_tropes('female/characters')
 
 top_male <- top_male %>% filter(mid %in% names(male_primary))
 top_female <- top_female %>% filter(mid %in% names(female_primary))
 
 top_male <- top_male %>% group_by(mid) %>% slice(which.max(V4))
 top_female <- top_female %>% group_by(mid) %>% slice(which.max(V4))

 top_male <- arrange(top_male, desc(V4))
 top_female <- arrange(top_female, desc(V4))[1:20,]
 
 ####### stacked bar plot of paths ######################
 top_male$path = paste(top_male$start, top_male$mid, top_male$end, sep = '--')
 names(top_male)[4] = 'llr'
 top_female$path = paste(top_female$start, top_female$mid, top_female$end, sep = '--')
 names(top_female)[4] = 'llr'

 mtt = ggplot(top_male, aes(x = reorder(path, llr), y = llr)) +
   geom_bar(stat = 'identity', fill = 'deepskyblue4', 
            alpha = 0.7, color = 'black', width = 0.8) + coord_flip() +
   xlab('word') + ylab('loglikelihood ratio') + theme_linedraw() +
   ggtitle('Male')
 
 ftt = ggplot(top_female, aes(x = reorder(path, -llr), y = llr)) +
   geom_bar(stat = 'identity', fill = 'darkorange3', 
            alpha = 0.7, color = 'black', width = 0.8) + coord_flip() +
   xlab('word') + ylab('loglikelihood ratio') + theme_linedraw() +
   ggtitle('Female')
 
 mtt
 ggsave('male_tropes.png', width = 8, height = 5)
 ftt
 ggsave('female_tropes.png', width = 8, height = 5)
 ####################################### Network
 #COLORING THESE EDGES
 all_edges = ends(g, es = E(g), names = T) #store all edges
 all_edges = as.data.frame(all_edges) #convert to dataframe
 head(all_edges)
 
 all_edges$V3 = paste(all_edges$V1, all_edges$V2, sep='-')
 head(all_edges)
 top_male$sec <- paste(top_male$mid, top_male$end, sep='-')
 top_male$pri <- paste(top_male$start, top_male$mid, sep='-')
 
 top_female$sec <- paste(top_female$mid, top_female$end, sep='-')
 top_female$pri <- paste(top_female$start, top_female$mid, sep='-')
 
 all_top = rbind(top_male, top_female)
 
 edge.start <- ends(g, es = E(g), names = F)[,1]
 
 E(g)$color <-  ifelse(all_edges$V3 %in% c(top_female$pri, 'wife/noun-female/characters', 'girlfriend/noun-female/characters', 'named/verb-female/characters', 'meets/verb-female/characters'), 
                       adjustcolor('orange', alpha=0.8), 
                          ifelse(all_edges$V3 %in% top_male$pri,
                              adjustcolor('cornflowerblue', alpha=0.8),
                                  ifelse(all_edges$V3 %in% all_top$sec,
                                      V(g)$color[edge.start],
                                          adjustcolor('grey', alpha=0.4))))
 visIgraph(g)
 
 all_edges$V3[all_edges$V3 == 'wife/noun-female/characters']
 
 
 #for visualization##############################
 
 top_male <- top_male[1:10,]
 top_female <- top_female[1:10,]
 all_top <- rbind(top_male, top_female)
 to_trim <- V(g)$name[!V(g)$name %in% c(all_top$end, 'male/characters', 'female/characters', all_top$mid)]
 g_trim = g - to_trim
 
 visIgraph(g_trim) %>% visNodes(font = list(size = 28))
 ###############################################
 
