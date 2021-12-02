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
#Wiedemann, Gregor; Niekler, Andreas (2017): Hands-on: A five day text mining course for humanists and social scientists in R. Proceedings of the 1st Workshop on Teaching NLP for Digital Humanities (Teach4DH@GSCL 2017), Berlin.

source("rawcounts.R") #find raw counts of co-occurrences
source("token_filter.R") #filter tokens

#load tokens, get it ready for analysis
load("token.all.RData")
#convert tokens to all lower
token.all <- tokens_tolower(token.all) #convert all tokens to lower

#create a token set with only generalized pos info
pos_replace <- function(toks.replace){
  toks.replace <- toks.replace %>% 
    tokens_replace(pattern = c("*/NOUN", "*/VERB", "*/ADJ"), replacement = c("NOUN", "VERB", "ADJ"))
  return(toks.replace)
}
token.pos <- pos_replace(token.all) 

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
    geom_point(size = 1.3) + geom_smooth(method = "lm", aes(fill = gender), alpha = 0.1) +
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
    geom_point(size = 1.2) + geom_smooth(method = "lm", aes(fill = gender), alpha = 0.1) +
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
    geom_line(size = 0.8) + theme_minimal() + ggtitle("Probability of co-occurence with Nouns") + geom_smooth(method = "lm", aes(fill = gender), alpha = 0.1) +
    theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
          legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
  ggsave("noun.png")
  
  #find significance
  ancova.noun <- aov(p ~ year*gender, data = p_decdat[pos == "noun",])
  summary(ancova.noun)
  
#2. Individual words - PPMI across decades 

all_ind <- data.frame() #initialise
term <- "led/verb" #term to find PPMI for
pos <- "verb" #pos of word 
for(i in 0 : 7){ #for loop to run across decades
  j = 1940 + 10*i
  male_ind = grapher("male/characters", 10 ,token_filter(pos, j, token.all), "MI")[[3]][] #get PPMI data for given decade
  male_ind$rank = 1 : nrow(male_ind) #rank words - redundant
  male_ind <- male_ind %>% filter(names == term) #filter term given
  male_ind$year = j #attach year info
  male_ind$gender = "male" #assign gender
  
  #same for females
  j = 1940 + 10*i
  female_ind = grapher("female/characters", 10 ,token_filter(pos, j, token.all), "MI")[[3]][]
  female_ind$rank = 1 : nrow(female_ind)
  female_ind <- female_ind %>% filter(names == term)
  female_ind$year = j
  female_ind$gender = "female"

  #bind to overall data
  all_ind <- rbind(all_ind, male_ind, female_ind)
}

#plot 
ggplot(all_ind, aes(x = year, y = loglik, color = gender)) +
  geom_point(color = "black") + 
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = TRUE, size = 1, aes(fill = gender), alpha = 0.1) + theme_minimal() +
  ylab("Pointwise Mutual Information") + ggtitle("Loves/Verb") +
  theme(axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12), legend.title = element_text(color = "black", size = 14),
        panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
ggsave("loves_verb.png", width = 6, height = 4)

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
male.perm <- data.frame() #initialise
  graph.m = grapher("male/characters", 12 ,token_filter2("noun", 1940, 2010, token.all)) #extract graph info
  gr.m <- graph.m[[3]] #pass graph object
  gr.m <- gr.m[gr.m$names != "female/characters",] #filter out female characters
  gr.m <- gr.m[1:20,] #filter 20 
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
  
  #network
  visIgraph(graph.m[[1]]) %>% visNodes(font = list(size = 28))
  
#female - same as above
  
  female.perm <- data.frame()
  graph.f = grapher("female/characters", 12 ,token_filter2("noun", 1940, 2010, token.all))
  gr.f <- graph.f[[3]]
  gr.f <- gr.f[gr.f$names != "male/characters",]
  gr.f <- gr.f[1:20,]
  gr.f$rank = 1 : nrow(gr.f)
  gr.f$gender = "female"
  
  #network
  visIgraph(graph.f[[1]]) %>% visNodes(font = list(size = 28))
  
  #bar plot
  ggplot(gr.f[1:20,], aes(reorder(names, -loglik), loglik)) +
    geom_bar(stat = "identity", fill = "black", color = "white", alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("co-occurring terms")
  ggsave("fv_wc.png")
  
  #word cloud
  wordcloud(gr.f$names, as.integer(gr.f$loglik), rot.per = 0, colors = brewer.pal(4, "RdYlBu"))
 

  
  