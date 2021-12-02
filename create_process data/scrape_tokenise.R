##### script to extract plots from Wikipedia and tokenise them #####

#connect your python to the environment where you installed spacy
reticulate::use_virtualenv("~/spacynlp-env", required = TRUE)	
reticulate::py_config() #check whether configuration is right

#load libraries
library(spacyr) #for NLP
spacy_initialize(model = "en_core_web_sm") #spacy language model
library(rvest) #for scraping
library(tidyverse) 
library(quanteda) #for text cleaning
library(igraph) #for creating graphs
library(visNetwork) #for visualizing graphs
library(genderizeR) #for assigning gender

#load function scripts
source("plot_scraper.R") #scrape plots
source("film_tokenizer.R") #tokenise films
source("film_gender_counts.R") #find number of unique male and female characters in each decade

#scrape all plots across all decades
s_all.i <- character() #declare string to hold all plot info
year_plots <- data.frame() #declare data frame to hold info on number of plots scraped per year

for(i in seq(from=1940, to=2010, by=10)){ #run for loop to get each decade from 1940 to 2010
  print(paste(i, "start", sep = " - ")) #print status
  s_this.i <- plot_scraper(i, 200) #plot 200 movies from every year, across the entire decade i
  s_all.i <- c(s_all.i, s_this.i) #merge with string for overall data
  n_plots = length(s_this.i) #find number of plots in particular decade
  year_plots.temp <- data.frame(year = as.character(i), times = n_plots) #organise
  year_plots <- rbind(year_plots, year_plots.temp) #bind to overall data for plots/decade info
}
year_plots.final <- year_plots #actually redundant
s_docvars <- rep(year_plots.final$year, times = year_plots.final$times) #create docvars to use for tokenising (year info for each plot)

#save stuff
save(s_all.i, file = "s_all.i.RData") #imp
save(s_docvars, file = "s_docvars.RData") #imp
save(year_plots.final, file = "number_decades.RData")

#tokenise data
token.all <- film_tokenizer(s_all.i, s_docvars)
#save tokens
save(token.all, file = "token.all.RData")
