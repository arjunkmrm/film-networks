##### Token filter functions 1, 2, 3 #####
#each token filter has a slight difference - please read through code
#annotation to note key difference

#main token filter - filter for particular decade
token_filter <- function(pos = "adj", year = 1940, toks.filter){
  rm = c("*/noun", "*/adj", "*/verb")
  pos.temp <- paste("*/", tolower(pos), sep = "")
  rm = rm[rm != pos.temp]
  #gender <- paste(gender,"/characters", sep = "")
  rm = c(rm)
  
  toks.filter <- toks.filter %>% tokens_subset(decade == year) 
  
  if(pos != "all"){
  toks.filter <- toks.filter %>% 
    #tokens_replace(pattern = c("*/NOUN", "*/VERB", "*/ADJ"), replacement = c("VERB", "NOUN", "ADJ")) %>% 
    tokens_remove(pattern = rm)
  }
  
  return(toks.filter)
}

#token filter 2 - filter across decade range, exclude given pos
token_filter2 <- function(pos = "noun", year_start = 1940, year_end = 1960, toks.filter){
  rm <- paste("*/", tolower(pos), sep = "")
  if(pos != "all"){
    toks.filter <- toks.filter %>% 
      tokens_remove(pattern = rm)
  }
  toks.filter <- toks.filter %>% tokens_subset(decade >= year_start) %>% tokens_subset(decade < year_end) 
  

#token filter 3 - filter across decade range, filter only given pos
token_filter3 <- function(pos = "adj", year_start = 1940, year_end = 1960, toks.filter){
  rm = c("*/noun", "*/adj", "*/verb")
  pos.temp <- paste("*/", tolower(pos), sep = "")
  rm = rm[rm != pos.temp]
  rm = c(rm)
  if(pos != "all"){
    toks.filter <- toks.filter %>% 
      tokens_remove(pattern = rm)
  }
  toks.filter <- toks.filter %>% tokens_subset(decade >= year_start) %>% tokens_subset(decade < year_end) 
  return(toks.filter)
}

###############


 return(toks.filter)
}
