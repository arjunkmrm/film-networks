##### Function to tokenise given strings #####

film_tokenizer <- function(plot_string, metadata){ #declare function
  
  #convert string into text corpus for cleaning
  print("converting into corpus") #print status
  s <- corpus(pllot_strings, docvars = metadata) #make a corpus object and attach doc variables (year info)
  s <- corpus_reshape(s, to = "sentences") #split it into sentences
  docvars_complete <- docvars(s)   #count number of sentences - useful later when assigning docvars to spacy object
  
  #parse it into tokens using spacy - this is where the magic happens
  print("starting parse using spacy") #print status
  toks.spacy <- spacy_parse(s) %>%
    entity_consolidate() %>% #this combines single entities into one unit
    #by replacing ' ' with '_' e.g. John Locke becomes John_Locke
    as.tokens(include_pos = "pos") #include parts of speech information
  
  #extract entities person entities from the text i.e. movie characters
  print("tokens intitial parse") #print status
  ents.spacy <- spacy_parse(s, entity = TRUE) %>% 
    entity_extract(concatenator = "_") %>% #extract entities
    filter(entity_type == "PERSON") %>% #filter persons
    distinct(entity) #find distrinct persons i.e. remove duplicates
  
  #extract gender information
  print("starting gender extract") #print status
  ents.spacy.temp <- ents.spacy %>% mutate(entity = str_replace_all(entity, "[_]", " ")) #replace '_' with ' ' for gender extraction
  print("finding given names") #print status
  #find given names
  givenNames = findGivenNames(ents.spacy.temp$entity, progress = FALSE, apikey = 
  "31d8c048c93f385ba2de144836d8d0f5") #identify given names
  #find gender from given names
  gender_data = genderize(ents.spacy.temp$entity, genderDB = givenNames, progress = FALSE)
  #store all entities
  entity.all <- gender_data %>% mutate(name = ents.spacy$entity) %>% select(name, gender) #keep only required columns
  
  #filter gender entities
  print("gender extraction complete") #print status
  entity.male <- entity.all %>% filter(gender == "male") #filter male entities
  entity.female <- entity.all %>% filter(gender == "female") #filter female entities
  n.males <- nrow(entity.male) #count male entites
  n.female <- nrow(entity.female) #count female entities
  #create string to use for replacing individual character names to general term
  m.repl <- rep("Male/CHARACTERS", n.males) #males
  f.repl <- rep("Female/CHARACTERS", n.female) #females
 
  #create final tokens
   print("creating tokens") #print status
    toks.all <- toks.spacy %>% 
    tokens_select(pattern = c("*/NOUN", "*/VERB", "*/ENTITY", "*/ADJ")) %>% #select only nouns, verbs, adjectives, entities
    tokens_replace(pattern = paste(entity.male$name, "/ENTITY", sep = ""), replacement = m.repl) %>% #replce ind. characters with genereal term - male
    tokens_replace(pattern = paste(entity.female$name, "/ENTITY", sep = ""), replacement = f.repl) %>% #replce ind. characters with genereal term - female
    tokens_remove(c("", "'s", "-", "ex", "-/NOUN", "*/ENTITY", "-/ADJ", "-/VERB")) #remove buggy tokens
  
  print("done") #print status
  toks.all$decade = docvars_complete #assign decade to tokens
  return(toks.all)
}