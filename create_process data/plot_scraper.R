##### Function to scrape plots from Wikipedia #####

# extracts film plots over the entire decade
# returns string of extracted plots

plot_scraper <- function(decade = 1940, n = 200){ #declare function
  s <- character() #initialize string to store plots
  for(j in 0 : 9){ #for loop to run for 10 years in decade
    year_full = decade + j #year initialized to decade start in main script
    print(year_full)
    s_ind <- character()
    
    #create url for scraping - attach year to Wikipedia URL
    url_start <- "https://en.wikipedia.org"
    url_mid <- "/wiki/List_of_American_films_of_"
    year_start = substr(year_full, 1, 2) #get first two numbers of year
    year_end = substr(year_full, 3, 3) #get third number
    url_end <- paste(year_start, paste(year_end, j, sep = ""), sep = "") #paste them together along with j
    url <- paste(url_start, url_mid, url_end, sep = "") #paste everything together for complete URL
    
    #extract plot info
    page <- read_html(url) #read html from url
    #access the links for different movies in that particular year
    links <- html_nodes(page, "table.wikitable td i a")
    #extract hyperlinks to movie main page
    links.href <- html_attr(links, "href")
    #create accessible URLs to each of the films
    plot.links <- paste("https://en.wikipedia.org", links.href, sep = "") 
    
    #detect and delete dead/red links and buggy links (important)
    plot.links <- plot.links[!plot.links %>% str_detect("redlink", negate = FALSE)]
    plot.links <- plot.links[!plot.links %>% str_detect("Fly_by_Night", negate = FALSE)]
    plot.links <- plot.links[!plot.links %>% str_detect("Monolith", negate = FALSE)]
    plot.links <- plot.links[!plot.links %>% str_detect("The_Cruel_Path", negate = FALSE)]
    
    #take a random sample of size n from obtained links
    if(length(plot.links) > n){plot.links = sample(plot.links, n)} 
    #initialize string to hold plots
    plot <- character()
    #extract plots from each individual Wikipedia page for the movies
    for(i in 1 : n){
      #take only things under the plot heading
      plot[i] <- plot.links[i]
      if(!is.na(plot[i])){
        plot[i] <- plot.links[i] %>% read_html() %>%
          html_nodes(xpath = '//p[preceding::h2[1][contains(.,"Plot")]]') %>%
          html_text() %>%  paste(collapse = "\n")
        }
      else{
        plot[i] <- ""
      }
      print(i)
    }
    #remove plots with no info - some movies don't have a plot section
    #mostly the less popular ones
    s_ind <- plot[plot != ""] #store all non-empty plots for the year into a new variable
    s <- c(s, s_ind) #bind the string for individual year with the string for the entire decade
  }
  return(s)
}
