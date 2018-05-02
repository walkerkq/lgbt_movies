######### PACKAGES
library(dplyr)
library(rvest)
library(reshape2)
library(magrittr)

########## FUNCTIONS
table_melt <- function(df, num, value_name){
  for(i in c(2:6)) df[,i] <- sapply(df[,i], function(x) strsplit(x, " ")[[1]][num])
  colnames(df)[1] <- "Gender"
  df <- melt(df, id="Gender")
  df$value <- as.numeric(gsub("\\,|\\\n", "", df$value))
  colnames(df)[2:3] <- c("Age", value_name)
  return(df)
}

########## DATA
# read in IMDb ids for movies listed on Wikipedia
# https://en.wikipedia.org/wiki/List_of_films_with_LGBT_characters
ids <- read.csv("wiki_movie_ids.csv", stringsAsFactors=F) 
ids$input <- "lgbt"

# create a weighted sample to mirror the year distribution of LGBT movies
set.seed(1234)
x <- density(ids$input_year)
weights <- data.frame(x=x$x, ww=x$y) %>% mutate(x = round(x)) 
weights <- aggregate(ww ~ x, weights, mean)

# read in data from a previous project, sample 300 movies
ids2 <- read.csv("https://raw.githubusercontent.com/walkerkq/fan_favorite_actors/master/movieInfo.csv", stringsAsFactors=F) %>%
  mutate(imdbVotes = as.numeric(gsub("\\,", "", imdbVotes)),
         id = paste("/title/", ID, "/", sep=""),
         input = "control",
         Year = as.numeric(Year)) %>%
  left_join(weights, by=c("Year" = "x")) %>%
  subset(imdbVotes > 2000 & !id %in% ids$id & Type %in% "movie") %>%
  sample_n(size=300, replace=FALSE, weight=ww) %>%
  select(c("Title", "Year", "id", "input")) %>%
  set_colnames(c("input_title", "input_year", "id", "input"))
ids <- rbind(ids, ids2)

# read in Wikipedia character data
wiki_chars <- read.csv("wiki_lgbt_movies.csv", stringsAsFactors=F)

########### SCRAPE IMDb
# get IMDb data for each ID
movies_unique <- NULL
characters_unique <- NULL
characters_flat <- NULL
rating_demo_info <- NULL

for(m in seq_along(ids$id)){
  URL <- paste("http://www.imdb.com", ids$id[m], sep="")
  output <- read_html(URL)
  
  # get overall movie info
  title <- output %>% html_nodes(xpath="//h1[@itemprop='name']") %>% html_text()
  title <- strsplit(title, "\\s\\(")[[1]][1]
  imdb_rating <- output %>% html_nodes(xpath="//span[@itemprop='ratingValue']") %>% html_text()
  mpaa_rating <- output %>% html_nodes(xpath="//meta[@itemprop='contentRating']") %>% html_attr("content")
  mpaa_rating <- ifelse(length(mpaa_rating)==0, NA, mpaa_rating)
  num_votes <- output %>% html_nodes(xpath="//span[@itemprop='ratingCount']") %>% html_text()
  num_votes <- as.numeric(gsub("\\,", "", num_votes))
  release_date <- output %>% html_nodes(xpath="//meta[@itemprop='datePublished']") %>% html_attr("content")
  genre <- output %>% html_nodes(xpath="//span[@itemprop='genre']") %>% html_text()
  genre <- paste(genre, collapse=", ")
  m_unique <-  data.frame(id=ids$id[m], title, input=ids$input[m], release_date=release_date[1], mpaa_rating, imdb_rating, num_votes,
                          genre_drama = ifelse(grepl("Drama", genre), 1, 0),
                          genre_comedy = ifelse(grepl("Comedy", genre), 1, 0),
                          genre_action = ifelse(grepl("Action", genre), 1, 0),
                          genre_romance = ifelse(grepl("Romance", genre), 1, 0),
                          genre_adventure = ifelse(grepl("Adventure|Fantasy", genre), 1, 0),
                          genre_thriller = ifelse(grepl("Crime|Thriller|Mystery|Horror", genre), 1, 0), 
                          stringsAsFactors=F)
  movies_unique <- rbind(movies_unique, m_unique)
  
  # get character billing info
  if(ids$input[m] %in% "lgbt"){
    cast <- output %>% html_nodes(xpath="//table[@class='cast_list']//tr[@class='odd']|//table[@class='cast_list']//tr[@class='even']") %>% html_text() 
    cast <- data.frame(cast, num=1:length(cast), stringsAsFactors=F)
    lgbt <- subset(wiki_chars, title %in% ids$input_title[m] & year == ids$input_year[m])
    for(l in seq_along(lgbt[,1])){
      match <- cast[grepl(lgbt$actor[l], cast$cast), ]
      if(length(match[,1])==0){ match <- cast[grepl(lgbt$character[l], cast$cast), ]} 
      if(length(match[,1])==0){ num <- NA } else { num <- match$num }
      lgbt$billing[l] <- num
    }
    c_unique <- data.frame(id=ids$id[m], lgbt[,c(3:6)], stringsAsFactors=F)
    characters_unique <- rbind(characters_unique, c_unique)
  }
  
  # get rating info too
  URL2 <- paste(URL, "ratings", sep="")
  output2 <- read_html(URL2) %>% html_nodes(xpath="//table") %>% html_table()
  suppressWarnings(rate <- table_melt(output2[[2]], 1, "Rating"))
  count <- table_melt(output2[[2]], 49, "Count")
  age_dist <- merge(rate, count, by=c("Gender", "Age"), all=T)
  age_dist$id <- ids$id[m]
  
  # save rating info
  rating_demo_info <- rbind(rating_demo_info, age_dist)

  # provide progress bar
  print(m)
}

# Save the data
#write.csv(characters_unique, "lgbt_characters.csv", row.names=F)
#write.csv(movies_unique, "movies_imdb.csv", row.names=F)
#write.csv(rating_demo_info, "movie_ratings.csv", row.names=F)