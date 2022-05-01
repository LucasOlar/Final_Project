
#Loading the required packages
library(readr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(DT)
library(knitr)
library(ggplot2)
library(ggpubr)
library(tibble)

rm(list = ls())

#We want to create a function that filters based on : 2 genres, vote_average, popularity, 2 release_date, runtime, original language, spoken languages
# colnames(movies_df)

#Show all the possible original languages
# list_languages <- movies_df[!duplicated(movies_df$original_language), ]
# list_languages = list_languages %>% 
#   select(original_language)
# unique(list_languages$original_language)

#Show all the possible genres of movies 
# list_genres <- unlist(strsplit(movies_df$genres,","))
# list_genres <- list_genres[!duplicated(list_genres)]
# print(list_genres)

#Show all the possible spoken languages
# list_spoken_languages <- unlist(strsplit(movies_df$spoken_languages,","))
# list_spoken_languages <- list_spoken_languages[!duplicated(list_spoken_languages)]
# print(list_spoken_languages)



data_filter = function(genre_1, genre_2, vote_average_input, popularity_input, date_before, 
                       date_after, runtime_input, original_language_input, spoken_languages_input){
  
  url <- "https://raw.githubusercontent.com/rengalv/Movies-Data-Analysis-Grab-a-Popcorn/master/tmdb_5000_movies.csv"

  #Reading the csv file from the URL
  movies <- read_csv(url,col_names = TRUE,na = "NA")

  movies <- movies[!duplicated(movies$title), ]

  #Takes in the dataframe and column name to be transformed
  json_to_df <- function(df, column){
    column_1 <- df[apply(df[,column],1,nchar)>2,]  #Checks if there is an entry
  
    list_1 <- lapply(column_1[[column]], fromJSON) #Converts the JSON to a list
    values <- data.frame(unlist(lapply(list_1, function(x) paste(x$name,collapse = ",")))) #Collapsing all the values of the list as a key value pair
  
    final_df <- cbind(column_1$id, column_1$title, values) #new data frame with the key and values a s columns
    names(final_df)  <- c("id", "title", column)
    return(final_df)
  }

  genres_df <- json_to_df(movies, "genres")
  keywords_df <- json_to_df(movies, "keywords")
  prod_cntry_df <- json_to_df(movies, "production_countries")
  prod_cmpny_df <- json_to_df(movies, "production_companies")
  spoken_lang_df <- json_to_df(movies, "spoken_languages")


  #Subset the movies dataframe by removing the JSON columns
  movies_1 <- subset(movies, select =  -c(genres,keywords,production_companies, production_countries,spoken_languages))
  
  #Join the columns from all the generated dataframes from previous step
  movies_df <- movies_1 %>%
    full_join(genres_df, by = c("id", "title")) %>%
    full_join(keywords_df, by = c("id", "title")) %>%
    full_join(prod_cntry_df, by = c("id", "title")) %>%
    full_join(prod_cmpny_df, by = c("id", "title")) %>%
    full_join(spoken_lang_df, by = c("id", "title"))
  
  #Here we say if user chooses all original languages, then we will pick all the possible languages
  if(original_language_input == "All"){
    original_language_input <- ""
  }
  
  #Here we say if user chooses all original languages, then we will pick all the possible languages
  if(spoken_languages_input == "All"){
    spoken_languages_input <- ""
  }
      
  #Here we say if user chooses all genres, then we will pick all the possible genres
  if(genre_1 == "All" | genre_2 == "All"){
    genre_1 <- ""
    genre_2 <- ""
  }
  
  movies_recommendation = movies_df %>% 
    filter(status == "Released") %>%
    filter(grepl(pattern = genre_1, x = genres)) %>%
    filter(grepl(pattern = genre_2, x = genres)) %>%
    filter(vote_average >= vote_average_input) %>%
    filter(popularity >= popularity_input) %>%
    filter(release_date <= date_before) %>%
    filter(release_date >= date_after) %>%
    filter(runtime <= runtime_input) %>%
    filter(grepl(pattern = original_language_input, x = original_language)) %>%
    filter(grepl(pattern = spoken_languages_input, x = spoken_languages))
    
return(movies_recommendation)
}


#Now we want to make another function that selects a film at random form this list 
choose_movie = function(df){
  random_number = round(runif(1, min = 1, max = nrow(df)), 0)
  random_movie = df[random_number,]
  return(random_movie)
}

