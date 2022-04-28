#Checking if the package is installed on your PC. ELse installing it or you!!
packages <- c("readr", "jsonlite","tidyr","dplyr","DT","knitr","ggplot2","ggpubr","lubridate","plotly")
for (i in packages){
  if( i %in% rownames(installed.packages()) == FALSE) 
  {install.packages(i)
  }
}

#Loading the required packages
library(readr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(DT)
library(knitr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(plotly)
library(kableExtra)

#URL to read the data from
url <- "https://raw.githubusercontent.com/rengalv/Movies-Data-Analysis-Grab-a-Popcorn/master/tmdb_5000_movies.csv"

#Reading the csv file from the URL
movies <- read_csv(url,col_names = TRUE,na = "NA")

#Preview of the data dimensions and column names
dim(movies)

#Examining the column names in the dataset
colnames(movies)

movies <- movies[!duplicated(movies$title), ]
dim(movies)

#Takes in the dataframe and column name to be trnasformed
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
movies_new <- movies_1 %>%
  full_join(genres_df, by = c("id", "title")) %>%
  full_join(keywords_df, by = c("id", "title")) %>%
  full_join(prod_cntry_df, by = c("id", "title")) %>%
  full_join(prod_cmpny_df, by = c("id", "title")) %>%
  full_join(spoken_lang_df, by = c("id", "title"))

#Have a look at the final dataset
glimpse(movies_new)

size <- dim(movies_new)

complete_data <- sum(complete.cases(movies_new))

movies_new <- select(movies_new, title, everything())
datatable(movies_new , options = list(pageLength = 5, scrollX = "200px"))

col <- data.frame(sapply(movies_new, class)) #Gettting the class of each column
Row_names <- rownames(col) #Getting the row names which ae the columns of our dataset
class <- col[,1] 

Data_types <- cbind(Column = Row_names, Class = as.character(class)) 
Data_types <- Data_types[2:nrow(Data_types),] #Removing the first entry alone because it is for the row number

kable(Data_types) %>%
  kable_styling(bootstrap_options = "striped")#Displays the table as shown below
