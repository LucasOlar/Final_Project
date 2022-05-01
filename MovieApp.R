
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)

source(file = "functions_final_project.R", local = T)



ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "darkly"),
  
  # Application title
  titlePanel("EL PELIROSTER"),
  
  # Tabset pannel to choose from each tab
  tabsetPanel(
    tabPanel("PeliRecommend", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
            selectInput("genre_1", "GENRES 1", c("All", "Action", "Adventure", "Fantasy", "Science Fiction", "Crime", "Drama", "Thriller", 
                                                 "Animation", "Family", "Western", "Comedy", "Romance", "Horror", "Mystery", "History", "War",
                                                 "Music", "Documentary", "Foreign", "TV Movie"), selected = NULL, multiple = FALSE, width = NULL),
            br(),
            selectInput("genre_2", "GENRES 2", c("All", "Action", "Adventure", "Fantasy", "Science Fiction", "Crime", "Drama", "Thriller", 
                                                 "Animation", "Family", "Western", "Comedy", "Romance", "Horror", "Mystery", "History", "War", 
                                                 "Music", "Documentary", "Foreign", "TV Movie"), selected = NULL, multiple = FALSE, width = NULL),
            br(),
            sliderInput("vote_average", "MINIMUM RATING", min = 0, max = 10, value = 0),
            br(),
            sliderInput("popularity", "POPULARITY", min = 0, max = 150, value = 0),
            br(),
            sliderInput("runtime", "MAXIMUM LENGTH (MINUTES)", min = 0, max = 340, value = 340),
            br(),
            dateInput("date_before", "FILMS BEFORE", value = "2017-02-03", format = "dd-mm-yyyy"),
            br(),
            dateInput("date_after", "FILMS AFTER", value = "1916-09-04", format = "dd-mm-yyyy"),
            br(),
            selectInput("original_language", "ORIGINAL LANGUAGE", 
                        c("All", "English" = "en", "Japanese" = "ja", "French" = "fr", "Chinese" = "zh", "Spanish" = "es", "German" = "de", 
                          "Hindi" = "hi", "Russian" = "ru", "Korean" = "ko", "Telugu" = "te", "Catalan" = "cn", "Italian" = "it", "Dutch" = "nl", 
                          "Tamil" = "ta", "Swedish" = "sv", "Thai" = "th", "Danish" = "da", "Hungarian" = "hu", "Czech" = "cs", "Portuguese" = "pt",
                          "Icelandic" = "is", "Turkish" = "tr", "Norwegian Bokmål" = "nb", "Afrikaans" = "af", "Polish" = "pl", "Hebrew" = "he", 
                          "Arabic" = "ar", "Vietnamese" = "vi", "Kyrgyz" = "ky", "Indonesian" = "id", "Romanian" = "ro", "Persian" = "fa", 
                          "Norwegian" = "no", "Slovenian" = "sl", "Pashto"= "ps", "Greek" = "el"
                          ), selected = NULL, multiple = FALSE, width = NULL),
            br(),
            selectInput("spoken_languages", "DUBBED LANGUAGE", 
                        c("All", "English", "Español", "Français", "Italiano", "Deutsch", "Türkçe", "ελληνικά",
                          "普通话", "ภาษาไทย", "Íslenska",  "Pусский", "svenska", "Română", "日本語", "Latin", 
                          "हिन्दी", "Português", "فارسی" , "اردو"  ,"العربية" , "Český", "广州话 / 廣州話",  
                          "한국어/조선말" ,"Norsk", "தமிழ்", "עִבְרִית", "Dansk", "Nederlands", "Afrikaans", "Gaeilge", 
                          "Somali", "suomi", "Kiswahili", "български език", "Tiếng Việt", "Magyar", "Український",
                          "Esperanto" , "Polski", "ਪੰਜਾਬੀ", "Eesti","shqip","Srpski","Bosanski","Hrvatski","Slovenčina","қазақ", 
                          "తెలుగు", "Cymraeg","Wolof","isiZulu", "پښتو", "No Language",  "Galego","ქართული"  , 
                          "বাংলা" , "Català", "Bahasa indonesia" , "Bamanankan" ,"Slovenščina"
                          ), selected = NULL, multiple = FALSE, width = NULL),
            br(),
            selectInput("country", "PRODUCTION COUNTRY", c("All", "United States of America", "United Kingdom", "Jamaica", "Bahamas", "Dominica", 
                                                                     "Czech Republic", "Poland", "Slovenia", "New Zealand", "Germany", "China", 
                                                                     "Canada", "Italy", "Japan", "Malta", "Australia", "France", "Belgium", "India",
                                                                     "Netherlands", "Spain", "United Arab Emirates", "Hong Kong", "Taiwan", "Ireland",
                                                                     "Morocco", "Hungary", "Singapore", "Norway", "Sweden", "South Africa", "Russia",
                                                                     "Romania", "Mexico", "Monaco", "Switzerland", "Pakistan", "Malaysia", "Finland",
                                                                     "Iceland", "Denmark", "Tunisia", "Philippines", "Bulgaria", "South Korea", "Brazil",
                                                                     "Peru", "Luxembourg", "Bosnia and Herzegovina", "Kazakhstan", "Portugal", "Aruba",
                                                                     "Libyan Arab Jamahiriya", "Serbia", "Ukraine", "Chile", "Argentina", "Panama",
                                                                     "Austria", "Greece", "Lithuania", "Cambodia", "Thailand", "Slovakia", "Israel",
                                                                     "Fiji", "Serbia and Montenegro", "Turkey", "Nigeria", "Cyprus", "Jordan", "Bolivia",
                                                                     "Ecuador", "Colombia","Egypt","Bhutan", "Lebanon", "Kyrgyz Republic", "Algeria",
                                                                     "Indonesia", "Guyana", "Iran", "Guadaloupe", "Afghanistan","Angola",
                                                                     "Dominican Republic", "Cameroon","Kenya"  
                                                                     ), selected = NULL, multiple = FALSE, width = NULL), 
            br(),
            actionBttn("compute", "SHOW ME A MOVIE", icon = icon("film"), color = "success", style = "fill")),
        
        mainPanel(tableOutput("recommend"))
      )),
    
  tabPanel("PeliSearch", fluid = TRUE,
    sidebarLayout(
       sidebarPanel(
          textInput("title", "OFFICIAL TITLE SEARCH"),
          textInput("key_words", "KEY WORDS"),
          actionBttn("compute_2", "SHOW ME THE MOVIES", icon = icon("film"), color = "success", style = "fill")),
       
       mainPanel(tableOutput("search"))
    )),
  
  tabPanel("PeliRandom")
  )
)


#--------------------------------------------------------------------------------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Find data
  df <- data_find("https://raw.githubusercontent.com/rengalv/Movies-Data-Analysis-Grab-a-Popcorn/master/tmdb_5000_movies.csv")
  
  #Use data_filter function from functions_final_project.R to filter with criteria 
  data_recommended <- eventReactive(input$compute,{
    data_recommend(
      df = df,
      genre_1 = input$genre_1,
      genre_2 = input$genre_2, 
      vote_average_input = input$vote_average,
      popularity_input = input$popularity, 
      date_before = input$date_before,
      date_after = input$date_after,
      runtime_input = input$runtime,
      original_language_input = input$original_language,
      spoken_languages_input = input$spoken_languages, 
      production_country_input = input$country)
  })
  
  #Prints out table (temporary)
  output$recommend <- renderTable({
    choose_movie(data_recommended())
  })


#Tab 2 --> Search
  
  #Use data_search function from functions_final_project.R to filter search 
  data_searched <- eventReactive(input$compute_2,{
    data_search(
      df = df,
      title_input = str_to_title(input$title),
      key_input = input$key_words)
  })

  #Prints out table 
  output$search <- renderTable({
    data_searched()
  })
  
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)


