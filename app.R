
library(shiny)

source(file = "functions_final_project.R", local = T)


ui <- fluidPage(
  
  # Application title
  titlePanel("EL PELIROSTER"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("genre_1", "GENRES 1", c("All", "Action", "Adventure", "Fantasy", "Science Fiction", "Crime", "Drama", "Thriller", 
                                           "Animation", "Family", "Western", "Comedy", "Romance", "Horror", "Mystery", "History", "War",
                                           "Music", "Documentary", "Foreign", "TV Movie"), selected = NULL, multiple = FALSE, width = NULL),
      selectInput("genre_2", "GENRES 2", c("All", "Action", "Adventure", "Fantasy", "Science Fiction", "Crime", "Drama", "Thriller", 
                                           "Animation", "Family", "Western", "Comedy", "Romance", "Horror", "Mystery", "History", "War", 
                                           "Music", "Documentary", "Foreign", "TV Movie"), selected = NULL, multiple = FALSE, width = NULL),
      sliderInput("vote_average", "AVERAGE RATING", min = 0, max = 10, value = 5),
      sliderInput("popularity", "POPULARITY", min = 0, max = 150, value = 75),
      sliderInput("runtime", "HOW LONG (IN MINUTES)", min = 0, max = 340, value = 170),
      dateInput("date_before", "FILMS BEFORE", value = "2017-02-03"),
      dateInput("date_after", "FILMS AFTER", value = "1916-09-04"),
      selectInput("original_language", "ORIGINAL LANGUAGE", 
                  c("All", "en", "ja", "fr", "zh", "es", "de", "hi", "ru", "ko", "te", "cn", "it", "nl", "ta", "sv", "th", "da", "xx", "hu", "cs",
                    "pt", "is", "tr", "nb", "af", "pl", "he", "ar", "vi", "ky", "id", "ro", "fa", "no", "sl", "ps", "el"
                    ), selected = NULL, multiple = FALSE, width = NULL),
      selectInput("spoken_languages", "DUBBED LANGUAGE", 
                  c("All", "English", "Español", "Français", "Italiano", "Deutsch", "Türkçe", "ελληνικά", "普通话", "ภาษาไทย", "Íslenska",  "Pусский", "svenska",
                    "Română", "日本語", "Latin", "हिन्दी", "Português", "فارسی" , "اردو"  ,"العربية" , "Český", "广州话 / 廣州話",  "한국어/조선말" ,"Norsk", "தமிழ்",
                    "עִבְרִית", "Dansk", "Nederlands", "Afrikaans", "Gaeilge", "Somali", "suomi", "Kiswahili", "български език", "Tiếng Việt", "Magyar", "Український",
                    "Esperanto" , "Polski", "ਪੰਜਾਬੀ", "Eesti","shqip","Srpski","Bosanski","Hrvatski","Slovenčina","қазақ","తెలుగు", "Cymraeg","Wolof","isiZulu",
                    "پښتو", "No Language",  "Galego","ქართული"  , "বাংলা" , "Català", "Bahasa indonesia" , "Bamanankan" ,"Slovenščina"  
                    ), selected = NULL, multiple = FALSE, width = NULL),
      actionButton("compute", "CONFIRM CRITERIA")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("RECOMMENDATIONS", tableOutput("peli")),
        tabPanel("RANDOMIZER"),
      ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_recommend <- eventReactive(input$compute,{
    data_filter(
      genre_1 = input$genre_1,
      genre_2 = input$genre_2, 
      vote_average_input = input$vote_average,
      popularity_input = input$popularity, 
      date_before = input$date_before,
      date_after = input$date_after,
      runtime_input = input$runtime,
      original_language_input = input$original_language,
      spoken_languages_input = input$spoken_languages)
  })
  
  output$peli <- renderTable({
    data_recommend()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
