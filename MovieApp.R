library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(shinycssloaders)
library(shinyalert)
library(bslib)


source(file = "functions_final_project.R", local = T)

options(spinner.color = "#0dc5c1", spinner.size = 1.4, spinner.type = 6)


# Show all the possible dubs of movies
df <- data_find("https://raw.githubusercontent.com/rengalv/Movies-Data-Analysis-Grab-a-Popcorn/master/tmdb_5000_movies.csv")

list_spoken_languages <- unlist(strsplit(df$spoken_languages, ","))
list_spoken_languages <- list_spoken_languages[!duplicated(list_spoken_languages)]
list_spoken_languages <- append("All", list_spoken_languages)

# Show all the possible genres of movies
list_genres <- unlist(strsplit(df$genres, ","))
list_genres <- list_genres[!duplicated(list_genres)]
list_genres <- append("All", list_genres)


# We used this to have the lists of... to be able to manipulate them (not directly) :

# Show all the possible original languages
# list_languages <- movies_df[!duplicated(movies_df$original_language), ]
# list_languages = list_languages %>%
#   select(original_language)
# unique(list_languages$original_language)

# Show all the possible countries of production
# list_country <- unlist(strsplit(movies_df$production_countries,","))
# list_country <- list_country[!duplicated(list_country)]
#  print(list_country)

#-----------------------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  shinythemes::themeSelector(),
  theme = bslib::bs_theme(bootswatch = "superhero"),
  checkboxInput(
    inputId = "themeToggle",
    label = icon("sun")
  ),

  # Application title
  titlePanel(tags$img(src = "MyImage.jpg", height = 200, width = 200)),

  # Tabset pannel to choose from each tab
  tabsetPanel(
    id = "tabset",
    tabPanel("PeliDaily", icon = icon("calendar"), 
             mainPanel(
               textOutput("daily_text"),
               tags$head(tags$style("#daily_text{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
               withSpinner(tableOutput("daily")))
             ),
    tabPanel("PeliRecommend",
      icon = icon("random"),
      fluid = TRUE,
      sidebarLayout(
        position = "right",
        sidebarPanel(
          align = "center",
          h3("MOVIES BASED ON YOUR CRITERIA", align = "center", style = "color:cyan"),
          br(),
          actionBttn("compute", "SHOW ME A MOVIE", icon = icon("film"), color = "success", style = "fill"),
          br(),
          br(),
          br(),
          selectInput("genre_1", "GENRES 1", list_genres, selected = NULL, multiple = FALSE, width = NULL),
          br(),
          selectInput("genre_2", "GENRES 2", list_genres, selected = NULL, multiple = FALSE, width = NULL),
          br(),
          sliderInput("vote_average", "MINIMUM RATING", min = 0, max = 10, value = 0),
          br(),
          dateRangeInput("date", "DATE", end = "2017-02-03", start = "1916-09-04", min = "1916-09-04", max = "2017-02-03",format = "dd-mm-yyyy", startview = "decade"),
          br(),
          sliderInput("runtime", "MAXIMUM LENGTH (MINUTES)", min = 0, max = 340, value = 340),
          br(),
          selectInput("original_language", "ORIGINAL LANGUAGE",
            c("All", "Afrikaans" = "af", "Arabic" = "ar", "Catalan" = "cn", "Chinese" = "zh", "Czech" = "cs", "Danish" = "da", "Dutch" = "nl", "English" = "en", "French" = "fr", "German" = "de", "Greek" = "el", "Hebrew" = "he", "Hindi" = "hi", "Hungarian" = "hu", "Icelandic" = "is", "Indonesian" = "id", "Italian" = "it", "Japanese" = "ja", "Korean" = "ko", "Kyrgyz" = "ky", "Norwegian Bokmål" = "nb", "Norwegian" = "no", "Pashto" = "ps", "Persian" = "fa", "Polish" = "pl", "Portuguese" = "pt", "Romanian" = "ro", "Russian" = "ru", "Slovenian" = "sl", "Spanish" = "es", "Swedish" = "sv", "Tamil" = "ta", "Telugu" = "te", "Thai" = "th", "Turkish" = "tr", "Vietnamese" = "vi"),
            selected = NULL, multiple = FALSE, width = NULL
          ),
          br(),
          selectInput("spoken_languages", "DUBBED LANGUAGE", list_spoken_languages, selected = NULL, multiple = FALSE, width = NULL),
          br(),
          actionBttn("clear_1", "CLEAR", icon = icon("ban"), color = "danger", style = "fill")
        ),
        mainPanel(
          textOutput("recommend_text"),
          tags$head(tags$style("#recommend_text{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          withSpinner(tableOutput("recommend")))
      )
    ),
    tabPanel("PeliSearch",
      icon = icon("search"),
      fluid = TRUE,
      sidebarLayout(
        position = "right", 
        sidebarPanel(
          align = "center",
          h3("MOVIE SEARCH", align = "center", style = "color:cyan"),
          br(),
          actionBttn("compute_2", "SHOW ME THE MOVIES", icon = icon("film"), color = "success", style = "fill"),
          br(),
          br(),
          h4("OFFICIAL TITLE"),
          textInput(inputId = "title", label = "", value = ""),
          br(),
          textInput(inputId = "key_words_1", label = "KEY WORDS 1", value = ""),
          br(),
          textInput(inputId = "key_words_2", label = "KEY WORD 2", value = ""),
          br(),
          textInput(inputId = "key_words_3", label = "KEY WORD 3", value = ""),
          br(),
          sliderInput("popularity", "MINIMUM POPULARITY", min = 0, max = 150, value = 0),
          br(),
          selectInput("country", "PRODUCTION COUNTRY", list(
            "All",
            "Africa" = c("Algeria", "Angola", "Cameroon", "Egypt", "Kenya", "Libyan Arab Jamahiriya", "Morocco", "Nigeria", "South Africa", "Tunisia"),
            "America" = c("Argentina", "Aruba", "Bahamas", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", "Dominica", "Dominican Republic", "Ecuador", "Guadaloupe", "Guyana", "Jamaica", "Mexico", "Panama", "Peru", "United States of America"),
            "Asia-Oceania" = c("Afghanistan", "Australia", "Bhutan", "Cambodia", "China", "Fiji", "Hong Kong", "India", "Indonesia", "Iran", "Israel", "Japan", "Jordan", "Kazakhstan", "Kyrgyz Republic", "Lebanon", "Malaysia", "New Zealand", "Pakistan", "Philippines", "Singapore", "South Korea", "Taiwan", "Thailand", "Turkey", "United Arab Emirates"),
            "Europe" = c("Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Lithuania", "Luxembourg", "Malta", "Monaco", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Serbia and Montenegro", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
          ), selected = NULL, multiple = FALSE, width = NULL),
          br(),
          actionBttn("clear_2", "CLEAR", icon = icon("ban"), color = "danger", style = "fill")
        ),
        mainPanel(
          textOutput("search_text"),
          tags$head(tags$style("#search_text{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          withSpinner(tableOutput("search"))
          )
      )
    ),
    tabPanel("PeliPopCorn", icon = icon("fire"),
             mainPanel(
               style = "font-family: 'Comic Sans MS';",
               h2("POP CORN RECIPE !!!!", align = "center"),
               h4("Ingredients : "),
               p(""),
               p("- 2 tbsp vegetable oil"),
               p("- 100g popcorn kernels"),
               p("- 250g caster sugar"),
               p("- 50g salted butter , cubed"),
               h3("Method", align = "center"),
               h4("STEP 1"),
               p("Put the oil in a large saucepan with a tight-fitting lid over a medium heat. Toss the popcorn kernels in the oil to coat. Put the lid on, and keep over a medium heat until you hear the first popcorn pop, then turn the heat to medium-low. When you begin to hear lots of popping, give the pan a shake. Continue to shake frequently until the popping stops. Turn off the heat and leave in the pan."),
               p(""),
               p(""),
               h4("STEP 2"),
               p("Line a large baking tray with baking parchment. Put the sugar and 60ml water into a medium heavy-based saucepan and bring to the boil. Stir until the sugar has dissolved, then leave over a medium heat, without stirring, for 6-8 mins. It should start to turn into a golden caramel, swirl it around and add the butter - stand back as it may spit a little. Stir well until combined."),
               p(""),
               p(""),
               h4("STEP 3"),
               p("Pour the caramel over the popcorn in the pan and stir immediately to coat the popcorn, being careful not to touch the hot caramel. Carefully transfer onto the lined baking tray and press down with the back of a spoon to spread evenly. Leave to cool for 5 mins, then break apart and eat. "),
               h2("ENJOY !!!!", align = "center"),
               p(""),
               div(img(src="https://cdn.dribbble.com/users/953617/screenshots/10404379/media/1402a0bc576dc70b2ba1785ef44194c2.png", height = 500, width = 700), align = "center"),
               p(""),
               p("")
             )),
    tabPanel("PeliData", icon = icon("info"))
  )
)

#---------------------------------------------------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Tab 1 --> Daily 
  output$daily <- renderTable({date <- as.POSIXct(Sys.Date(), format = "%y/%m/%d")
  numeric_date <- as.numeric(date)
  daily_movie <- daily_find(
    date = numeric_date,
    df = df
  )
  return(daily_movie)
  })
  
  #Title of daily table
  output$daily_text <- renderText({
    paste0("RECOMMENDATION OF THE ", format(Sys.Date(), "%d-%m-%Y"))
  })
  
  
  
  
  # Tab 2 --> recommend 
  
  # Use data_filter function from functions_final_project.R to filter with criteria
  data_recommended <- eventReactive(input$compute, {
    df_new_recommend <- data_recommend(
      df = df,
      genre_1 = input$genre_1,
      genre_2 = input$genre_2,
      vote_average_input = input$vote_average,
      date_before = input$date[2],
      date_after = input$date[1],
      runtime_input = input$runtime,
      original_language_input = input$original_language,
      spoken_languages_input = input$spoken_languages
    )

    if (nrow(df_new_recommend) == 0) {
      shinyalert(
        title = "No movies found",
        text = "Please choose other criteria"
      )
    }
    return(df_new_recommend)
  })

  # Prints out table (temporary)
  output$recommend <- renderTable({
    choose_movie(data_recommended())
  })
  
  #Clear button creation
  observeEvent(input$clear_1,{
    updateSelectInput(session, "genre_1", selected = "All")
    updateSelectInput(session, "genre_2", selected = "All")
    updateSliderInput(session, "vote_average", value = 0)
    updateDateRangeInput(session, "date", end = "2017-02-03", start = "1916-09-04")
    updateSliderInput(session, "runtime", value = 340)
    updateSelectInput(session, "original_language", selected = "All")
    updateSelectInput(session, "spoken_languages", selected = "All")
  })
  
  #Title of recommend table
  output$recommend_text <- renderText({
    paste0("SELECTED FROM ", nrow(data_recommended()), " POSSIBILITIES")
  })


  
  
  # Tab 3 --> Search

  # Use data_search function from functions_final_project.R to filter search
  data_searched <- eventReactive(input$compute_2, {
    df_new_search <- data_search(
      df = df,
      title_input = str_to_title(input$title),
      key_input_1 = tolower(input$key_words_1),
      key_input_2 = tolower(input$key_words_2),
      key_input_3 = tolower(input$key_words_3),
      production_country_input = input$country,
      popularity_input = input$popularity
    )
  
    #If empty data found from criteria, then display message and wipe criteria
    if (nrow(df_new_search) == 0) {
      shinyalert(
        title = "No movies found",
        text = "Please choose other criteria"
      )
    }
    
    return(df_new_search)
  })

  # Prints out table
  output$search <- renderTable({
    data_searched()
  })
  
  #Title of search table
  output$search_text <- renderText({
    paste0("WE FOUND ", nrow(data_searched()), " MOVIES")
  })
  
  #Clear button creation
  observeEvent(input$clear_2,{
    updateTextInput(session, "title", value = "")
    updateTextInput(session, "key_words_1", value = "")
    updateTextInput(session, "key_words_2", value = "")
    updateTextInput(session, "key_words_3", value = "")
    updateSelectInput(session, "country", selected = "All")
    updateSliderInput(session, "popularity", value = 0)
  })

  
  
  
 # Tab 4 --> Popcorn
  


  
  
}

#-------------------------------------------------------------------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
