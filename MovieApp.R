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

#-----------------------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  shinythemes::themeSelector(),
  theme = bslib::bs_theme(bootswatch = "superhero"),

  
    checkboxInput(
      inputId = "themeToggle",
      label = icon("sun")),

  

  
  # Application title
  titlePanel(tags$img(src = "MyImage.jpg", height = 200, width = 200)),
  
  # Tabset pannel to choose from each tab
  tabsetPanel(
    id = "tabset",
    tabPanel("PeliDaily", icon = icon("calendar"), ),
    tabPanel("PeliRecommend",
             icon = icon("random"),
             fluid = TRUE,
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 h3("MOVIES BASED ON YOUR CRITERIA", align = "center", style = "color:cyan"),
                 br(),
                 actionBttn("compute", "SHOW ME A MOVIE", icon = icon("film"), color = "success", style = "fill"), align = "center",
                 br(),
                 br(),
                 br(),
                 selectInput("genre_1", "GENRES 1", c("All","Action","Adventure","Animation","Comedy","Crime","Documentary","Drama","Family","Fantasy","Foreign","History","Horror","Music","Mystery","Romance","Science Fiction","Thriller","TV Movie","War","Western"
                 ), selected = NULL, multiple = FALSE, width = NULL),
                 br(),
                 selectInput("genre_2", "GENRES 2", c("All","Action","Adventure","Animation","Comedy","Crime","Documentary","Drama","Family","Fantasy","Foreign","History","Horror","Music","Mystery","Romance","Science Fiction","Thriller","TV Movie","War","Western"
                 ), selected = NULL, multiple = FALSE, width = NULL),
                 br(),
                 sliderInput("vote_average", "MINIMUM RATING", min = 0, max = 10, value = 0),
                 br(),
                 dateInput("date_before", "FILMS BEFORE", value = "2017-02-03", format = "dd-mm-yyyy"),
                 br(),
                 dateInput("date_after", "FILMS AFTER", value = "1916-09-04", format = "dd-mm-yyyy"),
                 br(),
                 sliderInput("runtime", "MAXIMUM LENGTH (MINUTES)", min = 0, max = 340, value = 340),
                 br(),
                 selectInput("original_language", "ORIGINAL LANGUAGE",
                             c("All", "Afrikaans" = "af","Arabic" = "ar","Catalan" = "cn","Chinese" = "zh","Czech" = "cs","Danish" = "da","Dutch" = "nl","English" = "en","French" = "fr","German" = "de","Greek" = "el","Hebrew" = "he","Hindi" = "hi","Hungarian" = "hu","Icelandic" = "is","Indonesian" = "id","Italian" = "it","Japanese" = "ja","Korean" = "ko","Kyrgyz" = "ky","Norwegian Bokmål" = "nb","Norwegian" = "no","Pashto" = "ps","Persian" = "fa","Polish" = "pl","Portuguese" = "pt","Romanian" = "ro","Russian" = "ru","Slovenian" = "sl","Spanish" = "es","Swedish" = "sv","Tamil" = "ta","Telugu" = "te","Thai" = "th","Turkish" = "tr","Vietnamese" = "vi"
                             ),
                             selected = NULL, multiple = FALSE, width = NULL
                 ),
                 br(),
                 selectInput("spoken_languages", "DUBBED LANGUAGE",
                             c(
                               "All", "English", "Español", "Français", "Italiano", "Deutsch", "Türkçe", "e<U+03BB><U+03BB><U+03B7><U+03BD><U+03B9><U+03BA><U+03AC>","<U+666E><U+901A><U+8BDD>", "<U+0E20><U+0E32><U+0E29><U+0E32><U+0E44><U+0E17><U+0E22>", "Íslenska", "P<U+0443><U+0441><U+0441><U+043A><U+0438><U+0439>", "svenska", "Româna", "<U+65E5><U+672C><U+8A9E>", "Latin",
                               "<U+0939><U+093F><U+0928><U+094D><U+0926><U+0940>", "Português", "<U+0641><U+0627><U+0631><U+0633><U+06CC>", "<U+0627><U+0631><U+062F><U+0648>", "<U+0627><U+0644><U+0639><U+0631><U+0628><U+064A><U+0629>", "Ceský", "<U+5E7F><U+5DDE><U+8BDD> / <U+5EE3><U+5DDE><U+8A71>","<U+D55C><U+AD6D><U+C5B4>/<U+C870><U+C120><U+B9D0>", "Norsk", "<U+0BA4><U+0BAE><U+0BBF><U+0BB4><U+0BCD>",
                               "<U+05E2><U+05B4><U+05D1><U+05B0><U+05E8><U+05B4><U+05D9><U+05EA>", "Dansk", "Nederlands", "Afrikaans", "Gaeilge","Somali", "suomi", "Kiswahili", "<U+0431><U+044A><U+043B><U+0433><U+0430><U+0440><U+0441><U+043A><U+0438> <U+0435><U+0437><U+0438><U+043A>", "Ti<U+1EBF>ng Vi<U+1EC7>t", "Magyar", "<U+0423><U+043A><U+0440><U+0430><U+0457><U+043D><U+0441><U+044C><U+043A><U+0438><U+0439>",
                               "Esperanto", "Polski", "<U+0A2A><U+0A70><U+0A1C><U+0A3E><U+0A2C><U+0A40>", "Eesti", "shqip", "Srpski", "Bosanski", "Hrvatski", "Slovencina", "<U+049B><U+0430><U+0437><U+0430><U+049B>","<U+0C24><U+0C46><U+0C32><U+0C41><U+0C17><U+0C41>", "Cymraeg", "Wolof", "isiZulu", "<U+067E><U+069A><U+062A><U+0648>", "No Language", "Galego", 
                               "<U+10E5><U+10D0><U+10E0><U+10D7><U+10E3><U+10DA><U+10D8>","<U+09AC><U+09BE><U+0982><U+09B2><U+09BE>", "Català", "Bahasa indonesia", "Bamanankan", "Slovenšcina"
                             ),
                             selected = NULL, multiple = FALSE, width = NULL
                 )
               ),
               mainPanel(withSpinner(tableOutput("recommend")))
             )
    ),
    tabPanel("PeliSearch",
             icon = icon("search"),
             fluid = TRUE,
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 h3("MOVIE SEARCH", align = "center", style = "color:cyan"),
                 br(),
                 actionBttn("compute_2", "SHOW ME THE MOVIES", icon = icon("film"), color = "success", style = "fill"), align = "center",
                 br(),
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
                 ), selected = NULL, multiple = FALSE, width = NULL)
               ),
               mainPanel(withSpinner(tableOutput("search")))
             )
    ),
    tabPanel("PeliPopCorn", icon = icon("fire")),
    tabPanel("PeliBoring", icon = icon("info"))
  )
)

#---------------------------------------------------------------------------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Find data
  df <- data_find("https://raw.githubusercontent.com/rengalv/Movies-Data-Analysis-Grab-a-Popcorn/master/tmdb_5000_movies.csv")
  
  # Use data_filter function from functions_final_project.R to filter with criteria
  data_recommended <- eventReactive(input$compute, {
    df_new_recommend <- data_recommend(
      df = df,
      genre_1 = input$genre_1,
      genre_2 = input$genre_2,
      vote_average_input = input$vote_average,
      date_before = input$date_before,
      date_after = input$date_after,
      runtime_input = input$runtime,
      original_language_input = input$original_language,
      spoken_languages_input = input$spoken_languages
    )
    
    if (nrow(df_new_recommend) == 0) {
      shinyalert(
        title = "No movies found",
        text = "Please choose other criteria"
      )
      updateSelectInput(session, "genre_1", selected = "All")
      updateSelectInput(session, "genre_2", selected = "All")
      updateSliderInput(session, "vote_average", value = 0)
      updateDateInput(session, "date_before", value = "2017-02-03")
      updateDateInput(session, "date_after", value = "1916-09-04")
      updateSliderInput(session, "runtime", value = 340)
      updateSelectInput(session, "original_language", selected = "All")
      updateSelectInput(session, "spoken_languages", selected = "All")
    }
    return(df_new_recommend)
  })
  
  # Prints out table (temporary)
  output$recommend <- renderTable({
    choose_movie(data_recommended())
  })
  
  
  # Tab 2 --> Search
  
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
    
    if (nrow(df_new_search) == 0) {
      shinyalert(
        title = "No movies found",
        text = "Please choose other criteria"
      )
      updateTextInput(session, "title", value = "")
      updateTextInput(session, "key_words_1", value = "")
      updateTextInput(session, "key_words_2", value = "")
      updateTextInput(session, "key_words_3", value = "")
      updateSelectInput(session, "country", selected = "All")
      updateSliderInput(session, "popularity", value = 0)
    }
    
    return(df_new_search)
  })
  
  # Prints out table
  output$search <- renderTable({
    data_searched()
  })
  
  # When we change tab it removes plot
  observeEvent(input$tabset, {
    # reset values of criteria and wipe table
  })
}

#-------------------------------------------------------------------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
