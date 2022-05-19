library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(shinycssloaders)
library(shinyalert)
library(bslib)
library(thematic)
library(showtext)

#To reference the other functions file 
source(file = "functions_final_project.R", local = T)

#To create spinner for charging graph and table
options(spinner.color = "#0dc5c1", spinner.size = 1.4, spinner.type = 6)

#To create a button that closes the app
jscode <- "shinyjs.closeWindow = function() {window.close();}"

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
    # list_languages <- df[!duplicated(df$original_language), ]
    # list_languages = list_languages %>%
    #   select(original_language)
    # unique(list_languages$original_language)
  
  # Show all the possible countries of production
    # list_country <- unlist(strsplit(df$production_countries,","))
    # list_country <- list_country[!duplicated(list_country)]
    #  print(list_country)


#setup the bslib theme object for dark/light switch mode
light <- bs_theme(bootswatch = "cerulean")
dark <- bs_theme(bootswatch = "superhero")
thematic_shiny(fg = "auto",
               bg = "auto",
               accent = "auto",
               font = "auto",
               inherit = TRUE)

# #creation table pour les categories
df_haloween <- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "gore", x= keywords)) %>%
  filter(grepl(pattern = "Horror", x= genres)) #%>%

best_haloween <- df_haloween[order(-df_haloween$vote_average),] %>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )


#Saint valentin

df_love <- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "love", x= keywords)) %>%
  filter(grepl(pattern = "Romance", x= genres))

best_love <- df_love[order(-df_love$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#Noel en famille

df_christmas<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "christmas", x= keywords)) %>%
  filter(grepl(pattern = "Family", x= genres))

best_christmas <- df_christmas[order(-df_christmas$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#invasion des aliens

df_alien<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "alien", x= keywords)) %>%
  filter(grepl(pattern = "Science Fiction", x= genres))

best_alien <- df_alien[order(-df_alien$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#Pour les enfants

df_kids<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "Animation", x= genres))%>%
  filter(grepl(pattern = "English", x = spoken_languages))##marche pas

best_kids <- df_kids[order(-df_kids$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#marvel
df_marvel<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "marvel", x= keywords))
best_marvel <- df_marvel[order(-df_marvel$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#DC
df_dc<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "dc comics", x= keywords))
best_dc <- df_dc[order(-df_dc$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#gangster

df_gangster<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "gangster", x= keywords))%>%
  filter(grepl(pattern = "Crime", x= genres))
best_gangster <- df_gangster[order(-df_gangster$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )

#japonais

df_japan<- df %>%
  select(-1,-2,-3,-4,-7,-8,-9,-10,-11,-15) %>%
  filter(grepl(pattern = "Japan", x=production_countries))

best_japan <- df_japan[order(-df_japan$vote_average),]%>%
  select(1,2,5)%>%
  rename(
    Title = original_title,
    Overview = overview,
    Rating = vote_average
  )



#-----------------------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  # Changing themes of page
  theme = dark, 
  checkboxInput(inputId = "light_mode", 
                label = icon("sun")),

  # Application logo
  titlePanel(tags$img(src = "MyImage2.png", height = 100, width = 400)),


  # setup needed to make the app close when button pressed
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  # Tabset pannel to choose from each tab
  tabsetPanel(
    id = "tabset",
    type = "pills",
    tabPanel("Daily", icon = icon("calendar"), 
             mainPanel(
               br(),
               textOutput("daily_text"),
               tags$head(tags$style("#daily_text{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
               br(),
               textOutput("text_1"),
               tags$head(tags$style("#text_1{color: orange;font-size: 37px;font-style: bold; text-align: center;}")),
               br(),
               textOutput("text_2"),
               tags$head(tags$style("#text_2{color: orange;font-size: 25px;font-style: bold; text-align: center;}")),
               br(),
               br(),
               textOutput("text_3"),
               tags$head(tags$style("#text_3{font-size: 20px; text-align: left;}")),
               br(),
               textOutput("text_4"),
               tags$head(tags$style("#text_4{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
               br(),
               textOutput("text_5"),
               tags$head(tags$style("#text_5{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
               br(),
               textOutput("text_6"),
               tags$head(tags$style("#text_6{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
               br(),
               textOutput("text_7"),
               tags$head(tags$style("#text_7{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
               br()
               )
             ),
    tabPanel("Filter",
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
          br(),
          textOutput("recommend_text"),
          tags$head(tags$style("#recommend_text{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          br(),
          textOutput("text_8"),
          tags$head(tags$style("#text_8{color: orange;font-size: 37px;font-style: bold; text-align: center;}")),
          br(),
          textOutput("text_9"),
          tags$head(tags$style("#text_9{color: orange;font-size: 25px;font-style: bold; text-align: center;}")),
          br(),
          br(),
          textOutput("text_10"),
          tags$head(tags$style("#text_10{font-size: 20px; text-align: left;}")),
          br(),
          textOutput("text_11"),
          tags$head(tags$style("#text_11{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          br(),
          textOutput("text_12"),
          tags$head(tags$style("#text_12{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          br(),
          textOutput("text_13"),
          tags$head(tags$style("#text_13{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          br(),
          textOutput("text_14"),
          tags$head(tags$style("#text_14{color: cyan;font-size: 17px;font-style: bold; text-align: center;}"))
          )
      )
    ),
    tabPanel("Search",
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
          br(),
          textOutput("search_text"),
          br(),
          tags$head(tags$style("#search_text{color: cyan;font-size: 17px;font-style: bold; text-align: center;}")),
          withSpinner(tableOutput("search"))
          )
      )
    ),
    tabPanel("Categories", icon = icon("book"),
             mainPanel(
               selectInput("categories", "What do you want to watch tonight?", c("Haloween", "Valentine's day", "Christman with the family", "Alien Invasion", "For Kids","Marvel","DC Comics", "Gangster", "Japan")),
               tableOutput("movietable1")
               )
             ),
               
    tabPanel("PopCorn", icon = icon("fire"),
             mainPanel(
               style = "font-family: 'Comic Sans MS';",
               h2("POP CORN RECIPE !!!!", align = "center", style = "color:cyan"),
               div(tags$img(src = "MyImage1.png", height = 300, width = 400), align = "center"),
               h4("Ingredients : ", style = "color:purple"),
               p(""),
               p("- 2 tbsp vegetable oil", style = "color:lime"),
               p("- 100g popcorn kernels", style = "color:lime"),
               p("- 250g caster sugar", style = "color:lime"),
               p("- 50g salted butter , cubed", style = "color:lime"),
               h3("Method", align = "center", style = "color:pink"),
               h4("STEP 1", style = "color:red"),
               p("Put the oil in a large saucepan with a tight-fitting lid over a medium heat. Toss the popcorn kernels in the oil to coat. Put the lid on, and keep over a medium heat until you hear the first popcorn pop, then turn the heat to medium-low. When you begin to hear lots of popping, give the pan a shake. Continue to shake frequently until the popping stops. Turn off the heat and leave in the pan.", style = "color:orange"),
               p(""),
               p(""),
               h4("STEP 2", style = "color:red"),
               p("Line a large baking tray with baking parchment. Put the sugar and 60ml water into a medium heavy-based saucepan and bring to the boil. Stir until the sugar has dissolved, then leave over a medium heat, without stirring, for 6-8 mins. It should start to turn into a golden caramel, swirl it around and add the butter - stand back as it may spit a little. Stir well until combined.", style = "color:orange"),
               p(""),
               p(""),
               h4("STEP 3", style = "color:red"),
               p("Pour the caramel over the popcorn in the pan and stir immediately to coat the popcorn, being careful not to touch the hot caramel. Carefully transfer onto the lined baking tray and press down with the back of a spoon to spread evenly. Leave to cool for 5 mins, then break apart and eat. ", style = "color:orange"),
               h2("ENJOY !!!!", align = "center", style = "color:blue"),
               p(""),
               p("")
             )),
    tabPanel("Info", icon = icon("info"),
             mainPanel(
               align = "center",
               br(),
               h3("Interactive plot of : BUDGET vs REVENUE", style = "color:cyan"),
               br(),
               withSpinner(plotlyOutput("plot_data")),
               br(),
               br(),
               br(),
               br(),
               br(), #Without these the text goes behind the graph
               br(),
               br(),
               br(),
               h3("Did that spectacular graph bore you ?", style = "color:lime"),
               br(),
               actionBttn("close", "If so, click here !! (goodbye)", color = "success", style = "fill"),
               br(),
               br(),
               br()
             )),

    
    tabPanel("Quizz", icon = icon("quiz"),
             mainPanel(
               align = "center",
               h2("Personality"),
               selectInput("UserInput", "Do you like movies ?", choices = c("", "YES, I LOVE IT", "nah, lame")),
               h4(textOutput("Result"))
             )),
    tabPanel("Calculator", icon = icon("calculator"),
             mainPanel(
               align = "center",
               h5("You want to know how much time you will spend enjoying your movie(s)?"),
               numericInput("num1", "How many minutes does your movie last ?", 0),
               numericInput("num2", "How many minutes does your other movie last ?", 0),
               selectInput("operator", "Select operator",
               choices = c("+","-","x","/")),
               h2("The result is"),
               h4(textOutput("output")))),
              mainPanel(
               
              )
  )
)

#---------------------------------------------------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session){

  
  
  #calculator
  output$output <- renderText({
          switch(input$operator,
                 "+" = input$num1 + input$num2,
                 "-" = input$num1 - input$num2,
                 "x" = input$num1 * input$num2, 
                 "/" = input$num1 / input$num2)
  })
  
  
  
  
  #personality questoin ???
  Personality = function(q.c){
    if (q.c == "YES, I LOVE IT"){
      QuizResult ="WE GRANT YOU AS THE MVP OF THE PLANET"
    } else if (q.c == "nah, lame"){
      QuizResult = "ERROR 404 - HUMANITY NOT DETECTED"
    } else {
      QuizResult = "" 
    }
    return(QuizResult)
    }
  
  
  output$Result <- renderText({Personality(input$UserInput)})
  # Dark light toggle swithc output
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$light_mode)) light else dark 
    )
  })

  
  # Notification Explanation app
    shinyalert(
    title = "Welcome To PeliRoster",
    text = "This app was made to help you chose a movie to watch ! \n \n So good luck, have fun"
  )
  
  
  
  # Tab 1 --> Daily 
  
  # Choose movie based on date
    date <- as.POSIXct(Sys.Date(), format = "%y/%m/%d")
    numeric_date <- as.numeric(date)
    
    daily_movie <- daily_find(
      date = numeric_date,
      df = df
    )
  
  #Title of daily table
  output$daily_text <- renderText({
    paste0("RECOMMENDATION OF THE ", format(Sys.Date(), "%d-%m-%Y"))
  })
  
  
  #Text 
  output$text_1 <- renderText({
    daily_movie$title
  })
  
  output$text_2 <- renderText({
    daily_movie$tagline
  })
  
  output$text_3 <- renderText({
    daily_movie$overview
  })
  
  output$text_4 <- renderText({
    paste0("Rating : ", daily_movie$vote_average, "/10")
  })

  output$text_5 <- renderText({
    paste0("Released on the ", format(daily_movie$release_date, "%d-%m-%Y"))
  })
    
  output$text_6 <- renderText({
    paste0("Movie genres : ", daily_movie$genres)
  })
  
  output$text_7 <- renderText({
    if(daily_movie$homepage != ""){
      paste0("Website link : ",daily_movie$homepage)
    } else {
      ""
    }
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
  
  data_movie_recommendation <- eventReactive(input$compute, {
    df_recommend_movie <- choose_movie(data_recommended())
    return(df_recommend_movie)
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

  #Text 
  output$text_8 <- renderText({
    data_movie_recommendation()$title
  })
  
  output$text_9 <- renderText({
    data_movie_recommendation()$tagline
  })
  
  output$text_10 <- renderText({
    data_movie_recommendation()$overview
  })
  
  output$text_11 <- renderText({
    paste0("Rating : ", data_movie_recommendation()$vote_average, "/10")
  })
  
  output$text_12 <- renderText({
    paste0("Released on the ", format(data_movie_recommendation()$release_date, "%d-%m-%Y"))
  })
  
  output$text_13 <- renderText({
    paste0("Movie genres : ", data_movie_recommendation()$genres)
  })
  
  output$text_14 <- renderText({
    if(data_movie_recommendation()$homepage != ""){
      paste0("Website link : ",data_movie_recommendation()$homepage)
    } else {
      ""
    }
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
    paste0("MOVIES ", nrow(data_searched()), " FOUND")
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

  
  
  
  
  
 # Tab 4 --> Categories
  
  datasetInput <- reactive({
    switch(input$categories,
           "Haloween" = best_haloween,
           "Valentine's day"= best_love,  
           "Christman with the family"= best_christmas,
           "Alien Invasion" = best_alien,
           "For Kids" = best_kids,
           "Marvel" = best_marvel,
           "DC Comics" = best_dc,
           "Gangster" = best_gangster,
           "Japoan" = best_japan)
  })
  
  output$movietable1 <- renderTable(datasetInput())
  
  
  
  
  
 # Tab 5 --> Popcorn
    # No code/data needed for tab 5
  
  
  
  

  
  # Tab 6 --> Data
  
  # Render plotly plot 
  output$plot_data <- renderPlotly({
    plot <- ggplotly(plot_movie(df = df)) %>% layout(height = 600, width = 700)
  })
  
  #tab 7
  
  # close app
  observeEvent(input$close,{
    js$closeWindow()
    stopApp()
  })
  
}

#-------------------------------------------------------------------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)
