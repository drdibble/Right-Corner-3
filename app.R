#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(formattable)
library(shinyWidgets)
library(shinydashboard)
library(stats)
library(MLmetrics)
library(healthcareai)
library(plotly)
library(shinythemes)
library(hash)


# read in CSV with table information
df = read.csv('official_free_agents.csv')
df1 = read.csv('college.csv')
df2 = read.csv('g_league.csv')
#df$Previous.Salary <- sub("^", "", df$Previous.Salary )



# #assign a class    
# class(df$Previous.Salary) <- c("money", class(df$Previous.Salary))
# 
# #S3 print method for the class    
# print.money <- function(x, ...) {
#   print.default(paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=",")))
# }
# 
# #format method, which is necessary for formating in a data.frame   
# format.money  <- function(x, ...) {
#   paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
# }

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "The Right Corner 3"),
  dashboardSidebar(sidebarMenu(
    #chart-bar, dribbble-square, horse-head, th, red-river
    menuItem("Welcome", tabName = "slide1", icon = icon("chevron-right")),
    menuItem("Data", tabName = "slide2", icon = icon("database")),
    menuItem("Correlation Analysis", tabName = "slide3", icon = icon("binoculars")),
    menuItem("Looking at the Right Corner", tabName = "slide4", icon = icon("microscope")),
    menuItem("Court Visual", tabName = "court", icon = icon("basketball-ball")),
    menuItem("The Magic Number", tabName = "slide6", icon = icon("hat-wizard")),
    menuItem("Where Teams Like To Shoot", tabName = "slide7", icon = icon("basketball-ball")),
    menuItem("Probability Model", tabName = "model", icon = icon("horse-head")),
    menuItem("Right Corner Scouting", tabName = "scouting", icon = icon("bullseye")),
    menuItem("Further Work", tabName = "furtherwork", icon = icon("business-time"))
  )),
  dashboardBody(
    tabItems(
      #Presentation slides ---------------------------------------------------------------------
      tabItem(tabName = "slide1",
              fluidPage(
                img(src = "slide1.png", height = 700, width = 1160), #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "slide2",
              fluidPage(
                img(src = "slide2.png", height = 700, width = 1160) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "slide3",
              fluidPage(
                img(src = "slide3.png", height = 700, width = 1160) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "slide4",
              fluidPage(
                img(src = "slide4.png", height = 700, width = 1160) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "court",
              fluidPage(
                img(src = "court.png", height = 700, width = 1150) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "slide6",
              fluidPage(
                img(src = "slide6.png", height = 700, width = 1160) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "slide7",
              fluidPage(
                img(src = "slide7.png", height = 700, width = 1160) #1024,951
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "furtherwork",
              fluidPage(
                img(src = "furtherwork.png", height = 700, width = 1160) #1024,951
              ) #fluidPage
              
      ), #tabItem
      #Probability Model page ---------------------------------------------------------------------
      tabItem(tabName = "model",
              fluidPage(
                fluidRow(
                  h2("Probability Model")
                ),
                fluidRow(
                  h4("Using our shooting data by zone, we created a probability model 
                     to predict the probability of a team winning based on their shooting
                     breakdown by zone. These numbers are based on a glmnet model which
                     achieved an accuracy of 70.4%. The plot below shows the size of each
                     metric's coefficient in the trained model. Since the features were
                     all scaled, the size of the coefficient represents the significance
                     of each metric. Notice that this trained model treats RC3 FGM as
                     the most important metric in determining a win or loss.")
                ),
                fluidRow(
                  img(src = 'model_coef.jpg', height = 376.8, width = 561.6, style="display: block; margin-left: auto; margin-right: auto;")
                ),
                fluidRow(
                  h3("Insert your own numbers and click \'Run Model\' to view your team's
                      probability of winning an NBA game!")
                ),
                fluidRow(
                  column(2,
                        numericInput("ra_fgm", label = h4("Restricted Area FGM"), min = 0, max = 40, value = 29)
                  ),
                  column(2,
                         numericInput("itp_fgm", label = h4("In the Paint FGM"), min = 0, max = 30, value = 6)
                  ),
                  column(2,
                         numericInput("mr_fgm", label = h4("Mid-Range FGM"), min = 0, max = 25, value = 7)
                  ),
                  column(2,
                         numericInput("lc3_fgm", label = h4("Left Corner 3 FGM"), min = 0, max = 15, value = 2)
                  ),
                  column(2,
                         numericInput("rc3_fgm", label = h4("Right Corner 3 FGM"), min = 0, max = 10, value = 2)
                  ),
                  column(2,
                         numericInput("atb_fgm", label = h4("Above The Break FGM"), min = 0, max = 30, value = 3)
                  )
                ),
                fluidRow(
                  column(2,
                         numericInput("ra_fga", label = h4("Restricted Area FGA"), min = 0, max = 60, value = 38)
                  ),
                  column(2,
                         numericInput("itp_fga", label = h4("In The Paint FGA"), min = 0,max = 45, value = 16)
                  ),
                  column(2,
                         numericInput("mr_fga", label = h4("Mid-Range FGA"), min = 0, max = 35, value = 16)
                  ),
                  column(2,
                         numericInput("lc3_fga", label = h4("Left Corner 3 FGA"), min = 0, max = 20, value = 4)
                  ),
                  column(2,
                         numericInput("rc3_fga", label = h4("Right Corner 3 FGA"), min = 0, max = 20, value = 2)
                  ),
                  column(2,
                         numericInput("atb_fga", label = h4("Above The Break FGA"), min = 0, max = 55, value = 14)
                  )

                ), #fluidRow
                fluidRow(
                  div(actionButton("run_model", label = h1(strong("Run Model"), style = "font-size:40px;"), icon = icon('horse-head', style = "font-size:30px;"), width = 300), align = 'center')
                ),#fluidRow
                fluidRow(
                         h3("Win/Loss Probabilities", align = 'center'),
                         div(plotlyOutput('pie_chart'), align = 'center')
                ) #fluidRow
              ) #fluidPage
              
      ), #tabItem
      tabItem(tabName = "scouting",
              navbarPage("Players",
                         
                         #NBA free agent page---------------------------------------------------------------------
                         tabPanel(img(src = "nba.png", height = 60, width = 30),
                                  fluidRow(
                                    h3("2021 Free Agents")
                                  ),
                                  fluidRow(
                                    #Row for filters.
                                    
                                    #Position
                                    #Two options for multiple select: check boxes, or dropdown select.
                                    column(6,
                                           #Check boxes, uncomment to use.
                                           checkboxGroupInput(
                                             inputId = "pos",
                                             label = "Position:",
                                             choices = c(unique(as.character(df$Position))),
                                                          selected = c(unique(as.character(df$Position))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                           #Dropdown select, uncomment to use.
                                           # selectInput(inputId = "pos",
                                           #             label = "Position:",
                                           #             choices = c(unique(as.character(df$Position))),
                                           #             selected = c(unique(as.character(df$Position))),
                                           #             multiple = TRUE) #selectInput
                                    ), #column
                                    
                                    #Free agent type
                                    column(6,
                                           checkboxGroupInput(
                                             inputId = "type",
                                             label = "Free Agent Type:",
                                             choices = c(unique(as.character(df$Type))),
                                             selected = c(unique(as.character(df$Type))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                    ), #column
                                  ), #fluid row
                                  
                                  #Row for sliders
                                  fluidRow(  
                                    #Previous salary range
                                    column(6,
                                           sliderInput(inputId = "sal",
                                                       label = "Previous Salary Range:",
                                                       min = 0,
                                                       max = 40000000,
                                                       value = c(0,40000000),
                                                       step = 1000000,
                                                       ticks = FALSE,
                                                       pre = '$'
                                           ) #sliderInput
                                    ), #column
                                    
                                    #Minimum attempts.
                                    column(6,
                                           sliderInput(inputId = "att",
                                                       label = "Minimum Attempts:",
                                                       min = 0,
                                                       max = 124,
                                                       value = 0,
                                                       step = 1,
                                                       ticks = FALSE,
                                           ) #sliderInput
                                    )
                                  ),
                                  fluidRow(
                                    actionButton("disclaimer_nba", label = "", icon = icon('info-circle')),
                                    actionButton("dh_pick_nba", label = "DarkHorse Pick", icon = icon('horse-head'))
                                  ),
                                  hr(),
                                  fluidRow(
                                    div(DTOutput("my_table", width = "100%"), style = "font-size: 91%; width: 91%")
                                     #DT output
                                  )
                                  
                         ), #tabPanel, NBA free agents
                         
                         #College player page---------------------------------------------------------------------
                         tabPanel(img(src = "draft.png", height = 60, width = 60),
                                  fluidRow(
                                    h3("NBA Draft Prospects")
                                  ),
                                  fluidRow(
                                    #Row for filters.
                                    
                                    #Position
                                    #Two options for multiple select: check boxes, or dropdown select.
                                    column(4,
                                           #Check boxes, uncomment to use.
                                           checkboxGroupInput(
                                             inputId = "pos1",
                                             label = "Position:",
                                             choices = c(unique(as.character(df$Position))),
                                                         selected = c(unique(as.character(df$Position))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                           #Dropdown select, uncomment to use.
                                           # selectInput(inputId = "pos",
                                           #             label = "Position:",
                                           #             choices = c(unique(as.character(df$Position))),
                                           #             selected = c(unique(as.character(df$Position))),
                                           #             multiple = TRUE) #selectInput
                                    ), #column
                                    
                                    #Draft projection
                                    column(4,
                                           sliderInput(inputId = "mock",
                                                       label = "ESPN's Top 100:",
                                                       min = 1,
                                                       max = 100,
                                                       value = c(1,100),
                                                       step = 1,
                                                       ticks = FALSE,
                                           ) #sliderInput
                                    ), #column
                                  
                                  #New row for slider
                                  fluidRow(  
                                    #Minimum attempts.
                                    column(4,
                                           sliderInput(inputId = "att1",
                                                       label = "Minimum Attempts:",
                                                       min = 0,
                                                       max = 33,
                                                       value = 0,
                                                       step = 1,
                                                       ticks = FALSE,
                                                       ) #sliderInput
                                          )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    actionButton("disclaimer_college", label = "", icon = icon('info-circle')),
                                    actionButton("dh_pick_college", label = "DarkHorse Pick", icon = icon('horse-head'))
                                  ),
                                  hr(),
                                  #New row for table
                                  fluidRow(
                                    DTOutput("my_table1", width = "100%"), #DT output
                                  )
                         ), #tabPanel, NBA free agents
                         #G League page---------------------------------------------------------------------
                         tabPanel(img(src = "g_league.png", height = 60, width = 30),
                                  fluidRow(
                                    h3("G League Prospects")
                                  ),
                                  fluidRow(
                                    #Row for filters.
                                    
                                    #Position
                                    #Two options for multiple select: check boxes, or dropdown select.
                                    column(6,
                                           #Check boxes, uncomment to use.
                                           checkboxGroupInput(
                                             inputId = "pos2",
                                             label = "Position:",
                                             choices = c(unique(as.character(df2$Position))),
                                             selected = c(unique(as.character(df2$Position))),
                                             inline = TRUE
                                           ) #checkboxGroupInput
                                           #Dropdown select, uncomment to use.
                                           # selectInput(inputId = "pos",
                                           #             label = "Position:",
                                           #             choices = c(unique(as.character(df$Position))),
                                           #             selected = c(unique(as.character(df$Position))),
                                           #             multiple = TRUE) #selectInput
                                    ), #column
                                    column(6,
                                           sliderInput(inputId = "att2",
                                                       label = "Minimum Attempts",
                                                       min = 0,
                                                       max = 33,
                                                       value = 0,
                                                       step = 1,
                                                       ticks = FALSE,
                                           ) #sliderInput
                                    ),
                                  ), #fluid row
                                  fluidRow(
                                    actionButton("disclaimer_g", label = "", icon = icon('info-circle')),
                                    actionButton("dh_pick_g", label = "DarkHorse Pick", icon = icon('horse-head'))
                                  ),
                                  hr(),
                                  #New row for table
                                  fluidRow(
                                    DTOutput("my_table2", width = "100%"), #DT output
                                  )
                         ) #tabPanel
              ) #navbarPage
      ) #tabItem
      
      
    ) #tabItems
    
    
  ) #dashboardBody
) #dashboardPage
  


# Define server
server <- function(input, output) {
  #populate dictionary with youtube links
  nba_vid <- read.csv('nba_video.csv')
  dict <- hash()
  for (row in 1:nrow(nba_vid)){
    dict[[nba_vid[row, 'Name']]] <- nba_vid[row, 'Video']
  }
  college_vid <- read.csv('college_video.csv')
  for (row in 1:nrow(college_vid)){
    dict[[college_vid[row, 'Name']]] <- college_vid[row, 'Video']
  }
  g_vid <- read.csv('g_league_video.csv')
  for (row in 1:nrow(g_vid)){
    dict[[g_vid[row, 'Name']]] <- g_vid[row, 'Video']
  }
  
  # a call creating input buttons:
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  subsetModal <- function(session, player_id, size) {
    #ns <- session$ns
    showModal(modalDialog(
      HTML('<iframe width="560" height="315" src=', dict[[player_id]], 'frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      size = size,
      easyClose = TRUE))
  }
  
  #Output for NBA panel-------------------------------------------------------
  values_nba <- reactiveValues(df = NULL)
  values_nba$df <- data.frame(df)
  values_nba$df <- data.frame(
    df,
    Highlights = shinyInput(actionButton, nrow(df), 'button_', label = "Watch",
                            onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    stringsAsFactors = FALSE
  )

  output$my_table = DT::renderDataTable(
    DT::datatable(
      {data <- isolate(values_nba$df)
      data <- data[data$Position %in% input$pos,]
      data <- data[data$Type %in% input$type,]
      data <- data[(data$Previous.Salary <= input$sal[2]) &
                     (data$Previous.Salary >= input$sal[1]),]
      data <- data[data$FGA >= input$att,]
      data
      },
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0)),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100)),
      selection = 'single'
    ) %>% #datatable
      formatCurrency(c("Previous.Salary"), "$")
  )#renderDataTable


  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    subsetted <- df[selectedRow, , drop = FALSE]
    subsetModal(session, subsetted[1, 2], size = "m")
  })
  
  observeEvent(input$disclaimer_nba, {
    showModal(modalDialog(
      title = "Disclaimer",
      "Note: The following table uses data from the entire 2020-21 season
      gathered from the NBA website. Field goals are solely from the right
      corner 3.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$dh_pick_nba, {
    showModal(modalDialog(
      title = "DarkHorse Pick",
      img(src = "powellnyc.jpg", width = '100%'),
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  #Output for college panel-------------------------------------------------
  values_col <- reactiveValues(df = NULL)
  values_col$df <- data.frame(df1)
  values_col$df <- data.frame(
    df1,
    Highlights = shinyInput(actionButton, nrow(df1), 'button_', label = "Watch",
                         onclick = 'Shiny.onInputChange(\"select_button1\",  this.id)' ),
    stringsAsFactors = FALSE
  )

  output$my_table1 = DT::renderDataTable(
    DT::datatable(
      {data1 <- isolate(values_col$df)
      #if (input$pos != 'All') {
      data1 <- data1[data1$Position %in% input$pos1,]
      data1 <- data1[(data1$Mock.Draft >= input$mock[1]) &
                     (data1$Mock.Draft <= input$mock[2]),]
      data1 <- data1[data1$FGA >= input$att1,]
      #}
      data1
      },
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0)),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100)),
      selection = 'single'
    ) #datatable
  ) #renderDataTable

  observeEvent(input$select_button1, {
    selectedRow1 <- as.numeric(strsplit(input$select_button1, "_")[[1]][2])
    subsetted1 <- df1[selectedRow1, , drop = FALSE]
    subsetModal(session, subsetted1[1, 2], size = "m")
  })

  
  observeEvent(input$disclaimer_college, {
    showModal(modalDialog(
      title = "Disclaimer",
      "Note: The following table uses data from the entire 2020-21 season
      gathered from CBB Analytics. Field goals are solely from the right
      corner 3. Gaps in ESPN's top 100 represent international players.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$dh_pick_college, {
    showModal(modalDialog(
      title = "DarkHorse Pick",
      img(src = "moodynyc.jpg", width = '100%'),
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  #Output for g-league panel------------------------------------------
  values_g <- reactiveValues(df = NULL)
  values_g$df <- data.frame(df2)
  values_g$df <- data.frame(
    df2,
    Highlights = shinyInput(actionButton, nrow(df2), 'button_', label = "Watch",
                            onclick = 'Shiny.onInputChange(\"select_button2\",  this.id)' ),
    stringsAsFactors = FALSE
  )

  output$my_table2 = DT::renderDataTable(
    DT::datatable(
      {data2 <- isolate(values_g$df)
      #if (input$pos != 'All') {
      data2 <- data2[data2$Position %in% input$pos2,]
      data2 <- data2[data2$FGA >= input$att2,]
      #}
      data2
      },
      escape = FALSE,
      rownames = FALSE,
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0)),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100)),
      selection = 'single'
    ) #datatable
  ) #renderDataTable

  observeEvent(input$select_button2, {
    selectedRow2 <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
    subsetted2 <- df2[selectedRow2, , drop = FALSE]
    subsetModal(session, subsetted2[1, 2], size = "m")
  })
  
  observeEvent(input$disclaimer_g, {
    showModal(modalDialog(
      title = "Disclaimer",
      "Note: The following table uses data from the entire 2020-21 season
      gathered from the NBA website. Field goals are solely from the right
      corner 3.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$dh_pick_g, {
    showModal(modalDialog(
      title = "DarkHorse Pick",
      img(src = "hallnyc.jpg", width = '100%'),
      easyClose = TRUE,
      size = 'l'
    ))
  })
  
  #Model ---------------------------------------------------------------------
  #NEW CODE: Pie chart for when users enter model tab.
  pre_data <- data.frame(
    outcome=c("Click Run Model Button"),
    p=c(1)
  )
  output$pie_chart <- renderPlotly(
    plot1 <- plot_ly(pre_data, labels = ~outcome, values = ~p, type = "pie",
                     textposition = 'inside',
                     textinfo = 'label',
                     insidetextfont = list(color = "#FFFFFF", size = 24),
                     hoverinfo = 'text',
                     text = ~paste(" ")) %>%
      layout(showlegend=FALSE,paper_bgcolor='transparent')
  ) #renderPlotly 
  
  observeEvent(input$run_model, {
    if(
      ( #if no na values
        (!is.na(input$ra_fgm)) &
        (!is.na(input$ra_fga)) &
        (!is.na(input$itp_fgm)) &
        (!is.na(input$itp_fga)) &
        (!is.na(input$mr_fgm)) &
        (!is.na(input$mr_fga)) &
        (!is.na(input$lc3_fgm)) &
        (!is.na(input$lc3_fga)) &
        (!is.na(input$rc3_fgm)) &
        (!is.na(input$rc3_fga)) &
        (!is.na(input$atb_fgm)) &
        (!is.na(input$atb_fga))
      ) &
      ( #and no fgm > fga
        (input$ra_fgm <= input$ra_fga) &
        (input$itp_fgm <= input$itp_fga) &
        (input$mr_fgm <= input$mr_fga) &
        (input$lc3_fgm <= input$lc3_fga) &
        (input$rc3_fgm <= input$rc3_fga) &
        (input$atb_fgm <= input$atb_fga)
      )
    ){
      #Run predictions like normal
      RA_fgm <- as.numeric(input$ra_fgm)
      RA_fga <- as.numeric(input$ra_fga)
      RA_fgper <- as.numeric(input$ra_fgm / input$ra_fga)
      ITP_fgm <- as.numeric(input$itp_fgm)
      ITP_fga <- as.numeric(input$itp_fga)
      ITP_fgper <- as.numeric(input$itp_fgm / input$itp_fga)
      MR_fgm <- as.numeric(input$mr_fgm)
      MR_fga <- as.numeric(input$mr_fga)
      MR_fgper <- as.numeric(input$mr_fgm / input$mr_fga)
      LC3_fgm <- as.numeric(input$lc3_fgm)
      LC3_fga <- as.numeric(input$lc3_fga)
      LC3_fgper <- as.numeric(input$lc3_fgm / input$lc3_fga)
      RC3_fgm <- as.numeric(input$rc3_fgm)
      RC3_fga <- as.numeric(input$rc3_fga)
      RC3_fgper <- as.numeric(input$rc3_fgm / input$rc3_fga)
      ATB_fgm <- as.numeric(input$atb_fgm)
      ATB_fga <- as.numeric(input$atb_fga)
      ATB_fgper <- as.numeric(input$atb_fgm / input$atb_fga)
      
      sample_df = read.csv('games_as_rows.csv')
      
      split_data <- split_train_test(d = sample_df,
                                     outcome = win,
                                     p = .8,
                                     seed = 2)
      prepped_training_data <- prep_data(split_data$train, ortg, pt_diff, X, outcome = win,
                                         scale = TRUE)
      
      row <- data.frame(X = 0, RA_FGM = RA_fgm, RA_FGA = RA_fga, RA_FG. = RA_fgper, ITP_FGM = ITP_fgm, ITP_FGA = ITP_fga, ITP_FG. = ITP_fgper, MR_FGM = MR_fgm, MR_FGA = MR_fga, MR_FG. = MR_fgper, LC3_FGM = LC3_fgm, LC3_FGA = LC3_fga, LC3_FG. = LC3_fgper, RC3_FGM = RC3_fgm, RC3_FGA = RC3_fga, RC3_FG. = RC3_fgper, ATB_FGM = ATB_fgm, ATB_FGA = ATB_fga, ATB_FG. = ATB_fgper, win = 1, ortg = 121.4, pt_diff = 29)
      model <- readRDS('bestModel.rds')
      predict_row <- predict(model, row, outcome_groups = TRUE)
      
      prob1 <- as.numeric(predict_row['predicted_win'][1,1])
      chart_data <- data.frame(
        outcome=c("Win", "Loss"),
        p=c(prob1,1-prob1)
      )
      
      output$pie_chart <- renderPlotly(
        plot1 <- plot_ly(chart_data,labels = ~outcome, values = ~p, type = "pie",
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = "#FFFFFF", size = 24),
                         hoverinfo = 'text',
                         text = ~paste("Probability of", outcome, ":", round(p*100, 1), "%")) %>%
          layout(showlegend=FALSE,paper_bgcolor='transparent'))
    } 
    #If data does not pass all checks, then report this to the user.
    else if (
      ( #if no na values
        (!is.na(input$ra_fgm)) &
        (!is.na(input$ra_fga)) &
        (!is.na(input$itp_fgm)) &
        (!is.na(input$itp_fga)) &
        (!is.na(input$mr_fgm)) &
        (!is.na(input$mr_fga)) &
        (!is.na(input$lc3_fgm)) &
        (!is.na(input$lc3_fga)) &
        (!is.na(input$rc3_fgm)) &
        (!is.na(input$rc3_fga)) &
        (!is.na(input$atb_fgm)) &
        (!is.na(input$atb_fga))
      ) &
      ( #and fgm > fga
        (input$ra_fgm > input$ra_fga) |
        (input$itp_fgm > input$itp_fga) |
        (input$mr_fgm > input$mr_fga) |
        (input$lc3_fgm > input$lc3_fga) |
        (input$rc3_fgm > input$rc3_fga) |
        (input$atb_fgm > input$atb_fga)
      )
    ){
      #Output error
      invalid_data <- data.frame(
        outcome=c("Can't have FGM > FGA"),
        p=c(1)
      )
      output$pie_chart <- renderPlotly(
        plot1 <- plot_ly(invalid_data, labels = ~outcome, values = ~p, type = "pie",
                         textposition = 'inside',
                         textinfo = 'label',
                         insidetextfont = list(color = "#FFFFFF", size = 24),
                         hoverinfo = 'text',
                         text = ~paste("Invalid")) %>%
          layout(showlegend=FALSE,paper_bgcolor='transparent')
      ) #renderPlotly 
    }
    else if (
      #if any value is na
      (is.na(input$ra_fgm)) |
      (is.na(input$ra_fga)) |
      (is.na(input$itp_fgm)) |
      (is.na(input$itp_fga)) |
      (is.na(input$mr_fgm)) |
      (is.na(input$mr_fga)) |
      (is.na(input$lc3_fgm)) |
      (is.na(input$lc3_fga)) |
      (is.na(input$rc3_fgm)) |
      (is.na(input$rc3_fga)) |
      (is.na(input$atb_fgm)) |
      (is.na(input$atb_fga))
    ){
      #Output error
      invalid_data <- data.frame(
        outcome=c("Missing value"),
        p=c(1)
      )
      output$pie_chart <- renderPlotly(
        plot1 <- plot_ly(invalid_data, labels = ~outcome, values = ~p, type = "pie",
                         textposition = 'inside',
                         textinfo = 'label',
                         insidetextfont = list(color = "#FFFFFF", size = 24),
                         hoverinfo = 'text',
                         text = ~paste("Invalid")) %>%
          layout(showlegend=FALSE,paper_bgcolor='transparent')
      ) #renderPlotly 
    }
  }) #observeEvent
  
  
  #row <- data.frame(X = 0, RA_FGM = 29.0, RA_FGA = 38.0, RA_FG. = 0.7631579, ITP_FGM = 6.0, ITP_FGA = 16.0, ITP_FG. = 0.3750000, MR_FGM = 7.0, MR_FGA = 16.0, MR_FG. = 0.4375000, LC3_FGM = 2.0, LC3_FGA = 4.0, LC3_FG. = 0.5, RC3_FGM = 2.0, RC3_FGA = 2.0, RC3_FG. = 1.0, ATB_FGM = 3.0, ATB_FGA = 14.0, ATB_FG. = 0.2142857, win = 1, ortg = 121.4, pt_diff = 29)
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
