#=================================================================
# build App for Boosted HP
#-----------------------------------------------------------------
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#-----------------------------------------------------------------
# By: Chen Yang (chen_yang@link.cuhk.edu.hk)
#     Mei Ziwei (zwmei@link.cuhk.edu.hk)
# Date: 2019-05-20
# Update: 2019-07-03 (by Chen Yang)
# Update: 2021-06-01 (by Mei Ziwei)
#=================================================================
# rm(list = ls())
# 

library(shiny)
library(ggplot2)
library(tseries)
library(expm)
library(xts)
library(reshape2)

source("BoostedHP.R")
source("plot_all.R")

# load("data/IRE.rda")

# User interface ----

ui <- fluidPage(
  titlePanel("Boosted HP App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --------------- Define UI for data upload app ------------------
      
      h2("Uploading Files"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", FALSE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # --------------- Define UI for data download app ------------------
      # Download Button
      
      h2("Downloading Files "),
      downloadButton("downloadtable", "Download"),
      helpText("Download Files After the Boosted HP Filter in the default 'Download Document' in your computer."),
      
      # Horizontal line ----
      tags$hr(),
      h2("About"),
      # img(src = "shiny from rstudio.png", height = 70, width = 100),
      br(),
      span("Shiny", style = "color:blue"),
      " is a product of RStudio to create user friendly apps.\n\n", 
      # helpText("Create user friendly app through 'Shiny'."),
      p("For more information of the APP, visit the Boosted_HP_App",
        a(" Github Repository.", 
          href = "https://github.com/metricshilab/Boosted_HP_App"))
      
      
      
      
      
    ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # tags$hr(),
      
      
      textOutput("currentTime"),
      
      # --------------- Define UI for argument in BoostedHP.R ------------------
      #column(5,
      h2("Argument Options"),
      
      #h3("Argument in the Boosted Function"),
      #checkboxInput("checkbox", "Choice A", value = TRUE),
      
      numericInput("lambda", label = "Set the lambda for HP filter",value = 1600),

      selectInput("testtype", 
                  label = "Choose you preferred type to of iteration",
            
                  choices = list("nonstop", 
                                 "bHP-ADF",
                                 "bHP-BIC"),
                  selected = "bHP-BIC"),
      
      selectInput("p_value", 
                  label = "Significance level (for bHP-ADF only)",
                  
                  choices = list("0.01", 
                                 "0.05",
                                 "0.1"),
                  selected = "0.05"),
      
      numericInput("maxnum", label = "Maximum Iterated Number (Set as 1 to run simple HP filter without iteration)",value = 100),
      
      #),
      
      #column(5,
      #h3("Single checkbox")    
      #),
      
      # Output: Data file ----
      #column(3,
      #h2("View the Input Data"),
      #tableOutput("contents"), 
      #h2("Summary"),
      #verbatimTextOutput("summary"),
      #),
      
      # Horizontal line ----
      tags$hr(),
      h2("Plot"),
      # selectInput("Type", 
      #             label = "Choose the Type of Plot",
      #             choices = list("Time Series", 
      #                            "Plain",
      #                            "SVG",
      #                            "GGPlot",
      #                            "Dynamic"),
      #             selected = "Time Series"),
      selectInput("Frequency", 
                  label = "Choose the frequency of data",
                  choices = list("Daily" = "day", 
                                 "Weakly" = "week", 
                                 "Monthly" = "month",
                                 "Quarterly" = "quarter",
                                 "Yearly" = "year",
                                 "Not Available" = NULL
                                 ),
                  selected = "Quarterly"),
      dateInput("date", 
                label = "Beginning Date of Input Data", 
                value = "1900-01-01"),
      
      #plotOutput("Boostedplot"),
      tags$hr(),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Input Data", tableOutput("contents")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Plot Trend", plotOutput("Trendplot")),
                  tabPanel("Plot Cycle", plotOutput("Cycleplot")),
                  tabPanel("Iteration History", plotOutput("Iterationplot")),
                  tabPanel("Results Table", tableOutput("table")))
      
      
    )
    
  )
)








# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  #req(input$file1)
  
  
  myresults <- reactive({
    
    # req(input$file1)
    rawdata <- read.csv(input$file1$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)[,1]
    
    arg <- list()
    
    arg$x <- rawdata
    
    arg$lambda <- input$lambda
    
    arg$test_type <- switch(input$testtype, 
                            "nonstop" = "none",
                            "bHP-ADF" = "adf",
                            "bHP-BIC" = "BIC")
    
    arg$sig_p <- input$p_value
    
    arg$Max_Iter <- input$maxnum
    
    do.call(BoostedHP, arg)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    req(input$file1)
    
    #dataset <- datasetInput()
    #summary( as.numeric(input$file1))
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    summary(df)
    
  })
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  
  output$Trendplot <- renderPlot({
    
    bt_results <- myresults()
    #function(rawdata, trend, p_history, plot_type){
    Boostedplot(bt_results, "trend", input$date, input$Frequency)
    
    
  })
  
  
  output$Cycleplot <- renderPlot({
    
    
    bt_results <- myresults()
    # bt_results <- do.call(BoostedHP, arg)
    #function(rawdata, trend, p_history, plot_type){
    Boostedplot(bt_results, "cycle", input$date, input$Frequency)
    
    
  })
  
  
  output$Iterationplot <- renderPlot({
    # bt_results <- do.call(BoostedHP, arg)
    bt_results <- myresults()
    Boostedplot(bt_results, "history")
  })
  
  datasetoutput <- reactive({
    # bt_results <- do.call(BoostedHP, arg)
    bt_results <- myresults()
    out <- list()
    if (is.null(input$Frequency) || is.null(input$date) ){
      Date_series = 1:length(bt_results$cycle)
    }else{
      Date_series = seq(from = input$date, 
                        length.out = length(bt_results$cycle), 
                        by = input$Frequency)
    }
    
    out$date <- Date_series
    out$cycle <- bt_results$cycle
    out$trend <- bt_results$trend 
    out$trend_history <- bt_results$trend_hist
    out <- as.data.frame(out)
    out$date <- as.character(Date_series)
    out 
  })
  
  
  output$table <- renderTable({
    datasetoutput()
    # summary(bt_results[[2]])
  }
  
  )
  
  
  
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    output$downloadtable <- downloadHandler(
      filename = function() {
        "BoostedHP_Result.csv"
      },
      content = function(file) {
        write.csv(datasetoutput(), file, row.names = FALSE)
      }
    )
    
    
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)


