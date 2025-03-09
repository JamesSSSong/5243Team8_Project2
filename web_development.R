# Load Required Libraries
library(shiny)
library(shinythemes)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Data Cleaning Function
clean_data <- function(df, remove_na = FALSE, remove_duplicates = FALSE) {
  df <- df %>% mutate(across(where(is.character), ~ ifelse(. %in% c("?", "N/A", "NaN", "", " "), NA, .)))
  df <- df %>% mutate(across(where(is.character), ~ trimws(.) %>% tolower()))
  
  if (remove_na) {
    df <- na.omit(df)
  }
  
  if (remove_duplicates) {
    df <- unique(df)
  }
  
  return(df)
}

# User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Advanced Data Analysis App",
    
    # Data Preview Tab
    tabPanel(
      title = "Data Preview",
      titlePanel("Data Preview"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("dataInputMethod", "Enter Data:",
                      choices = c("Upload CSV File" = "upload", 
                                  "Use Sample Dataset" = "sample")),
          
          conditionalPanel(
            condition = "input.dataInputMethod == 'upload'",
            fileInput("file", "Choose CSV File", accept = c(".csv"))
          ),
          
          conditionalPanel(
            condition = "input.dataInputMethod == 'sample'",
            selectInput("dataset", "Select Sample Dataset", 
                        choices = c("mtcars", "iris"))
          ),
          
          selectInput("selectedVars", "Name of Variable:",
                      choices = NULL, multiple = TRUE, selected = NULL),
          
          radioButtons("removeNA", "Removing Missing Value",
                       choices = c("No" = FALSE, "Yes" = TRUE),
                       selected = FALSE, inline = TRUE),
          
          radioButtons("removeDup", "Removing Duplicates",
                       choices = c("No" = FALSE, "Yes" = TRUE),
                       selected = FALSE, inline = TRUE)
        ),
        
        mainPanel(
          h4("Data Preview"),
          DTOutput("dataTable")
        )
      )
    ),
    
    # Data Statistics Tab
    tabPanel(
      title = "Data Statistics",
      titlePanel("Data Statistics"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("statVar", "Select Variable for Analysis:",
                      choices = NULL, selected = NULL),
          
          # Horizontal layout for Plot Type Selection
          fluidRow(
            column(4, checkboxInput("histogram", "Histogram")),
            column(4, checkboxInput("boxplot", "Boxplot")),
            column(4, checkboxInput("dotplot", "Dotplot"))
          ),
          
          # Histogram Options
          conditionalPanel(
            condition = "input.histogram == true",
            
            sliderInput("binWidth", "Select Binwidth For Histogram:",
                        min = 5, max = 150, value = 30),
            
            checkboxGroupInput("histOptions", "Histogram Options:",
                               choices = c("Enter Binwidth", 
                                           "Select Starting Bin", 
                                           "Display Percent")),
            
            conditionalPanel(
              condition = "input.histOptions.indexOf('Enter Binwidth') > -1",
              numericInput("customBinwidth", "Custom Binwidth:", value = 30, min = 1, step = 1)
            ),
            
            conditionalPanel(
              condition = "input.histOptions.indexOf('Select Starting Bin') > -1",
              numericInput("startBin", "Lower Bound of First Bin:", value = 0, step = 1)
            )
          ),
          
          # Boxplot Options
          conditionalPanel(
            condition = "input.boxplot == true",
            h4("Boxplot Options:"),
            checkboxInput("verticalPlot", "Vertical Plot", value = FALSE)
          )
        ),
        
        mainPanel(
          h4("Statistical Summary"),
          verbatimTextOutput("statSummary"),
          
          h4("Distribution Plots"),
          plotOutput("histPlot", height = "300px"),
          plotOutput("boxPlot", height = "300px"),
          plotOutput("dotPlot", height = "300px")
        )
      )
    ),
    
    # About Tab
    tabPanel(
      title = "About",
      titlePanel("About This App"),
      
      fluidPage(
        h3("Advanced Data Analysis App"),
        p("This application is designed to provide a comprehensive platform for interactive data analysis. 
          Users can upload their own datasets or choose from pre-loaded sample datasets, explore the data, 
          visualize distributions, and analyze statistical properties with a range of customizable options."),
        
        h4("Key Features:"),
        tags$ul(
          tags$li("Data Preview: Upload datasets or select sample datasets, filter variables, and handle missing or duplicate data."),
          tags$li("Data Statistics: Generate dynamic visualizations including Histograms, Boxplots, and Dotplots with customizable settings."),
          tags$li("Interactive Controls: Easily adjust plot settings, choose variables for analysis, and manage data cleaning options.")
        ),
        
        h4("How to Use:"),
        tags$ol(
          tags$li("Navigate to 'Data Preview' to upload a dataset or select a sample dataset."),
          tags$li("Use the 'Data Statistics' tab to select a variable and visualize the data with interactive plots."),
          tags$li("Configure plot-specific options like binwidth for histograms or orientation for boxplots.")
        ),
        
        h4("Contact Information:"),
        p("For feedback or support, please contact: developer@example.com")
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  reactiveData <- reactiveVal(NULL)  
  
  observe({
    req(input$dataInputMethod)
    
    df <- if (input$dataInputMethod == "upload" && !is.null(input$file)) {
      read_csv(input$file$datapath)
    } else if (input$dataInputMethod == "sample") {
      get(input$dataset)
    } else {
      return(NULL)
    }
    
    df <- clean_data(df, 
                     remove_na = as.logical(input$removeNA), 
                     remove_duplicates = as.logical(input$removeDup))
    
    reactiveData(df)
    
    updateSelectInput(session, "selectedVars", choices = names(df), selected = names(df))
    updateSelectInput(session, "statVar", choices = names(df), selected = names(df)[1])
  })
  
  output$dataTable <- renderDT({
    df <- reactiveData()
    req(df)
    
    if (!is.null(input$selectedVars) && length(input$selectedVars) > 0) {
      df <- df[, input$selectedVars, drop = FALSE]
    }
    
    datatable(df)
  })
  
  output$statSummary <- renderPrint({
    df <- reactiveData()
    req(df, input$statVar)
    
    summary(df[[input$statVar]])
  })
  
  # Render Histogram
  output$histPlot <- renderPlot({
    df <- reactiveData()
    req(df, input$statVar, input$histogram)
    
    ggplot(df, aes_string(x = input$statVar)) +
      geom_histogram(binwidth = input$binWidth, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Histogram", x = input$statVar, y = "Frequency")
  })
  
  # Render Boxplot
  output$boxPlot <- renderPlot({
    df <- reactiveData()
    req(df, input$statVar, input$boxplot)
    
    is_vertical <- input$verticalPlot
    
    # Default to horizontal boxplot
    if (!is_vertical) {
      p <- ggplot(df, aes_string(y = input$statVar, x = "1")) +
        geom_boxplot(fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = "Boxplot", y = input$statVar, x = "Values") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else {
      p <- ggplot(df, aes_string(x = input$statVar, y = "1")) +
        geom_boxplot(fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = "Boxplot", x = input$statVar, y = "Values") +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    
    p
  })
  
  # Render Dotplot
  output$dotPlot <- renderPlot({
    df <- reactiveData()
    req(df, input$statVar, input$dotplot)
    
    ggplot(df, aes_string(x = input$statVar)) +
      geom_dotplot(binwidth = 0.1, dotsize = 0.5, fill = "skyblue") +
      theme_minimal() +
      labs(title = "Dotplot", x = input$statVar, y = "Count")
  })
}

shinyApp(ui, server)
