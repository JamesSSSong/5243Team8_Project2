# Load Required Libraries
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(readr)
library(readxl)
library(jsonlite)
library(tools)
library(dplyr)

# Data Cleaning Function
clean_data <- function(df, missing_strategy = "Remove Rows") {
  df <- df %>% mutate(across(where(is.character), ~ ifelse(. %in% c("?", "N/A", "NaN", "", " "), NA, .)))
  df <- df %>% mutate(across(where(is.character), ~ trimws(.) %>% tolower()))
  df <- df %>% mutate(across(where(is.character), ~ ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", .),
                                                           lubridate::ymd(.), .)))
  df <- df %>% mutate(across(where(is.character), ~ ifelse(grepl("^[0-9\\.]+$", .),
                                                           readr::parse_number(.), .)))
  df <- df %>% mutate(across(where(is.character), ~ {if(length(unique(.)) <= 10) {
    factor(.)
  } else {.}
  }))
  
  if(missing_strategy == "Remove Rows") {
    df <- na.omit(df)
  } else if(missing_strategy == "Impute Values") {
    df <- df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
    df <- df %>% mutate(across(where(is.factor), ~ {
      if(any(is.na(.))) {
        mode_val <- names(sort(table(.), decreasing = TRUE))[1]
        replace(., is.na(.), mode_val)
      } else {.}
    }))
  }
  
  return(df)
}

# Standardization Function
standardize <- function(df, scale_cols) {
  if (!is.null(scale_cols) && length(scale_cols) > 0) {
    df <- df %>% mutate(across(all_of(scale_cols), ~ as.numeric(scale(.))))
  }
  return(df)
}

# User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Interactive Data Analysis App",
    
    # Analysis Page with Tabs
    tabPanel(
      title = "Analysis",
      titlePanel("Analysis"),  # Main title for the Analysis section
      
      sidebarLayout(
        sidebarPanel(
          h4("Inputs"),
          fileInput("file", "Upload Dataset", accept = c(".csv", ".xlsx", ".json", ".rds")),
          selectInput("dataset", "Or Select a Sample Dataset", 
                      choices = c("mtcars", "iris")),
          selectInput("missingStrategy", "Missing Value Strategy", 
                      choices = c("Remove Rows", "Impute Values")),
          actionButton("loadData", "Load Data"),
          actionButton("removeDup", "Remove Duplicates"),
          
          selectizeInput("scaleCols", "Columns to Scale", 
                         choices = NULL, multiple = TRUE),
          actionButton("scaleBtn", "Standardize"),
          
          hr(),
          checkboxInput("header", "Header", TRUE)
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Data Preview", DTOutput("dataTable")),
            tabPanel("Data Statistics", verbatimTextOutput("dataSummary")),
            tabPanel("Duplicates", DTOutput("dupTable")),
            tabPanel("Distribution",
                     selectInput("distCol", "Select Column for Impact Analysis", choices = NULL),
                     plotOutput("distPlot")),
            tabPanel("Summary", verbatimTextOutput("summaryInfo")),  
            tabPanel("Visualization", 
                     selectInput("xvar", "X-axis", choices = NULL),
                     selectInput("yvar", "Y-axis", choices = NULL),
                     plotOutput("plot"))
          )
        )
      )
    ),
    
    # About Page
    tabPanel(
      title = "About",
      titlePanel("About"),
      p("This Shiny app is designed for interactive data analysis."),
      p("You can upload your dataset, clean the data, and visualize the results."),
      p("Created with R Shiny, March 2025")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  origionData <- reactiveVal(NULL)  
  reactiveData <- reactiveVal(NULL)  
  summaryLog <- reactiveVal("No modifications made yet.")
  
  # Upload & Read Data
  observeEvent(input$loadData, {
    df <- if (!is.null(input$file)) {
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv = read_csv(input$file$datapath),
             xlsx = read_excel(input$file$datapath),
             json = fromJSON(input$file$datapath),
             rds = readRDS(input$file$datapath),
             stop("Invalid file format"))
    } else {
      get(input$dataset)
    }
    
    df <- clean_data(df, missing_strategy = input$missingStrategy)
    
    origionData(df)
    reactiveData(df)
    summaryLog("Data loaded successfully.")
  })
  
  observe({
    df <- reactiveData()
    if (!is.null(df)) {
      updateSelectInput(session, "xvar", choices = names(df)) 
      updateSelectInput(session, "yvar", choices = names(df))
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectizeInput(session, "scaleCols", choices = numeric_cols, server = TRUE)
      updateSelectInput(session, "distCol", choices = numeric_cols)
    }
  })
  
  observeEvent(input$removeDup, {
    df <- reactiveData()
    num_duplicates <- sum(duplicated(df))
    if (num_duplicates > 0) {
      df_clean <- unique(df)
      reactiveData(df_clean)
      summaryLog(paste(summaryLog(), "Removed Duplicates:", num_duplicates))
    } else {
      summaryLog(paste(summaryLog(), "No duplicates found."))
    }
  })
  
  observeEvent(input$scaleBtn, {
    req(origionData())
    df <- origionData()
    df <- standardize(df, input$scaleCols)
    reactiveData(df)
  })
  
  output$summaryInfo <- renderPrint({
    summaryLog()
  })
  
  output$dataTable <- renderDT({
    datatable(reactiveData())
  })
  
  output$dataSummary <- renderPrint({
    summary(reactiveData())
  })
  
  output$dupTable <- renderDT({
    df <- reactiveData()
    if (!is.null(df)) {
      dup_rows <- df[duplicated(df), ]
      datatable(dup_rows)
    }
  })
  
  output$plot <- renderPlot({
    req(input$xvar, input$yvar) 
    ggplot(reactiveData(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point() + theme_minimal()
  })
  
  output$distPlot <- renderPlot({
    req(input$distCol, origionData(), reactiveData())
    col <- input$distCol
    orig_values <- origionData()[[col]]
    scaled_values <- reactiveData()[[col]]
    
    plot_data <- data.frame(
      value = c(orig_values, scaled_values),
      Dataset = rep(c("Original", "Scaled"), each = length(orig_values))
    )
    
    ggplot(plot_data, aes(x = value, fill = Dataset)) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
      geom_density(alpha = 0.2) +
      facet_wrap(~Dataset, scales = "free_x") +
      theme_minimal() +
      labs(title = paste("Distribution Impact for", col), x = col, y = "Density")
  })
}

# Run Shiny App
shinyApp(ui, server)
