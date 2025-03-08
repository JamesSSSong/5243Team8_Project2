library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(readr)
library(readxl)
library(jsonlite)
library(tools)
library(dplyr)

clean_data <- function(df, missing_strategy = "Remove Rows") {
  # Replace common missing value tokens with NA
  df <- df %>% mutate(across(where(is.character), ~ ifelse(. %in% c("?", "N/A", "NaN", "", " "), NA, .)))
  
  # Standardize char columns
  df <- df %>% mutate(across(where(is.character), ~ trimws(.) %>% tolower()))
  
  # Convert necessary columns to date format
  df <- df %>% mutate(across(where(is.character), ~ ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", .),
                                                           ymd(.), .)))
  
  # Convert to numeric columns
  df <- df %>% mutate(across(where(is.character), ~ ifelse(grepl("^[0-9\\.]+$", .),
                                                           parse_number(.), .)))
  
  # Convert to categorical cols
  df <- df %>% mutate(across(where(is.character), ~ {if(length(unique(.)) <= 10) {
    factor(.)
  } else {.}
  }))
  
  # Handle missing values
  if(missing_strategy == "Remove Rows") {
    # Remove rows with any missing values
    df <- na.omit(df)
  } else if(missing_strategy == "Impute Values") {
    # For numeric col: impute median
    df <- df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
    # For categorical: impute mode
    df <- df %>% mutate(across(where(is.factor), ~ {
      if(any(is.na(.))) {
      mode_val <- names(sort(table(.), decreasing = TRUE))[1]
      # Replace with mode
      replace(., is.na(.), mode_val)} 
      else {.}
    }))
  }
  
  return(df)
}

standardize <- function(df, scale_cols) {
  if (!is.null(scale_cols) && length(scale_cols) > 0) {
    df <- df %>% mutate(across(all_of(scale_cols), ~ as.numeric(scale(.))))
  }
  return(df)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Interactive Data Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Dataset", accept = c(".csv", ".xlsx", ".json", ".rds")),
      selectInput("dataset", "Or Select a Sample Dataset", 
                  choices = c("mtcars", "iris")),
      selectInput("missingStrategy", "Missing Value Strategy", 
                  choices = c("Remove Rows", "Impute Values")),
      actionButton("loadData", "Load Data"),
      actionButton("removeDup", "Remove Duplicate"),
      selectizeInput("scaleCols", "Columns to Scale", 
                     choices = NULL, multiple = TRUE),
      actionButton("scaleBtn", "Standardize"),
      hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("dataTable")),
        tabPanel("Data Summary", verbatimTextOutput("dataSummary")),
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
)

server <- function(input, output, session) {
  # Store 
  origionData <- reactiveVal(NULL)  
  reactiveData <- reactiveVal(NULL)  
  summaryLog <- reactiveVal("No modifications made yet.")  # Store modification logs
  
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
    
    # Clean & standardize the data
    df <- clean_data(df, missing_strategy = input$missingStrategy)
    
    origionData(df)
    reactiveData(df)
    summaryLog("Data loaded successfully.")  # Log the event
  })
  
  #Save the Choice
  observe({
    df <- reactiveData()  # Get dataset
    if (!is.null(df)) {
      updateSelectInput(session, "xvar", choices = names(df)) 
      updateSelectInput(session, "yvar", choices = names(df))
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectizeInput(session, "scaleCols", choices = numeric_cols, server = TRUE)
      
      # For distribution analysis, show all numeric columns
      updateSelectInput(session, "distCol", choices = numeric_cols)
    }
  })
  
  # Identify & Remove Duplicates
  observeEvent(input$removeDup, {
    df <- reactiveData()
    num_duplicates <- sum(duplicated(df))  # Count exact duplicate rows
    dup_rows <- df[duplicated(df), ] # identify duplicates_rows
    
    if (num_duplicates > 0) {
      df_clean <- unique(df)  # Remove duplicates
      reactiveData(df_clean)  
      summaryLog(paste(summaryLog(), "Removed Duplicates:", num_duplicates))  # Append log
    } else {
      summaryLog(paste(summaryLog(), "No duplicates found."))  # Append log
    }
  })
  
  # After selecting cols to scale, update data by applying scaling
  observeEvent(input$scaleBtn, {
    req(origionData())
    df <- origionData()
    df <- standardize(df, input$scaleCols)
    reactiveData(df)
  })
  
  # Display Modification Summary
  output$summaryInfo <- renderPrint({
    summaryLog()  # Display all changes made to data
  })
  
  # Show Data Preview
  output$dataTable <- renderDT({
    datatable(reactiveData())
  })
  
  # Show Data Summary
  output$dataSummary <- renderPrint({
    summary(reactiveData())
  })
  
  # Show duplicated rows
  output$dupTable <- renderDT({
    df <- reactiveData()
    if (!is.null(df)) {
      dup_rows <- df[duplicated(df), ]
      datatable(dup_rows)
    }
  })
  
  #Show Plot
  output$plot <- renderPlot({
    req(input$xvar, input$yvar) 
    ggplot(reactiveData(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point() + theme_minimal()
  })
  
  # Histograms with density curves
  output$distPlot <- renderPlot({
    req(input$distCol, origionData(), reactiveData())
    col <- input$distCol
    # Extract the original and transformed values for the chosen columns
    orig_values <- origionData()[[col]]
    scaled_values <- reactiveData()[[col]]
    
    # Combine into one data frame with an indicator variable.
    if (col %in% input$scaleCols) {
      # Show both original and scaled distributions
      plot_data <- data.frame(
        value = c(orig_values, scaled_values),
        Dataset = rep(c("Original", "Scaled"), each = length(orig_values))
      )
      p <- ggplot(plot_data, aes(x = value, fill = Dataset)) +
        geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
        geom_density(alpha = 0.2) +
        facet_wrap(~Dataset, scales = "free_x") +
        theme_minimal() +
        labs(title = paste("Distribution Impact for", col),
             x = col, y = "Density")
    } else {
      # If not scaled, show a single distribution from the original data
      p <- ggplot(data.frame(value = orig_values), aes(x = value)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
        geom_density(alpha = 0.2, fill = "lightblue") +
        theme_minimal() +
        labs(title = paste("Distribution for", col),
             x = col, y = "Density")
    }
    p
  })
    
}

shinyApp(ui, server)