library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(readr)
library(readxl)
library(jsonlite)
library(tools)
library(dplyr)
library(glmnet)
library(olsrr)
library(stats)
library(lubridate)
library(xts)
library(tseries)

# User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Interactive Data Analysis App",
    
    # 1. Load Datasets
    tabPanel(
      title = "Loading Datasets",
      titlePanel("Loading Datasets"),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Upload Dataset", accept = c(".csv", ".xlsx", ".json", ".rds")),
          selectInput("dataset", "Or Select a Sample Dataset", choices = c("mtcars", "iris")),
          actionButton("loadData", "Load Data"),
        ),
        
        mainPanel(
          h4("Instructions"),
          p("Upload your dataset or select a sample dataset (e.g., mtcars or iris), then click 'Load Data'."),
          
          tabsetPanel(
            tabPanel("Original Data Preview", DTOutput("originalDataTable"))
          )
        )
      )
    ),
    
    # 2. Data Processing
    tabPanel(
      title = "Data Preprocess",
      titlePanel("Data Preprocess"),
      
      sidebarLayout(
        sidebarPanel(
          h4("Data Cleaning & Preprocessing"),
          
          selectInput("missingStrategy", "Missing Value Strategy", 
                      choices = c("Remove Rows", "Impute Values")),
          
          checkboxInput("removeDuplicates", "Remove Duplicates", value = FALSE),
          
          hr(),
          selectizeInput("scaleCols", "Columns to Scale", 
                         choices = NULL, multiple = TRUE),
          
          hr(),
          selectizeInput("encodeCols", "Categorical Columns to Encode", choices = NULL, multiple = TRUE),
          selectInput("encodingStrategy", "Categorical Encoding Strategy",
                      choices = c("None", "One-Hot Encoding", "Dummy Encoding", "Label Encoding")),
          
          hr(),
          
          selectInput("outlierStrategy", "Outlier Handling Strategy", 
                      choices = c("None", "Remove Outliers", "Winsorize Outliers")),
          hr(),
          
          selectizeInput("timeVars", "Select Date Variables", choices = NULL, multiple = TRUE),
          p("The date variables need to be in a format that includes day, month and year values."),
          
          hr(),
          actionButton("processData", "Process Data", class = "btn-primary")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Data Statistics", verbatimTextOutput("dataSummary")),
            tabPanel("Duplicates", DTOutput("dupTable")),
            tabPanel("Distribution",
                     selectInput("distCol", "Select Column for Impact Analysis", choices = NULL),
                     radioButtons("distPlotType", "Plot Type", choices = c("Histogram", "Boxplot")),
                     plotOutput("distPlot")
            ),
            tabPanel("Summary", verbatimTextOutput("summaryInfo")),  
            tabPanel("Processed Data", DTOutput("processedDataTable"))
          )
        )
      )
    ),
    
    # 3. Feature Engineering
    tabPanel(
      title = "Feature Engineering",
      titlePanel("Feature Engineering"),
      
      
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            # PCA
            tabPanel("PCA",
                     h4("PCA (Principal Component Analysis"),
                     selectizeInput("pcaCols", "Select Columns for PCA", choices = NULL, multiple = TRUE),
                     numericInput("numPCA", "Number of Principal Components", value = 2, min = 1, max = 10, step = 1),
                     actionButton("applyPCA", "Apply PCA", class = "btn-primary"),
                     actionButton("savePCA", "Save PCA Result", class = "btn-primary")
            ),
            #Feature Selection
            tabPanel("Feature Selection",
                     h4("Feature Selection"),
                     selectizeInput("FSCols", "Select Methods for Feature Selection", 
                                    choices = c("LASSO", "Elastic Net", "Forward Stepwise", "Backward Stepwise","Bothway Stepwise")),
                     selectizeInput("FSyCols", "Select Dependent Variable", choices = NULL, multiple = FALSE),
                     numericInput("lambdaL", "Value of Lambda for Regularization", value = 0.01, min = 0, max = 100, step = 0.01),
                     checkboxInput("lambdaCV", "Cross Validation for Select Lambda", value = FALSE),
                     selectizeInput("criteriaFS", "Selection Criteria for Stepwise", 
                                    choices = c("p-value","adjust R^2", "AIC","SBIC")),
                     checkboxInput("detailFS", "Show the more detail", value = FALSE),
                     actionButton("applyFS", "Apply Feature Selection", class = "btn-primary")
            ),
            #New Feature
            tabPanel("Making New Feature",
                     h4("Making New Feature"),
                     uiOutput("col1_ui"),
                     radioButtons("input_type", "Choose Input Type:", choices = c("Select Column", "Enter Number"), selected = "Select Column"),
                     uiOutput("col2_or_number_ui"),
                     textInput("NewF", "New Feature Name",value = "New Feature"),
                     selectizeInput("opF", "Operation Choice", choices = c("Addition","Subtraction","Multiplication","Division","Natural Log","Power")),
                     actionButton("applyOP", "Apply Operation", class = "btn-primary"),
                     actionButton("saveNF", "Save the New Feature", class = "btn-primary")
            )
          ) 
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("PCA Summary", verbatimTextOutput("pcaSummary")),
            tabPanel("PCA Transformed Data", DTOutput("pcaDataTable")),
            tabPanel("Feature Selection Summary", verbatimTextOutput("FSSummary")),
            tabPanel("New Feature Summary", verbatimTextOutput("newSummary"))
          )
        )
      )
      
      
      
    ),
    
    # 4. EDA - Visualization tab
    tabPanel(
      title = "EDA - Visualization",
      titlePanel("EDA - Visualization"),
      
      tabsetPanel(
        # Univariate Data Visualization Tab
        tabPanel(
          title = "Univariate Analysis",
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
        
        # Bivariate Data Visualization Tab
        tabPanel(
          title = "Bivariate Analysis",
          sidebarLayout(
            sidebarPanel(
              # Select two variables for analysis in the same row
              fluidRow(
                column(6, selectInput("xVar", "X-Variable 1:", choices = NULL, selected = NULL)),
                column(6, selectInput("yVar", "Y-Variable 2:", choices = NULL, selected = NULL))
              ),
              
              # Checkboxes for plot selection
              fluidRow(
                column(6, checkboxInput("scatterPlot", "Scatter Plot")),
                column(6, checkboxInput("linePlot", "Line Plot"))
              ),
              
              # Smooth option appears only if Scatter Plot is selected
              conditionalPanel(
                condition = "input.scatterPlot == true",
                h4("Scatter Plot Option"),
                checkboxInput("smooth", "Smooth")
              ),
              
              conditionalPanel(
                condition = "input.linePlot == true",
                h4("Line Plot Option"),
                checkboxInput("lineSmooth", "Smooth")
              ),
            ),
            
            mainPanel(
              h4("Visualization"),
              plotOutput("scatterPlotOutput", height = "350px"),
              plotOutput("linePlotOutput", height = "350px")
            )
          )
        ),
        
        # Heat Map Tab
        tabPanel(
          title = "Heat Map",
          sidebarLayout(
            sidebarPanel(
              h4("Correlation Heatmap"),
              p("Displays the correlation between all numeric variables in the dataset.")
            ),
            
            mainPanel(
              plotOutput("heatmapOutput", height = "500px")
            )
          )
        ),
        
        # Time Series Tab
        tabPanel(
          title = "Time Series",
          sidebarLayout(
            sidebarPanel(
              h4("Time Series Visual Analysis"),
              p("Displays time series distribution, ACF and PACF plots."),
              selectizeInput("timeVars3", "Time Variable of Reference", choices = NULL),
              selectizeInput("ts_var", "Variable for Analysis", choices = NULL)
            ),
            
            mainPanel(
              plotOutput("ts_plot", height = "350px"),
              plotOutput("acf_plot", height = "350px"),
              plotOutput("pacf_plot", height = "350px")
              
            )
          )
        ),
      )
    ),
    
    # 5. EDA - Statistical Tests Tab
    tabPanel(title = "EDA - Statistical Tests",
             titlePanel("Statistical Tests"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("stat_test", "Select Statistical Test", 
                             choices = c("Independence Test (Categorical)",
                                         "Non-Linear Correlation/Independence Test (Numeric)",
                                         "Normality Test", 
                                         "Comparing Values for 2 Independent Groups",
                                         "Comparing Values for at least 3 Independent Groups",
                                         "Stationarity")),
                 uiOutput("test_ui_inputs"),  
                 actionButton("run_test", "Run Test")  
               ),
               mainPanel(
                 verbatimTextOutput("Test Results"),
                 tableOutput("shapiro_results"),
                 tableOutput("chisquare_results"),
                 tableOutput("spearman_results"),
                 tableOutput("wilcoxon_results"),
                 tableOutput("kruskal_results"),
                 tableOutput("dicky_results")
               )
             )
    ),
    
    # 6. About Page
    tabPanel(
      title = "How to Use the App",
      titlePanel("About This Project"),
      tags$ol(
        tags$li("Upload a dataset or use a provided sample dataset."),
        tags$li("Navigate to the 'Data Preprocess' tab."),
        tags$li("Clean and transform your data by selecting the appropriate variables and strategies."),
        tags$li("Navigate to the 'Feature Engineering' tab."),
        tags$li("Choose preferred method."),
        tags$li("Specify columns, parameters, or operations as needed."),
        tags$li("Apply and review the results."),
        tags$li("Navigate to the 'EDA' tab."),
        tags$li("Choose the appropriate analysis type."),
        tags$li("Select variables and visualization options."),
        tags$li("Analyze and interpret insights dynamically.")
      ),
      hr(),
      
      h3("Data Cleaning and Preprocessing"),
      p("The Data Cleaning and Preprocessing modules allows users to initially clean, transform, and enrich raw data using the following functions:"),
      tags$ul(
        tags$li(strong("Missingness and Duplication"), " – Handle missing and duplicated values."),
        tags$li(strong("Data Type Conversion"), " – Convert columns to appropriate data type."),
        tags$li(strong("Transformation"), " – Standardize numeric columns, and encode categorical columns."),
        tags$li(strong("Outliers"), " – Detect and handle outliers."),
        tags$li(strong("Date Variable"), " – Conserves time format of specified variables.")
      ),
      
      h3("1️⃣ Missingness and Duplication"),
      tags$ul(
        tags$li("The system automatically identifies missing values."),
        tags$li("Select strategy to deal with null values: remove or impute with median for numeric columns or mode for categorical columns."),
        tags$li("The system will identify duplicated values, see 'Duplicates'."),
        tags$li("Remove duplicated values by clicking 'Remove Duplicates'.")
      ),
      
      h3("2️⃣ Data Type Conversion"),
      p("This function allows users to manually select columns to convert."),
      tags$ul(
        tags$li("The system can automatically recognize data type of every columns, with the exception of time variables."),
        tags$li("For columns that are incorrectly recognized, user can select column(s) to convert to appropriate types.")
      ),
      
      h3("3️⃣ Transformation"),
      p("This function perform necessary transformations to numeric and categorical columns."),
      tags$ul(
        tags$li("Select numeric column(s) to standardize."),
        tags$li("Select categorical column(s) to encode through One-Hot encoding, Dummy encoding, or Label Encoding."),
        tags$li("Click 'New Feature Summary' to  view the effect of transformations.")
      ),
      
      h3("4️⃣ Outliers"),
      p("This function allows user to handle outliers"),
      tags$ul(
        tags$li("The system can automatically detect outliers using an interquartile range (IQR)."),
        tags$li("Select strategy to handle outliers."),
        tags$li("Click 'Processed Data' to  view the current dataset.")
      ),
      hr(),
      
      h3("Feature Engineering"),
      p("The Feature Engineering module allows users to modify and enhance dataset features. It consists of three main functions:"),
      
      tags$ul(
        tags$li(strong("Principal Component Analysis (PCA)"), " – Reduce dimensionality and extract important components."),
        tags$li(strong("Feature Selection"), " – Identify the most relevant features for modeling."),
        tags$li(strong("Custom Feature Creation"), " – Generate new features based on mathematical operations.")
      ),
      
      h3("1️⃣ Principal Component Analysis (PCA)"),
      p("PCA helps users transform features into principal components for dimensionality reduction."),
      tags$ul(
        tags$li("Select features for PCA transformation."),
        tags$li("Choose the number of principal components."),
        tags$li("Remove incorrect selections using the Backspace key."),
        tags$li("The summary of principal components appears in the 'PCA Summary' subpanel."),
        tags$li("Click 'Apply PCA' to transform data and view results in 'PCA Transformed Data'."),
        tags$li("If satisfied, save the transformed data for further analysis in the EDA section by clicking 'Save PCA Result' .")
      ),
      
      h3("2️⃣ Feature Selection"),
      p("This function helps users select the most important features for modeling."),
      tags$ul(
        tags$li("Select the dependent variable and feature selection method."),
        tags$li("Set relevant parameters (irrelevant ones can be ignored)."),
        tags$li("If using regularization, enable cross-validation via 'Cross Validation for Select Lambda'."),
        tags$li("View selected features in 'Feature Selection Summary'."),
        tags$li("For detailed insights, enable 'Show More Detail'.")
      ),
      
      h3("3️⃣ Custom Feature Creation"),
      p("Users can create new features using mathematical operations."),
      tags$ul(
        tags$li("Choose 'Selected Column' for operations on two features or 'Enter Number' for single feature operations."),
        tags$li("Operations include Addition, Subtraction, Multiplication, Division, and Logarithm."),
        tags$li("For division, the first feature is the dividend, the second is the divisor. The same goes for subtraction."),
        tags$li("For the natural logarithm option, only the first feature will take effect. "),
        tags$li("For time series variables, choose the time variable of reference to correctly sort the data in chronological order before applying differencing or rolling means."),
        tags$li("Choose 'Enter Number' in Input Type to specify the number of lags for differencing and rolling means."),
        tags$li("Results appear in 'New Feature Summary' after clicking 'Apply Operation'."),
        tags$li("To save the new feature, click 'Save the New Feature' for further analysis in the EDA section.")
      ),
      hr(),
      
      h3("Exploratory Data Analysis (EDA) - Visulaization"),
      p("The EDA - Visualization module helps users explore and visualize datasets interactively. It consists of four sections:"),
      tags$ul(
        tags$li(strong("Univariate Analysis"), " – Analyze individual variables."),
        tags$li(strong("Bivariate Analysis"), " – Analyze relationships between two variables."),
        tags$li(strong("Heat Map"), " – Visualize correlations between numerical variables."),
        tags$li(strong("Time Series"), " – Visualize distribution, ACF and PACF plots of Time Series data.")
      ),
      
      h3("1️⃣ Univariate Analysis"),
      p("Examine the distribution of a single variable:"),
      tags$ul(
        tags$li(strong("Numerical Analysis:"), "Histogram (custom binwidth, starting bin, percent display), Boxplot (horizontal/vertical), Dotplot."),
        tags$li(strong("Categorical Analysis:"), "Bar Chart (option to display percentages), Pie Chart.")
      ),
      
      h3("2️⃣ Bivariate Analysis"),
      p("Analyze relationships between two variables:"),
      tags$ul(
        tags$li(strong("Numerical vs. Numerical:"), "Scatter Plot (optional trend line), Line Plot (optional smoothing)."),
        tags$li(strong("Categorical vs. Categorical:"), "Grouped Bar Chart, Stacked Bar Chart, 100% Stacked Bar Chart."),
        tags$li(strong("Numerical vs. Categorical:"), "Boxplot, Violin Plot.")
      ),
      
      h3("3️⃣ Heat Map Analysis"),
      p("Displays correlations between numerical variables:"),
      tags$ul(
        tags$li("Color-coded matrix (darker = stronger correlation)."),
        tags$li("Helps identify patterns and dependencies.")
      ),
      
      h3("Time Series Analysis"),
      p("Explore time-dependent patterns and trends:"),
      tags$ul(
        tags$li("Plot distribution of variable of interest against specified time sequence."),
        tags$li("Plot Autocorrelation Function of variable, sorted by specified time variable."),
        tags$li("Plot Partial Autocorrelation Function of variable, sorted by specified time variable."),
      ),
      
      hr(),
      h3("Statistical Testing and Model Assumptions"),
      p("Perform hypothesis tests to validate key statistical assumptions."),
      tags$ul(
        tags$li(strong("Shapiro-Wilk Test"), " – Check if a variable follows a normal distribution."),
        tags$li(strong("Pearson’s Chi-Squared Test"), " – Assess independence between categorical variables."),
        tags$li(strong("Spearman Correlation"), " – Test for non-linear relationships between numeric variables."),
        tags$li(strong("Wilcoxon Rank-Sum Test"), " – Compare distributions of two independent groups."),
        tags$li(strong("Kruskal-Wallis Test"), " – Compare distributions across multiple groups."),
        tags$li(strong("Augmented Dickey-Fuller Test"), " – Determine stationarity in time series data."),
        tags$li("For each test, select variable types as is specified on the web app screen.")
      ),
      hr(),
      
      p("Created with R Shiny, March 2025."),
      p("Created by Dailin Song, Sara Hassani, Yi Lu, Ruoshi Zhang."),
      p("STAT5243 - Applied Data Science, Spring 2025, Columbia University")
    )
  )
)


# Data Cleaning Function
clean_data <- function(df, missing_strategy) {
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

# Encoding categorical features
encode_categorical <- function(df, encode_cols, strategy = "None") {
  if (!is.null(encode_cols) && length(encode_cols) > 0 && strategy != "None") {
    for (col in encode_cols) {
      if (is.factor(df[[col]])) {
        if(strategy == "One-Hot Encoding") {
          # One-hot encoding
          dummies <- model.matrix(~ . -1, data = df[col])
          dummies <- as.data.frame(dummies)
          df[[col]] <- NULL
          df <- cbind(df, dummies)
        } else if (strategy == "Dummy Encoding") {
          # Dummy encoding
          dummies <- model.matrix(~ . , data = df[col])
          dummies <- as.data.frame(dummies[,-1, drop = FALSE])
          df[[col]] <- NULL
          df <- cbind(df, dummies)
        } else if (strategy == "Label Encoding") {
          # Label encoding
          df[[col]] = as.integer(factor(df[[col]]))
        }
      }
    }
  }
  return(df)
}

# Detect and handle outliers
handle_outliers <- function(df, outlier_strategy = "None") {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if(outlier_strategy == "Remove Outliers") {
    for(col in numeric_cols) {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      # Remove rows with outliers
      df <- df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
    }
  } else if(outlier_strategy == "Winsorize Outliers") {
    for(col in numeric_cols) {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR_val
      upper_bound <- Q3 + 1.5 * IQR_val
      df[[col]] <- ifelse(df[[col]] < lower_bound, lower_bound,
                          ifelse(df[[col]] > upper_bound, upper_bound, df[[col]]))
    }
  }
  return(df)
}

#PCA funtion
apply_pca <- function(df, pca_cols, n_components) {
  if (!is.null(pca_cols) && length(pca_cols) > 0) {
    pca_model <- prcomp(df[, pca_cols, drop = FALSE], center = TRUE, scale. = TRUE)
    pca_data <- as.data.frame(pca_model$x[, 1:n_components])
    colnames(pca_data) <- paste0("PC", 1:n_components)
    df <- cbind(df, pca_data)
  }
  return(df)
}

ols_step_way_c <- function(lm, way, c) {
  if (way == "for") {
    if (c == "p-value") {
      return(ols_step_forward_p(lm))
    }
    if (c == "adjust R^2") {
      return(ols_step_forward_adj_r2(lm))
    }
    if (c == "AIC") {
      return(ols_step_forward_aic(lm))
    }
    if (c == "SBIC") {
      return(ols_step_forward_sbic(lm))
    }
  }
  if (way == "back") {
    if (c == "p-value") {
      return(ols_step_backward_p(lm))
    }
    if (c == "adjust R^2") {
      return(ols_step_backward_adj_r2(lm))
    }
    if (c == "AIC") {
      return(ols_step_backward_aic(lm))
    }
    if (c == "SBIC") {
      return(ols_step_backward_sbic(lm))
    }
    
  }
  if (way == "both") {
    if (c == "p-value") {
      return(ols_step_both_p(lm))
    }
    if (c == "adjust R^2") {
      return(ols_step_both_adj_r2(lm))
    }
    if (c == "AIC") {
      return(ols_step_both_aic(lm))
    }
    if (c == "SBIC") {
      return(ols_step_both_sbic(lm))
    }
  }
  return(NULL) 
}


#feature_selection function
applyFS <- function(df, y_cols,method, lambdaL=0.01, lambdaCV=FALSE, criteriaFS="p-value"){
  if(!is.data.frame(df)){
    df <-as.data.frame(df)
  }
  
  X <- df[, setdiff(colnames(df), y_cols)] 
  y<- df[,y_cols]
  lambda_val <- lambdaL
  message<-""
  error_check <- FALSE
  
  if (!all(sapply(X, is.numeric)) || !is.numeric(y)) {
    message <- "The data contained non-numeric data. This program automatically converts the data into numeric. If you want to customize the data, please considering the function in Data Preprocess Page."
  } else {
    X <- as.matrix(sapply(X, as.numeric))  
    y <- as.numeric(y)  
  }
  
  
  if (method == "LASSO") {
    if (lambdaCV) {
      cv_fit <- cv.glmnet(X, y, alpha = 1)
      lambda_val <- cv_fit$lambda.min  
    }
    model <- glmnet(X, y, alpha = lambdaL, lambda = lambda_val)
    selected_features <- rownames(coef(model))[coef(model)[, 1] != 0]
    
    
  } else if (method == "Elastic Net") {
    if (lambdaCV) {
      cv_fit <- cv.glmnet(X, y, alpha = 0.5)
      lambda_val <- cv_fit$lambda.min
    }
    model <- glmnet(X, y, alpha = lambdaL, lambda = lambda_val)
    selected_features <- rownames(coef(model))[coef(model)[, 1] != 0]
    
    
  } else if (method == "Forward Stepwise") {
    lm_model <- lm(as.formula(paste(y_cols, "~", paste(colnames(X), collapse = " + "))), data = df)
    model <- ols_step_way_c(lm_model,"for",criteriaFS) 
    selected_features <-  names(coef(model$model))[-1]  
    
  } else if (method == "Backward Stepwise") {
    lm_model <- lm(as.formula(paste(y_cols, "~", paste(colnames(X), collapse = " + "))), data = df)
    model <- ols_step_way_c(lm_model,"back",criteriaFS) 
    selected_features <- names(coef(model$model))[-1]  
  }else if(method == "Bothway Stepwise"){
    lm_model <- lm(as.formula(paste(y_cols, "~", paste(colnames(X), collapse = " + "))), data = df)
    model <- ols_step_way_c(lm_model,"both",criteriaFS) 
    selected_features <- names(coef(model$model))[-1]  
  }
  result<-list(method = method,
               lambda = lambda_val,
               selected_features = selected_features,
               error_check = error_check,
               model = model)
  return(result)
}

#new Feature
new_maker <- function(df, col1, operation, input_type, col2, number_input, new_col_name, time) {
  operation_map <- list(
    "Addition" = `+`,
    "Subtraction" = `-`,
    "Multiplication" = `*`,
    "Division" = `/`,
    "Natural Log" = log,
    "Power" = `^`
  )
  
  if(operation %in% names(operation_map)){
    if (input_type == "Select Column" && !is.null(col2)) {
      df[[new_col_name]] <- operation_map[[operation]](df[[col1]], df[[col2]])
    } else if (input_type == "Enter Number" && !is.null(number_input)) {
      if (operation == "Natural Log") {
        df[[new_col_name]] <- log(df[[col1]])
      } else {
        df[[new_col_name]] <- operation_map[[operation]](df[[col1]], number_input)
      }
    }
  }
  
  # Time Series Feature Engineering
  if(operation == "Month"){
    df[[new_col_name]] = month(df[[col1]])
  }
  if(operation == "Year"){
    df[[new_col_name]] = year(df[[col1]])
  }
  if(operation == "Day of the Week"){
    df[[new_col_name]] = weekdays(df[[col1]])
  }
  if(operation == "Differencing" && input_type == "Enter Number" && !is.null(number_input)){
    temp = xts(df[[col1]], order.by = df[[time]])
    df[[new_col_name]] = c(rep(NA, number_input), diff(df[[col1]], lag = number_input))
  }
  if(operation == "Rolling Mean" && input_type == "Enter Number" && !is.null(number_input)){
    temp = xts(df[[col1]], order.by = df[[time]])
    df[[new_col_name]] = rollmean(df[[col1]], k = number_input, fill = NA, align = "right")
  }
  
  return(df)
}

# Server Logic
server <- function(input, output, session) {
  origionData <- reactiveVal(NULL)  
  reactiveData <- reactiveVal(NULL)  
  summaryLog <- reactiveVal("No modifications made yet.")
  PCA_transformed_Data<- reactiveVal(NULL)  
  FS_result<- reactiveVal(NULL)  
  NF_Data <- reactiveVal(data.frame())  
  new_feature_name <- reactiveVal(NULL)  
  
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
      updateSelectInput(session, "distCol", choices = numeric_cols)
      updateSelectInput(session, "ts_var", choices = names(df))
      updateSelectizeInput(session, "scaleCols", choices = numeric_cols, server = TRUE)
      
      categorical_cols <- names(df)[sapply(df, is.factor)]
      updateSelectizeInput(session, "encodeCols", choices = categorical_cols, server = TRUE)
      
      updateSelectInput(session, "timeVars", choices = names(df))
      updateSelectInput(session, "timeVars2", choices = names(df))
      updateSelectInput(session, "timeVars3", choices = names(df))
    }
  })
  
  observeEvent(input$processData, {
    req(origionData())
    df <- origionData()
    
    # 1. Remove Duplicates if checked
    if (input$removeDuplicates) {
      num_duplicates <- sum(duplicated(df))
      if (num_duplicates > 0) {
        df <- unique(df)
        summaryLog(paste(summaryLog(), "Removed Duplicates:", num_duplicates))
      } else {
        summaryLog(paste(summaryLog(), "No duplicates found."))
      }
    }
    
    # 2. Scale if user selected columns
    if (!is.null(input$scaleCols) && length(input$scaleCols) > 0) {
      df <- standardize(df, input$scaleCols)
      summaryLog(paste(summaryLog(), "Scaled columns:", paste(input$scaleCols, collapse = ", ")))
    }
    
    # 3. Encode categorical cols if user selected cols & strategy != "None"
    if (!is.null(input$encodeCols) && length(input$encodeCols) > 0 && input$encodingStrategy != "None") {
      df <- encode_categorical(df, input$encodeCols, strategy = input$encodingStrategy)
      summaryLog(paste(summaryLog(), "Encoded columns:", paste(input$encodeCols, collapse = ", "),
                       "Strategy:", input$encodingStrategy))
    }
    
    # 4. Handle Outliers
    if (input$outlierStrategy != "None") {
      df <- handle_outliers(df, 
                            outlier_strategy = input$outlierStrategy)
      summaryLog(paste(summaryLog(), "Outlier Handling:", input$outlierStrategy))
    }
    
    # 5. Turn variables into Date format
    if(!is.null(input$timeVars)){
      l = length(input$timeVars)
      for(i in 1:l){
        var = input$timeVars[i]
        df[[var]] = as.Date(df[[var]])
      }
    }
    
    # Update reactiveData
    reactiveData(df)
  })
  
  
  output$originalDataTable <- renderDT({
    req(origionData())
    datatable(origionData())
  })
  
  output$processedDataTable <- renderDT({
    req(reactiveData())
    datatable(reactiveData())
  })
  
  output$summaryInfo <- renderPrint({
    summaryLog()
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
    
    # Extract the chosen column from both original and processed data
    orig_values <- origionData()[[col]]
    proc_values <- reactiveData()[[col]]
    
    # Build two data frames 
    plot_data_orig <- data.frame(value = orig_values, Dataset = "Original")
    plot_data_proc <- data.frame(value = proc_values, Dataset = "Processed")
    
    # Combine
    plot_data <- rbind(plot_data_orig, plot_data_proc)
    
    if (input$distPlotType == "Histogram") {
      ggplot(plot_data, aes(x = value, fill = Dataset)) +
        geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
        geom_density(alpha = 0.2) +
        facet_wrap(~Dataset, scales = "free_x") +
        theme_minimal() +
        labs(title = paste("Distribution Impact for", col),
             x = col, y = "Density")
    } else { # boxplot
      ggplot(plot_data, aes(x = Dataset, y = value, fill = Dataset)) +
        geom_boxplot(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Boxplot Comparison for", col),
             x = "Dataset", y = col)
    }
  })
  
  #PCA 
  observe({
    df <- reactiveData()
    if (!is.null(df)) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectizeInput(session, "pcaCols", choices = numeric_cols, server = TRUE)
    }
  })
  
  
  observeEvent(input$applyPCA, {
    req(reactiveData(), input$pcaCols, input$numPCA)
    df <- reactiveData()
    df <- apply_pca(df, input$pcaCols, input$numPCA)
    PCA_transformed_Data(df)
  })
  
  
  output$pcaSummary <- renderPrint({
    req(input$pcaCols)
    df <- reactiveData()
    pca_model <- prcomp(df[, input$pcaCols, drop = FALSE], center = TRUE, scale. = TRUE)
    summary(pca_model)
  })
  
  output$pcaDataTable <- renderDT({
    req(PCA_transformed_Data())
    datatable(PCA_transformed_Data(),options = list(scrollX = TRUE))
  })
  
  observeEvent(input$savePCA, {
    df<- PCA_transformed_Data()
    reactiveData(df)
  })
  
  #feature selection
  observeEvent(input$applyFS, {
    req(reactiveData(), input$FSyCols, input$FSCols)
    df <- reactiveData()
    result<- applyFS(df, input$FSyCols, input$FSCols,input$lambdaL,input$lambdaCV,input$criteriaFS)
    FS_result(result)
  })
  
  observe({
    df <- reactiveData()
    if (!is.null(df)) {
      updateSelectizeInput(session, "FSyCols", choices = colnames(df), server = TRUE)
    }
  })
  
  # Output feature selection 
  output$FSSummary <- renderPrint({
    result <- FS_result() 
    req(result)
    if(result$error_check){
      cat("Error Message:", result$message, "\n") 
    }  
    cat("Feature Selection Method:", result$method, "\n")    
    if (result$method %in% c("LASSO", "Elastic Net")) {
      cat("Lambda:", result$lambda, "\n")
    }    
    cat("Selected Features:", 
        if (length(result$selected_features) > 0) {
          paste(result$selected_features, collapse = ", ")
        } else {
          "No features selected."
        }, "\n"
    )
    
    if(input$detailFS){
      cat("\n---------- Model Detail ----------\n")
      if (result$method %in% c("LASSO", "Elastic Net")){
        cat("\nCoefficients at Best Lambda:\n") 
        print(coef(result$model, s = "lambda.min"))
        cat("\nDeviance Ratio:\n")
        print(result$model$dev.ratio)      			
      }
      else{
        print(result$model)
      }
    }
    
  })
  
  #new feature 
  output$col1_ui <- renderUI({
    data <- reactiveData()
    req(data)
    selectInput("col1", "Select First Column:", choices = names( data))
  })
  
  output$col2_or_number_ui <- renderUI({
    data <- reactiveData()
    req(data)
    if (input$input_type == "Select Column") {
      selectInput("col2", "Select Second Column:", choices = names( data))
    } else {
      numericInput("number_input", "Enter Number:", value = 1)
    }
  })
  
  observeEvent(input$applyOP, {
    df <- reactiveData()  
    
    updated_df <- new_maker(
      df = df,
      col1 = input$col1,
      operation = input$opF,
      input_type = input$input_type,
      col2 = input$col2,
      number_input = input$number_input,
      new_col_name = input$NewF,
      time = input$timeVars2
    )
    
    NF_Data(updated_df)  
    new_feature_name(input$NewF)
  })
  
  
  observeEvent(input$saveNF,{
    req(NF_Data())
    reactiveData(NF_Data())
  })
  
  output$newSummary <- renderPrint({
    req(NF_Data(), new_feature_name())  
    df <- NF_Data()
    colname <- new_feature_name()
    cat("New Feature Name:",colname,"\n")
    cat("Distribution:","\n")
    print(summary(df[[colname]]))
  })
  
  
  observe({
    df <- reactiveData()
    if (!is.null(df)) {
      updateSelectInput(session, "statVar", choices = names(df), selected = names(df)[1])
    }
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
  
  observe({
    df <- reactiveData()
    req(df)
    
    updateSelectInput(session, "xVar", choices = names(df), selected = names(df)[1])
    updateSelectInput(session, "yVar", choices = names(df), selected = names(df)[2])
  })
  
  # Render Scatter Plot
  output$scatterPlotOutput <- renderPlot({
    df <- reactiveData()
    req(df, input$xVar, input$yVar, input$scatterPlot)
    
    p <- ggplot(df, aes_string(x = input$xVar, y = input$yVar)) +
      geom_point(color = "blue") +
      theme_minimal() +
      labs(title = "Scatter Plot", x = input$xVar, y = input$yVar)
    
    # Add smooth line if selected
    if (input$smooth) {
      p <- p + geom_smooth(method = "loess", color = "red", se = FALSE)
    }
    
    p
  })
  
  output$linePlotOutput <- renderPlot({
    df <- reactiveData()
    req(df, input$xVar, input$yVar, input$linePlot)
    
    p <- ggplot(df, aes_string(x = input$xVar, y = input$yVar)) +
      geom_line(color = "black") +
      theme_minimal() +
      labs(title = "Line Plot", x = input$xVar, y = input$yVar)
    
    # Add smooth line if selected
    if (input$lineSmooth) {
      p <- p + geom_smooth(method = "loess", color = "blue", se = FALSE)
    }
    
    p
  })
  
  # Render Correlation Heatmap
  output$heatmapOutput <- renderPlot({
    df <- reactiveData()
    req(df)
    
    # Select only numeric columns
    numeric_df <- df %>% select(where(is.numeric))
    
    # Check if there are numeric variables to compute correlation
    if (ncol(numeric_df) < 2) {
      showNotification("Not enough numeric variables for correlation heatmap.", type = "warning")
      return(NULL)
    }
    
    # Compute correlation matrix
    cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
    
    # Convert correlation matrix into long format for ggplot
    cor_long <- reshape2::melt(cor_matrix)
    
    # Create heatmap using ggplot2
    ggplot(cor_long, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Correlation Heatmap", x = "", y = "")
  })
  
  # Output Time Series Plots
  TS_PLOT <- reactive({
    df <- reactiveData()
    req(df, input$ts_var, input$timeVars3)
    
    temp <- xts(df[[input$ts_var]], order.by = df[[input$timeVars3]])
    title <- paste0("Time Series Distribution of ", input$ts_var)
    plot(temp, main = title)
  })
  
  ACF_PLOT <- reactive({
    df <- reactiveData()
    req(df, input$ts_var, input$timeVars3)
    
    var1 = df[[input$ts_var]]
    var2 = df[[input$timeVars3]]
    
    var1 = var1[order(var2)]
    title <- paste0(input$ts_var, "ACF")
    acf(var1, main = title)
  })
  
  PACF_PLOT <- reactive({
    df <- reactiveData()
    req(df, input$ts_var, input$timeVars3)
    
    var1 = df[[input$ts_var]]
    var2 = df[[input$timeVars3]]
    
    var1 = var1[order(var2)]
    title <- paste0(input$ts_var, "PACF")
    pacf(var1, main = title)
  })
  
  # Use renderPlot() to display the plots in the UI
  output$ts_plot <- renderPlot({
    TS_PLOT()  # Call the reactive expression
  })
  
  output$acf_plot <- renderPlot({
    ACF_PLOT()  # Call the reactive expression
  })
  
  output$pacf_plot <- renderPlot({
    PACF_PLOT()  # Call the reactive expression
  })
  
  # Running Statistical Tests
  shapiro_results = reactiveVal()
  chisquare_results = reactiveVal()
  spearman_results = reactiveVal()
  wilcoxon_results = reactiveVal()
  kruskal_results = reactiveVal()
  dicky_results = reactiveVal()
  
  output$test_ui_inputs <- renderUI({
    req(input$stat_test)  
    
    df <- reactiveData()
    req(df)
    
    if (input$stat_test == "Normality Test") {
      selectInput("var_normality", "Select Variable", choices = names(df))
    } else {
      if (input$stat_test == "Independence Test (Categorical)") {
        selectizeInput("indep_categ", "Select Two Variables", 
                       choices = names(df)[sapply(df, is.factor)], 
                       multiple = TRUE, 
                       options = list(maxItems = 2, 
                                      placeholder = 'Select exactly two variables'))
      } else {
        if(input$stat_test == "Non-Linear Correlation/Independence Test (Numeric)"){
          selectizeInput("non_linear", "Select Two Variables", 
                         choices = names(df)[sapply(df, is.numeric)], 
                         multiple = TRUE, 
                         options = list(maxItems = 2, 
                                        placeholder = 'Select exactly two variables'))
        }else{
          if(input$stat_test == "Comparing Values for 2 Independent Groups"){
            
            fluidRow(
              column(6, selectInput("factor_var", "Select Categorical Variable", 
                                    choices = names(df)[sapply(df, is.factor)], 
                                    selected = NULL)),
              column(6, selectInput("numeric_var", "Select Numeric Variable", 
                                    choices = names(df)[sapply(df, is.numeric)], 
                                    selected = NULL))
            )
            
          }else{
            if(input$stat_test == "Comparing Values for at least 3 Independent Groups"){
              
              fluidRow(
                column(6, selectInput("ordinal_var", "Select Ordinal Variable", 
                                      choices = names(df)[sapply(df, is.factor)], 
                                      selected = NULL)),
                column(6, selectInput("numeric_var2", "Select Numeric Variable", 
                                      choices = names(df)[sapply(df, is.numeric)], 
                                      selected = NULL))
              )
              
            }else{
              if(input$stat_test == "Stationarity"){
                
                fluidRow(
                  column(6, selectInput("timesVar4", "Select Time Variable", 
                                        choices = names(df), 
                                        selected = NULL)),
                  column(6, selectInput("station", "Select Variable for Analysis", 
                                        choices = names(df), 
                                        selected = NULL))
                )
                
              }else{
                return(NULL)  
              }
            }
          }
        }  
      }
    }
    
  })
  
  # Running the test
  observeEvent(input$run_test, {
    req(input$stat_test, reactiveData())  # Ensure test type and data exist
    
    df <- reactiveData()
    
    if (input$stat_test == "Normality Test") {
      req(input$var_normality)  
      
      # Ensure the selected variable is numeric
      req(is.numeric(df[[input$var_normality]]))
      
      # Perform Shapiro-Wilk test
      shapiro = shapiro.test(df[[input$var_normality]])
      shapiro = data.frame(
        '.' = c("test statistic", "p-value"),
        'Shapiro-Wilk Normality Test' = c(shapiro$statistic, shapiro$p.value)
      )
      
      # Store results
      shapiro_results(shapiro)
      
    }else{
      if (input$stat_test == "Independence Test (Categorical)"){
        req(input$indep_categ)
        
        if (length(input$indep_categ) != 2) {
          showNotification("Error: Please select exactly 2 categorical variables.", 
                           type = "error")
          return(NULL)
        }
        
        var1 = input$indep_categ[1]  
        var2 = input$indep_categ[2]
        
        chi_sq = chisq.test(table(df[[var1]], df[[var2]]))
        chi_sq = data.frame(
          '.' = c("test statistic", "p-value"),
          'Pearson.s Chi-squared test' = c(chi_sq$statistic, chi_sq$p.value)
        )
        chisquare_results(chi_sq)
        
      }else{
        if(input$stat_test == "Non-Linear Correlation/Independence Test (Numeric)"){
          req(input$non_linear)
          
          if (length(input$indep_categ) != 2) {
            showNotification("Error: Please select exactly 2 numeric variables.", 
                             type = "error")
            return(NULL)
          }
          
          var1 = input$non_linear[1]  
          var2 = input$non_linear[2]
          
          spearman = cor.test(df[[var1]], df[[var2]], method = "spearman")
          spearman = data.frame(
            '.' = c("test statistic", "p-value"),
            'Spearman Correlation Coefficient' = c(spearman$statistic, spearman$p.value)
          )
          spearman_results(spearman)
        }else{
          if(input$stat_test == "Comparing Values for 2 Independent Groups"){
            req(input$factor_var)
            req(input$numeric_var)
            
            var1 = input$factor_var
            var2 = input$numeric_var
            
            wilcox = wilcox.test(df[[var1]], df[[var2]])
            wilcox = data.frame(
              '.' = c("test statistic", "p-value"),
              'Wilcoxon Two Sided Rank Sum Test' = c(wilcox$statistic, wilcox$p.value)
            )
            wilcoxon_results(wilcox)
          }else{
            if(input$stat_test == "Comparing Values for at least 3 Independent Groups"){
              req(input$ordinal_var)
              req(input$numeric_var2)
              
              var1 = input$ordinal_var
              var2 = input$numeric_var2
              
              kruskal = kruskal.test(df[[var2]] ~ df[[var1]])
              kruskal = data.frame(
                '.' = c("test statistic", "p-value"),
                'Kruskal Wallis Test' = c(kruskal$statistic, kruskal$p.value)
              )
              kruskal_results(kruskal)
            }else{
              if(input$stat_test == "Stationarity"){
                req(input$station)
                req(input$timesVar4)
                
                var1 = df[[input$station]]
                var2 = df[[input$timesVar4]]
                
                var1 = var1[order(var2)]
                
                dicky = adf.test(var1)
                dicky = data.frame(
                  '.' = c("test statistic", "p-value"),
                  'Augmented Dicky Fueller Test' = c(dicky$statistic, dicky$p.value)
                )
                dicky_results(dicky)
              }
            }
          }
        }
      }
    }
  })
  
  # Displaying the test results
  output$shapiro_results <- renderTable({
    req(shapiro_results)
    shapiro_results()  
  })
  
  output$chisquare_results = renderTable({
    req(chisquare_results)
    chisquare_results()
  })
  
  output$spearman_results <- renderTable({
    req(spearman_results)
    spearman_results()  
  })
  
  output$wilcoxon_results <- renderTable({
    req(wilcoxon_results)
    wilcoxon_results()  
  })
  
  output$kruskal_results <- renderTable({
    req(kruskal_results)
    kruskal_results()  
  })
  
  output$dicky_results <- renderTable({
    req(dicky_results)
    dicky_results()  
  })
}


# Run Shiny App
shinyApp(ui, server)