# Project 2 : Web Application Development and Deployment

Deployed Web App: URL https://ruoshi-zhang.shinyapps.io/STAT5243_Project2_Team8/

## Project Overview

The Interactive Data Analysis App ("IDA") is a one-stop solution designed to make your data workflow seamless and intuitive. Whether you're a beginner or an experienced analyst, this app equips you with everything you need to load, preprocess, analyze, and visualize datasets—all in one interactive platform.

IDA incorporates an intuitive workflow that allows for:
  -   Data Cleaning: Easily handle missing values, remove duplicates, and preprocess data with automated tools.
  -   Feature Engineering: Apply various transformations, including PCA, feature selection, and custom feature creation.
  -   Data Visualization: Create interactive graphs, dynamically select variables and plot types, and visualize numerical, categorical, and mixed data.
  -   Statistical Tests for Model Assumptions: Conduct normality, independence, and stationarity tests to validate analytical models.

## GitHub Files
* The `web_dev.R` file includes coding for data preprocessing (Done by Ruoshi Zhang).
* The `app_yi` file adds code for feature engineering based on the materials from `web_dev.R` (Done by Yi Lu).
* The `web_development_re.R` file adds code for EDA based based on materials from the previous files (Done by Dailin Song).
* The `app.R` file adds code for Statistical Tests, as well as the inclusion of time series data visualization, transformation and pre-processing (Done by Sara Hassani).
* Please refer to **`web_dev.R`** for final version of the web application.

## STEP 1 - Load your dataset

Load your dataset on the landing page of our app. It was designed to cater to multiple file formats, of which are CSV, XLSX, JSON and RDS files. You can also choose one of our two built-in datasets to get acquainted with the features of the IDA.
(Please refer to the 'How to Use The App' page on the web app for more detailed instructions on navigating the app.)

## STEP 2 - Clean your dataset and pre-process variables of interest

The Data Cleaning and Preprocessing modules will allow you to initially clean, transform, and enrich your raw data using the following functions:
  -   Missing Value Strategy: to correct for missing and duplicated values.
  -   Data Type Conversion: Convert columns to appropriate data type
  -   Columns to Scale: to standardize the values of a numeric variable, allowing for better statistical testing down the line.
  -   Categorical Columns to Encode: to transform the categorical variables of your choosing into factor levels or dummy variables, depending on your use case.
  -   Outlier Handling Strategy: winsorize or impute outliers in chosen variables to allow for more precise analysis down the line.
  
## STEP 3 - Conduct Statistical Analysis

### 3.a. Feature Engineering

Once your raw data ready, you can explore a full suite of functionalities on IDA, starting with feature engineering.
On one hand, our app allows for multiple forms of feature selection, whether it be through Principal Component Analysis, LASSO regression, Elastic Net, or Backward Stepwise Regression, among other options.
On the other hand, you can also generate new features from both numeric panel data and time series data. This includes algebraic transformations like variable multiplication and logarithmic transformations, as well as time series adjustments such as differencing and rolling means. These tools make it easy to experiment with feature engineering and immediately see the results.

### 3.b. Exploratory Data Analysis - Visualization

The EDA section provides several ways to explore data visually. IDA enables you to generate insightful visual representations of variable distributions through:
  -   Univariate Analysis: Displaying histograms, boxplots, and other visual tools to understand individual variable distributions.
  -   Bivariate Analysis: Utilizing correlation heatmaps, line plots, and scatter plots to explore relationships between two variables.
  -   Heatmap: Displaying correlations between numerical variables.
  -   Statistical Tests: Helps validate potential relationships between variables and provides initial diagnostics for commonly used model assumptions.
  
These interactive visualizations make data exploration more intuitive and will allow you to uncover key insights efficiently. Furthermore, the statistical tools will allow you to explore and validate patterns in their data before proceeding to more advanced modeling.

