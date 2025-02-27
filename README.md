# Project 2 : Web Application Development and Deployment

## Project Overview

In today's data-driven world, the ability to develop web applications is an important skill for
data scientists and analysts. Web applications allow for seamless interaction with data,
enabling users to explore, process, and analyze datasets in an intuitive and scalable way.
Many industries require professionals who can build interactive tools for data visualization,
preprocessing, and decision-making. R Shiny is a powerful framework that bridges the gap
between data science and web development, allowing users to create dynamic applications
with minimal web development knowledge. Through this project, you will gain hands-on
experience in building an interactive web application that can be deployed and shared with
stakeholders, enhancing your professional skill set.

For this project, you are tasked with creating a versatile web application using _R Shiny_. Your
application should be capable of performing various functions, including but not limited to
_dataset uploading, data cleaning, preprocessing, feature engineering, and exploratory data
analysis (EDA_ ). The goal is to develop an intuitive, user-friendly, and functional tool that
allows users to interactively explore and analyze data. Your app should be designed with
flexibility in mind, allowing users to input their own datasets and apply various
transformations dynamically. This project will challenge your problem-solving skills,
encourage creativity, and strengthen your understanding of how data science applications
are developed in real-world scenarios.

## Project Tasks

Below are some essential features that should be included in your application. However, you
are encouraged to expand upon these features and implement additional functionalities that
enhance the user experience and analytical capabilities of your app.

**1. Loading Datasets**
    - Users should be able to upload their own datasets in various formats (e.g., CSV,
       Excel, JSON, and RDS).
    - Provide one or two built-in datasets to illustrate the functionality of the app for users
       who do not have a dataset on hand.


**2. Data Cleaning and Preprocessing**
    - Clean inconsistencies and standardize data formats
    - Identify and handle duplicate values
    - Handle missing values through imputation or removal strategies
    - Perform necessary transformations such as scaling, encoding categorical
       features, and handling outliers


**3. Feature Engineering**
    - Create new features that enhance data insights
    - Provide an interactive way for users to generate and modify features
    - Allow users to see the impact of the transformation modifications


**4. Exploratory Data Analysis (EDA)**
    - Implement interactive visualization
    - Display summary of data (e.g., statistical distributions, correlation etc.)
    - Allow users to dynamically filter and explore the dataset


**5. User Interface (UI) and User Experience (UX)**
    - The UI should be complex yet user-friendly, allowing users to navigate easily and
       interact with all functionalities seamlessly
    - User Guide: provide clear instructions and tooltips to guide users through your
       application (could be the first tab or page of the application)
    - Ensure responsiveness, with a well-structured layout


**6. Web Application Functionality (Interactivity, Usability, and Responsiveness)**
    - The app should be highly interactive, allowing users to dynamically manipulate
       data and visualizations
    - Ensure the app responds quickly to user inputs, with minimal delays in processing
    - Provide a smooth user experience where elements adjust dynamically based on
       selections and inputs


## Project Deliverables [Due on March 12 th at 11:59PM]

1. **Report - Documentation and Deployment** : Write a short [1-2 pages] report explaining
    the app’s functionalities and how to use it. Host/deploy your application on _shinyapps.io_
    or similar platforms and include the deployed application link in the report. Also include
    each team member’s contribution to the project.
    You must submit the report on Courseworks (one member of a team can submit the
    report on behalf of the team).


2. **Shiny Application Source Code Files** : A well-commented Python and/or R script(s)
    containing the full workflow (these files should be submitted in a GitHub repository with
    proper documentation; include a README file with instructions on how to run the code).


## Evaluation Rubrics

**1. Loading Datasets [0 – 8pt]**
    - Basic [2pt] - Supports only one file format
    - Intermediate [5pt] - Supports multiple file formats
    - Advanced [8pt] – Supports various file formats, includes built-in datasets


**2. Data Cleaning and Preprocessing [0 – 8pt]**
    - Basic [2pt] - Simple handling of missing values and duplicates with minimal
       automation
    - Intermediate [5pt] - Includes multiple cleaning options such as scaling and encoding
       categorical variables
    - Advanced [8pt] – Provides an interactive interface for users to choose preprocessing
       steps dynamically with real-time feedback


**3. Feature Engineering [0 – 8pt]**
    - Basic [2pt] - Implements only basic transformations such as creating new features
       manually
    - Intermediate [5pt] - Offers a range of feature transformations with explanations
    - Advanced [8pt] - Provides an intuitive interactive feature engineering tool with real-
       time updates and visual feedback


**4. Exploratory Data Analysis (EDA) [0 – 8pt]**
    - Basic [2pt] - Displays only static summary statistics and basic plots
    - Intermediate [5pt] - Provides interactive visualizations with basic filtering options
    - Advanced [8pt] - Offers fully interactive data exploration with filtering, custom
       visualization settings, and dynamic statistical insights


**5. User Interface (UI) and User Experience (UX) [0 – 8pt]**
    - Basic [2pt] - UI is functional but lacks user-friendliness, with minimal instructions and
       poor design
    - Intermediate [5pt] - UI is well-structured, visually appealing, and includes clear
       navigation and tooltips
    - Advanced [8pt] - UI is highly interactive, visually polished, user-friendly, responsive
       across devices, and does not break easily when handling different inputs


**6. Web Application Functionality (Interactivity, Usability, and Responsiveness) [0 – 8pt]**
    - Basic [2pt] - Limited interactivity, with slow response times and minimal dynamic
       updates
    - Intermediate [5pt] - Offers reasonable interactivity with some dynamic updates and
       a fair level of responsiveness
    - Advanced [8pt] - Highly interactive, smooth, and responsive app that updates
       dynamically based on user input with minimal lag
