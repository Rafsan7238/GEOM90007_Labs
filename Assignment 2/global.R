# Import necessary libraries

# Data manipulation
library(data.table)         # Efficient data manipulation
setDTthreads(1)             # Set data.table threads to 1 (parallel processing)

# Date and time manipulation
library(lubridate)          # Date and time functions

# Time series representation
library(TSrepr)             # Time series representation functions

# Reading Excel files
library(readxl)             # Read Excel files

# Shiny web application components
library(shiny)              # Shiny web application framework
library(shinydashboard)     # Dashboard layout for Shiny apps
library(shinydashboardPlus) # Enhanced dashboard components
library(shinyEffects)       # Add effects to Shiny elements
library(shinyWidgets)       # Custom widgets for Shiny
library(shinycssloaders)    # CSS loading animations for Shiny
library(shinyTime)          # Time input widgets for Shiny
library(DT)                 # Interactive data tables for Shiny
library(dygraphs)           # Interactive time series graphs for Shiny
library(ggplot2)            # Data visualization with ggplot2
library(ggrepel)            # Repel overlapping text labels in ggplot2
library(dendextend)         # Extending dendrogram functionality
library(plotly)             # Interactive plots with Plotly
library(shinyjs)            # JavaScript integration for Shiny apps
library(tableHTML)          # Create HTML tables in Shiny apps
library(smooth)             # Smoothing functions
library(dtwclust)           # Dynamic time warping clustering
library(COVID19)            # COVID-19 data analysis tools
library(bit64)              # 64-bit integers support
library(padr)               # Data reshaping and filling missing values
library(waiter)             # Loading animations and notifications