### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-19

# Script name:  00-library
# R version:    4.4.1
 
# Script Description:
# Setup the packages needed for the specific project.


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SETUP ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Clears the console.
# Remove all variables of the work space.

cat("\014")
rm(list = ls())

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LIBRARY ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Add below your list of packages required for this project.

# List of required libraries
required_libraries <- c(

  # Access the RStudio API
  "rstudioapi",
  
  # Several important packages
  "tidyverse",
  
  # Code profiling
  "profvis",
  
  # Directory tree in R
  "fs",
  
  # Script logs
  "futile.logger",
  
  # Read files
  "readxl", "readr",
  
  # Grid plots
  "gridExtra",
  
  # Plot improvements
  "ggrepel", "viridis",
  
  # For map plots
  "tidygeocoder",
  
  # For maps
  "sf", "maps",
  
  # Code processing timer
  "tictoc",
  
  # Time series clustering algorithms
  "dtwclust",
  
  # Convert the dataframe into a matrix
  "reshape2",
  
  # Strings manipulation
  "stringi"
) 

# Install missing libraries automatically
install_if_missing <- function(packages) {
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(missing_packages)) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
 }
}

# Call the function to install missing packages
install_if_missing(required_libraries)

# Load the libraries
invisible(lapply(required_libraries, library, character.only = TRUE))

# Error handling for package loading
loaded_libraries <- sapply(required_libraries, require, character.only = TRUE)

if (any(!loaded_libraries)) {
  stop("Error: Some required libraries failed to load: ", 
    paste(names(loaded_libraries[!loaded_libraries]), collapse = ", "))
} else {
  # Clear log file before each run
  write("", file = "logs/output.log")  
  flog.appender(appender.file("logs/output.log")) 
  flog.info("All libraries loaded successfully.")
}


# Ensure that the system language is set to English.
# Set all locale settings to English.
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Directory tree in R
fs::dir_tree(path = ".", recurse = 1)

