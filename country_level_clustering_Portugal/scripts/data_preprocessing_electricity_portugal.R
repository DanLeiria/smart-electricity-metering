### HEADER -----------------------------------------------------------------
#
# Author:        Daniel Leiria
# Copyright      Copyright 2024 - Daniel Leiria
# Email:         daniel.h.leiria@gmail.com
#
# Date:          2024-07-06
#
# Script Name:   data_preprocessing_electricity_portugal.R
#
# Description:   ETL process for this project
#
#
#
### ------------------------------------------------------------------------

### ------------------------------------------------------------------------
# SETUP
### ------------------------------------------------------------------------

# Clears the console.
cat("\014")

# Remove all variables of the work space.
rm(list = ls())


### ------------------------------------------------------------------------
# LIBRARY
### ------------------------------------------------------------------------

### Check and install missing packages
required_packages <- c("ggplot2",
                       "dplyr",
                       "readxl",
                       "readr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(required_packages, library, character.only = TRUE)



### ------------------------------------------------------------------------
# WORKING DIRECTORY
### ------------------------------------------------------------------------

# Set the working directory to the parent directory of the directory containing this script
if (rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  # Fallback for running outside of RStudio
  script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
}
parent_dir <- dirname(script_dir)  # Navigate one level up
setwd(parent_dir)

# Print the current working directory for debugging
print(paste("Current working directory:", getwd()))


# Ensure that the system language is set to English.
Sys.setenv(LANG = "en")

# Set all locale settings to English.
Sys.setlocale("LC_ALL", "English")

### ------------------------------------------------------------------------
# LOAD DATA
### ------------------------------------------------------------------------

# Fetch the electricity data from local computer
filename_elec <- 'input_data/smart_electricity_meter_portugal_2023.csv'
df_original <- read.csv(filename_elec, sep=';')

names(df_original)[1] <- "year"
names(df_original)[2] <- "month"
names(df_original)[3] <- "date"
names(df_original)[4] <- "postcodes"
names(df_original)[5] <- "energy_kwh"


# Fetch the postcodes data from local computer
filename_post <- 'input_data/cod_post_freg_matched.csv'
df_postcodes = read.csv(filename_post, encoding = "UTF-8") %>% 
  select(CodigoPostal, Distrito, Concelho) %>% 
  rename(
    postcodes = CodigoPostal,
    district = Distrito,
    county = Concelho,
  )


# Fetch the Portuguese industrial data from local computer
filename_industry <- 'input_data/industry_type_portugal_2022.xlsx'
df_original_industry <- read_excel(filename_industry, sheet = 'industry_type')
df_original_industry_normalized <- read_excel(filename_industry, sheet = 'industry_type_normalized')


# Fetch the Portuguese energy usage by sector from local computer
filename_consumption_type <- 'input_data/elec_energy_by_type_portugal_2022.xlsx'
df_original_consumption_type <- read_excel(filename_consumption_type, sheet = 'consumption_type')
df_original_consumption_type_normalized <- read_excel(filename_consumption_type, sheet = 'consumption_type_normalized')


### ------------------------------------------------------------------------
# PREPROCESS POSCODES DATASET
### ------------------------------------------------------------------------

# Function to remove the last 3 digits of the postcodes
remove_last_3_digits <- function(number) {
  as.integer(substr(number, 1, nchar(number) - 3))
}



# Apply the function to the 'postcodes' column
df_postcodes <- df_postcodes %>%
  mutate(postcodes = sapply(postcodes, remove_last_3_digits),
         district_county = paste0(district,"_",county)) %>% 
  distinct()


# These names are repetitive as municipality names
df_postcodes$county[df_postcodes$postcodes == 9560] <- "Lagoa_Azores"

df_postcodes$county[df_postcodes$postcodes == 9370] <- "Calheta_Madeira"
df_postcodes$county[df_postcodes$postcodes == 9374] <- "Calheta_Madeira"
df_postcodes$county[df_postcodes$postcodes == 9385] <- "Calheta_Madeira"

df_postcodes$county[df_postcodes$postcodes == 9850] <- "Calheta_Azores"
df_postcodes$county[df_postcodes$postcodes == 9875] <- "Calheta_Azores"


# Small dataset adjustments before combining with other dataset
df_postcodes$county[df_postcodes$postcodes == 6430] <- "Mêda"
Encoding(df_postcodes$county[df_postcodes$postcodes == 6430]) <- "UTF-8"

df_postcodes$county[df_postcodes$county == "Lagoa "] <- "Lagoa"
df_postcodes$county[df_postcodes$postcodes == 9760] <- "Vila da Praia da Vitória"
Encoding(df_postcodes$county[df_postcodes$postcodes == 9760]) <- "UTF-8"



print(paste("The total number of rows of postcodes on the Portuguese location dataset is:", nrow(df_postcodes)))

df_postcodes_group <- df_postcodes %>% 
  group_by(postcodes) %>% 
  summarise(number_cases = n())

### ------------------------------------------------------------------------
# PREPROCESS ELECTRICITY DATASET
### ------------------------------------------------------------------------

# Obtain data that has 12 months measurements - lower values should be removed
df_original_group <- df_original %>% 
  group_by(postcodes, ) %>% 
  summarise(number_months = n()) %>% 
  filter(number_months == 12)


print(paste("The total number of rows of postcodes on the Portuguese electricity dataset is:", nrow(df_original_group)))


# Filter out all postcodes with less than 12 months measurements.
df_electricity <- df_original %>% 
  filter(postcodes %in% df_original_group$postcodes) %>% 
  arrange(postcodes, month)



# Normalize electricity 
df_electricity <- df_electricity %>% 
  group_by(postcodes) %>% 
  mutate(energy_normalized = (energy_kwh - min(energy_kwh))/(max(energy_kwh) - min(energy_kwh))) %>% 
  ungroup()



### ------------------------------------------------------------------------
# COMBINE DATASETS WITH THEIR ASSOCIATED LOCATION AND POSTCODE
### ------------------------------------------------------------------------

df_industry <- df_postcodes %>% left_join(df_original_industry, by ='county')
df_industry_normalized <- df_postcodes %>% left_join(df_original_industry_normalized, by ='county')

df_consumption_type <- df_postcodes %>% left_join(df_original_consumption_type, by ='county')
df_consumption_type_normalized <- df_postcodes %>% left_join(df_original_consumption_type_normalized, by ='county')


### ------------------------------------------------------------------------
# SAVE DATASETS
### ------------------------------------------------------------------------

# Electricity dataset
filename_1 <- "output_preprocessed/electricity_preprocessed.csv"
write_csv(df_electricity, filename_1)

# Postcodes dataset
filename_2 <- "output_preprocessed/postcodes_preprocessed.csv"
write_csv(df_postcodes, filename_2)


### Industries

# Industries by counties dataset
filename_3 <- "output_preprocessed/county_industries_preprocessed.csv"
write_csv(df_industry, filename_3)

# Industries by counties (normalized) dataset
filename_4 <- "output_preprocessed/county_industries_normalized_preprocessed.csv"
write_csv(df_industry_normalized, filename_4)


### Electricity usage by municipalities

# Electricity usage by counties dataset
filename_5 <- "output_preprocessed/county_consumption_preprocessed.csv"
write_csv(df_consumption_type, filename_5)

# Electricity usage by counties (normalized) dataset
filename_6 <- "output_preprocessed/county_consumption_normalized_preprocessed.csv"
write_csv(df_consumption_type_normalized, filename_6)


