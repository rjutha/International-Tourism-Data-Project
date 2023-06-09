# The goal of this script is to combine the 8 datasets on international tourism into one master dataset.
# They will be combined using pivot_longer to make the data easy to use to make data visualizations in the future.
# The script also makes a short version of the data all combined into one csv

# Link to download the data: https://data.worldbank.org/indicator/st.int.arvl
# I changed the names of the csv files after I downloaded them for ease of use

# Tidyverse Functions for general data manipulation
library(tidyverse)
# country_code Function to get iso codes
library(countrycode)

# setting the working directory to the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# names of all the csv files
file_names <- c(
  'International tourism, expenditures (% of total imports).csv',
  'International tourism, expenditures (current US$).csv',
  'International tourism, expenditures for passenger transport items (current US$).csv',
  'International tourism, number of arrivals.csv',
  'International tourism, number of departures.csv',
  'International tourism, receipts (current US$).csv',
  'International tourism, receipts for passenger transport items (current US$).csv',
  'International tourism, receipts for travel items (current US$).csv'
)

# function that combines the yearly data into 1 single column
clean_data_long <- function(file_name){
  # read the csv file and remove columns
  df <- read_csv(paste0('data/raw_files/', file_name), skip = 4) %>%
    select(
      -(`1960`:`1999`),
      -starts_with('...'),
      -`Indicator Code`)
  
  # obtain the year for the oldest record
  min_year <-
    df %>%
    names() %>%
    parse_number() %>%
    min(na.rm = TRUE) %>%
    as.character()
  
  # obtain the year for the newest record
  max_year <-
    df %>%
    names() %>%
    parse_number() %>%
    max(na.rm = TRUE) %>% 
    as.character()
    
  
  # Use pivot_longer to combine the year rows
  # use country_code function to get country names and to remove regions from the data.
  df %>%
    mutate(iso3c = countrycode(`Country Code`, "iso3c", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    select(-`Country Code`) %>%
    pivot_longer(
      cols = min_year:max_year,
      names_to = "year") %>%
    rename(country = `Country Name`,
           indicator = `Indicator Name`) %>%
    select(
      country,
      iso3c,
      indicator,
      year,
      value) %>%
    return()
}

# function that combines the datasets into one csv
clean_data_short <- function(file_name){
  
  # read the csv file and remove columns
  df <- read_csv(paste0('data/raw_files/', file_name), skip = 4) %>%
    select(
      -(`1960`:`1999`),
      -starts_with('...'),
      -`Indicator Code`)
  
  # obtain the year for the oldest record
  min_year <-
    df %>%
    names() %>%
    parse_number() %>%
    min(na.rm = TRUE) %>%
    as.character()
  
  # obtain the year for the newest record
  max_year <-
    df %>%
    names() %>%
    parse_number() %>%
    max(na.rm = TRUE) %>% 
    as.character()
  
  # use country_code function to get country names and to remove regions from the data.
  df %>%
    mutate(iso3c = countrycode(`Country Code`, "iso3c", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    select(-`Country Code`) %>%
    rename(country = `Country Name`,
           indicator = `Indicator Name`) %>%
    select(country, iso3c, indicator, min_year:max_year) %>%
    return()
}

# run the clean_data function on each csv file
# then combine them and save them to a new csv file

map(file_names, clean_data_long) %>%
  bind_rows() %>%
  write_csv('data/International_Tourism_Dataset_Long.csv')

map(file_names, clean_data_short) %>%
  bind_rows() %>%
  write_csv('data/International_Tourism_Dataset_Short.csv')