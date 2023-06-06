# The goal of this script is to combine the 8 datasets on international tourism into one master dataset.
# They will be combined using pivot_longer to make the data easy to use to make data visualizations in the future.

# Link to download the data: https://data.worldbank.org/indicator/st.int.arvl
# I changed the names of the csv files after I downloaded them for ease of use

# Tidyverse Functions for general data manipulation
library(tidyverse)

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
clean_data <- function(file_name){
  # read the csv file
  df <- read_csv(paste0('data/raw_files/', file_name), skip = 3)
  
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
  df %>%
    pivot_longer(
      cols = min_year:max_year,
      names_to = "year") %>%
    select(-starts_with('...')) %>%
    return()
}

# run the clean_data function on each csv file
# then combine them and save them to a new csv file
map(file_names, clean_data) %>%
  bind_rows() %>%
  write_csv('data/International_Tourism_Dataset.csv')
