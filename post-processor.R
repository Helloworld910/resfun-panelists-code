# Metadata ----------------------------------------------------------------
# Title: post-process
# Description: This script loads all the saved data, applies some post-processing and then saves them as one csv file in the post processed data folder inside the data folder.
# Date created: 05/04/2024
# Author: Shivam Sen
# Encoding: UTF-8
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# RStudio 2022.07.2+576 "Spotted Wakerobin" Release (e7373ef832b49b2a9b88162cfe7eac5f22c40b34, 2022-09-06) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
# Tidyverse version 1.3.2
# Magrittr version 2.0.3
# Here version 1.0.1


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(here)





post_process <- function() {
  # Post process 2007 --------------------------------------------------------------------
  # Load 2007 data
  data_sub_2007 <- as_tibble(read.csv("data/data_sub_2007.csv"))
  
  # Add url
  data_sub_2007$url <- "https://wayback.webarchiv.cz/wayback/20071007210841/http://pala.gacr.cas.cz/wordpress/"
  
  # Define function to process panelist names
  proc_names <- function(x) {
    # Separate names by space
    names_temp <- unlist(str_split(x, " "))
    # Extract first name
    first_name <- names_temp[1]
    #Extract second name
    second_name <- names_temp[2]
    # Form easily separable string with info
    names_temp <- paste0(first_name, "...CUT HERE...", second_name)
    # Return formed string
    return(names_temp)
  }
  
  # Add easily separable temp column with names
  data_sub_2007 %<>% 
    rowwise() %>% 
    mutate(names_temp = proc_names(panelist_name))
  
  # Process data
  data_sub_2007 %<>%
    # Separate temp column
    separate(names_temp, into = c("first_name", "second_name"), sep="...CUT HERE...") %>% 
    # Remove whitespaces from panel name
    mutate(panel_name = str_squish(panel_name)) %>% 
    # Add panel_code and member_type with missing values
    mutate(panel_code = NA, member_type = NA) %>% 
    # Convert year to character
    mutate(year=as.character(year)) %>% 
    # Select standardised columns
    select(panel_code, panel_name, first_name, last_name=second_name, titles, member_type, 
           affiliation=panelist_affiliation, year, url)
  
  # Note that the titles contain some noise for this year like 'a. s.' and 'v. v. i.'
  
  # Post process 2008 --------------------------------------------------------------------
  # Load 2008 data
  data_sub_2008 <- as_tibble(read.csv("data/data_sub_2008.csv"))
  
  # Add url
  data_sub_2008$url <- "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/"
  
  # Define function to process panelist names
  proc_names <- function(x) {
    # Extract titles from string
    titles <- unlist(str_extract_all(x, "\\b\\w+\\b\\."))
    # Collapse titles list
    titles <- paste(titles, collapse = " ")
    # Remove titles from original string
    name_temp <- str_remove_all(x, "\\b\\w+\\b\\.")
    # Clear any white space
    name_temp <- str_squish(name_temp)
    # Split again by space
    name_temp <- unlist(str_split(name_temp, " "))
    # Fetch first name
    first_name <- name_temp[1]
    # Fetch second name
    second_name <- name_temp[2]
    # Put together all member info into one string ready to seperate
    mem_temp <- paste0(first_name, "...CUT HERE...", second_name, "...CUT HERE...", titles)
    # Return string
    return(mem_temp)
  }
  
  # Process panelist names
  data_sub_2008 %<>% 
    rowwise() %>% 
    # Make easy to separate temp column with panelist info
    mutate(temp_names = proc_names(panelist_name))
  
  # Separate the temp column
  data_sub_2008 %<>% 
    # Separate the temp column
    separate(temp_names, sep="...CUT HERE...", into=c("first_name", "second_name", "titles"))
  
  # Define function to find member type
  mem_type <- function(x,y) {
    # Split chairperson's name by comma
    ch_temp <- unlist(str_split(y, ","))
    # Pick first part containing name
    ch_temp <- ch_temp[1]
    # Fetch the first four words from chairperson's name
    #name_check <- str_extract(x, "^\\b\\w+\\b(?:.?\\s\\b\\w+\\b){0,3}")
    #chair_check <- str_extract(ch_temp, "^\\b\\w+\\b(?:.?\\s\\b\\w+\\b){0,3}")
    if(ch_temp == x){
      mem_t <- "Chairperson"
    } else{
      mem_t <- "Member"
    }
    return(mem_t)
  }
  
  # Add member type to data
  data_sub_2008 %<>% 
    rowwise() %>% 
    mutate(member_type = mem_type(panelist_name, chair))
  
  # Apply further processing
  data_sub_2008 %<>% 
    # Remove extra whitespaces from affiliation string
    mutate(affiliation = str_squish(affiliation)) %>% 
    # Change panel codes to characters
    mutate(panel_code = as.character(panel_code)) %>%
    # Change year to characters
    mutate(year=as.character(year)) %>% 
    # Select standardised column names
    select(panel_code, panel_name, first_name, last_name = second_name, titles, member_type, affiliation, year, url)
  
  # Post process 2021 --------------------------------------------------------------------
  # Load 2021 datas
  data_2021 <- as_tibble(read.csv("data/data_2021.csv"))
  
  # Define function to remove English names from panel names
  remove_english <- function(x) {
    panel_name_temp <- str_remove(x, "\\(.+")
    panel_name_temp <- str_squish(panel_name_temp)
  }
  
  # Post process data
  data_2021 %<>% 
    # Use defined function to remove english names from panel_name
    mutate(panel_name = remove_english(panel_name)) %>% 
    # Merge titles and qualifications to form a single titles column
    unite("titles", titles, qualifications, sep = " ") %>% 
    # Add url and date
    mutate(date = "2021", 
           url = "https://wayback.webarchiv.cz/wayback/20211025232040/https://gacr.cz/o-ga-cr/poradni-organy/panely/") %>% 
    # Add a affiliation column with missing values
    mutate(affiliation = NA) %>% 
    # Remove P from panel codes
    mutate(panel_code = str_remove(panel_code, "P")) %>% 
    # Select the standardised columns
    select(panel_code, panel_name, first_name, last_name, titles, member_type, affiliation, year=date, url)
  
  
  # Post process 2022 --------------------------------------------------------------------
  # Load 2022 data
  data_2022 <- as_tibble(read.csv("data/data_2022.csv"))
  
  # Post process data
  data_2022 %<>% 
    # Use defined function to remove english names from panel_name
    mutate(panel_name = remove_english(panel_name)) %>% 
    # Merge titles and qualifications to form a single titles column
    unite("titles", titles, qualifications, sep = " ") %>% 
    # Add url and date
    mutate(date = "2022", 
           url = "https://wayback.webarchiv.cz/wayback/20221024073116/https://gacr.cz/o-ga-cr/poradni-organy/panely/") %>% 
    # Add affiliation with missing values
    mutate(affiliation=NA) %>% 
    # Remove P from panel codes
    mutate(panel_code = str_remove(panel_code, "P")) %>% 
    # Select the standardised columns
    select(panel_code, panel_name, first_name, last_name, titles, member_type, affiliation, year=date, url)
  
  
  # Merge and Save ----------------------------------------------------------
  # Merge data
  data_2007_2008_2021_2022 <- rbind(data_sub_2007, data_sub_2008, data_2021, data_2022)
  
  # Save merged data
  write.csv(data_2007_2008_2021_2022, 
            file = here("data/post processed data", "data_2007_2008_2021_2022.csv"), 
            row.names = FALSE)
  
  # End
}

