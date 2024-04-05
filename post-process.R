# Metadata ----------------------------------------------------------------
# Title: post-process
# Description: This script loads the saved data from 2008 and 2009 and applies some processing to them. E.g. separates first name and last name of panelists.
# Date created: 05/04/2024
# Author: Shivam Sen
# Encoding: UTF-8
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# RStudio 2022.07.2+576 "Spotted Wakerobin" Release (e7373ef832b49b2a9b88162cfe7eac5f22c40b34, 2022-09-06) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
# Tidyverse version 1.3.2
# Rvest version 1.0.3
# Magrittr version 2.0.3
# Here version 1.0.1

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(rvest)
library(magrittr)
library(here)


# 2008 --------------------------------------------------------------------

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

# Separate the temp column and select standardised columns
data_sub_2008 %<>% 
  # Separate the temp column
  separate(temp_names, sep="...CUT HERE...", into=c("first_name", "second_name", "titles")) %>% 
  mutate(affiliation = str_squish(affiliation)) %>% 
  select(panel_name, panel_code, first_name, second_name, titles, affiliation, year, url)


# View post processed tibble
data_sub_2008

# Save tibble as csv
write.csv(data_sub_2008, file = here("data/post processed data", "data_sub_2008.csv"), 
          row.names = FALSE)


# 2007 --------------------------------------------------------------------

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
  # Select standardised columns
  select(panel_name, first_name, second_name, affiliation=panelist_affiliation, year, url)

# View processed data
data_sub_2007

# Save tibble as csv
write.csv(data_sub_2007, file = here("data/post processed data", "data_sub_2007.csv"), 
          row.names = FALSE)


# 2022 --------------------------------------------------------------------
# Load 2022 data
data_2022 <- as_tibble(read.csv("data/panelists_2022.csv"))


# Remove row numbers
data_2022 %<>% 
  select(-X)

# Seperate panel name and code
data_2022 %<>%
  separate(panel, into = c("panel_name", "panel_code"), sep = "\\) - ")


# Define your function to check member type
mem_type <- function(x) {
  # Check member type by looking for non numeric characters inside brackets in the member text
  if(str_detect(x, "\\(\\w+\\)")){
    # Extract designation
    mem_t <- str_extract(x, "\\(\\w+\\)")
    # Remove brackets from designation
    mem_t <- str_remove(mem_t, "\\(")
    mem_t <- str_remove(mem_t, "\\)")
    # Remove any white spaces
    mem_t <- str_squish(mem_t)
  }
  # If no designation found, then assign member type as 'Member'
  else{
    mem_t <- "Member"
  }
  # Return member type
  return(mem_t)
}


# Add member type
data_2022 %<>%
  rowwise() %>%
  mutate(member_type = mem_type(panelist))


# Remove any missing values from member names
# Define function to find missing values
find_missing_values <- function(x) {
  # Check if name is present
  if(str_detect(x, "\\w+")) {
    # If name is present store name
    mem_clean <- x
  } else {
    # If name is not present assign missing
    mem_clean <- "MISSING"
  }
  # Return missing assignment or member name
  return(mem_clean)
}


# Find and remove missing member names
data_2022 %<>% 
  rowwise() %>% 
  mutate(members_clean = find_missing_values(panelist)) %>% 
  filter(members_clean!="MISSING")

# Define function to process member names
member_info_temp <- function(x) {
  # Remove any designations
  proc_temp <- str_remove(x, "\\(\\w+\\)")
  # Split by comma
  proc_temp <- unlist(str_split(proc_temp, ","))
  # Fetch member qualifications from the second part of the split
  mem_qua <- proc_temp[2]
  # Remove any whitespaces from member qualifications
  mem_qua <- str_squish(mem_qua)
  # Fetch member titles from the first part of the split
  mem_titles <- unlist(str_extract_all(proc_temp[1], "\\b\\w+\\."))
  # Collapse member's titles list
  mem_titles <- paste(mem_titles, collapse = " ")
  # Remove member titles from the first part of the split
  proc_temp[1] <- str_remove_all(proc_temp[1], "\\b\\w+\\.")
  # Clear any white space
  proc_temp[1] <- str_squish(proc_temp[1])
  # Split again by space
  mem_names <- unlist(str_split(proc_temp[1], " "))
  # Fetch first name
  first_name <- mem_names[1]
  # Fetch second name
  second_name <- mem_names[2]
  # Put together all member info into one string ready to seperate
  mem_temp <- paste0(first_name, "...CUT HERE...", second_name, "...CUT HERE...", mem_qua, "...CUT HERE...", mem_titles)
  # Return string
  return(mem_temp)
}

# Process members info
data_2022 %<>% 
  # Process members column to form easily separable temp member info column
  mutate(members_temp = member_info_temp(panelist)) %>% 
  # Separate temp column into member information
  separate(members_temp, into = c("first_name", "last_name", "qualifications", "titles"), sep = "...CUT HERE...")


# Form final data frame with standardised column names
data_2022 %<>%
  select(panel_name, panel_code, 
         first_name, last_name, member_type, 
         titles, qualifications, year, url)


# View final dataframe
data_2022


# Save final dataframe as csv
write.csv(data_2022, file = here("data/post processed data", "data_2022.csv"), 
          row.names = FALSE)







