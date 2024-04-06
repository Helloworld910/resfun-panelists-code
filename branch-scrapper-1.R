# Metadata ----------------------------------------------------------------
# Title: branch-scrapper-1
# Description: This script scrapes panelists branch data from the Grant Agency of the Czech Republic website for the year 2008 and saves it in the data folder.
# Date created: 06/04/2024
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


# Scrapper ----------------------------------------------------------------

branch_scrapper_1 <- function(){
  
  # Scrape branch commission pages----------------------------------------------------------------
  # Encode URLs and store them in a urls vector
  urls_branch <- c(
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=185",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=186",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=187",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=188",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=189")
  
  # Assign function to extract page title and content given url
  extract_content_branch <- function(x) {
    # Fetch html of url
    resp <- read_html(x)
    # Extract title from html
    title <- resp %>%
      # Find div with class name 'center-widget-title'
      html_elements(xpath="//div[@class = 'center-widget-title']//a") %>% 
      # Extract its text
      html_text2()
    # Extract content from html
    content <- resp %>%
      # Find the first p in the div with class name 'post-content'
      html_elements(xpath="//div[@class='post-content']/p[1]") %>%
      # Extract its text
      html_text2()
    # Wait for 1 second approx as the function is to be used iteratively
    Sys.sleep(rnorm(1, 1, 0.2))
    # Put together the url, title and content in a dataframe
    df <- data.frame(url = x, title = title, content = content)
    # Return the dataframe
    return(df)
  }
  
  # Iterate over urls vector and apply extraction function
  raw_data_branch <- map_df(urls_branch, extract_content_branch)
  
  # View extracted data
  raw_data_branch
  
  # Save raw data
  saveRDS(raw_data_branch, file = here("data", "raw_branch_2008.rds"))
  
  
  
  # Process raw branch data --------------------------------------------------------
  # Convert data to tibble
  # Load raw branch data
  raw_data_branch <- readRDS(here("data", "raw_branch_2008.rds"))
  
  # Convert loaded data to tibble
  df_tib <- as_tibble(raw_data_branch)
  
  # Define main processing function to split each content by new lines
  splitter_line <- function (x) {
    # Split content by new lines
    each_line <- unlist(str_split(x, pattern = "\n"))
    # Remove whitespace from start and end of each line
    each_line <- str_trim(each_line)
    # Filter out empty lines
    each_line <- each_line[each_line != ""]
    # Process each line to extract panelist data
    panelists_df <- map_df(each_line, splitter_comma)
    # Return extracted data
    return(panelists_df)
  }
  
  # Define function to extract panelist information given a line
  splitter_comma <- function(y) {
    # Split line by comma
    info <- unlist(str_split(y, ","))
    # Extract panelist affiliation
    # Identified by the largest string (except panelist name), spaces removed
    # Remove spaces
    no_space <- str_remove_all(info, " ")
    # Remove panelist name
    no_space <- no_space[-1]
    # Locate largest string
    pos_aff <- which.max(nchar(no_space))+1
    # Extract affiliation
    affi <- info[pos_aff]
    # Clean affiliation off panel codes
    affi <- str_remove(affi, "\\(\\d+\\)")
    # Extract panelist name
    panelist <- info[1]
    # Extract panel code
    panel_code <- str_extract(info, "\\(\\d+\\)")
    # Clean brackets from panel code
    panel_code <- str_remove(panel_code, "\\(")
    panel_code <- str_remove(panel_code, "\\)")
    # Locate panel code value
    panel_code <- panel_code[is.na(panel_code) == FALSE]
    # Find missing values
    if (length(panel_code) == 0) {
      # Assign NA to missing panel codes
      panel_code <- NA
    }
    # Put together extracted info in a dataframe
    data <- data.frame(panelist = panelist, panel = panel_code, 
                       affiliation = affi)
    # Return dataframe
    return(data)
  }
  
  
  # Process raw branch data
  processed_branch <- map_df(df_tib$content, splitter_line)
  
  # Load panel key as tible
  panel_key <- as_tibble(read.csv(here("data", "panel_key_2008.csv")))
  
  # Continue processing
  processed_branch %<>%
    # Convert into tibble
    as_tibble() %>% 
    # Convert panel code to int for use in adding panel names
    mutate(panel_code = as.integer(panel)) %>% 
    # Add panel names by using panel key
    left_join(panel_key, by = c("panel_code" = "panel_code")) %>% 
    # Add year
    mutate(year = "2008") %>% 
    # Add data category
    mutate(data_category = "branch") %>% 
    # Select out obsolete panel column
    select(panel_code, panel_name, panelist, affiliation, year, data_category)
  
  
  
  # View processed data
  processed_branch
  
  
  # Save processed data as csv
  write.csv(processed_branch, file = here("data", "data_branch_2008.csv"), 
            row.names = FALSE)
  
  
  

}

# End