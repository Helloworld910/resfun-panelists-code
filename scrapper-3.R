# Metadata ----------------------------------------------------------------
# Title: scrapper-3
# Description: This script scrapes panelists data from the Grant Agency of the Czech Republic website for the year 2008 and saves it as well as the panel keys for the same year.
# Date created: 05/03/2024
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



# Scraper for sub-branch commission pages --------------------------------------------------
scrapper_3 <- function(){
  # Encode URLs and store them in a urls vector
  urls_sub <- c(
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=191",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=192",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=193",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=194",
    "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=195")
  
  # Define main extraction function to extract panelist data given url
  extract_data_sub <- function (x) {
    # Fetch html
    resp <- read_html(x)
    # These contain panel codes and names
    panel_info <- resp %>% 
      html_elements(xpath="//div[@class = 'post-content']/p/strong") %>% 
      html_text2()
    # Extract chairperson names
    panel_chair <- panel_info[seq(2, length(panel_info), by = 2)]
    # Filter out chairperson names to keep only panel names
    panel_info <- panel_info[seq(1, length(panel_info), by = 2)]
    # Form a dataframe of panels and their chairs
    panel_chair_df <- data.frame(panel = panel_info, chair = panel_chair)
    # Iterate over each panel to extract panel data
    panelist_data <- map_df(panel_info, extract_panelist_data, resp)
    # Add chairpersons to panel data
    panelist_data <- merge(panelist_data, panel_chair_df, by="panel", all.x=TRUE)
    # Return panelist data
    return(panelist_data)
  }
  
  # Define function to extract panelist data given panels in a page
  extract_panelist_data <- function (y, resp) {
    # Assign xpath query to select the html element with the panel content
    # Here this is the first p tag following the p tag with the panel info text
    xpath_query <- paste0("//div[@class = 'post-content']/p/strong[text()='",
                          y, "']/following::p[1]")
    # Fetch panel content text
    panel_content <- resp %>% 
      html_elements(xpath=xpath_query) %>% 
      html_text2()
    # Split content by new line as each line is a panelist
    lines_panel <- unlist(str_split(panel_content,"\n"))
    # Iterate over each line to extract panelist information in a dataframe
    panelist_df <- map_df(lines_panel, extract_panelist_from_line)
    # Add panel information to the received panelist names and affiliations
    panelist_df$panel <- y
    # Return panelists info dataframe
    return(panelist_df)
  }
  
  # Define function to extract panelist's information given a line
  extract_panelist_from_line <- function (z) {
    # Fetch panelist information
    panelist_info <- unlist(str_split(z, ","))
    # Fetch panelist name
    panelist_name <- panelist_info[1]
    # Locate panelist affiliation by finding the largest string in panelist info
    # Remove spaces
    no_space <- str_remove_all(panelist_info, " ")
    # Remove panelist name
    no_space <- no_space[-1]
    # Fetch position of largest string
    pos_aff <- which.max(nchar(no_space))+1
    # Fetch affiliation
    affi <- panelist_info[pos_aff]
    # Form data frame with panelist's name and affiliation
    panelists_return_df <- data.frame(panelist_name = panelist_name,
                                      affiliation=affi)
    # Return the data frame
    return(panelists_return_df)
  }
  
  # Extract data for sub-branch commissions
  data_sub <- map_df(urls_sub, extract_data_sub)
  
  # Process extracted data
  data_sub %<>% 
    # Convert to tibble
    as_tibble() %>% 
    # Separate panel code and panel name
    separate(panel, into = c("panel_code", "panel_name"), sep = " - ") %>% 
    # Add tag for sub-branch commission
    mutate(panel_category = "sub-branch") %>% 
    # Add year to data
    mutate(year = "2008")
  
  # Save sub-branch data
  write.csv(data_sub, file = here("data", "data_sub_2008.csv"), 
            row.names = FALSE)
  
  # Fetch panel code key from the extracted data
  panel_key <- data_sub %>% 
    select(panel_code, panel_name) %>% 
    distinct()
  
  # Save panel key as csv
  write.csv(panel_key, file = here("data", "panel_key_2008.csv"), 
            row.names = FALSE)
  
}


# References --------------------------------------------------------------
#' 
#' @Manual{,
#'   title = {purrr: Functional Programming Tools},
#'   author = {Hadley Wickham and Lionel Henry},
#'   year = {2023},
#'   note = {R package version 1.0.2, https://github.com/tidyverse/purrr},
#'   url = {https://purrr.tidyverse.org/},
#' }
#' 
#' @Manual{,
#'   title = {rvest: Easily Harvest (Scrape) Web Pages},
#'   author = {Hadley Wickham},
#'   year = {2024},
#'   note = {R package version 1.0.4, https://github.com/tidyverse/rvest},
#'   url = {https://rvest.tidyverse.org/},
#' }