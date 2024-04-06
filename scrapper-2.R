# Metadata ----------------------------------------------------------------
# Title: scrapper-2
# Description: This script scrapes panelists data from the Grant Agency of the Czech Republic website for the year 2022 and saves it.
# Date created: 27/02/2024
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
scrapper_2 <- function(){
  # Encode URL
  url <- URLencode("https://wayback.webarchiv.cz/wayback/20221024073116/https://gacr.cz/o-ga-cr/poradni-organy/panely/",
                   reserved = FALSE)
  
  # Fetch html
  resp <- read_html(url)
  
  # Form xpath query to fetch sections
  section_query <- paste0("//h2[text()='Členění vědních oborů do hodnoticích panelů']/following::h3[@class='collapsible-header']")
  
  # Form xpath query to fetch panels
  panel_query <- paste0("//h2[text()='Členění vědních oborů do hodnoticích panelů']/following::div[@class='accordeons']")
  
  # Fetch section names
  section_names <- resp %>% 
    html_elements(xpath = section_query) %>%
    html_text2()
  
  # Fetch panels
  panels_html <- resp %>% 
    html_elements(xpath = panel_query)
  
  # Fetch panel names
  panel_names <- map(panels_html, .f = function(x) {
    x |>
      html_elements(".accordeon") |>
      html_elements("label") |>
      html_text2()
  }) |>
    stats::setNames(section_names)
  
  # Form tibble from named list of section and panel names
  intermediate_df <- tibble::enframe(panel_names, name = "section", value = "panel") |>
    tidyr::unnest(panel)
  
  # Fetch panelist data
  final_df <- intermediate_df |>
    # Fetch panel members
    mutate(members = purrr::map(panel, .f = function(panel_subquery, source = resp) {
      # Set xpath query to select panel contents
      members_query <- paste0("//label[text()='", panel_subquery, "']/following-sibling::div[@class='accordeon-content']")
      # Fetch panel members
      source |>
        html_elements(xpath = members_query) |>
        html_elements("li") |>
        html_text2()
    })) |>
    tidyr::unnest(members) |>
    # Attach additional information
    mutate(
      # Attach url
      url = url,
      # Attach dates
      date = stringr::str_replace(url, "https://wayback.webarchiv.cz/wayback/(\\d{8}).*", "\\1"),
      date = stringr::str_replace(date, "(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3")
    )
  
  # Seperate panel name and code
  final_df %<>%
    separate(panel, into = c("panel name", "panel code"), sep = "\\) - ")
  
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
  final_df %<>%
    rowwise() %>%
    mutate(member_type = mem_type(members))
  
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
  final_df %<>% 
    rowwise() %>% 
    mutate(members_clean = find_missing_values(members)) %>% 
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
  final_df %<>% 
    # Process members column to form easily separable temp member info column
    mutate(members_temp = member_info_temp(members)) %>% 
    # Separate temp column into member information
    separate(members_temp, into = c("first name", "second name", "qualifications", "titles"), sep = "...CUT HERE...")
  
  # Form final data frame with standardised column names
  final_df %<>% 
    mutate(section_name = section, panel_name = `panel name`,
           panel_code = `panel code`, first_name = `first name`,
           last_name = `second name`) %>% 
    select(section_name, panel_name, panel_code, 
           first_name, last_name, member_type, 
           titles, qualifications)
  
  # Save final dataframe as csv
  write.csv(final_df, file = here("data", "data_2022.csv"), 
            row.names = FALSE)

}



# References --------------------------------------------------------------

#' @Manual{,
#'   title = {rvest: Easily Harvest (Scrape) Web Pages},
#'   author = {Hadley Wickham},
#'   year = {2024},
#'   note = {R package version 1.0.4, https://github.com/tidyverse/rvest},
#'   url = {https://rvest.tidyverse.org/},
#' }

