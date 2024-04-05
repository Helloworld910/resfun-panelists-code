# Metadata ----------------------------------------------------------------
# Title: scrapper-3
# Description: This script scrapes panelists data from the Grant Agency of the Czech Republic website for the year 2008.
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

# Scrape sub-branch commission pages --------------------------------------------------

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
  # Fetch panel info from html
  # For this, extract text from p with strong text inside div with class 'post-content'
  # These contain panel codes and names
  panel_info <- resp %>% 
    html_elements(xpath="//div[@class = 'post-content']/p/strong") %>% 
    html_text2()
  # Filter out chairperson names from extracted texts
  panel_info <- panel_info[seq(1, length(panel_info), by = 2)]
  # Iterate over each panel to extract panel data
  panelist_data <- map_df(panel_info, extract_panelist_data, resp)
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

# View extracted data
head(data_sub)

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