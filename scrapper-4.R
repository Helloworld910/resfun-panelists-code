# Metadata ----------------------------------------------------------------
# Title: scrapper-4
# Description: This script scrapes panelists data from the Grant Agency of the Czech Republic website for the year 2007.
# Date created: 21/03/2024
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


# Scrape sub-branch commission pages --------------------------------------------------
# Encode URLs and store them in a urls vector
urls_sub <- c("https://wayback.webarchiv.cz/wayback/20071007210841/http://pala.gacr.cas.cz/wordpress/?p=191",
              "https://wayback.webarchiv.cz/wayback/20071007210841/http://pala.gacr.cas.cz/wordpress/?p=192",
              "https://wayback.webarchiv.cz/wayback/20071007210841/http://pala.gacr.cas.cz/wordpress/?p=193",
              "https://wayback.webarchiv.cz/wayback/20071007210841/http://pala.gacr.cas.cz/wordpress/?p=194",
              "https://wayback.webarchiv.cz/wayback/20071007210841/http://pala.gacr.cas.cz/wordpress/?p=195")

# Extract data from page 1
# Define page 1 main data extraction function
extract_page_1 <- function (x) {
  # Fetch html
  resp <- read_html(x)
  # Extract all the panel names in page, found in the strong tags of p tags inside div with class
  # name 'post-content'
  panel_names <- resp %>% 
    html_elements(xpath="//div[@class='post-content']/p/strong")
  # Count how many panels in page
  panels_count <- length(panel_names)
  # Extract the panels data found in the p(from first til number of panels) in the div with class 
  # name 'post-content'
  panels <- resp %>% 
    html_elements(xpath="//div[@class='post-content']/p")
  panels <- panels[1:panels_count]
  panels <- panels %>% 
    html_text2()
  # Iterate over each panel content to extract data
  p1_data_df <- map_df(panels, extract_panelist_data_p134_main)
  # Return the extracted data
  return(p1_data_df)
}

# Define function to extract panelist data given panel for pages 1,3 and 4
extract_panelist_data_p134_main <- function (y) {
  # Split panel content by new line
  panelists <- unlist(str_split(y,"\n"))
  # Extract panel name which is the first line
  panel_name <- panelists[1]
  # Remove first line from panel content
  panelists <- panelists[-1]
  # Iterate over each line to extract panelist data
  panelist_df <- map_df(panelists, extract_panelist_data_p1_sub, panel_name)
  # Return panlist data
  return(panelist_df)
}


# Define function to extract panelist data given a line for pages 1,3 and 4
extract_panelist_data_p1_sub <- function (z, panel_name) {
  # Remove all words ending with a dot
  panelist_edited <- str_remove_all(z, "\\b\\w+\\.")
  # Remove any 4 digit number
  panelist_edited <- str_remove_all(panelist_edited, "\\b\\d{4}\\b")
  # Remove all extra spaces
  panelist_edited <- str_squish(panelist_edited)
  # Extract the first two words and store it as the panelist name
  panelist_name <- str_extract(panelist_edited, "\\b\\w+\\b \\b\\w+\\b-\\b\\w+\\b|\\b\\w+\\b \\b\\w+\\b")
  # Remove panelist names
  panelist_edited <- str_remove(panelist_edited, "\\b\\w+\\b \\b\\w+\\b-\\b\\w+\\b|\\b\\w+\\b \\b\\w+\\b")
  # Remove spaces and store remaining text as panelist's affiliation
  affi <- str_squish(panelist_edited)
  # Put together panelist name and affiliation with panel name in a dataframe
  panelists_info <- data.frame(panelist_name = panelist_name, 
                               panel_name = panel_name,
                               panelist_affiliation = affi)
  # Return the dataframe
  return(panelists_info)
}


# Extract data from page 1
data_p1 <- map_df(urls_sub[1], extract_page_1)

# Convert extracted data to tibble
data_p1 <- as_tibble(data_p1)

# View extracted data
head(data_p1)
tail(data_p1)


# Exctract data from page 3,4
# Define pages 3 and 4 main data extraction function
extract_page_34 <- function (x) {
  # Fetch html
  resp <- read_html(x)
  # Fetch panel names in page
  panel_names <- resp %>% 
    html_elements(xpath="//div[@class='post-content']/p/strong") %>% 
    html_text2()
  # Extract the panel content from page that is in the first p of div with class name 'post-content'
  panel_content <- resp %>% 
    html_element(xpath="//div[@class='post-content']/p") %>% 
    html_text2()
  # Break up panel content by new line
  panel_cont_temp <- unlist(str_split(panel_content, "\n"))
  # Find panel end points
  break_points <- which(panel_cont_temp==" ")
  # Add marker to end points
  panel_cont_temp[break_points] <- "BREAK HERE - PANEL ENDS HERE"
  # Put back together panel content 
  panel_cont_temp <- paste(panel_cont_temp, collapse = "\n")
  # Break up panel content by panel end points
  panels_in_page <- unlist(str_split(panel_cont_temp, "BREAK HERE - PANEL ENDS HERE"))
  # Make sure panelists and panel names are on different lines
  # By first removing any new lines on panel names
  panel_names <- str_remove_all(panel_names, "\n")
  # Then looping over panel names
  for (panel_name in panel_names) {
    # To add new line after each panel name
    panels_in_page <- str_replace(panels_in_page, panel_name, "\\0\n")
  }
  # Clean artifacts of panel content reassembly
  # Remove first line breaks
  panels_in_page[-1] <- str_remove(panels_in_page[-1], "\n")
  # Remove any multiple new lines
  panels_in_page <- str_replace_all(panels_in_page, "\n{2,}", "\n")
  # Remove whitespace at the start and end
  panels_in_page <- str_trim(panels_in_page)
  # Iterate over each panel content to extract data
  p34_data_df <- map_df(panels_in_page, extract_panelist_data_p134_main)
  # Return the extracted data
  return(p34_data_df)
}


# Extract data from pages 3 and 4
data_p34 <- map_df(urls_sub[3:4], extract_page_34)

# Convert extracted data to tibble
data_p34 <- as_tibble(data_p34)

# View extracted data
data_p34
head(data_p34)
tail(data_p34)


# Extract data from page 2 and 5

# Exctract data from page 2,5
# Form url vector with correct page urls
urls_25 <- urls_sub[c(2,5)]


# Define pages 2 and 5 main data extraction function
extract_page_25 <- function (x) {
  # Fetch html
  resp <- read_html(x)
  # Exract panel names by selecting the text inside strong tag inside p tags of div with class
  # name 'post-content'
  panel_names <- resp %>% 
    html_elements(xpath="//div[@class='post-content']/p/strong") %>% 
    html_text2()
  # Extract panel contents by selecting the text inside p tag immediately following the p tag 
  # that is the parent of a panel name selected previously
  panel_contents <- resp %>% 
    html_elements(xpath="//div[@class='post-content']/p/strong/parent::p/following::p[1]") %>% 
    html_text2()
  # Paste together panel names and panel contents
  panels <- map2_chr(panel_names, panel_contents, ~ paste(.x, .y, sep = "\n"))
  # Iterate over each panel to extract panelist data
  p25_data_df <- map_df(panels, extract_panelist_data_p134_main)
  # Return the extracted data
  return(p25_data_df)
}


# Call extraction function for pages 2 and 5
data_p25 <- map_df(urls_25, extract_page_25)

# Convert extracted data to tibble
data_p25 <- as_tibble(data_p25)

# View extracted data
data_p25
head(data_p25)
tail(data_p25)


# Put together data from each page
data_2007_sub <- rbind(data_p1, data_p25, data_p34)

# Process data
data_2007_sub %<>% 
  # Add tag for sub-branch commission
  mutate(data_category = "sub-branch") %>% 
  # Add year to data
  mutate(year = "2007")

# Save 2007 sub-branch data
write.csv(data_2007_sub, file = here("data", "data_sub_2007.csv"), 
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