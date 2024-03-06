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


# Scrapper ----------------------------------------------------------------

# Encode URLs and store them in a urls vector
urls <- c(
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=185",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=186",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=187",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=188",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=189",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=191",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=192",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=193",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=194",
  "https://wayback.webarchiv.cz/wayback/20081129223741/http://pala.gacr.cas.cz/wordpress/?p=195")


# Assign function to extract page title and content given url
extract_content <- function(x) {
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
raw_data <- map_df(urls, extract_content)

# View extracted data
raw_data


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