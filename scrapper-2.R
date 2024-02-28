# Metadata ----------------------------------------------------------------

# Title: scrapper-2
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


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(rvest)
library(magrittr)

# Settings ----------------------------------------------------------------
setwd("E:/Postgrad Projects/Czech Academy of Science Internship/resfun-panelists-code")
getwd()
rm(list=ls())


# Scrapper ----------------------------------------------------------------

# Encode URL
url <- URLencode("https://wayback.webarchiv.cz/wayback/20221024073116/https://gacr.cz/o-ga-cr/poradni-organy/panely/",
                 reserved = FALSE)

# Fetch html
resp <- read_html(url)


# Make empty data frame to collect panel and panelists' names
data <- data.frame(matrix(ncol=2, nrow=0))
names(data) <- c("panelist", "panel")

# Count number of sections
num_sections <- length(resp %>%
                         html_elements(xpath = "//h2[text()='Členění vědních oborů do hodnoticích panelů']/following::div[@class='accordeons']"))

# Loop over each section
i <- 1
for (i in c(1:num_sections)) {
  # Count number of panels in each section
  xpath_query <- paste0("//h2[text()='Členění vědních oborů do hodnoticích panelů']/following::div[@class='accordeons'][", i, "]/div[@class='accordeon']")
  num_panels <- length(resp %>%
                         html_elements(xpath=xpath_query)
  )
  # Loop over each panel
  j <- 1
  for (j in c(1:num_panels)) {
    # Fetch panel name
    xpath_query <- paste0("//h2[text()='Členění vědních oborů do hodnoticích panelů']/following::div[@class='accordeons'][", i, "]/div[@class='accordeon'][", j, "]/label")
    panel <- resp %>%
      html_elements(xpath=xpath_query) %>%
      html_text2()
    # Fetch panelists' names
    xpath_query <- paste0("//h2[text()='Členění vědních oborů do hodnoticích panelů']/following::div[@class='accordeons'][", i, "]/div[@class='accordeon'][", j, "]//ul/li")
    panelist <- resp %>%
      html_elements(xpath=xpath_query) %>%
      html_text2()
    # Put together panel and panelists names
    data_unit <- data.frame(panelist=panelist, panel=panel)
    
    # Add panel and panelists names to final data
    data <- rbind(data, data_unit)
  }
}

# Convert data to tibble
data <- as_tibble(data)

# Add year and url to data
data %<>%
  mutate(year=2022, url=url)

# View data
data

# Save data as csv
write.csv(data, file="data/panelists_2022.csv")


# References --------------------------------------------------------------

#' @Manual{,
#'   title = {rvest: Easily Harvest (Scrape) Web Pages},
#'   author = {Hadley Wickham},
#'   year = {2024},
#'   note = {R package version 1.0.4, https://github.com/tidyverse/rvest},
#'   url = {https://rvest.tidyverse.org/},
#' }

