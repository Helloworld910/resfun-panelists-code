# Metadata ----------------------------------------------------------------

# Title: scrapper-1
# Date created: 24/02/2024
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
# setwd("E:/Postgrad Projects/Czech Academy of Science Internship/resfun-panelists-code")
# getwd()
# rm(list=ls())
# please read https://www.tidyverse.org/blog/2017/12/workflow-vs-script/ !!!!!



# Scrapper ----------------------------------------------------------------
urls <- c(
  "https://wayback.webarchiv.cz/wayback/20211025232040/https://gacr.cz/o-ga-cr/poradni-organy/panely/",
  "https://wayback.webarchiv.cz/wayback/20221024073116/https://gacr.cz/o-ga-cr/poradni-organy/panely/"
)

scrape_2021_2022 <- function(url) {
  url <- URLencode(url,
    reserved = FALSE
  )
  # Fetch html
  resp <- read_html(url)


  section_query <- paste0("//h2[text()='Složení hodnoticích panelů']/following-sibling::h3[@class='collapsible-header']")
  panel_query <- paste0("//h2[text()='Složení hodnoticích panelů']/following::div[@class='accordeons']")
  section_names <- resp |>
    html_elements(xpath = section_query) |>
    html_text2()
  panels_html <- resp |> html_elements(xpath = panel_query)
  panel_names <- purrr::map(panels_html, .f = function(x) {
    x |>
      html_elements(".accordeon") |>
      html_elements("label") |>
      html_text2()
  }) |>
    stats::setNames(section_names)

  intermediate_df <- tibble::enframe(panel_names, name = "section", value = "panel") |>
    tidyr::unnest(panel)

  final_df <- intermediate_df |>
    mutate(members = purrr::map(panel, .f = function(panel_query, source = resp) {
      my_query <- paste0("//label[text()='", panel_query, "']/following-sibling::div[@class='accordeon-content']")
      source |>
        html_elements(xpath = my_query) |>
        html_elements("li") |>
        html_text2()
    })) |>
    tidyr::unnest(members) |>
    mutate(
      url = url,
      date = stringr::str_replace(url, "https://wayback.webarchiv.cz/wayback/(\\d{8}).*", "\\1"),
      date = stringr::str_replace(year, "(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3")
    )
}

data <- purrr::map_df(urls, scrape_2021_2022)

# TODO: parse names into titles before and after name, first name, last name, chair, vicechair, panel_id

# Save data as csv
write.csv(data, file = "data/panelists_2021.csv")


# References --------------------------------------------------------------

#' @Manual{,
#'   title = {rvest: Easily Harvest (Scrape) Web Pages},
#'   author = {Hadley Wickham},
#'   year = {2024},
#'   note = {R package version 1.0.4, https://github.com/tidyverse/rvest},
#'   url = {https://rvest.tidyverse.org/},
#' }
