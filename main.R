# Metadata ----------------------------------------------------------------
# Title: main
# Description: This script loads all the scrappers and the post processor and runs them to produce the final data that is saved in the post processed data folder in the data folder
# Date created: 06/04/2024
# Author: Shivam Sen
# Encoding: UTF-8
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# RStudio 2022.07.2+576 "Spotted Wakerobin" Release (e7373ef832b49b2a9b88162cfe7eac5f22c40b34, 2022-09-06) for Windows
# Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36


# Script ------------------------------------------------------------------
# Load all scrappers
source("scrapper-1.R")
source("scrapper-2.R")
source("scrapper-3.R")
source("scrapper-4.R")

# Load post-processor
source("post-processor.R")

# Run all scrappers
scrapper_1()
scrapper_2()
scrapper_3()
scrapper_4()

# Run post-processor
post_process()

#End