# Load Libraries ---------------------------------------------------------------
library(magrittr)
library(httr2)
library(rjson)
library(lubridate)


# Settings ----------------------------------------------------------------
setwd("E:/Postgrad Projects/Czech Academy of Science Internship/resfun-panelists-code")
getwd()
rm(list=ls())


# Fetch Snapshots Dates ------------------------------------------------

# Set up query towards wayback-cdx-server
req <- request("http://web.archive.org/cdx/search/cdx?url=https://gacr.cz/o-ga-cr/poradni-organy/panely/&from=2021&&output=json")
req %<>%
  req_headers("Accept" = "application/json", 
              "User-Agent" = "Shivam Sen; Shivamsen910@gmail.com")
req %>%
  req_dry_run()

# Make query
resp <- req_perform(req)

# Process json response to list 
json_resp <- resp_body_string(resp)
list_resp <- fromJSON(json_resp)

# Fetch vector of timestamps
time_stamps <- lapply(list_resp, `[[`, 2)
time_stamps <- unlist(second_value)
time_stamps <- time_stamps[2:24]
time_stamps <- ymd_hms(time_stamps)


# View fetched vector
time_stamps


# References --------------------------------------------------------------

#' @Manual{,
#'   title = {httr2: Perform HTTP Requests and Process the Responses},
#'   author = {Hadley Wickham},
#'   year = {2023},
#'   note = {R package version 1.0.0, https://github.com/r-lib/httr2},
#'   url = {https://httr2.r-lib.org},
#' }
#' 
#' @Article{,
#'   title = {Dates and Times Made Easy with {lubridate}},
#'   author = {Garrett Grolemund and Hadley Wickham},
#'   journal = {Journal of Statistical Software},
#'   year = {2011},
#'   volume = {40},
#'   number = {3},
#'   pages = {1--25},
#'   url = {https://www.jstatsoft.org/v40/i03/},
#' }

