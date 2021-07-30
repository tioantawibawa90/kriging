# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_google_geocoding <- function(search_query_url, key_url) {
  # load libraries
  library(RCurl)
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # google gecoding api url
  url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  # construct search request for geocode
  url_geocoding_call <- paste0(url_geocoding_api, "json",
                               "?address=", search_query_url, "&key=", key_url)
  return(url_geocoding_call)
}

# ///////////////////////////////////////////////

url_google_place_search <- function(search_query_url, key_url) {
  # load libraries
  library(RCurl)
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # google places api url
  url_places_api <- "https://maps.googleapis.com/maps/api/place/"
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  # construct search request for place id
  url_place_search_call <- paste0(url_places_api, "findplacefromtext/",
                                  "json", "?input=", search_query_url,
                                  "&inputtype=textquery","&fields=place_id",
                                  "&key=", key_url)
  return(url_place_search_call)
}

# ///////////////////////////////////////////////

url_google_place_details <- function(place_id_url, key_url) {
  # load libraries
  library(RCurl)
  # google places api url
  url_places_api <- "https://maps.googleapis.com/maps/api/place/"
  # in case you would want to add "fields" as an argument
  # fields_url <- paste(fields_url, collapse = ",")
  # construct search request for place details
  url_place_details_call <- paste0(url_places_api, "details/",
                                   "json", "?place_id=", place_id_url,
                                   "&fields=formatted_phone_number,website",
                                   "&key=", key_url)
  return(url_place_details_call)
}

# ///////////////////////////////////////////////