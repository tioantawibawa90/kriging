# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

geocode_google <- function(search_query, fields = "coordinates", key) {
  
  # LOAD LIBRARIES
  library(RCurl)
  
  # EXTRACT COORDINATES
  if (any(c("coordinates", "address") %in% fields) || "all" %in% fields) {
    # construct url for geocoding
    url_geocode <- url_google_geocoding(search_query, key)
    # get data from google
    geodata_json <- getURL(url_geocode)
    # get data from json output
    geodata_df <- as.data.frame(sapply(search_query, as.character),
                                stringsAsFactors = FALSE)
    names(geodata_df) <- "search query"
    rownames(geodata_df) <- NULL
    geodata_df[, 2:5] <- get_geodata_from_json_google(geodata_json)
    # return dataframe with the geodata
    if (all(c("coordinates", "address") %in% fields) || "all" %in% fields) {
      geodata_df
    } else if ("coordinates" %in% fields) {
      geodata_df <- geodata_df[, 1:3]
    } else {
      geodata_df <- geodata_df[, c(1, 4:5)]
    }
  }
  
  # EXTRACT CONTACTS
  if ("contacts" %in% fields || "all" %in% fields) {
    # /// get place_id from Place Search API ///
    # construct url for place search
    url_place_search <- url_google_place_search(search_query, key)
    # get data from google
    place_json <- getURL(url_place_search)
    # get place_id from json output
    place_id <- get_place_id_from_json_google(place_json)
    # /// get contacts from Place Details API ///
    # construct url for place details
    url_place_details <- url_google_place_details(place_id, key)
    # get data from google
    place_details_json <- getURL(url_place_details)
    # get place_id from json output
    contacts <- get_contacts_from_json_google(place_details_json)
    # /// add contacts to our output data frame ///
    if (!exists("geodata_df")) {
      geodata_df <- as.data.frame(sapply(search_query, as.character),
                                  stringsAsFactors = FALSE)
      names(geodata_df) <- "search query"
      rownames(geodata_df) <- NULL
    }
    geodata_df[, c("phone", "web page")] <- contacts
  }
  return(geodata_df)
}

# ///////////////////////////////////////////////

pubs_google <- geocode_google(pubs, "all", api_key)
