
# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 2. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_geodata_from_json_google <- function(geodata_json) {
  # load library
  library(jsonlite)
  # convert json output into r object
  geodata <- lapply(geodata_json, fromJSON,simplifyVector = FALSE)
  # extract coordinates, address and city name
  lat_lng_a <- data.frame(lat = NA, lng = NA, address = NA, city = NA)
  for (i in 1:length(geodata)) {
    if (geodata[[i]]$status=="OK") {
      # extract coordinates and address
      lat <- geodata[[i]]$results[[1]]$geometry$location$lat
      lng <- geodata[[i]]$results[[1]]$geometry$location$lng
      address <- geodata[[i]]$results[[1]]$formatted_address
      # find out how many elements there are in "address_components"
      n <- length(geodata[[i]]$results[[1]]$address_components)           
      # extract city and country
      for (j in 1:n) {
        # extract the type of the "address_components"
        type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
        # extract the city name
        if (type == "postal_town") {
          city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
        }
      }                
      lat_lng_a[i, ] <- c(lat, lng, address, city)
    } else {
      lat_lng_a[i, ] <- NA
    }
  }
  return(lat_lng_a)
}

# ///////////////////////////////////////////////

get_place_id_from_json_google <- function(place_json) {
  # load library
  library(jsonlite)
  # convert json output into r object
  place_search <- lapply(place_json, fromJSON,simplifyVector = FALSE)
  # extract place id
  place_id <- list()
  for (i in 1:length(place_search)) {
    if (place_search[[i]]$status=="OK") {
      place_id[[i]] <- place_search[[i]]$candidates[[1]]$place_id
    } else {
      place_id[[i]] <- NA
    }
  }
  return(place_id)
}

# ///////////////////////////////////////////////

get_contacts_from_json_google <- function(place_details_json) {
  # load library
  library(jsonlite)
  # convert json output into r object
  place_details <- lapply(place_details_json, fromJSON, simplifyVector = FALSE)
  # extract phone number and website
  contacts <- data.frame("phone number" = NA, "website" = NA)
  for (i in 1:length(place_details)) {
    if (place_details[[i]]$status=="OK") {
      # get data
      phone_number <- place_details[[i]]$result$formatted_phone_number
      website <- place_details[[i]]$result$website
      # get rid of NULLs
      info <- list(phone_number, website)
      for (j in 1:length(info)) {
        if (is.null(info[[j]])) info[[j]] <- NA
      }
      # create output data frame
      contacts[i, ] <- info
    } else {
      contacts[i, ] <- NA
    }
  }
  return(contacts)
}

# ///////////////////////////////////////////////