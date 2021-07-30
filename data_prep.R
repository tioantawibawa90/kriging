library(data.table)# fast read/write function
library(dplyr)
library(sp)
library(gstat)
library(sf)
library(mapview)
library(tmaptools)
#get longlat
library(ggmap)
map <- get_googlemap(center = c(8.4, 49.5), zoom = 6)
register_google(key = "AIzaSyCvZSQ8l4UkenQKzdXVdhKiET8-_wKgFxc", write = TRUE)
api_key = "AIzaSyCvZSQ8l4UkenQKzdXVdhKiET8-_wKgFxc"
library(ggmap)
library(mapview)
addr <- data.frame(Address = c("4000 mayflower Hill, Waterville, Maine", 
                               "2 Andrews Rd, Lewiston, ME 04240",
                               "255 Maine St, Brunswick, ME 04011",
                               "90 Quaker Hill Rd, Unity, ME 04988",
                               "105 Eden St, Bar Harbor, ME 04609"),
                   College = c("Colby", "Bates", "Bowdoin", "Unity", "CoA"),
                   stringsAsFactors = FALSE)
addr.geo <- mutate_geocode(addr, location = Address, output = "latlona")
View(addr.geo)


# create a list of London pubs
pubs <- c("The Angel, Bermondsey", 
          "The Churchill Arms, Notting Hill", 
          "The Auld Shillelagh, Stoke Newington", 
          "The Sekforde, Clerkenwell", 
          "The Dove, Hammersmith", 
          "The Crown and Sugar Loaf, Fleet Street", 
          "The Lamb, Holborn", 
          "Prince of Greenwich, Greenwich",
          "Ye Olde Mitre, Hatton Garden",
          "The Glory, Haggerston", 
          "The Blue Posts, Soho", 
          "The Old Bank of England, Fleet Street")
pubs_df <- data.frame(Pubs = pubs, stringsAsFactors = FALSE)

# run the geocode function from ggmap package
pubs_ggmap <- geocode(location = pubs, output = "more", source = "google")
pubs_ggmap <- cbind(pubs_df, pubs_ggmap)

# print the results
pubs_ggmap[, 1:6]


# extract the coordinates of London pubs
pubs_ggmap_crd <- list()
for (i in 1:dim(pubs_ggmap)[1]) {
  lon <- pubs_ggmap$lon[i]
  lat <- pubs_ggmap$lat[i]
  pubs_ggmap_crd[[i]] <- c(lon, lat)
}

# reverse geocode the coordinates and save them to the list
pubs_ggmap_address <- list()
for (i in 1:length(pubs_ggmap_crd)) {
  pub <- pubs[i]
  crd <- pubs_ggmap_crd[[i]]
  address <- revgeocode(location = crd, output = "address")
  pubs_ggmap_address[[i]] <- list(pub, crd, address)
}

# print the details of the first pub
pubs_ggmap_address[[1]]


#cara kedua
# modifying some search requests
pubs_m <- pubs
pubs_m[pubs_m=="The Crown and Sugar Loaf, Fleet Street"] <- "The Crown and Sugar Loaf"
pubs_m[pubs_m=="Ye Olde Mitre, Hatton Garden"] <- "Ye Olde Mitre"
pubs_m_df <- data.frame(Pubs = pubs_m, stringsAsFactors = FALSE)

# geocoding the London pubs
# "bar" is special phrase added to limit the search
pubs_tmaptools <- geocode_OSM(paste(pubs_m, "bar", sep = " "),
                              details = TRUE, as.data.frame = TRUE)

# extracting from the result only coordinates and address
pubs_tmaptools <- pubs_tmaptools[, c("lat", "lon", "display_name")]
pubs_tmaptools <- cbind(Pubs = pubs_m_df[-10, ], pubs_tmaptools)

# print the results
pubs_tmaptools

