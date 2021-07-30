library(googleway)

key='xxxxxx'

df_places <- google_places(search_string = "warung makan", 
                           location = c(-6.2758903,106.8073032),   ## melbourne, AU
                           key =key)

df_places$results$name

#df_details <- google_place_details(place_id = df_places$results[1, "place_id"],
 #                                  key = key)


data_final <- data.frame(df_places$results$geometry$location$lat, df_places$results$geometry$location$lng, df_places$results$user_ratings_total)
names(data_final)[1] <- "Latitude"
names(data_final)[2] <- "Longitude"
names(data_final)[3] <- "n"


df_places <- google_places(search_string = "warung makan", 
                           location = c(max(data_final$Latitude),max(data_final$Longitude)),   ## melbourne, AU
                           key =key)
df_places$results$name
data_final_1 <- data.frame(df_places$results$geometry$location$lat, df_places$results$geometry$location$lng, df_places$results$user_ratings_total)
names(data_final_1)[1] <- "Latitude"
names(data_final_1)[2] <- "Longitude"
names(data_final_1)[3] <- "n"

data_final <- rbind(data_final,data_final_1)
data_final <- data_final[!duplicated(data_final$Latitude, data_final_1$Longitude, data_final$n), ]
data_final <- data_final[data_final$n > 1 , ]
dim(data_final)
