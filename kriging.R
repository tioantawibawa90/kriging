#kriging final
library(data.table)
library(dplyr)
df1 <- data_final %>% 
  select(Latitude, Longitude, n) %>%
  mutate(location_id = group_indices(., Latitude, Longitude))


#Creating Training Set
random_generator <- sample(1:nrow(df1), round(nrow(df1)*.3), replace=F)
train_df <- df1 %>%
  filter(!location_id %in% random_generator) %>% 
  slice(1:60)
df_train<- as.data.frame(df1)
df_train <- df_train[!duplicated(df_train$location_id), ]
#Building the Variogram
library(gstat)
library(mapview)
library(tmaptools)
library(sp)
coordinates(train_df) <- c("Longitude", "Latitude")
proj4string(train_df) <- CRS("+proj=longlat +datum=WGS84")
lzn.vgm <- variogram(log(n) ~ Latitude + Longitude,data =  train_df, width=0.1)
lzn.fit = fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
plot(lzn.vgm, lzn.fit)

grid <- makegrid(train_df, cellsize = 0.1) # cellsize in map units!
names(grid)[1] <- "Longitude"
names(grid)[2] <- "Latitude"
# grid is a data.frame. To change it to a spatial data set we have to
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(train_df)))

plot(train_df)
plot(grid, pch = ".", add = T)

lzn.kriged <-krige(log(n) ~ Latitude + Longitude, train_df, grid, model = lzn.fit, maxdist=10, nmax=50)

coordinates(grid) <- c("Longitude", "Latitude")
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
lzn.kriged <-krige(log(n) ~ Latitude + Longitude, train_df, grid, model = lzn.fit, maxdist=10, nmax=50)
# Retrieve interpolated values
predicted <- lzn.kriged@data$var1.pred %>%
  as.data.frame() %>%
  rename(krige_pred = 1) %>% 
  mutate(krige= exp(krige_pred))
variance <- lzn.kriged@data$var1.var %>%
  as.data.frame() %>%
  mutate(variance = exp(.))

lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=Longitude, y=Latitude)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  theme_bw()


#tampilan peta
library(sf)
library(mapview)

locations_sf <- st_as_sf(df1, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(locations_sf)

library(maps) 
library(mapdata)
plot(dat,pch=16,col="red")
map("worldHires",add=TRUE,fill=TRUE)

library(ggplot2)
xlims <- range(pretty(dat$lon))
ylims <- range(pretty(dat$lat))
ggplot(dat,aes(lon,lat))+borders(fill="black",colour="black") +
  geom_point(col="red",pch=16) +
  coord_fixed(xlim = xlims,ylim=ylims)

n <- 50
dat <- data.frame(lon=runif(n,-88,-87),
                  lat=runif(n,41,42))