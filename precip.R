library(bomrang)
library(tidyverse)
library(plotly)
library(ggthemes)

datos <- read_csv("datos.csv")
datos <- datos %>% filter((-43.00311<=latitude & latitude <= -12.46113)) %>%
  filter(113.6594 <= longitude & longitude <= 153.61194)

latlong <- datos %>% select(longitude, latitude) %>%
  distinct()

update_station_locations()

i <- sample(1:nrow(latlong), 1)
rain <- get_historical(latlon = c(latlong$longitude[i], latlong$latitude[i]), type="rain")
head(rain)

ggplot(latlong, aes(x=longitude, y=latitude)) + geom_point() +
  geom_point(data=latlong[i,], aes(x=longitude, y=latitude), colour="red") +
  geom_point(data=stns[1,], aes(x=lon, y=lat), colour="orange")

stns %>% count(site, sort=TRUE)

#rain <- NULL
for (i in 801:883) {
  cat(i, "\n")
  x <- get_historical(stationid=stns$site[i], type="rain")
  rain <- bind_rows(rain, x)
}
rain <- rain %>% filter(!is.na(Rainfall))

save(rain, file="precip701_800.rda")

rain1_100 <- rain
rain101_200 <- rain
rain201_300 <- rain
rain301_400 <- rain
rain401_500 <- rain
rain501_600 <- rain
rain601_700 <- rain
rain701_800 <- rain
rain801_883 <- rain

save(stns, file="stns.rda")

map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha=1/3) +
  theme_bw() + coord_map() + theme_map()

rain_stns <- stns %>% filter(as.integer(site) %in% rain$Station_number)
map + geom_point(data=rain_stns, aes(x=lon, y=lat))

# Global pattern lat/long diversity gradient
# Holds globally
# Assume that holds for Australia
# But flowering plants don't! Diversity gradient is opposite says Lyn
# more diversity in the south
# Which ancestral plants were temperate and suspect that they don't
