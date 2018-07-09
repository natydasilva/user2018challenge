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
for (i in 883:883) {
  cat(i, "\n")
  x <- get_historical(stationid=stns$site[i], type="rain")
  rain <- bind_rows(rain, x)
}
rain <- rain %>% filter(!is.na(Rainfall))

save(rain, file="precip.rda")

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

rain1 <- load("precip1_100.rda")
#load("precip701_800.rda")
rain2 <- load("precip201_300.rda")
rain3 <- load("precip301_400.rda")
rain4 <- load("precip401_500.rda")
rain5 <- load("precip501_600.rda")
rain6 <- load("precip601_700.rda")
rain7 <- load("precip801_883.rda")

rain <- rbind(rain1, rain2, rain3, rain4, rain5, rain6, rain7)


rain_yr_smry <- rain %>% filter(Year > 1969) %>%
  group_by(Station_number, Year) %>%
  summarise(yr_rain = mean(Rainfall, na.rm=T)*365, n=length(Rainfall))
ggplot(rain_yr_smry, aes(x=Year, y=yr_rain, group=Station_number)) + geom_line()
rain_yr_mean <- rain_yr_smry %>%
  group_by(Station_number) %>%
  summarise(yr_rain = mean(yr_rain))
rain_mth_smry <- rain %>% filter(Year > 1969) %>%
  group_by(Station_number, Month) %>%
  summarise(mth_rain = mean(Rainfall, na.rm=T)*31, n=length(Rainfall))
ggplot(rain_mth_smry, aes(x=Month, y=mth_rain, group=Station_number)) + geom_line()
rain_season_smry <- rain_mth_smry %>%
  group_by(Station_number) %>%
  summarise(sw_diff = sum(mth_rain[Month %in% c(12, 1, 2)])-
              sum(mth_rain[Month %in% c(6, 7, 8)]))
ggplot(rain_season_smry, aes(x=sw_diff)) + geom_histogram()
# Need some quality control, remove when not 12 months
# If diff is big negative? One outlier in 1_100 data

rain_stn_patterns <- left_join(rain_yr_mean, rain_season_smry, by="Station_number")
#rain_stn_patterns_all <- rain_stn_patterns
rain_stn_patterns_all <- bind_rows(rain_stn_patterns_all, rain_stn_patterns)
save(rain_stn_patterns_all, file="rain_stn_patterns_all.rda")

stns <- stns %>% mutate(Station_number = as.numeric(site))
rain_stn_patterns_all <- left_join(rain_stn_patterns_all, stns, by="Station_number")
ggplot(rain_stn_patterns_all, aes(x=yr_rain)) + geom_histogram()
rain_stn_patterns_all <- rain_stn_patterns_all %>% filter(yr_rain<5000)
ggplot(rain_stn_patterns_all, aes(x=sw_diff)) + geom_histogram()

ggplot(rain_stn_patterns_all, aes(x=yr_rain, y=sw_diff)) + geom_point() +
  scale_y_continuous(limits=c(-1800, 1800)) +
  scale_x_sqrt() +
  theme(aspect.ratio=1)

map + geom_point(data=rain_stn_patterns_all, aes(x=lon, y=lat, colour=yr_rain)) +
  scale_colour_viridis_c()
map + geom_point(data=rain_stn_patterns_all, aes(x=lon, y=lat, colour=sw_diff)) +
  scale_colour_distiller(palette="RdYlBu", limits=c(-1800, 1800))

# Bin precip
rain_stn_patterns_all_bin <- rain_stn_patterns_all %>%
  mutate(lat_bin = round(lat*2, 0)/2, lon_bin = round(lon*2, 0)/2)
map + geom_point(data=rain_stn_patterns_all_bin, aes(x=lon_bin, y=lat_bin, colour=yr_rain)) +
  scale_colour_viridis_c()

# Merge species richness with precip
diversity_rain <- left_join(diversity_noyr, rain_stn_patterns_all_bin,
                            by=c("longitude"="lon_bin", "latitude"="lat_bin"))
genus_list <- c("Hakea", "Daviesia", "Callitris", "Triodia", "Brachychiton", "Ficus")

diversity_rain %>% filter(genus %in% genus_list) %>%
  ggplot(aes(x=yr_rain, y=n)) + geom_point() +
  facet_wrap(~genus, ncol=3)
# Double check counts
nrow(unique(datos %>% filter(grepl("Hakea", scientificNameOriginal)) %>% select(scientificNameOriginal))) # Yes, its ok

# Filter locations by rain being more than 500
diversity_rain_onlysix <- diversity_rain %>%
  filter(genus %in% genus_list) %>%
  filter(yr_rain > 500 & yr_rain < 2500) %>%
  mutate(genus = factor(genus, levels=c("Hakea", "Daviesia", "Callitris", "Triodia", "Brachychiton", "Ficus"))) %>%
  group_by(genus) %>%
  mutate(p=n/max(n)) %>%
  ungroup()

map +
  geom_point(data=diversity_rain_onlysix, aes(x=lon, y=lat, colour=p), alpha=0.5) +
  scale_colour_viridis_c() + facet_wrap(~genus)
save(diversity_rain_onlysix, file="diversity_rain_onlysix.rda")
