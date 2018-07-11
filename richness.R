library(tidyverse)
library(plotly)
library(ggthemes)
load("aus_map.Rda")
map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha=1/3, colour="grey90",
               fill="white") +
  theme_bw() + coord_map() + theme_map()

datos <- read_csv("datos.csv")
datos <- datos %>% filter((-43.00311<=latitude & latitude <= -9)) %>%
  filter(113.6594 <= longitude & longitude <= 153.61194) %>%
  filter(!is.na(year)) %>%
  filter(year>1969)


load("stns.rda")
stns <- stns %>% filter((-43.00311<=lat & lat <= -9)) %>%
  filter(113.6594 <= lon & lon <= 153.61194)

datos <- datos %>% separate(scientificNameOriginal, c("genus", "species"),
                            remove = FALSE)

datos_g_sp <- datos %>%
  dplyr::select(scientificNameOriginal, genus, species) %>%
  distinct() %>%
  count(genus, species)

genus <- c("Triodia", "Brachychiton", "Flindersia", "Livistona", "Callitris", "Daviesia", "Ficus", "Hakea")

spec_counts <- datos %>% filter(!(species %in% genus))

spec_counts %>% filter(genus=="Hakea") %>%ggplot( aes(x=longitude, y=latitude)) + geom_hex(bins = 55)

spec_counts %>% filter(genus=="Hakea") %>% mutate(lat2 =round(latitude)) %>%
  group_by( lat2,longitude,)%>%
  summarise(n = n()) %>% head()
  ggplot( aes(x=longitude, y=lat2, colour = n)) + scale_color_viridis()

  geom_hex(bins = 55)

#spec_counts %>% count(year) %>% print(n=250)

# Generate a hexagon grid of Australia
# From Steph Kobakian's atlas code
radius <- 0.5

lon_rg <- range(spec_counts$longitude)
lon_rg[1] <- lon_rg[1] - 1
lon_rg[2] <- lon_rg[2] + 1
lat_rg <- range(spec_counts$latitude)
lat_rg[1] <- lat_rg[1] -1
oz_hex <- expand.grid(lon = seq(lon_rg[1], lon_rg[2], radius),
                       lat = seq(lat_rg[1], lat_rg[2], radius))
latList <- oz_hex %>% select(lat) %>% distinct()
latShift <- latList %>% filter(row_number() %% 2 == 1)

oz_hex <- oz_hex %>% rowwise %>%
  mutate(lon = ifelse(lat %in% latShift$lat, lon, lon + radius/2))

map + geom_point(data=oz_hex, aes(x=lon, y=lat), size=0.1)
# that code is not needed, lat/long already gridded

spec_counts_bin <- spec_counts %>%
  group_by(year, genus, species, longitude, latitude) %>%
  tally() %>%
  ungroup()

diversity <- spec_counts_bin %>%
  select(-n) %>%
  group_by(year, genus, longitude, latitude) %>%
  tally() %>%
  ungroup()

diversity %>% count(genus, sort=TRUE)

library(viridis)
sub <- diversity %>%
  filter(genus == "Hakea", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Daviesia", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Callitris", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Triodia", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Ficus", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Brachychiton", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Livistona", ((year > 1995) & (year < 2005)))
sub <- diversity %>%
  filter(genus == "Flindersia", ((year > 1995) & (year < 2005)))

ggplot() + geom_point(data=sub, aes(x=n, y=latitude)) +
  scale_x_log10() +
  facet_wrap(~year, ncol=3) +
  ggtitle(sub$genus[1])

ggplot() + geom_point(data=sub, aes(y=latitude, x=longitude, colour=n)) +
  scale_colour_viridis() + theme_map() 
+ facet_wrap(~year, ncol=3)

# Gondwanaland: Hakea, Daviesia, Callitris
# NOT: Triodia, Ficus, Brachychiton
# https://en.wikipedia.org/wiki/Latitudinal_gradients_in_species_diversity
# Gondwanaland (original Australian plants) don't exhibit this pattern
# The diversity is richer closer to the pol, not the equator

diversity_noyr <- spec_counts_bin %>%
  dplyr::select(-n) %>%
  group_by(genus, longitude, latitude) %>%
  tally() %>%
  ungroup()

sub <- diversity_noyr %>%
  filter(genus == "Hakea")
sub <- diversity_noyr %>%
  filter(genus == "Daviesia")
sub <- diversity_noyr %>%
  filter(genus == "Callitris")
sub <- diversity_noyr %>%
  filter(genus == "Triodia")
sub <- diversity_noyr %>%
  filter(genus == "Ficus")
sub <- diversity_noyr %>%
  filter(genus == "Brachychiton")
sub <- diversity_noyr %>%
  filter(genus == "Livistona")
sub <- diversity_noyr %>%
  filter(genus == "Flindersia")

ggplot() + geom_point(data=sub, aes(x=n, y=latitude)) +
  scale_x_log10() +
  ggtitle(sub$genus[1])

map + geom_point(data=sub, aes(x=longitude, y=latitude, colour=n)) +
  scale_colour_viridis() +
  ggtitle(sub$genus[1])

spec_counts_bin %>% select(-n) %>% 
  ungroup() %>% filter(genus == "Livistona") %>% 
  count(species, sort=TRUE) %>% print(n=50)
