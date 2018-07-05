# Libraries
library(tidyverse)
library(ALA4R)
library(vegan)
library(RColorBrewer)
library(ggthemes)

##
# Triodia, Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea. Need more?
#
#   Some look to be limited by rainfall (in wetter areas) but they are not in the north. Why not? What other factors might be limiting distribution?
#   Some are limited to north and e coast? Why? Some of these are also in what looks like arid zone. Do some species live in very different conditions from other members of genus or are there small patches of matched climate.
# Rainfall is important in biodiversity. Is there a gradient in biodiversity in line with rainfall gradients?

###NDS Examples
taxa <- c("triodia", "brachychiton", "flindersia", "livistona", "callitris", "daviesia", "ficus", "hakea")

## define some environmental layers of interest [see ala_fields()]
env_layers <- c("Precipitation - summer reliability",
                "Precipitation - winter reliability",
                "Precipitation - autumn reliability",
                "Precipitation - spring reliability",
                "Precipitation - summer",
                "Precipitation - winter",
                "Precipitation - autumn",
                "Precipitation - spring",
                "Precipitation - annual",
                "Temperature - annual max mean")

# can not select more than one taxon at the same time
triodia <- occurrences(taxon = "triodia", qa = "none",
                       download_reason_id = "testing", extra = env_layers)$data %>% mutate(plant ="Triodia")

brachychiton <- occurrences(taxon = "brachychiton", qa = "none",
                            download_reason_id = "testing",extra = env_layers)$data %>% mutate(plant ="Brachychiton")

flindersia <- occurrences(taxon = "flindersia", qa = "none",
                          download_reason_id = "testing",extra = env_layers)$data %>% mutate(plant ="Flindersia")

livistona <- occurrences(taxon = "livistona", qa = "none",
                         download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Livistona")

callitris <- occurrences(taxon = "callitris", qa = "none",
                         download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Callitris")

daviesia <- occurrences(taxon = "daviesia", qa = "none",
                        download_reason_id = "testing", extra=env_layers)$data %>% mutate(plant ="Daviesia")

ficus <- occurrences(taxon = "ficus", qa = "none",
                     download_reason_id = "testing", extra=env_layers)$data %>% mutate(plant ="Ficus")

hakea <- occurrences(taxon = "hakea", qa = "none",
                     download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Hakea")

datos <- rbind(triodia, brachychiton, flindersia, livistona,
               callitris, daviesia, ficus, hakea) %>%  dplyr::filter(rank %in%
                                                                       c("species", "subspecies", "variety", "form", "cultivar")) 

#mutate(longitude = round(longitude*2)/2, latitude = round(latitude*2)/2) %>%
#group_by(longitude,latitude)  %>%
# dplyr::select(year, month, eventDate, longitude, latitude, scientificNameOriginal,
#               state, plant, precipitationAnnual, temperatureAnnualMaxMean)

write.csv(datos, file="datos.csv")


datos %>%group_by( year, state) %>%  filter(state!="")%>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year)) + geom_point() + geom_line() + facet_wrap(~state, scales="free_y")

datos %>% group_by( year, plant) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y = total, x = year, color = plant)) + geom_point() + geom_line() +
  scale_color_brewer(palette = "Dark2")

#Is this evolution different by state?
datos %>% filter(state!="") %>% group_by( year,  plant, state) %>% 
  summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year, color=plant)) + geom_point() + geom_line() +
  facet_wrap(~state, scales = "free_y") + scale_colour_brewer(palette = "Dark2") + theme(legend.position = "bottom")



#presipitiation and temperature

datos %>% group_by(state, year) %>%filter(state!="") %>% filter(year>1990) %>%
  mutate(precipitationAnnual = mean(precipitationAnnual, na.rm = TRUE),
         temperatureAnnualMaxMean = mean(temperatureAnnualMaxMean, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y=precipitationAnnual)) +geom_line() + geom_point()+
  facet_wrap(~state)


### Australian MAP
load("aus_map.Rda")

library(tidyverse)
library(ochRe)


map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha=1/3) +
  theme_bw() + coord_map() + theme_map()

#MAP with totals by year

dat3 <- datos %>% group_by(state, year)  %>%filter(state!="") %>%
  filter(year == 2016)  %>% filter(plant=="Triodia")
map + geom_point(data = dat3, aes(x = longitude, y =latitude, size = precipitationSummer), alpha=1/3) 

#Animate presipitation  
library(gganimate)
plants_each <- datos %>% filter(grepl("Callitris", scientificName)) %>% 
  filter(year > 1990) %>%select(longitudeOriginal, latitudeOriginal, year, precipitationSummer)

# can't include the map
mapani <-  ggplot(data = plants_each,  aes(x = longitudeOriginal, y = latitudeOriginal, size = precipitationSummer, frame = year )) +
  geom_point( ) 

gganimate(mapani)

