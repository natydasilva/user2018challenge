
# Libraries
library(tidyverse)
library(ALA4R)
library(RColorBrewer)
library(ggthemes)
library(data.table)
library(ochRe)

# 1. Data reduction, to run examples go to 2

#read all the plants data from https://downloads.ala.org.au
plants <- fread("Plants-brief.csv")

# Reduce the data set selecting only some plants
# Triodia, Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea

# plants_sub <- plants %>% filter(grepl("Hakea", scientificName))
# write.csv(plants_sub, file ="hakea.csv")

#read all the csv files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

#Filter only data from Australia based on long and lat
plants_sub  <- rbind(triodia, brachychiton, flindersia, livistona,
               callitris, daviesia, ficus, hakea) %>%
  mutate(latitudeOriginal = as.numeric(latitudeOriginal)) %>% filter((-43.00311<=latitudeOriginal& latitudeOriginal<=-12.46113)) %>%
  filter(113.6594<=longitudeOriginal& longitudeOriginal<=153.61194)

write.csv(plants_sub, file ="plant_sub.csv")

#2. Read reduced Data
plants_sub <- read_csv("plant_sub.csv")


# Australia map

load("aus_map.Rda")

map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha = 1/3) +
  theme_bw() + coord_map() + theme_map()


pl_plant <- function(pl, y ="all"){
  if(y == "all"){
plants <- plants_sub %>% filter(grepl(pl, scientificName))

map + geom_point(data = plants, aes(x = longitudeOriginal, y = latitudeOriginal), colour="orange")
  }else{
    plants <- plants_sub %>% filter(grepl(pl, scientificName), year == y )
    
    map + geom_point(data = plants, aes(x = longitudeOriginal, y = latitudeOriginal), colour="orange")
    
  
  }
}

# Triodia, Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea
#Map ok, data looks like in https://bie.ala.org.au/species/http://id.biodiversity.org.au/node/apni/2901419
pl_plant("Triodia", y = 2016)
pl_plant("Brachychiton")
pl_plant("Flindersia")
pl_plant("Livistona")
pl_plant("Callitris")
pl_plant("Daviesia")
pl_plant("Ficus")
pl_plant("Hakea")


pl_plant <- function(pl){
  plants <- plants_sub %>% filter(grepl(pl, scientificName))
  
  map + geom_point(data = plants, aes(x = longitudeOriginal, y = latitudeOriginal), colour="orange")
}


# Animated example
library(gganimate)
library(gapminder)
library(ggplot2)
theme_set(theme_bw())



#see how to reduce the map
#only to check if the animation works
red_map <- aus_map[sample(1:nrow(aus_map), 100 ),]

mapita <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha = 1/3) +
  theme_bw() + coord_map() + theme_map()

#Select a plant to see the evolution over the years
plants_each <- plants_sub %>% filter(grepl("Callitris", scientificName)) %>% 
filter(year > 1990) %>%select(longitudeOriginal, latitudeOriginal, year)

#can include the map
mapani <-  ggplot(data = plants_each,  aes(x = longitudeOriginal, y = latitudeOriginal, frame = year )) +
   geom_point( colour = "orange") 

gganimate(mapani)


plants_all <- plants_sub  %>% 
  filter(year > 1990) %>% select(longitudeOriginal, latitudeOriginal,
                                 year, plant)

mapani_facet <-  ggplot(data = plants_all,  aes(x = longitudeOriginal, y = latitudeOriginal, frame = year )) +
  geom_point( colour = "orange") + facet_wrap(~plant)

gganimate(mapani_facet)

#Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea

# Environ vars to get
# Precipitation - annual, spring, summer, winter, autumn + reliability
#
