
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
taxa <- c("Triodia", "Brachychiton", "Flindersia", "Livistona", "Callitris",
          "Daviesia", "Ficus", "Hakea")

for(i in taxa){
plants_sub <- plants %>% filter(grepl(i, scientificName))
write.csv(plants_sub, file =paste("", i, ".csv", sep=""))
}
#read all the csv files
temp = list.files(pattern = "*.csv")
myfiles = lapply(temp, read.delim)

#Filter only data from Australia based on long and lat
plants_sub  <- rbind(triodia, brachychiton, flindersia, livistona,
               callitris, daviesia, ficus, hakea) %>%
  mutate(latitudeOriginal = as.numeric(latitudeOriginal)) %>% filter((-43.00311<=latitudeOriginal& latitudeOriginal <= -12.46113)) %>%
  filter(113.6594 <= longitudeOriginal & longitudeOriginal <= 153.61194)

write.csv(plants_sub, file = "plant_sub.csv")

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


#Select a plant to see the evolution over the years
plants_each <- plants_sub %>% filter(grepl("Callitris", scientificName)) %>% 
filter(year > 1990) %>%select(longitudeOriginal, latitudeOriginal, year)

#can include the map
mapani <- ggplot(data = plants_each,  aes(x = longitudeOriginal, y = latitudeOriginal, frame = year )) +
   geom_point( colour = "orange") 

gganimate(mapani)


plants_all <- plants_sub  %>% 
  filter(year > 1990) %>% select(longitudeOriginal, latitudeOriginal,
                                 year, plant)

mapani_facet <-  ggplot(data = plants_all,  aes(x = longitudeOriginal, y = latitudeOriginal )) +
  geom_point( colour = "orange") + facet_wrap(~plant)+
  transition_manual(year)
ease_aes('linear')
gganimate(mapani_facet)

#Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea

# Environ vars to get
# Precipitation - annual, spring, summer, winter, autumn + reliability
#




load("rain_all.rda")
load("stns.rda")

stations<- stns %>%mutate(Station_number = as.numeric(site)) 
meteoro <- inner_join(stations, rain1, by = "Station_number") %>%
  filter((-43.00311<=lat & lat<= -12.46113)) %>%
  filter(113.6594 <= lon & lon <= 153.61194)


mete_y <- meteoro  %>% group_by( Year) %>% mutate(Rainfallm = mean(Rainfall))
map + geom_point( data = mete_y,  aes(x = lon, y = lat, colour = Rainfallm))

#January to March ~summer time
mete_anual <- meteoro %>%  group_by(Year, lon, lat) %>% summarise(Rainfallm = mean(Rainfall))  

map + geom_point( data = mete_anual,  aes(x = lon, y = lat, colour = Rainfallm)) 

ggplot(mete_anual, aes(x=lon, y=lat)) + geom_hex(bins = 55)

                                                                                                                                   sep = "-")

mete_summer <- meteoro %>% filter(Month%in%c(12,1,2)) %>% group_by(Year, lon, lat) %>% summarise(Rainfallm = mean(Rainfall))  
  

map + geom_point( data = mete_summer,  aes(x = lon, y = lat, colour = Rainfallm)) +    scale_colour_viridis() 
 a<-  mete_summer %>% dplyr::filter(Year==2016)

 map + geom_point( data = a,  aes(x = lon, y = lat, colour = Rainfallm), size=I(4), alpha=1/3) +   scale_colour_viridis() 
   scale_colour_gradient(low ="blue", high="red")
 
  scale_colour_log10()

p = ggplot(data = mete_summer,
           aes(x=lon, y=lat,group=Rainfallm,fill=Rainfallm))
p = p+geom_tile(aes(fill=Rainfallm)) +scale_fill_gradient()
#+scale_fill_gradient(name=&quot;Rain(mm/day)&quot;,low=&quot;white&quot;,high=&quot;blue&quot;)
p = p+ggtitle( aes(Rainfallm))

mete_winter <- meteoro %>%  filter(Month%in%c(6,7,8)) %>% group_by(Year, lon, lat) %>% summarise(Rainfallm = mean(Rainfall)) 
 

mete_winter %>% ggplot(aes(x="", y=Rainfallm)) +geom_boxplot()
mete_winter <- meteoro %>%  group_by(Year,Month, lon, lat) %>% summarise(Rainfallm = mean(Rainfall))  %>%
  filter(Month%in%c(6,7,8)) %>% filter(Rainfallm>3.4) 
a <-map + geom_point( data = mete_winter,  aes(x = lon, y = lat, colour = Rainfallm)) 

library(plotly)
ggplotly(a)

pl_n <- plants_sub  %>% filter(state == "Northern Territory") %>% 
  filter(year == 2016) %>% select(longitudeOriginal, latitudeOriginal,
                                 year, plant)


map  +
  geom_point(data = pl_n,  aes(x = longitudeOriginal, y = latitudeOriginal ), colour = "orange") 

#check the colosest distance to a station based on long lat and 

function(data)
  
  

str(meteoro)
