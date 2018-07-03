# Libraries
library(tidyverse)
library(ALA4R)
library(vegan)
library(RColorBrewer)

##
# Triodia, Brachychiton, Flindersia, Livistona, Callitris, Daviesia, Ficus, Hakea. Need more?
#   
#   Some look to be limited by rainfall (in wetter areas) but they are not in the north. Why not? What other factors might be limiting distribution?
#   Some are limited to north and e coast? Why? Some of these are also in what looks like arid zone. Do some species live in very different conditions from other members of genus or are there small patches of matched climate.
# Rainfall is important in biodiversity. Is there a gradient in biodiversity in line with rainfall gradients?

###NDS Examples

## define some environmental layers of interest [see ala_fields()]
env_layers <- c("Precipitation - annual","Temperature - annual max mean")

# can not select more than one taxon at the same time
triodia <- occurrences(taxon = "triodia", qa = "none",
                        download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Triodia")


brachychiton <- occurrences(taxon = "brachychiton", qa = "none",
                       download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Brachychiton")


flindersia <- occurrences(taxon = "flindersia", qa = "none",
                            download_reason_id = "testing",extra=env_layers)$data %>% mutate(plant ="Flindersia")


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
                c("species", "subspecies", "variety", "form", "cultivar")) %>%

  mutate(longitude = round(longitude*2)/2, latitude = round(latitude*2)/2) %>%
  group_by(longitude,latitude)  %>%
  dplyr::select(year, month, eventDate, longitude, latitude, scientificNameOriginal, 
                state, plant, precipitationAnnual, temperatureAnnualMaxMean) 

datos %>% drop_na() %>% filter(state!="") %>%group_by( year,state) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year)) + geom_point() + geom_line() + facet_wrap(~state) 

datos %>% drop_na() %>% filter(state!="") %>%group_by( year, plant) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year, color=plant)) + geom_point() + geom_line() + 
  scale_color_brewer(palette = "Dark2") 

#Is this evolution different by state?
datos %>% drop_na() %>%filter(state!="")%>%group_by( year,  plant, state) %>% summarise(total=n()) %>%
  filter(year>1990) %>%
  ggplot(aes(y=total, x=year, color=plant)) + geom_point() + geom_line() + facet_wrap(~state) +
  scale_colour_brewer(palette = "Dark2") + theme(legend.position = "bottom")
#


#presipitiation and temperature

datos %>% group_by(state, year) %>% drop_na() %>%filter(state!="") %>% filter(year>1990) %>%
  mutate(precipitationAnnual = mean(precipitationAnnual, na.rm = TRUE),
         temperatureAnnualMaxMean = mean(temperatureAnnualMaxMean, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y=precipitationAnnual)) +geom_line() + geom_point()+
  facet_wrap(~state)


### OLD code to check

brachychiton <- specieslist("brachychiton")  
dim(brachychiton)
#Not useful only counts


fabaceae <- occurrences(taxon = "family:Fabaceae", qa = "none",
                        download_reason_id = "testing")



fabaceaedat <- fabaceae$data %>%
  ## discard genus- and higher-level records
  dplyr::filter(rank %in%
                  c("species", "subspecies", "variety", "form", "cultivar")) %>%
  
  ## bin into 0.5-degree bins
  mutate(longitude = round(longitude*2)/2, latitude = round(latitude*2)/2) %>%
  group_by(longitude,latitude)  %>%
  dplyr::select(year, month, eventDate, longitude, latitude, scientificNameOriginal, 
                state) 

#remove NA
#How is the temporal evolution of fabaceae?
#select data from 19900
fabaceaedat %>% group_by( year) %>% summarise(total=n()) %>% drop_na() %>%
  filter(year>19900) %>%
  ggplot(aes(y=total, x=year)) + geom_point() + geom_line() 

#Is this evolution different by state?
fabaceaedat %>% group_by( year, state) %>% summarise(total=n()) %>% drop_na() %>%
  filter(year>19900) %>%
  ggplot(aes(y=total, x=year, color=state)) + geom_point() + geom_line() 