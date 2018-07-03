# Libraries
library(tidyverse)
library(ALA4R)
library(vegan)



#example from https://atlasoflivingaustralia.github.io/ALA4R/articles/ALA4R.html

wkt <- "POLYGON((152.5 -35,152.5 -32,140 -32,140 -35,152.5 -35))"

## define some environmental layers of interest [see ala_fields()]
env_layers <- c("Precipitation - annual","Temperature - annual max mean")

## Download the data.  We use the `occurrences()` function, adding environmental
##   data via the 'extra' parameter. Note that method="offline" supports
##   unlimited download size and more fields (but is slower).

## You should adjust the `download_reason_id` to match your purposes if using
##   this function for your own analyses; see `ala_reasons()`

x <- occurrences(taxon = "family:Fabaceae", wkt = wkt, qa = "none",
                 download_reason_id = "testing", extra = env_layers)

xgridded <- x$data %>%
  ## discard genus- and higher-level records
  dplyr::filter(rank %in%
                  c("species", "subspecies", "variety", "form", "cultivar")) %>%
  
  ## bin into 0.5-degree bins
  mutate(longitude=round(longitude*2)/2, latitude=round(latitude*2)/2) %>%
  
  ## average environmental vars within each bin
  group_by(longitude,latitude) %>%
  mutate(precipitationAnnual=mean(precipitationAnnual, na.rm=TRUE),
         temperatureAnnualMaxMean=mean(temperatureAnnualMaxMean, na.rm=TRUE)) %>%
  
  ## subset to vars of interest
  dplyr::select(longitude, latitude, scientificName, precipitationAnnual,
                temperatureAnnualMaxMean) %>%
  
  ## take one row per cell per species (presence)
  distinct() %>%
  
  ## calculate species richness
  mutate(richness=n()) %>%
  
  ## convert to wide format (sites by species)
  mutate(present=1) %>%
  do(tidyr::spread(data=., key=scientificName, value=present, fill=0)) %>%
  ungroup()

## where a species was not present, it will have NA: convert these to 0
sppcols <- setdiff(names(xgridded),
                   c("longitude", "latitude", "precipitationAnnual", "temperatureAnnualMaxMean",
                     "richness"))
xgridded <- xgridded %>% mutate_at(sppcols, function(z) ifelse(is.na(z), 0, z))

ggplot(xgridded, aes(longitude, richness)) + geom_point() + theme_bw()

