#Load aus map
load("aus_map.Rda")
map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha=1/3, colour="grey90",
               fill="white") +
  theme_bw() + coord_map() + theme_map()

#Load datos and manipulate

datos <- read_csv("datos.csv")
datos <- datos %>% filter((-43.00311<=latitude & latitude <= -9)) %>%
  filter(113.6594 <= longitude & longitude <= 153.61194) %>%
  filter(!is.na(year)) %>%
  filter(year>1969)

datos <- datos %>% separate(scientificNameOriginal, c("genus", "species"),
                            remove = FALSE)

# datos_g_sp <- datos %>%
#   dplyr::select(scientificNameOriginal, genus, species) %>%
#   distinct() %>%
#   count(genus, species)

#genus <- c("Triodia", "Brachychiton", "Flindersia", "Livistona", "Callitris", "Daviesia", "Ficus", "Hakea")

#spec_counts <- datos %>% filter(!(species %in% genus))

# spec_counts %>% 
#   filter(genus=="Hakea") %>%
#   ggplot( aes(x=longitude, y=latitude)) + geom_hex(bins = 55)


spec_counts_bin <- spec_counts %>%
  group_by(year, genus, species, longitude, latitude) %>%
  tally() %>%
  ungroup()

# diversity <- spec_counts_bin %>%
#   select(-n) %>%
#   group_by(year, genus, longitude, latitude) %>%
#   tally() %>%
#   ungroup()

#Plot the richness
diversity_noyr <- spec_counts_bin %>%
  dplyr::select(-n) %>%
  mutate(locate = ifelse(genus == "Callitris", "Native",
                         ifelse(genus == "Hakea", "Native",
                                ifelse(genus == "Daviesia", "Native",
                                       ifelse(genus == "Flindersia", "Native",
                                              ifelse(genus == "Livistona", "Native","Introduced")))))) %>% 
  group_by(genus, longitude, latitude, locate) %>%
  tally() %>% 
  ungroup() %>% head()




sub <- diversity_noyr %>%
  filter(genus == "Triodia")

ggplot() + geom_point(data=sub, aes(x=n, y=latitude)) +
  scale_x_log10() +
  ggtitle(sub$genus[1])

map + geom_point(data=sub, aes(x=longitude, y=latitude, colour=n)) +
  scale_colour_viridis() +
  ggtitle(sub$genus[1])


 tabPanel("Richness",
   sidebarPanel(
     width = 3, selectInput('year', 'Year', c("2016", 1990:2016)),
     selectInput('locate', 'Location')),
   mainPanel(fluidRow(withSpinner(
     plotOutput(outputId = 'plot2')
   ))
   ))

 
 

  output$plot2 <- renderPlot({
 
   spec_counts_bin %>% filter(year==input$year) %>%
     filter(genus=="input$locate") %>% 
      ggplot(aes(x=longitude, y=latitude)) + facet_wrap(~plant)
      geom_hex(bins = 55)
   })
 

  




