# This is a Shiny Web Application created for useR!2018 Data Challange by Natalia Da Silva and Hazel Kavili. 
# The app is about 10 plant species mostly observed in New South Wales and Queensland. 

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gganimate)
library(ggthemes)
library(ALA4R)
library(ggthemes)
library(shinycssloaders)
library(factorMerger)
library(data.table)
library(viridis)
plants_sub <- read_csv("plant_sub.csv")
datos <- fread("datos.csv")
load("rain_all.rda")
load("stns.rda")
datos <- datos %>% separate(scientificNameOriginal, c("genus", "species"),
                            remove = FALSE)

datos_g_sp <- datos %>%
  select(scientificNameOriginal, genus, species) %>% 
  distinct() %>% 
  count(genus, species)

genus <- c("Triodia", "Brachychiton", "Flindersia", "Livistona", "Callitris", "Daviesia", "Ficus", "Hakea")

spec_counts <- datos %>% filter(!(species %in% genus))




stations<- stns %>%mutate(Station_number = as.numeric(site)) 
meteoro <- inner_join(stations, rain1, by = "Station_number") %>%
  filter((-43.00311<=lat & lat<= -12.46113)) %>%
  filter(113.6594 <= lon & lon <= 153.61194)
mete_summer <- meteoro %>% filter(Month %in% c(12,1,2)) %>% group_by(Year, lon, lat) %>% summarise(Rainfallm = mean(Rainfall))  
mete_winter <- meteoro %>%  filter(Month %in% c(6,7,8)) %>% group_by(Year, lon, lat) %>% summarise(Rainfallm = mean(Rainfall)) 

load("aus_map.Rda")

#red_map <- aus_map[sample(1:nrow(aus_map), 2000 ),]

map <- aus_map %>%
  ggplot() +
  geom_polygon(aes(long, lat, group = group), alpha = 1/3) +
  theme_bw() + coord_map() + theme_map()


# Define UI for app that draws a histogram ----

ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage("Australia Plants", fluid = TRUE,
                           tabPanel("Descriptions",
                                    tabsetPanel(
                                      tabPanel("Temperature", withSpinner(plotOutput(outputId = 'tempplot'))),
                                      tabPanel("Precipitation",withSpinner(plotOutput(outputId = 'prpplot'))),
                                      tabPanel("Observations by State", withSpinner(plotOutput(outputId = 'obsvplot'))),
                                      tabPanel("Observations by Plants", withSpinner(plotOutput(outputId = 'obsvplot2')))
                                    )),
                           
                           tabPanel("Plants and rain",
                                    sidebarPanel(width = 3,
                                                 selectInput('year', 'Year', c("all",1990:2016), selected=2016),
                                                 selectInput('plant', 'Plants', c("Brachychiton", "Triodia", "Flindersia", "Livistona","Callitris", "Daviesia", "Ficus","Hakea"), selected="Brachychiton")),
                                    mainPanel(fluidRow(withSpinner(plotOutput(outputId = 'plot1'))), 
                                              fluidRow(column(width = 6,withSpinner(plotOutput(outputId = 'plotrainsummer'))),
                                              column(width = 6,withSpinner(plotOutput(outputId = 'plotrainwinter')))
                                              ))),
                           
                           # tabPanel("Richness",
                           #          sidebarPanel(width = 3,
                           #                       selectInput('year', 'Year', c("all",1990:2016), selected=2016),
                           #                       selectInput('plant', 'Plants', c("Brachychiton", "Triodia", "Flindersia", "Livistona","Callitris", "Daviesia", "Ficus","Hakea"), selected="Brachychiton")),
                           #          mainPanel(fluidRow(withSpinner(plotOutput(outputId = 'plot2'))),
                           #                    fluidRow(column(width = 6,withSpinner(plotOutput(outputId = 'plotrainsummer'))),
                           #                             column(width = 6,withSpinner(plotOutput(outputId = 'plotrainwinter')))
                           #                    ))),
                           
                           tabPanel("Taxonomic Trees",
                                    sidebarPanel(width = 3,
                                                 selectInput('genus', 'Genus', c("Brachychiton", "Triodia", "Flindersia", "Livistona","Callitris", "Daviesia", "Ficus","Hakea"), selected = "Brachychiton")),
                                    mainPanel(withSpinner(plotOutput(outputId = 'plotTaxo')))))

    )


# Define server logic required to draw a histogram ----
server <- function(input, output, session){
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    plants_sub <- read_csv("plant_sub.csv")
  })
  
  selectedData2 <- reactive({
    datos <- read_csv("datos.csv")
  })
  
  
  
  #Taxonomic Trees by Plants
  
  plantSub <- function(genus){
    
    tx <- plants_sub %>% filter(rank %in% c("species","subspecies")) %>% 
      select(scientificName, genus, rank) %>% 
      distinct(scientificName, genus, rank) %>% 
      mutate_all(as.factor)
    
    ax <- as.phylo(~genus/scientificName, data = tx)
    
  }
  
  reactivePlants <- reactive({plantSub(input$genus)})
  output$plotTaxo <- renderPlot({
    plotTree(reactivePlants(), type = "fan", color = "orange", fsize = 3) 
  })
  
  #Descriptives
  datosDesc <- datos %>% group_by(state, year) %>% 
    filter(state != "") %>% filter(year > 1990) %>%
    mutate(precipitationAnnual = mean(precipitationAnnual, na.rm = TRUE),
           temperatureAnnualMaxMean = mean(temperatureAnnualMaxMean, na.rm = TRUE))

  output$prpplot <- renderPlot({
    ggplot(data = datosDesc, aes(x = year, y= precipitationAnnual)) +
      geom_line() + geom_point() +
      facet_wrap(~state)
  })
  
  output$tempplot <- renderPlot({
    ggplot(data = datosDesc, aes(x = year, y = temperatureAnnualMaxMean)) +
      geom_line() + geom_point() +
      facet_wrap(~state)
  })

  output$obsvplot <- renderPlot({
    datosDesc %>% group_by(year, state) %>%
      summarise(total = n()) %>%
      filter(year > 1990) %>%
      ggplot(aes(x = year, y = total)) +
      geom_point() + geom_line() + facet_wrap(~state)
  })

  output$obsvplot2 <- renderPlot({
    datosDesc %>% group_by(year, plant) %>%
      summarise(total = n()) %>%
      filter(year > 1990) %>%
      ggplot(aes(y = total, x = year, color = plant)) +
      geom_point() + geom_line()
  })
  
  
  output$plot1 <- renderPlot({
    
    ### Australian MAP
    
    
    #MAP with totals by year
    
    pl_plant <- function(pl, y = "all", dat){
      if(y == "all"){
        
        plants <- dat %>% filter(grepl(pl, scientificName))
        
        map + 
          geom_point(data = plants, aes(x = longitudeOriginal, y = latitudeOriginal), colour = "orange") +
          labs(y = "Latitude", x = "Longitude", title = paste(input$plant, input$year))
      } else{
        plants <- plants_sub %>% filter(grepl(pl, scientificName), year == y )
        
        map + 
          geom_point(data = plants, aes(x = longitudeOriginal, y = latitudeOriginal), colour = "orange") +
          labs(y = "Latitude", x = "Longitude", title = paste(input$plant, input$year))
      }
    }
    
   pl_plant(y = input$year, pl = input$plant, dat = selectedData())
    
  })
  
  output$plotrainsummer <- renderPlot({
    
if(input$year=="all"){
  map + geom_point( data = mete_summer,  aes(x = lon, y = lat, colour = Rainfallm), size = I(4), alpha = 1/3) + 
    scale_colour_viridis() + labs(title="Summer rain")
}else{
    
    a <-  mete_summer %>% dplyr::filter(Year==input$year)
    
    map + geom_point( data = a,  aes(x = lon, y = lat, colour = Rainfallm), size = I(4), alpha = 1/3) + labs( title = paste("Summer rain", input$year)) + 
      scale_colour_viridis()
}    
  })
  
  output$plotrainwinter <- renderPlot({
    
    if(input$year=="all"){
      map + geom_point( data = mete_winter,  aes(x = lon, y = lat, colour = Rainfallm), size = I(4), alpha = 1/3) + 
        scale_colour_viridis() + labs(colour= "Mean rain", title="Winter rain")
    }else{
      
      a <-  mete_winter %>% dplyr::filter(Year==input$year)
      
      map + geom_point( data = a,  aes(x = lon, y = lat, colour = Rainfallm), size = I(4), alpha = 1/3) + 
        labs( colour= "Mean rain", title = paste("Winter rain", input$year)) + 
        scale_colour_viridis()
    }    
  })
  
  
  # output$plot2 <- renderPlot({
  #   
  #  
  #   spec_counts %>% filter(year==input$year)%>%
  #     filter(genus=="input$plant") %>% ggplot(aes(x=longitude, y=latitude)) + geom_hex(bins = 55)
  #   
  #       
  # })
  
  
}



shinyApp(ui, server)
