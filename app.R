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
                           
                           tabPanel("Plants on Map",
                                    sidebarPanel(width = 3,
                                                 selectInput('year', 'Year', c("all",1990:2016)),
                                                 selectInput('plant', 'Plants', c("Brachychiton", "Triodia", "Flindersia", "Livistona","Callitris", "Daviesia", "Ficus","Hakea"), selected="Brachychiton")),
                                    mainPanel(withSpinner(plotOutput(outputId = 'plot1')))),
                           
                           
                           tabPanel("Sub"))
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
    
    load("aus_map.Rda")
    #red_map <- aus_map[sample(1:nrow(aus_map), 2000 ),]
    
    map <- aus_map %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group), alpha = 1/3) +
      theme_bw() + coord_map() + theme_map()
    
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
  
}



shinyApp(ui, server)
