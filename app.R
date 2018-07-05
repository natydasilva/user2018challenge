library(shiny)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gganimate)
library(ggthemes)
library(ALA4R)
library(ggthemes)


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
                                                 selectInput('year', 'Year', 1990:2016),
                                                 selectInput('plant', 'Plants', c("Brachychiton", "Triodia", "Flindersia", "Livistona","Callitris", "Daviesia", "Ficus","Hakea"), selected="Brachychiton")),
                                    mainPanel(withSpinner(plotOutput(outputId = 'plot1')))),
                           
                           
                           tabPanel("Sub"))
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    datos <- read_csv("plant_sub.csv")
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
          geom_point(data = plants, aes(x = longitudeOriginal, y = latitudeOriginal), colour="orange") +
          labs(y = "Latitude", x = "Longitude", title = paste(input$plant, input$year))
      }
    }
    
    
    pl_plant(y = input$year, pl = input$plant ,dat = selectedData())
    
  })
  
}



shinyApp(ui, server)
