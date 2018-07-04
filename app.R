library(shiny)
library(tidyverse)
library(ALA4R)


# Define UI for app that draws a histogram ----

ui <- pageWithSidebar(
  headerPanel('Australia Plants'),
  sidebarPanel(
    selectInput('year', 'Year', 1990:2016),
    selectInput('plant', 'Plants', c("Brachychiton", "Triodia", "Flindersia",
                                     "Livistona","Callitris", "Daviesia", "Ficus","Hakea"),
                selected="Brachychiton")
   
  ),
  mainPanel(
    plotOutput('plot1')
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    datos <- read.csv("datos.csv")
  })
  
 
  
  output$plot1 <- renderPlot({
    
    ### Australian MAP
    load("aus_map.Rda")
    
    
    map <- aus_map %>%
      ggplot() +
      geom_polygon(aes(long, lat, group = group), alpha=1/3) +
      theme_bw() + coord_map() 
    
    #MAP with totals by year
    plcant <-  function(y, siz = FALSE, col = TRUE, pl = "all", dat){
      if(pl == "all"){
        dat3 <- dat %>% group_by(state, year) %>% drop_na() %>%filter(state!="") %>% 
          filter(year == y) %>% mutate(total = n())   
      }else{
        dat3 <- dat %>% group_by(state, year) %>% drop_na() %>%filter(state!="") %>% 
          filter(year == y) %>% mutate(total = n()) %>% filter(plant == pl)
      }
      # xs=quantile(dat3$total)
      # datall <- dat3 %>% mutate(cattot = cut(total, breaks=c(xs[1], xs[2]), xs[3], xs[4], xs[5]))
      if(col){
        map + geom_point(data = dat3, aes(x = longitude, y = latitude, colour = total), alpha = 1/3) +
          labs(colour = "Total", title = paste(pl, y))
      }else{
        map + geom_point(data = dat3, aes(x = longitude, y = latitude, size = total), alpha = 1/3) +
          labs(sizw = "Total", title = paste(pl, y))
      }
    }
    
    
    
    plcant(y = input$year, pl = input$plant ,dat = selectedData())
    
    
    
  })
  
}



shinyApp(ui, server)
