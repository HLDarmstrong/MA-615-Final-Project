library(shiny)
library(tidyverse)
library(dplyr)
library(fmsb)
library(leaflet)

stop1 <- read_csv("stops1.txt") 
stop2 <- read_csv("stops2.txt")
stop3 <- read_csv("stops3.txt")
stop4 <- read_csv("stops4.txt")
stop5 <- read_csv("stops5.txt")
stop6 <- read_csv("stops6.txt")
stop7 <- read_csv("stops7.txt")
stop8 <- read_csv("stops8.txt")
stop9 <- read_csv("stops9.txt")
stop10 <- read_csv("stops10.txt")
stop11 <- read_csv("stops11.txt")
stop12 <- read_csv("stops12.txt")
stop_total <- rbind(stop1,stop2,stop3,stop4,stop5,stop6,stop7,stop8,stop9,stop10,stop11,stop12) %>% 
  distinct()

stop_times_total <- read_csv("stop_times1.txt")

trips1 <- read_csv("trips1.txt")
trips2 <- read_csv("trips2.txt")
trips3 <- read_csv("trips3.txt")
trips4 <- read_csv("trips4.txt")
trips5 <- read_csv("trips5.txt")
trips6 <- read_csv("trips6.txt")
trips7 <- read_csv("trips7.txt")
trips8 <- read_csv("trips8.txt")
trips9 <- read_csv("trips9.txt")
trips10 <- read_csv("trips10.txt")
trips11 <- read_csv("trips11.txt")
trips12 <- read_csv("trips12.txt")
trips_total <- rbind(trips1,trips2,trips3,trips4,trips5,trips6,
                     trips7,trips8,trips9,trips10,trips11,trips12) %>% distinct()

trips_stop_time <- inner_join(stop_times_total,trips_total)
data <- inner_join(trips_stop_time,stop_total) %>% distinct()

data <- data%>% 
  select(route_id,route_pattern_id,stop_id,trip_id,time=arrival_time,
         pickup_type,drop_off_type,checkpoint_id,service_id,trip_headsign,
         bikes_allowed,stop_code,stop_name,stop_desc,stop_lat,stop_lon,zone_id,
         level_id,wheelchair_boarding,municipality)


Route <- unique(data$route_id)
Municipality <- unique(data$municipality)

ui <- fluidPage(
  # Application title
  titlePanel("MBTA"),
  sidebarPanel(
    selectInput("Route","Which route do you want see",Route),
    selectInput("Municipality","Where city has stops?",Municipality)
  ),
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Map",leafletOutput("map")),
                        tabPanel("Table",tableOutput("table"))
  )
  )
)
server <- function(input, output) {
  newdf <- reactive({
    data %>% filter(route_id%in%input$Route,municipality %in% input$Municipality)
  })  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = newdf()$stop_lat,
                 lng = newdf()$stop_lon,
                 popup= newdf()$stop_name)
  })
  output$table <- renderTable({newdf()})
}

# Run the application 
shinyApp(ui = ui, server = server)
