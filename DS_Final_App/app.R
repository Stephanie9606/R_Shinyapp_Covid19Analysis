# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)

readr::read_rds("data/tidy_covid19_case.rds") -> covid19_data

library(shiny)

ui <- fluidPage(
  titlePanel("Covid-19 Data Analysis"),
  tabsetPanel(
    tabPanel("US map",
             sidebarLayout(
               sidebarPanel(
                 varSelectInput("state", "State")
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    ),
    tabPanel("Data Analysis"),
    tabPanel("Info")
    
  )
  
  
)

server <- function(input, output, session) {
 
  
  
}

shinyApp(ui, server)