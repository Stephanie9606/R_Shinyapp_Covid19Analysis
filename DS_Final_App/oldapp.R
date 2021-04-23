# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

library(shiny)

# UI
ui <- fluidPage(
  tabsetPanel(type = "pills",
    tabPanel("USmap"
      
    ),
    tabPanel("Data Analysis"
      
    ),
    tabPanel("Info"
      
    )
    
  )
)

# Server
server <- function(input, output){
  
}

# Application
shinyApp(ui, server)