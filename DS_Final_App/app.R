# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

readr::read_rds("./Data/tidy_covid19_case.rds") -> 
  covid19_data

library(shiny)

# UI
ui <- fluidPage(
  tabsetPanel(type = "pills",
    tabPanel("USmap",
      
    ),
    tabPanel("Data Analysis",
      
    ),
    tabPanel("Info", 
      
    )
    
  )
)

# Server

# Application
shinyApp(ui, server)