# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

readRDS("../Data/tidy_covid19_case.rds") -> 
  covid19_data

library(shiny)

# UI
ui <- fluidPage()

# Server

# Application
shinyApp(ui, server)