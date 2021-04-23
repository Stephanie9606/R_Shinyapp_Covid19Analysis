# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

readr::read_rds("./data/tidy_covid19_case.rds") -> covid19_data

library(shiny)

# UI
ui <- fluidPage()

# Server

# Application
shinyApp(ui, server)