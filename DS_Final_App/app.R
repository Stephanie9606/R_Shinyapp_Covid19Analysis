# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

readr::read_csv("../../DS_final_data/COVID-19_Case.csv") %>% 
    drop_na() ->
    covid19_data

library(shiny)

# UI
ui <- fluidPage()

# Server

# Application
shinyApp(ui, server)