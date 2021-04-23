# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

readr::read_rds("data/tidy_covid19_case.rds") -> 
  covid19_data

covid19_data %>% 
  na_if("Missing") %>% 
  na_if("Unknown") %>% 
  mutate(case_month = ym(case_month)) %>% 
  mutate(sex = as.factor(sex),
         hosp_yn = as.factor(hosp_yn),
         icu_yn = as.factor(icu_yn),
         death_yn = as.factor(death_yn),
         underlying_conditions_yn = as.factor(underlying_conditions_yn)) ->
  covid19_tidy

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