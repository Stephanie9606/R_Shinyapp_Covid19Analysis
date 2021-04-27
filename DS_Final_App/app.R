# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sf)
library(lubridate)

readr::read_rds("./data/covid19_tidy.rds") -> 
  covid19_tidy

# count death cases
n_death <- covid19_tidy %>%
  group_by(state) %>% 
  count(death_yn) %>% 
  pivot_wider(names_from = death_yn, values_from = n) %>% 
  rename(death_cases = Yes, recovery_cases = No, status_unknown = `NA`)

# convert latitude and longitude data in csv to a simple features object
covid19_tidy %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326, agr = "field")->cord_covid

library(shiny)

# rbuts1 choices
case_types <- c("Case", "Death", "Hospitalization", "ICU", "Underlying")

# UI
ui <- fluidPage(
  titlePanel("Covid-19 Data Analysis"),
  tabsetPanel(type = "pills",
    tabPanel("Covid-19 USmap",
             sidebarLayout(
               sidebarPanel(
                 varSelectInput("state", "State", data = covid19_tidy)
               ),
               mainPanel(
                 leafletOutput("map")
               )
      
    )
    ),
    tabPanel("Data Analysis",
      sidebarLayout(
        sidebarPanel(
          radioButtons("rbuts1", "What type of the data are you interested in?", choices = case_types, selected = "Case"),
          varSelectInput("var1", "Check the data based on?", data = covid19_tidy, selected = "res_state"),
          sliderInput("slider1", "Select date range",
                      min = as.Date("2020-01-01","%Y-%m-%d"),
                      max = as.Date("2020-12-01","%Y-%m-%d"),
                      value = c(as.Date("2020-01-01"), as.Date("2020-12-01")), timeFormat="%Y %b")
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("Cumulative Data",
                     plotOutput("plot1")
                      ),
            tabPanel("Race Analysis",
                     plotOutput("plot2")
                     ),
            tabPanel("lm Summary",
                     verbatimTextOutput("lms1")
                     )
            )
        )
      )
    ),
    tabPanel("Rank",
             dataTableOutput("rank")
    )
    
  )
)

# Server
server <- function(input, output){
  ### second tab
  
  ## plot1
  output$plot1 <- renderPlot({

    # if-else
    if(input$rbuts1 == "Case"){
      d1 <- covid19_tidy
    } else if(input$rbuts1 == "Death"){
      d1 <- covid19_tidy %>% 
        filter(death_yn == "Yes")
    } else if(input$rbuts1 == "Hospitalization"){
      d1 <- covid19_tidy %>% 
        filter(hosp_yn == "Yes")
    } else if(input$rbuts1 == "ICU"){
      d1 <- covid19_tidy %>% 
        filter(icu_yn == "Yes")
    } else if(input$rbuts1 == "Underlying"){
      d1 <- covid19_tidy %>% 
        filter(underlying_conditions_yn == "Yes")
    }
    
    # filter date
    filter_date <- reactive({
     d1 %>%
        filter(between(case_month, input$slider1[1], input$slider1[2]))
    })
    
    # reactive for plot1
    total_case <- reactive({
      filter_date() %>% 
        group_by(case_month, !!input$var1) %>% 
        summarise(n = n(), .groups = "keep") %>% 
        drop_na(!!input$var1)
    })
    
    # modularity
    p1 <- ggplot(total_case(), aes(x = case_month, y = n, color = !!input$var1)) +
      geom_smooth(se = F) +
      labs(x = "Date") +
      theme_bw()
    
    # output plot1
    p1
  })
  
  ## plo2
  output$plot2 <- renderPlot({
    
    # if-else
    if(input$rbuts1 == "Case"){
      d2 <- covid19_tidy
    } else if(input$rbuts1 == "Death"){
      d2 <- covid19_tidy %>% 
        filter(death_yn == "Yes")
    } else if(input$rbuts1 == "Hospitalization"){
      d2 <- covid19_tidy %>% 
        filter(hosp_yn == "Yes")
    } else if(input$rbuts1 == "ICU"){
      d2 <- covid19_tidy %>% 
        filter(icu_yn == "Yes")
    } else if(input$rbuts1 == "Underlying"){
      d2 <- covid19_tidy %>% 
        filter(underlying_conditions_yn == "Yes")
    }
    
    # reactive for plot2
    race_df <- reactive({
      d2 %>% 
        group_by(race, !!input$var1) %>% 
        summarise(total_case = n(), .groups = "keep") %>% 
        drop_na(!!input$var1)
    })
    
    # modularity
    p2 <- ggplot(race_df(), aes(x = !!input$var1, y = total_case, fill = race)) +
      geom_col() +
      coord_flip() +
      theme_bw()
    
   # output plot2
    p2  
  })
   # tab 3 rank
  output$rank <- renderDataTable({
    covid19_tidy %>%
      group_by(state) %>%
      summarize(confirmed_cases = n()) %>% 
      left_join(n_death, by = "state") %>% 
      mutate(`death_rate(%)` = round((death_cases / confirmed_cases)*100, digits = 2),
             `recovery_rate(%)` = round((recovery_cases / confirmed_cases)*100, digits = 2)) %>% 
      mutate(rank_confirmed = rank(confirmed_cases),
             rank_death_rate = rank(`death_rate(%)`, ties.method = "first"))
  }, options = list(pageLength = 10))

}



# Application

shinyApp(ui, server)