# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sf)

readr::read_rds("data/covid19_tidy.rds") -> 
  covid19_tidy

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
          varSelectInput("var1", "Check the data based on?", data = covid19_tidy, selected = "res_state")
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
    tabPanel("Info",
             dataTableOutput("rank")
    )
    
  )
)

# Server
server <- function(input, output){
  ## second tab
  
  # reactive for plot1
  total_case <- reactive({
    covid19_tidy %>% 
      group_by(case_month, !!input$var1) %>% 
      summarise(n = n(), .groups = "keep") %>% 
      drop_na(!!input$var1)
  })
  
  total_death <- reactive({
    covid19_tidy %>% 
      filter(death_yn == "Yes") %>% 
      group_by(case_month, !!input$var1) %>% 
      summarise(n = n(), .groups = "keep") %>% 
      drop_na(!!input$var1)
  })
  
  total_hosp <- reactive({
    covid19_tidy %>% 
      filter(hosp_yn == "Yes") %>% 
      group_by(case_month, !!input$var1) %>% 
      summarise(n = n(), .groups = "keep") %>% 
      drop_na(!!input$var1)
  })
  
  total_icu <- reactive({
    covid19_tidy %>% 
      filter(icu_yn == "Yes") %>% 
      group_by(case_month, !!input$var1) %>% 
      summarise(n = n(), .groups = "keep") %>% 
      drop_na(!!input$var1)
  })
  
  total_uc <- reactive({
    covid19_tidy %>% 
      filter(underlying_conditions_yn == "Yes") %>% 
      group_by(case_month, !!input$var1) %>% 
      summarise(n = n(), .groups = "keep") %>% 
      drop_na(!!input$var1)
  })
  
  # plot1
  output$plot1 <- renderPlot({
    # modularity
    
    # if-else
    if(input$rbuts1 == "Case"){
      p1 <- ggplot(total_case(), aes(x = case_month, y = n, color = !!input$var1)) +
        geom_smooth(se = F) +
        labs(x = "Date", y = "Cumulative Cases") +
        theme_bw()
    } else if(input$rbuts1 == "Death"){
      p1 <- ggplot(total_death(), aes(x = case_month, y = n, color = !!input$var1)) +
        geom_smooth(se = F) +
        labs(x = "Date", y = "Cumulative Deaths") +
        theme_bw()
    } else if(input$rbuts1 == "Hospitalization"){
      p1 <- ggplot(total_hosp(), aes(x = case_month, y = n, color = !!input$var1)) +
        geom_smooth(se = F) +
        labs(x = "Date", y = "Cumulative Hospitalization") +
        theme_bw()
    } else if(input$rbuts1 == "ICU"){
      p1 <- ggplot(total_icu(), aes(x = case_month, y = n, color = !!input$var1)) +
        geom_smooth(se = F) +
        labs(x = "Date", y = "Cumulative ICU Condition") +
        theme_bw()
    } else if(input$rbuts1 == "Underlying"){
      p1 <- ggplot(total_uc(), aes(x = case_month, y = n, color = !!input$var1)) +
        geom_smooth(se = F) +
        labs(x = "Date", y = "Cumulative Underlying Condition") +
        theme_bw()
    }
    
    # output plot1
    p1
  })
  
  # plo2
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
      summarize(confirmed_cases = n(),
                death_cases = )
  }, options = list(pageLength = 10))
  
}



# Application

shinyApp(ui, server)