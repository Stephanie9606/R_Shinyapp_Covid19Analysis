# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)

readr::read_rds("data/tidy_covid19_case.rds") -> 
  covid19_data

covid19_data %>% 
  na_if("Missing") %>% 
  na_if("Unknown") %>% 
  mutate(case_month = ym(case_month)) %>% 
  mutate(age_group = str_replace(age_group, "to", "-"),
         age_group = str_replace(age_group, "years", "")) %>% 
  mutate(age_group = as.factor(age_group),
         sex = as.factor(sex),
         hosp_yn = as.factor(hosp_yn),
         icu_yn = as.factor(icu_yn),
         death_yn = as.factor(death_yn),
         underlying_conditions_yn = as.factor(underlying_conditions_yn))  ->
  covid19_tidy

library(shiny)

# rbuts1 choices
case_types <- c("Case", "Death", "Hospitalization", "ICU", "Underlying")

# UI
ui <- fluidPage(
  titlePanel("Covid-19 Data Analysis"),
  tabsetPanel(type = "pills",
    tabPanel("USmap"
      
    ),
    tabPanel("Data Analysis",
      sidebarLayout(
        sidebarPanel(
          radioButtons("rbuts1", "What type of the data are you interested in?", choices = case_types, selected = "Case"),
          varSelectInput("var1", "Check the data based on?", data = covid19_tidy, selected = "res_state"),
          sliderInput("slider1", "Select date range",
                      min = as.Date("2020-10-01","%Y-%m-%d"),
                      max = as.Date("2021-01-01","%Y-%m-%d"),
                      value = c(as.Date("2020-01-01"), as.Date("2021-03-01")), timeFormat="%Y-%m")
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
    tabPanel("Info"
      
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
}



# Application
shinyApp(ui, server)