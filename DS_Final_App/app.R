# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)

readr::read_rds("data/tidy_covid19_case.rds") -> 
  covid19_data

covid19 %>% 
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
          radioButtons("rbuts1", "Interesting in Case or Death data?", choices = case_types, selected = "Case"),
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
    tabPanel("Info"
      
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
    # p1 <- ggplot(total_case(), aes(x = case_month, y = n, color = !!input$var1)) +
    #   geom_smooth(se = F) +
    #   labs(x = "Date", y = "Cumulative Cases") +
    #   theme_bw()
    
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
  
}

# Application
shinyApp(ui, server)