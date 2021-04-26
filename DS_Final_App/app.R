# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)

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

# UI
ui <- fluidPage(
  titlePanel("Covid-19 Data Analysis"),
  tabsetPanel(type = "pills",
    tabPanel("Covid-19 USmap",
             sidebarLayout(
               sidebarPanel(
                 varSelectInput("state", "State")
               ),
               mainPanel(
                 leafletOutput("map")
               )
      
    )
    ),
    tabPanel("Data Analysis",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("cboxg1", "Interesting in Case or Death data?", choices = c("Case", "Death")),
          # varSelectInput("var", "Y Variable?", data = covid19_tidy),
          varSelectInput("var1", "Check the data based on?", data = covid19_tidy)
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("check case by ?",
                     plotOutput("plot1")
                      ),
            tabPanel("ggplot",
                     plotOutput("plot2")
                     ),
            tabPanel("lm summary",
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
  
  # reactive
  # total_case <- reactive({
  #   covid19_tidy %>% 
  #     group_by(case_month, !!input$var1) %>% 
  #     summarise(n = n(), .groups = "keep")
  # })
  # 
  # total_death <- reactive({
  #   covid19_tidy %>% 
  #     filter(death_yn == "Yes") %>% 
  #     group_by(case_month, !!input$var1) %>% 
  #     summarise(n = n(), .groups = "keep")
  # })
  # 
  # # plot1
  # output$plot1 <- renderPlot({
  #   # modularity
  #   # p1 <- ggplot(total_case(), aes(x = case_month, y = n, color = !!input$var1)) +
  #   #   geom_smooth(se = F) +
  #   #   labs(x = "Date", y = "Cumulative Cases") +
  #   #   theme_bw()
  #   
  #   # if-else
  #   if(input$cboxg1 == "Case"){
  #     p1 <- ggplot(total_case(), aes(x = case_month, y = n, color = !!input$var1)) +
  #       geom_smooth(se = F) +
  #       labs(x = "Date", y = "Cumulative Cases") +
  #       theme_bw()
  #   } else if(input$cboxg1 == "Death"){
  #     p1 <- ggplot(total_death(), aes(x = case_month, y = n, color = !!input$var1)) +
  #       geom_smooth(se = F) +
  #       labs(x = "Date", y = "Cumulative Deaths") +
  #       theme_bw()
  #   }
  #   
  #   # output plot1
  #   p1
  # })

  
}

shinyApp(ui, server)