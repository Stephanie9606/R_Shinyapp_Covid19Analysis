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

# UI
ui <- fluidPage(
  titlePanel("Covid-19 Data Analysis"),
  tabsetPanel(type = "pills",
    tabPanel("USmap"
      
    ),
    tabPanel("Data Analysis",
      sidebarLayout(
        sidebarPanel(
          varSelectInput("var1", "X Variable?", data = covid19_tidy),
          varSelectInput("var2", "Y Variable?", data = covid19_tidy),
          varSelectInput("var3", "color code by ?", data = covid19_tidy)
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
  total_case <- reactive({
    covid19_tidy %>% 
      group_by(case_month, !!input$var3) %>% 
      summarise(n = n(), .groups = "keep")
  })
  
  # plot1
  output$plot1 <- renderPlot({
    # modularity
    p1 <- ggplot(total_case(), aes(x = case_month, y = n, color = !!input$var3)) +
      geom_smooth(se = F)
    # output plot1
    p1
  })
  
  # plot2
  output$plot2 <- renderPlot({
    
    # modularity
    p2 <- ggplot(data = covid19_tidy, aes(x = !!input$var1, y = !!input$var2))
    
    # if-else numeric/factor
    if(is.numeric(estate[[input$var2]]) && is.numeric(estate[[input$var3]])){
      p2 <- p2 +
        geom_point()
    } else if (is.factor(estate[[input$var2]]) && is.factor(estate[[input$var3]])){
      p2 <- p2 +
        geom_jitter()
    } else if (is.factor(estate[[input$var2]]) && is.numeric(estate[[input$var3]])){
      p2 <- p2 +
        geom_boxplot()
    } else{
      p2 <- p2 +
        ggstance::geom_boxploth()
    }
    # output p2
    p2
  })
}

# Application
shinyApp(ui, server)