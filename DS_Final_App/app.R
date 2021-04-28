# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sf)
library(lubridate)
library(usmap)

readr::read_rds("./data/covid19_tidy.rds") -> 
  covid19_tidy

readr::read_rds("./data/covid19_lmdf.rds") -> 
  covid19_lmdf

readr::read_rds("./data/covid19_by_state.rds") -> 
  covid19_by_state

readr::read_rds("./data/covid19_by_county.rds") -> 
  covid19_by_county

# count death cases
covid19_tidy %>%
  group_by(state) %>% 
  count(death_yn) %>% 
  pivot_wider(names_from = death_yn, values_from = n) %>% 
  rename(Death_Cases = Yes, Recovery_Cases = No, Status_Unknown = `NA`) ->
  n_death

# convert latitude and longitude data in csv to a simple features object
covid19_tidy %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326, agr = "field") -> cord_covid

library(shiny)
library(bslib)

# rbuts1 choices
case_types <- c("Case", "Death", "Hospitalization", "ICU", "Underlying")

# UI
ui <- fluidPage(
  # theme and title
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Covid-19 Data Analysis"),
  # main pages
  tabsetPanel(type = "pills",
    tabPanel("Covid-19 USmap",
             tabPanel("Covid-19 USmap",
                      selectInput(inputId = "mapinput",
                                  label = "Choose Map Type",
                                  choices = c("By State", "By County"),
                                  mainPanel(
                                    plotOutput("map")
                                  )
                      )
             )
    ),
    tabPanel("Plot Analysis",
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
                     )
            )
        )
      )
    ),
    tabPanel("Data Analysis",
      fluidRow(column(4,
                      varSelectInput("var2", "X variable?", data = covid19_lmdf)
                      ),
               column(4,
                      varSelectInput("var3", "Y variable?", data = covid19_lmdf)
                      ),
               column(4,
                      checkboxInput("cbox1", "Check residual plot?"),
                      checkboxInput("cbox2", "Check QQ plot?")
                      )
        
      ),
      fluidRow(
        column(8, plotOutput("ggplotlm")),
        column(4, verbatimTextOutput("slm"))
      ),
      fluidRow(
        column(6, plotOutput("plotrsd")),
        column(6, plotOutput("plotqq"))
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
  
  ### third tab
  # ggplot
  output$ggplotlm <- renderPlot({
    
    # modularity
    gglm <- ggplot(data = covid19_lmdf, aes(x = !!input$var2, y = !!input$var3))
    
    # if-else numeric/factor
    if(is.numeric(covid19_lmdf[[input$var2]]) && is.numeric(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_point()
    } else if (is.factor(covid19_lmdf[[input$var2]]) && is.factor(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_jitter()
    } else if (is.factor(covid19_lmdf[[input$var2]]) && is.numeric(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_boxplot()
    } else{
      gglm <- gglm +
        ggstance::geom_boxploth()
    }
    
    # output gglm
    gglm  
  })
  
  # lm summary
  output$slm <- renderPrint({
    lmout <- lm(covid19_lmdf[[input$var3]] ~ covid19_lmdf[[input$var2]], data = covid19_lmdf)
    print(summary(lmout), digits = 2)
  })
  
  # optional: residual plot
  output$plotrsd <- renderPlot({
    if(isTRUE(input$cbox1)){
      lmout <- lm(covid19_lmdf[[input$var3]] ~ covid19_lmdf[[input$var2]])
      qplot(x = lmout$fitted, y = lmout$residuals,
            main = "Residuals vs Fitted",
            xlab = "x",
            ylab = "y")
    }
  })
  
  # optional: QQ plot 
  output$plotqq <- renderPlot({
    if(isTRUE(input$cbox2)){
      lmout <- lm(covid19_lmdf[[input$var3]] ~ covid19_lmdf[[input$var2]])
      qplot(sample = lmout$residuals, geom = "qq",
            main = "QQ Plot",
            xlab = "theoretical",
            ylab = "sample") +
        geom_qq_line()
    }
  })
  
  ### forth tab
  output$rank <- renderDataTable({
    covid19_tidy %>%
      group_by(state) %>%
      summarize(Confirmed_Cases = n()) %>% 
      left_join(n_death, by = "state") %>% 
      mutate(`Death_Rate(%)` = round((Death_Cases / Confirmed_Cases)*100, digits = 2),
             `Recovery_Rate(%)` = round((Recovery_Cases / Confirmed_Cases)*100, digits = 2)) %>% 
      mutate(Rank_Confirmed = rank(Confirmed_Cases),
             Rank_Death_Rate = rank(`Death_Rate(%)`, ties.method = "first")) %>% 
      rename(State = state)
  }, options = list(pageLength = 10))
  
  output$map <- reactive({
    if (input$mapinput == "By State"){
      
      p <- plot_usmap(data = covid19_by_state, values = "Cases", color = "white", labels = FALSE) + 
        scale_fill_continuous(name = "Number of Cases", label = scales::comma) + 
        theme(legend.position = "right")+
        labs(title = "Cases of COVID-19 by State")
      
    } else {
      
      p <- plot_usmap(data = covid19_by_county, values = "Cases", labels = FALSE) + 
        scale_fill_continuous(name = "Number of Cases", label = scales::comma) + 
        theme(legend.position = "right")+
        labs(title = "Cases of COVID-19 by County")
    }
    p
})
}



# Application

shinyApp(ui, server)