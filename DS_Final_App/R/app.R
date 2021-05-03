# Author: Chiyun, Sihyuan, Trevor

library(readr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sf)
library(lubridate)
library(raster)
library(shiny)
library(bslib)
library(usmap)

readr::read_rds("../data/covid19_tidy.rds") -> 
  covid19_tidy

readr::read_rds("../data/covid19_lmdf.rds") -> 
  covid19_lmdf

readr::read_rds("../data/covid19_n_death.rds") -> 
  covid19_n_death

readr::read_rds("../data/covid19_geom.rds") -> 
  covid19_geom


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
             leafletOutput("map"),
             absolutePanel(top = 10, right = 10,
                           varSelectInput("type1", "Covid19 Case Type", data = covid19_geom[c(5,7)]))
        
    ),
    tabPanel("Plot Analysis",
      sidebarLayout(
        sidebarPanel(
          radioButtons("rbuts1", "What type of the data are you interested in?", choices = case_types, selected = "Case"),
          varSelectInput("var1", "Check the data based on?", data = covid19_tidy, selected = "`State(Abbrev)`"),
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
    tabPanel("Ranking",
      fluidRow(column(2, radioButtons(inputId = "var4", label = "Map of Rate",choices = c("Rate of Recovery", "Rate of Death"))),
               column(5, plotOutput("usPlot")),
               column(5, dataTableOutput("spRank"))
      ),
      fluidRow(
        column(12, dataTableOutput("rank"))
      ))
             
    )
)


# Server
server <- function(input, output){
  ### first tab Us map

  

  ### second tab
  ## plot1
  output$plot1 <- renderPlot({

    # if-else
    if(input$rbuts1 == "Case"){
      d1 <- covid19_tidy
    } else if(input$rbuts1 == "Death"){
      d1 <- covid19_tidy %>% 
        filter(Death == "Yes")
    } else if(input$rbuts1 == "Hospitalization"){
      d1 <- covid19_tidy %>% 
        filter(Hospitalization == "Yes")
    } else if(input$rbuts1 == "ICU"){
      d1 <- covid19_tidy %>% 
        filter(ICU == "Yes")
    } else if(input$rbuts1 == "Underlying"){
      d1 <- covid19_tidy %>% 
        filter(`Underlying Conditions` == "Yes")
    }
    
    # filter date
    filter_date <- reactive({
     d1 %>%
        filter(between(`Date(Monthly)`, input$slider1[1], input$slider1[2]))
    })
    
    # reactive for plot1
    total_case <- reactive({
      filter_date() %>% 
        group_by(`Date(Monthly)`, !!input$var1) %>% 
        summarise(n = n(), .groups = "keep") %>% 
        drop_na(!!input$var1)
    })
    
    # modularity
    p1 <- ggplot(total_case(), aes(x = `Date(Monthly)`, y = n, color = !!input$var1)) +
      geom_smooth(se = F) +
      geom_text(aes(label = !!input$var1, color = !!input$var1)) +
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
        filter(Death == "Yes")
    } else if(input$rbuts1 == "Hospitalization"){
      d2 <- covid19_tidy %>% 
        filter(Hospitalization == "Yes")
    } else if(input$rbuts1 == "ICU"){
      d2 <- covid19_tidy %>% 
        filter(ICU == "Yes")
    } else if(input$rbuts1 == "Underlying"){
      d2 <- covid19_tidy %>% 
        filter(`Underlying Conditions` == "Yes")
    }
    
    # reactive for plot2
    race_df <- reactive({
      d2 %>% 
        group_by(Race, !!input$var1) %>% 
        summarise(total_case = n(), .groups = "keep") %>% 
        drop_na(!!input$var1)
    })
    
    # modularity
    p2 <- ggplot(race_df(), aes(x = !!input$var1, y = total_case, fill = Race)) +
      geom_col() +
      coord_flip() +
      theme_bw()
    
   # output plot2
    p2  
  })
  
  ### third tab
  # ggplot
  output$ggplotlm <- renderPlot({
    
    # reactive for ggplotlm
    plot_lm <- reactive({
      covid19_lmdf %>% 
        drop_na(!!input$var2) %>% 
        drop_na(!!input$var3)
    })
    
    # modularity
    gglm <- ggplot(data = plot_lm(), aes(x = !!input$var2, y = !!input$var3))
    
    # if-else numeric/factor
    if(is.numeric(covid19_lmdf[[input$var2]]) && is.numeric(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_point()
    } else if (is.factor(covid19_lmdf[[input$var2]]) && is.factor(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_jitter()
    } else if (is.factor(covid19_lmdf[[input$var2]]) && is.numeric(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_boxplot() +
        scale_y_log10()
    } else if (is.numeric(covid19_lmdf[[input$var2]]) && is.factor(covid19_lmdf[[input$var3]])){
      gglm <- gglm +
        geom_boxplot() +
        scale_x_log10()
    }
    
    # output gglm
    gglm  
  })
  
  # lm summary
  output$slm <- renderPrint({
    # if y input is numeric print lm
    if(is.numeric(covid19_lmdf[[input$var3]])) {
      lmout <- lm(covid19_lmdf[[input$var3]] ~ covid19_lmdf[[input$var2]], data = covid19_lmdf)
      print(summary(lmout), digits = 2)
    }
    
    validate(
      need(is.numeric(covid19_lmdf[[input$var3]]), "Please select Y as Numeric Variable to Check out Linear Model Summary!")
    )
  })
  
  # optional: residual plot
  output$plotrsd <- renderPlot({
    # if check to see residual plot
    if(isTRUE(input$cbox1)){
      # if y input is factor print plot
      if(is.factor(covid19_lmdf[[input$var3]])) {
        validate(
          need(is.numeric(covid19_lmdf[[input$var3]]), "Please select Y as Numeric Variable to Check out Risidual Plot!")
        )
      } else{
        lmout <- lm(covid19_lmdf[[input$var3]] ~ covid19_lmdf[[input$var2]])
        qplot(x = lmout$fitted, y = lmout$residuals,
              main = "Residuals vs Fitted")
      }
    }
  })
  
  # optional: QQ plot 
  output$plotqq <- renderPlot({
    if(isTRUE(input$cbox2)){
      # if y input is factor print plot
      if(is.factor(covid19_lmdf[[input$var3]])) {
        validate(
          need(is.numeric(covid19_lmdf[[input$var3]]), "Please select Y as Numeric Variable to Check out QQ Plot!")
        )
      } else{
        lmout <- lm(covid19_lmdf[[input$var3]] ~ covid19_lmdf[[input$var2]])
        qplot(sample = lmout$residuals, geom = "qq",
              main = "QQ Plot",
              xlab = "theoretical",
              ylab = "sample") +
          geom_qq_line()
      }
    }
  })
  
  ### forth tab
  output$usPlot <- renderPlot({
    if (input$var4 == "Rate of Recovery") {
      plot_usmap(data = covid19_geom, values = "Recovery Rate(%)", color = "blue")+
        scale_fill_continuous(low ="white", high = "red",
                              name = "Recovery Rate(%) 
                              \n Grey as Unknown Value", label = scales::comma)+
        labs(title = "Covid-19 Recovery Rate",
             subtitle = paste0("Recovery Rate by States in 2020"))+
        theme(panel.background = element_rect(color = "black", fill = "white"))+
        theme(legend.position = "top")
      
    } else if (input$var4 == "Rate of Death") {
      plot_usmap(data = covid19_geom, values = "Death Rate(%)", color = "blue")+
        scale_fill_continuous(low ="white", high = "red",
                              name = "Death Rate(%)
                              \n Grey as Unknown Value", label = scales::comma)+
        labs(title = "Covid-19 Death Rate",
             subtitle = paste0("Death Rate by States in 2020"))+
        theme(panel.background = element_rect(color = "black", fill = "white"))+
        theme(legend.position = "top")
      
    } 
})

# reference :
# https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
      output$spRank <- renderDataTable({
        if (input$var4 == "Rate of Recovery") {
          covid19_geom %>%
          dplyr::select(State, `Number of Recovery`, `Recovery Rate(%)`)
        } else if (input$var4 == "Rate of Death"){
          covid19_geom %>%
            dplyr::select(State, `Number of Death`, `Death Rate(%)`)
        } 
        }, options = list(pageLength = 5,
                           autoWidth = FALSE,
                           columnDefs = list(list(width = '300px', targets = "_all")),
                           scrollx = TRUE))
      output$rank <- renderDataTable({
          covid19_geom %>%
            dplyr::select(State, `Rank(Confirmed)`, `Number of Confirmed`,
                          `Number of Recovery`, `Recovery Rate(%)`, `Number of Death`, `Rank(Death Rate)`,
                          `Death Rate(%)`, `Status Unknown`)
      }, options = list(pageLength = 10,
                       autoWidth = FALSE,
                       columnDefs = list(list(width = '300px', targets = "_all")),
                       scrollx = TRUE))
 
}

# Application

shinyApp(ui, server)