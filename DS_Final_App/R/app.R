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
library(shinydashboard)
library(sp)
library(rgdal)
library(geojsonio)

readr::read_rds("../data/covid19_tidy.rds") -> 
  covid19_tidy

readr::read_rds("../data/covid19_lmdf.rds") -> 
  covid19_lmdf

readr::read_rds("../data/covid19_n_death.rds") -> 
  covid19_n_death

readr::read_rds("../data/covid19_geom.rds") -> 
  covid19_geom

geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
                       what = "sp") ->
   covid19_states

merge(covid19_states, covid19_geom, by.x = "name", by.y = "State") ->
  covid19_mapdf

# if not necessary in map, we can just delete it(leaflet map may need it)
# convert latitude and longitude data in csv to a simple features object
covid19_tidy %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326, agr = "field") -> cord_covid

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
             # place the contents inside a box
             shinydashboard::box(
               width = 12
               , title = "Click on Any State on the Map!"
               # separate the box by a column
               , column(
                 width = 2
                 , shiny::actionButton(inputId = "clearHighlight",
                                       icon = icon( name = "eraser"),
                                       label = "Clear the Map",
                                       style = "color: #fff; background-color: #D75453; border-color: #C73232")
               )
               , column(
                 width = 10
                 , leaflet::leafletOutput(outputId = "myMap",
                                          height = 850
                 )
               )
             ),
             plotOutput("data")
    ),
    tabPanel("Plot Analysis",
      sidebarLayout(
        sidebarPanel(
          radioButtons("rbuts1", "What type of the data are you interested in?", choices = case_types, selected = "Case"),
          varSelectInput("var1", "Check the data based on?", data = covid19_tidy, selected = "State(Abbrev)"),
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
                      varSelectInput("var2", "X variable?", data = covid19_lmdf, selected = "Symptom Status")
                      ),
               column(4,
                      varSelectInput("var3", "Y variable?", data = covid19_lmdf, selected = "Case")
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
      fluidRow(column(4,
                      checkboxInput("var4", "Rate of Recovery"),
                      checkboxInput("var5", "Rate of Death")
                      ),
               column(4, plotOutput("rPlot")
                      ),
               column(4, plotOutput("dPlot")
                      )
      ),
      fluidRow(
        column(12, dataTableOutput("rank"))
      ))
             
    )
)


# Server
server <- function(input, output){
  ### first tab
  # Create the polygon popup
  polygon_popup <- paste0("<strong>State: </strong>", covid19_states$name, "<br>",
                          "<strong> Confirmed Cases: </strong>", covid19_mapdf$"Number of Confirmed",
                          "<strong> Recovered: </strong>", covid19_mapdf$"Number of Recovery",
                          "<strong> Number of Deaths : </strong>", covid19_mapdf$"Number of Death",
                          "<strong> Rank (Deaths) : </strong>", covid19_mapdf$"Rank(Death Rate)",
                          "<strong> Rank (Confirmed Cases) : </strong>", covid19_mapdf$"Rank(Confirmed)")
  
  
  # create foundational map and input the polygon popup
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-98.35, 39.7, zoom = 4) %>%
      addPolygons(data = covid19_mapdf,
                  fillOpacity = 0,
                  opacity = 0.2,
                  color = "#000000",
                  weight = 2,
                  layerId = covid19_mapdf$name,
                  group = "click.list",
                  popup = polygon_popup
      )
  })
  
  output$myMap <- renderLeaflet({
    foundational.map()
    
  })
  
  # store the list of clicked polygons into a vector
  click.list <- shiny::reactiveValues(ids = vector())
  
  # observe where the user clicks on the leaflet map
  shiny::observeEvent(input$myMap_shape_click, {
    
    # store the click over time
    click <- input$myMap_shape_click
    
    # store the polygon ids which are being clicked
    click.list$ids <- c(click.list$ids, click$id)
    
    # filter the spatial data frame by only including polygons which are stored in the click.list$ids object
    lines.of.interest <- covid19_mapdf[which(covid19_mapdf$name %in% click.list$ids ) , ]
    
    # create if statement
    if(is.null(click$id)){
      # check for required values, if true, then the issue
      # is "silent". See more at: ?req
      req(click$id)
      
    } else if(!click$id %in% lines.of.interest@data$id){
      
      # call the leaflet proxy
      leaflet::leafletProxy(mapId = "myMap") %>%
        # add the polygon lines using the data stored from the lines.of.interest object
        addPolylines(data = lines.of.interest
                     , layerId = lines.of.interest@data$id
                     , color = "#6cb5bc"
                     , weight = 5
                     , opacity = 1)
    }
  })
  
  # Create the logic for the "Clear the map" button
  shiny::observeEvent(input$clearHighlight, {
    
    # recreate $myMap
    output$myMap <- leaflet::renderLeaflet({
      
      # set the reactive value of click.list$ids to NULL
      click.list$ids <- NULL
      # recall the foundational.map() object
      foundational.map()
    })
  })
  
  
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
        drop_na(Race) %>% 
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
    if (is.numeric(covid19_lmdf[[input$var2]]) && is.numeric(covid19_lmdf[[input$var3]])){
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
    
    shiny::validate(
      need(is.numeric(covid19_lmdf[[input$var3]]), "Please select Y as Numeric Variable to Check out Linear Model Summary!")
    )
  })
  
  # optional: residual plot
  output$plotrsd <- renderPlot({
    # if check to see residual plot
    if(isTRUE(input$cbox1)){
      # if y input is factor print plot
      if(is.factor(covid19_lmdf[[input$var3]])) {
        shiny::validate(
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
        shiny::validate(
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
  
  output$rPlot <- renderPlot({
    
    if (isTRUE(input$var4)) {
      plot_usmap(data = covid19_geom, values = "Recovery Rate(%)", color = "blue")+
        scale_fill_continuous(low ="white", high = "red",
                              name = "Recovery Rate(%)", label = scales::comma)+
        labs(title = "Covid-19 Recovery Rate",
             subtitle = paste0("Recovery Rate by States in 2020"))+
        theme(panel.background = element_rect(color = "black", fill = "white"))+
        theme(legend.position = "top")
      
      output$rank <- renderDataTable({
        covid19_geom %>%
          dplyr::select(State, `Number of Recovery`, `Recovery Rate(%)`)
      }, options = list(pageLength = 10, 
                        autoWidth = FALSE, 
                        columnDefs = list(list(width = '600px', targets = "2")),
                        scrollx = TRUE
      ))
      
    } else {
      output$rank <- renderDataTable({
        
        covid19_geom %>%
          dplyr::select(State, `Rank(Confirmed)`, `Number of Confirmed`, 
                        `Number of Recovery`, `Recovery Rate(%)`, `Number of Death`, `Rank(Death Rate)`,
                        `Death Rate(%)`, `Status Unknown`)
      }, options = list(pageLength = 10, 
                        autoWidth = FALSE, 
                        columnDefs = list(list(width = '600px', targets = "2")),
                        scrollx = TRUE
      ))
    }
  })
  output$dPlot <- renderPlot({ 
    if (isTRUE(input$var5)) {
          plot_usmap(data = covid19_geom, values = "Death Rate(%)", color = "blue")+
          scale_fill_continuous(low ="white", high = "red",
                                name = "Death Rate(%)", label = scales::comma)+
          labs(title = "Covid-19 Death Rate",
               subtitle = paste0("Death Rate by States in 2020"))+
          theme(panel.background = element_rect(color = "black", fill = "white"))+
          theme(legend.position = "top")
      
      output$rank <- renderDataTable({
        covid19_geom %>%
          dplyr::select(State, `Number of Death`, `Rank(Death Rate)`, `Death Rate(%)`)
      }, options = list(pageLength = 10, 
                        autoWidth = FALSE, 
                        columnDefs = list(list(width = '600px', targets = "2")),
                        scrollx = TRUE
      ))
    } else {
      output$rank <- renderDataTable({
        
        covid19_geom %>%
          dplyr::select(State, `Rank(Confirmed)`, `Number of Confirmed`, 
                        `Number of Recovery`, `Recovery Rate(%)`, `Number of Death`, `Rank(Death Rate)`,
                        `Death Rate(%)`, `Status Unknown`)
      }, options = list(pageLength = 10, 
                        autoWidth = FALSE, 
                        columnDefs = list(list(width = '600px', targets = "2")),
                        scrollx = TRUE
      ))
    }
 })
  
 
}

# Application

shinyApp(ui, server)