library(leaflet)    
library(shiny)
library(shinydashboard)
library(sp)
library(rgdal)
library(geojsonio)
library(tidyverse)

setwd("~/desktop/fp_final-project-group-5/ds_final_app/clickable_map")

covid <- read_rds("covid19_geom.rds")

states <- geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
        what = "sp")

covid_df <- merge(states, covid, by.x = "name", by.y = "State")

# create the UI
ui <- fluidPage(
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
)


server <- function(input, output, session){
    # Create the polygon popup
    polygon_popup <- paste0("<strong>Name: </strong>", states$name, "<br>",
                            "<strong> Confirmed Cases: </strong>", covid_df$"Number of Confirmed",
                            "<strong> Recovered: </strong>", covid_df$"Number of Recovery",
                            "<strong> Number of Deaths : </strong>", covid_df$"Number of Death",
                            "<strong> Rank (Deaths) : </strong>", covid_df$"Rank(Death Rate)",
                            "<strong> Rank (Confirmed Cases) : </strong>", covid_df$"Rank(Confirmed)")
    
    
    # create foundational map and input the polygon popup
    foundational.map <- shiny::reactive({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(-98.35, 39.7, zoom = 4) %>%
            addPolygons(data = covid_df,
                        fillOpacity = 0,
                        opacity = 0.2,
                        color = "#000000",
                        weight = 2,
                        layerId = covid_df$name,
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
        lines.of.interest <- covid_df[which(covid_df$name %in% click.list$ids ) , ]
        
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

}






## run shinyApp
shiny::shinyApp( ui = ui, server = server)