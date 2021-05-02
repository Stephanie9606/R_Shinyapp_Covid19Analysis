# load necessary packages
library(leaflet)    
library(shiny)
library(shinydashboard)
library(sp)
library(rgdal)
library(geojsonio)



states <- geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
        what = "sp")


# create the UI
ui <- fluidPage(
    # place the contents inside a box
    shinydashboard::box(
        width = 12
        , title = "Click on the map!"
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
    ) 
)

# create the server
server <- function( input, output, session ){
    
    # create foundational map
    foundational.map <- shiny::reactive({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(-98.35, 39.7, zoom = 4) %>%
            addPolygons(data = states
                         , fillOpacity = 0
                         , opacity = 0.2
                         , color = "#000000"
                         , weight = 2
                         , layerId = states$name
                         , group = "click.list"
            )
    })

output$myMap <- renderLeaflet({
        foundational.map()
        
    })
    
    # store the list of clicked polygons in a vector
    click.list <- shiny::reactiveValues(ids = vector())
    
    # observe where the user clicks on the leaflet map
    # during the Shiny app session
    # Courtesy of two articles:
    # https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet
    # https://rstudio.github.io/leaflet/shiny.html
    shiny::observeEvent(input$myMap_shape_click, {
        
        # store the click(s) over time
        click <- input$myMap_shape_click
        
        # store the polygon ids which are being clicked
        click.list$ids <- c(click.list$ids, click$id)
        
        # filter the spatial data frame
        # by only including polygons
        # which are stored in the click.list$ids object
        lines.of.interest <- states[which(states$name %in% click.list$ids ) , ]
        
        # if statement
        if(is.null(click$id)){
            # check for required values, if true, then the issue
            # is "silent". See more at: ?req
            req(click$id)
            
        } else if(!click$id %in% lines.of.interest@data$id){
            
            # call the leaflet proxy
            leaflet::leafletProxy(mapId = "myMap") %>%
                # and add the polygon lines
                # using the data stored from the lines.of.interest object
                addPolylines(data = lines.of.interest
                              , layerId = lines.of.interest@data$id
                              , color = "#6cb5bc"
                              , weight = 5
                              , opacity = 1)
        }
    })
    
    
    # Create the logic for the "Clear the map" action button
    # which will clear the map of all user-created highlights
    # and display a clean version of the leaflet map
    shiny::observeEvent( input$clearHighlight, {
        
        # recreate $myMap
        output$myMap <- leaflet::renderLeaflet({
            
            # first
            # set the reactive value of click.list$ids to NULL
            click.list$ids <- NULL
            
            # second
            # recall the foundational.map() object
            foundational.map()
        })
    })
}

## run shinyApp
shiny::shinyApp( ui = ui, server = server)