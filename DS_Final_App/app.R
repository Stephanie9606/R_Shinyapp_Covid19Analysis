# Upload packages
library(ggthemes)
library(rgdal)
library(sp)
library(leaflet)
library(shinythemes)



readr::read_rds("covid19_geom.rds") -> 
    covid19_geom

# Get USA polygon data

USA <- raster::getData("GADM", country = "USA", level = 2)

covid.df <- merge(USA, covid19_geom, by.x = "NAME_1", by.y = "State")

class(covid.df)

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> US Covid Death Rates
                                        </font></center></h1>")), 
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("stateInput", label = h3("State"),
                                            choices = colnames(covid19_geom$State), 
                                            selected = "Alabama")),
                                mainPanel(leafletOutput(outputId = 'map', height = 
                                                            800)
                                )
                        )
)
)



# SERVER
server <- function(input, output) {
    output$map <- renderLeaflet({
        addProviderTiles(providers$Esri.NatGeoWorldMap)
        leaflet(covid.df) %>% 
            setView(lng = -98.583, lat = 39.833, zoom = 4) #%>% 
        
    })

    # selected state
    selectedState <- reactive({
        covid.df[covid.df$NAME_1 == input$stateInput, ] 
    })
    
    observe({
        state_popup <- paste0("<strong>State: </strong>", 
                              selectedState()$NAME_1, 
                              "<br><strong> Death Rate: </strong>",
                              selectedState()$"Death Rate(%)",
                              "<br><strong>% Recovery Rate: </strong>",
                              selectedState()$"Recovery Rate(%)",
                              "<br><strong>% Rank Confirmed Cases: </strong>",
                              selectedState()$"Rank(Confirmed)",
                              "<br><strong>% Rank Deaths: </strong>",
                              selectedState()$"Rank(Death Rate)")
        
        leafletProxy("map", data = selectedState()) %>%
            clearShapes() %>%
            addPolygons(fillColor = "orange",
                        popup = state_popup,
                        color = "#BDBDC3",
                        fillOpacity = 0.8,
                        weight = 1)
    })
}




# Run app! 
shinyApp(ui = ui, server = server)

