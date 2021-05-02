# Upload packages
library(ggthemes)
library(rgdal)
library(sp)
library(leaflet)
library(shinythemes)
library(geojsonio)



readr::read_rds("covid19_geom.rds") -> 
    covid19_geom

# Get USA polygon data

states <- geojson_read("gz_2010_us_040_00_500k.json",what = "sp")

covid.df <- merge(states, covid19_geom, by.x = "NAME", by.y = "State")

class(covid.df)

pal2 <- colorNumeric(palette = "Reds", domain=NULL)


# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> US Covid Death Rates
                                        </font></center></h1>")), 
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("stateInput", label = h3("State"),
                                            choices = c("Choose state",
                                                        "Alabama",
                                                        "Alaska",
                                                        "Arizona",
                                                        "Arkansas",
                                                        "California",
                                                        "Colorado",
                                                        "Connecticut",
                                                        "Delaware",
                                                        "Florida",
                                                        "Georgia",
                                                        "Hawaii",
                                                        "Idaho",
                                                        "Illinois",
                                                        "Indiana",
                                                        "Iowa",
                                                        "Kansas",
                                                        "Kentucky",
                                                        "Louisiana",
                                                        "Maine",
                                                        "Maryland",
                                                        "Massachusetts",
                                                        "Michigan",
                                                        "Minnesota",
                                                        "Mississippi",
                                                        "Missouri",
                                                        "Montana",
                                                        "Nebraska",
                                                        "Nevada",
                                                        "New Hampshire",
                                                        "New Jersey",
                                                        "New Mexico",
                                                        "New York",
                                                        "North Carolina",
                                                        "North Dakota",
                                                        "Ohio",
                                                        "Oklahoma",
                                                        "Oregon",
                                                        "Pennsylvania",
                                                        "Rhode Island",
                                                        "South Carolina",
                                                        "South Dakota",
                                                        "Tennessee",
                                                        "Texas",
                                                        "Utah",
                                                        "Vermont",
                                                        "Virginia",
                                                        "Washington",
                                                        "West Virginia",
                                                        "Wisconsin",
                                                        "Wyoming"
                                            ),
                                            selected = "Choose a State")),
                                mainPanel(leafletOutput(outputId = 'map', height = 
                                                            800)
                                )
                        )
)
)



# SERVER
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(covid.df) %>% 
            addProviderTiles(providers$Stamen.TonerLite) %>% 
            setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
            addPolygons(data = covid.df ,fillColor = ~pal2(selectedState()),
                        popup = paste0("<strong>State: </strong>", 
                                       covid.df$NAME),
                        color = "#BDBDC3",
                        fillOpacity = 0.8,
                        weight = 1)
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
    # selected year
    selectedYear <- reactive({
        covid.df[covid.df$adult_smoking_2015 == input$yearInput &
                       smoking.df$adult_smoking_2016 == input$yearInput &
                       smoking.df$adult_smoking_2017 == input$yearInput,] 
    })
    
    observe({
        state_popup1 <- paste0("<strong>State: </strong>", 
                               selectedState()$NAME)
        
        leafletProxy("map", data = selectedYear()) %>%
            clearShapes() %>%
            addPolygons(fillColor = ~pal(selectedYear()$yearInput),
                        popup = state_popup1,
                        color = "#BDBDC3",
                        fillOpacity = 0.8,
                        weight = 1)
    })
    
    
})


# Run app! 
shinyApp(ui = ui, server = server)

