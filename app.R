require(shiny)
require(RColorBrewer)
require(leaflet)

# load munged data from R native binary file
hourlyAvg <- readRDS("processedData/hourlyData_annual.Rds")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  selectInput("region", "Region",
                              c("All", unique(hourlyAvg$region))
                  ),
                  selectInput("period", "Period",
                              unique(hourlyAvg$period)
                  ),
                  selectInput("layer", "Layer",
                              unique(hourlyAvg$layer)
                  ),
                  sliderInput("range", "Windspeed",
                              min(floor(hourlyAvg$value)),
                              max(ceiling(hourlyAvg$value)),
                              value = range(hourlyAvg$value), 
                              step = 0.5
                  ),
                  selectInput("colors", "Color Scheme", c("YlOrRd","Greys")),
                  checkboxInput("legend", "Show legend", TRUE)
    )
)

server <- function(input, output, session) {
    # reactively filter data based on user selections
    filteredData <- reactive({
        if (input$region == "All") {
            regions = unique(hourlyAvg$region)
        } else {
            regions = input$region
        }
        
        hourlyAvg <- hourlyAvg %>%
            dplyr::filter(value >= input$range[1],
                         value <= input$range[2],
                          region %in% regions,
                          period == input$period,
                          layer == input$layer)
    })
    
    colorpal <- reactive({
        colorNumeric(input$colors, hourlyAvg$value)
    })
    
    output$map <- renderLeaflet({
        leaflet(hourlyAvg) %>%
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), 
                      ~max(longitude), ~max(latitude))
    })
    
    observe({
        pal <- colorpal()
        
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(radius = ~(value * 100), weight = 1, color = ~pal(value),
                       fillColor = ~pal(value), fillOpacity = 0.7, popup = ~paste(value)
            )
    })
    
    observe({
        proxy <- leafletProxy("map", data = hourlyAvg)
        
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal, values = ~value
            )
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)