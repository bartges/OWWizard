require(shiny)
require(RColorBrewer)
require(leaflet)

# load munged data from R native binary file
data <- readRDS("processedData/hourlyData_annual.Rds")
annualWS <- data %>%
    dplyr::filter(period == "annual",
                  layer == "WS")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  selectInput("region", "Region",
                              c("All", unique(annualWS$region))
                  ),
                  sliderInput("range", "Windspeed",
                              min(floor(annualWS$value)),
                              max(ceiling(annualWS$value)),
                              value = range(annualWS$value), 
                              step = 0.5
                  ),
                  checkboxInput("legend", "Show legend", FALSE)
    )
)

server <- function(input, output, session) {
    # reactively filter data based on user selections
    filteredWS <- reactive({
        if (input$region == "All") {
            regions = unique(data$region)
        } else {
            regions = input$region
        }
        
        df <- annualWS %>%
            dplyr::filter(value >= input$range[1],
                          value <= input$range[2],
                          region %in% regions
            )
    })
    
    colorpal <- reactive({colorNumeric("YlOrRd", annualWS$value)})
    
    output$map <- renderLeaflet({
        if (input$region == "All") {
            regions = unique(data$region)
        } else {
            regions = input$region
        }
       
        annualWS %>%
            dplyr::filter(region %in% regions) %>%
        leaflet() %>%
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), 
                      ~max(longitude), ~max(latitude))
    })
    
    observe({
        pal <- colorpal()
        
        leafletProxy("map", data = filteredWS()) %>%
            clearShapes() %>%
            addCircles(radius = ~(1E4), weight = 1, color = ~pal(value),
                       fillColor = ~pal(value), fillOpacity = 0.4, 
                       popup = ~paste0("WS = ", round(value, 1), " m/s")
            )
    })
    
    observe({
        proxy <- leafletProxy("map", data = annualWS)
        
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal, values = ~value,
                                title = "Wind Speed [m/s]"
            )
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)