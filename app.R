require(shiny)
require(RColorBrewer)
require(leaflet)

source("R/energy.R")
source("R/visualize.R")

# load munged data from R native binary file
data <- readRDS("processedData/hourlyData_annual.Rds")
annualWS <- data %>%
    dplyr::filter(period == "annual",
                  layer == "WS")

powerCurves <- loadPCs("processedData/wind-o-matic_powerCurves.csv")


ui <- navbarPage(
    "OWW", id="nav",
    tabPanel("Map",
             div(class="outer",
                 
                 tags$head(
                     # Include our custom CSS
                     includeCSS("web/styles.css"),
                     includeScript("web/gomap.js")
                 ),
                 
                 # no idea why height requires "px" to work; taken from:
                 # https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
                 leafletOutput("map", width="100%", height = "500px"),
                 
                 absolutePanel(id = "controls", class = "panel panel-default", 
                               fixed = TRUE, draggable = TRUE, 
                               top = 60, bottom = "auto",
                               left = "auto", right = 20, 
                               width = 330, height = "auto",
                               
                               h2("Offshore Wind Prospector"),
                               
                               selectInput("region", "Region",
                                           c("USA - All", 
                                             "USA - Atlantic",
                                             "USA - Pacific",
                                             "USA - Gulf")
                               ),
                               sliderInput("wsRange", "Windspeed",
                                           min(floor(annualWS$value)),
                                           max(ceiling(annualWS$value)),
                                           value = range(annualWS$value), 
                                           step = 0.5
                               ),
                               checkboxInput("legend", "Legend", FALSE)
                 )
             )
    ),
    tabPanel("Power Curve",
             fluidRow(
                 column(3,
                        selectInput("rotorD", "Rotor Diameter [m]", 
                                    unique(powerCurves$RotorDiameter_m)
                        )
                 )
             )
    )
)

server <- function(input, output, session) {
    colorpal <- reactive({
        colorNumeric("YlOrRd", annualWS$value)
    })
    
    regions <- reactive({
        if (input$region == "USA - All") {
            regions <-  unique(data$region)
        } else {
            regions = input$region %>%
                stringr::str_remove("USA - ") %>%
                tolower()
            
            regions <- paste0("annual_", regions)
        }
    })
    
    # reactively filter data based on user selections
    filteredWS <- reactive({
        df <- annualWS %>%
            dplyr::filter(value >= input$wsRange[1],
                          value <= input$wsRange[2],
                          region %in% regions()
            )
    })
    
    
    output$map <- renderLeaflet({
        annualWS %>%
            dplyr::filter(region %in% regions()) %>%
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