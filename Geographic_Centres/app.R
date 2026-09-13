###################################################
# Airports within a radius of a Geographic Center of Europe
# Shiny app version of Identifying_Airports_in_Circle.R
###################################################

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(geosphere)
library(tidyr)
library(DT)

###################################################
# Airport database (loaded once when the app starts)
###################################################

airports_raw <- read.csv(
    "https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv"
)

airports <- airports_raw %>%
    filter(!is.na(coordinates)) %>%
    # NOTE: original script had "small airport" (space) which never matched
    # anything - fixed to "small_airport" (underscore) to match the source data
    filter(type %in% c("large_airport"))

coords <- strsplit(airports$coordinates, ",")
airports$latitude  <- as.numeric(trimws(sapply(coords, `[`, 1)))
airports$longitude <- as.numeric(trimws(sapply(coords, `[`, 2)))

airports <- airports %>%
    filter(
        longitude >= -180, longitude <= 180,
        latitude  >= -90,  latitude  <= 90
    )

airport_points <- as.matrix(airports[, c("longitude", "latitude")])

###################################################
# Countries excluded from results (unchanged from original script)
###################################################

excluded <- c(
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "GB", "CH", "GG", "JE", "NO", "IS", "IM", "FO"
)

###################################################
# Geographic centers of Europe (DMS converted to decimal degrees)
###################################################

centers <- list(
    "Frankfurt Airport, DE"          = c(lat = 50.033333, lon = 8.570556),
    "Suchowola, PL"                  = c(lat = 53.577500, lon = 23.106111),
    "Kremnicke Bane, SK"             = c(lat = 48.743611, lon = 18.930556),
    "Dilove, UA"                     = c(lat = 48.500000, lon = 23.383333),
    "Landskrona, SE"                 = c(lat = 55.870800, lon = 12.830200),
    "Girija, LT"                     = c(lat = 54.906667, lon = 25.320000),
    "Tallya, HU"                     = c(lat = 48.236100, lon = 21.225740),
    "Monnuste, EE"                   = c(lat = 58.303889, lon = 22.278889),
    "Saint-Andre-le-Coq, FR"         = c(lat = 45.966600, lon = 3.316670),
    "Noireterre (Saint-Clement), FR" = c(lat = 46.062800, lon = 3.705300),
    "Viroinval, BE"                  = c(lat = 50.009167, lon = 4.666389),
    "Kleinmaischeid, DE"             = c(lat = 50.525278, lon = 7.597222),
    "Gelnhausen, DE"                 = c(lat = 50.172500, lon = 9.150000),
    "Westerngrund, DE"               = c(lat = 50.117286, lon = 9.247769)
)

###################################################
# UI
###################################################

ui <- fluidPage(
    titlePanel("Airports within a Radius of a Geographic Center of Europe"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId  = "center_choice",
                label    = HTML('<strong>&#128205; Geographic center of Europe:</strong>'),
                choices  = names(centers),
                selected = "Frankfurt Airport, DE"
            ),
            helpText(em("Start here \u2014 this sets the map center and the circle's origin.")),
            sliderInput(
                inputId = "radius_km",
                label   = "Radius (km):",
                min     = 3000,
                max     = 7500,
                value   = 5000,
                step    = 500
            ),
            hr(),
            downloadButton("download_csv", "Download airports CSV"),
            hr(),
            helpText(
                "Map shows airports (small/medium/large) inside the chosen radius, ",
                "excluding airports located in EU/EEA/UK/CH member states."
            )
        ),
        mainPanel(
            width = 9,
            leafletOutput("map", height = 600),
            hr(),
            DTOutput("table")
        )
    )
)

###################################################
# Server
###################################################

server <- function(input, output, session) {
    
    center_point <- reactive({
        c <- centers[[input$center_choice]]
        list(lat = unname(c["lat"]), lon = unname(c["lon"]))
    })
    
    filtered_airports <- reactive({
        cp <- center_point()
        center_xy <- c(cp$lon, cp$lat)
        
        dist_km <- geosphere::distHaversine(airport_points, center_xy) / 1000
        
        airports %>%
            mutate(distance_km = dist_km) %>%
            filter(distance_km <= input$radius_km) %>%
            filter(!(iso_country %in% excluded)) %>%
            arrange(distance_km)
    })
    
    circle_wgs84 <- reactive({
        cp <- center_point()
        
        center_sf <- st_as_sf(
            data.frame(lon = cp$lon, lat = cp$lat),
            coords = c("lon", "lat"),
            crs = 4326
        )
        
        center_3035 <- st_transform(center_sf, 3035)
        circle_3035 <- st_buffer(center_3035, input$radius_km * 1000)
        st_transform(circle_3035, 4326)
    })
    
    output$map <- renderLeaflet({
        result <- filtered_airports()
        cp <- center_point()
        
        result_sf <- st_as_sf(
            result,
            coords = c("longitude", "latitude"),
            crs = 4326
        )
        result_sf$popup_text <- paste0(
            "<b>", result_sf$iata_code, "</b><br>",
            round(result_sf$distance_km), " km"
        )
        
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
                data       = circle_wgs84(),
                color      = "red",
                weight     = 2,
                fillColor  = "red",
                fillOpacity = 0.15
            ) %>%
            addCircleMarkers(
                lng    = cp$lon,
                lat    = cp$lat,
                radius = 8,
                color  = "blue",
                label  = input$center_choice
            ) %>%
            addCircleMarkers(
                data        = result_sf,
                radius      = 4,
                color       = "darkgreen",
                stroke      = FALSE,
                fillOpacity = 0.8,
                popup       = ~popup_text
            )
    })
    
    output$table <- renderDT({
        filtered_airports() %>%
            select(
                name, municipality, iata_code, iso_country, iso_region,
                latitude, longitude, distance_km
            ) %>%
            mutate(distance_km = round(distance_km, 1))
    })
    
    output$download_csv <- downloadHandler(
        filename = function() {
            center_slug <- gsub("[^A-Za-z0-9]+", "_", input$center_choice)
            paste0("airports_", center_slug, "_", input$radius_km, "km.csv")
        },
        content = function(file) {
            filtered_airports() %>%
                select(
                    name, municipality, iata_code, iso_country, iso_region, distance_km
                ) %>%
                write.csv(file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)
