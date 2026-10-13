###################################################
# Airports within a radius of a Geographic Center of Europe
# + EU ETS (Emissions Trading System) flight exposure analysis
###################################################

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(geosphere)
library(tidyr)
library(DT)
library(readr)
library(reactable)

###################################################
# Airport database (loaded once when the app starts)
# Same OurAirports source used for the circle map. It also
# doubles as our ICAO <-> IATA <-> country lookup table for
# the flights data below (join key = "ident", which is the
# ICAO code in this dataset).
#
# Airports with no published IATA code are excluded entirely,
# everywhere in this app - map, tables, and flight aggregations -
# since all results are reported using the IATA code.
###################################################

airports_raw <- read.csv(
    "https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv"
)

airports <- airports_raw %>%
    filter(!is.na(coordinates)) %>%
    # NOTE: original script had "small airport" (space) which never matched
    # anything - fixed to "small_airport" (underscore) to match the source data
    filter(type %in% c("large_airport")) %>%
    filter(!is.na(iata_code), iata_code != "")

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
# Countries excluded from the map results.
# These are the EU / EEA / UK / CH member states - referred to
# throughout this app as "EU" for brevity, and as the set of
# countries whose intra-flights are subject to the EU ETS today.
###################################################

excluded <- c(
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "GB", "CH", "GG", "JE", "NO", "IS", "IM", "FO"
)

# Countries to keep out of the ETS-specific tables (non-EU EEA members,
# UK, and Crown Dependencies - not covered by the same regime as EU-27)
non_ets_countries <- c("GB", "CH", "IS", "GG", "JE", "IM", "FO")
ets_countries <- setdiff(excluded, non_ets_countries)

###################################################
# Geographic centers of Europe
###################################################

centers <- list(
    "Frankfurt Airport, DE"          = c(lat = 50.033333, lon = 8.570556),
    "Suchowola, PL"                  = c(lat = 53.577500, lon = 23.106111),
    "Kremnicke Bane, SK"             = c(lat = 48.743611, lon = 18.930556),
    "Landskrona, SE"                 = c(lat = 55.870800, lon = 12.830200),
    "Girija, LT"                     = c(lat = 54.906667, lon = 25.320000),
    "Monnuste, EE"                   = c(lat = 58.303889, lon = 22.278889),
    "Saint-Andre-le-Coq, FR"         = c(lat = 45.966600, lon = 3.316670),
    "Noireterre (Saint-Clement), FR" = c(lat = 46.062800, lon = 3.705300),
    "Viroinval, BE"                  = c(lat = 50.009167, lon = 4.666389),
    "Kleinmaischeid, DE"             = c(lat = 50.525278, lon = 7.597222),
    "Gelnhausen, DE"                 = c(lat = 50.172500, lon = 9.150000),
    "Westerngrund, DE"               = c(lat = 50.117286, lon = 9.247769),
    "Brussels - the centre of EUrope" = c(lat = 50.8505, lon = 4.3488)
)

###################################################
# ICAO -> IATA -> country lookup
# Airports with no IATA code are dropped from the lookup, so any
# flight whose ADEP or ADES has no IATA code will fail to join and
# be excluded from every table below.
###################################################

airport_lookup <- airports_raw %>%
    filter(!is.na(ident), ident != "") %>%
    filter(!is.na(iata_code), iata_code != "") %>%
    distinct(ident, .keep_all = TRUE) %>%
    transmute(
        ident,
        iata_code,
        iso_country,
        airport_name = name,
        municipality
    )

###################################################
# Flights data (Eurocontrol ADR&R extract)
###################################################

FLIGHTS_CSV_PATH <- "ECTRL_ADRR_Flights_20240601_20240630.csv"

flights_raw <- read_csv(FLIGHTS_CSV_PATH, show_col_types = FALSE)

adep_lookup <- airport_lookup %>%
    rename(
        ADEP              = ident,
        adep_iata         = iata_code,
        adep_country      = iso_country,
        adep_name         = airport_name,
        adep_municipality = municipality
    )

ades_lookup <- airport_lookup %>%
    rename(
        ADES              = ident,
        ades_iata         = iata_code,
        ades_country      = iso_country,
        ades_name         = airport_name,
        ades_municipality = municipality
    )

flights <- flights_raw %>%
    left_join(adep_lookup, by = "ADEP") %>%
    left_join(ades_lookup, by = "ADES") %>%
    # Exclude entirely any flight where either end has no known IATA code
    filter(!is.na(adep_iata), !is.na(ades_iata))

###################################################
# Table: Airport_Flights
# Total departing flights per ADEP airport, for the month covered
# by this extract. Static - does not depend on the map's
# radius/center inputs.
###################################################

Airport_Flights <- flights %>%
    group_by(ADEP, adep_iata, adep_name, adep_country) %>%
    summarise(n_flights = n(), .groups = "drop") %>%
    arrange(desc(n_flights))

###################################################
# Table: Airport_Flights_to_ETS_EU
# For every departure airport, how many of its flights land in
# one of the "excluded" (EU/EEA/UK/CH) countries. This is the
# ETS baseline - flights already subject to EU ETS today. Static.
###################################################

Airport_Flights_to_ETS_EU <- flights %>%
    filter(ades_country %in% excluded) %>%
    group_by(ADEP, adep_iata, adep_name, adep_country) %>%
    summarise(n_to_EU = n(), .groups = "drop")

###################################################
# reactable column definitions (shared between each tab's pinned
# 1-row summary table and its sortable data table, so the two
# stay visually aligned).
###################################################

pct_col_def <- colDef(
    name   = "% increase in flights subject to ETS compliance costs",
    width  = 260,
    format = colFormat(suffix = "%", digits = 1)
)

country_col_defs <- list(
    Country = colDef(width = 110),
    `Flights to EU` = colDef(
        width = 150, format = colFormat(separators = TRUE, digits = 0)
    ),
    `Flights to selection` = colDef(
        width = 170, format = colFormat(separators = TRUE, digits = 0)
    ),
    `Percent increase in flights subject to ETS compliance costs` = pct_col_def
)

airport_col_defs <- list(
    Airport = colDef(width = 100),
    `Airport name` = colDef(width = 200),
    Country = colDef(width = 90),
    `Monthly flights` = colDef(
        width = 140, format = colFormat(separators = TRUE, digits = 0)
    ),
    `Flights to EU` = colDef(
        width = 140, format = colFormat(separators = TRUE, digits = 0)
    ),
    `Flights to selection` = colDef(
        width = 160, format = colFormat(separators = TRUE, digits = 0)
    ),
    `Percent increase in flights subject to ETS compliance costs` = pct_col_def
)

SUMMARY_ROW_STYLE <- list(fontWeight = "bold", background = "#e8e8e8")

###################################################
# UI
###################################################

ui <- fluidPage(
    titlePanel("ETS Proposal Aviation Scope Expansion | Airports within a Radius of a Geographic Center of Europe"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId  = "center_choice",
                label    = "Geographic center of Europe:",
                choices  = names(centers),
                selected = "Frankfurt Airport, DE"
            ),
            sliderInput(
                inputId = "radius_km",
                label   = "Radius (km):",
                min     = 1000,
                max     = 17000,
                value   = 5000,
                step    = 500
            ),
            hr(),
            actionButton("show_readme", "\u2139\ufe0f About this app", width = "100%"),
            hr(),
            helpText(
                "The map shows large airports inside the chosen radius, ",
                "excluding airports located in EU/EEA/UK/CH member states. "
            ),
            helpText(
                "The tabs provide details on % increases in flights which would",
                "have to surrender ETS allowances by country/airport.",
                "Based on *June 2024* Flight data from ECTRL ADRR."
            ),
            helpText(
                "This is a personal project and offers no policy prescriptions."
            )
        ),
        mainPanel(
            width = 9,
            tabsetPanel(
                id = "main_tabs",
                tabPanel(
                    "Map & Airports",
                    br(),
                    leafletOutput("map", height = 600),
                    hr(),
                    DTOutput("table")
                ),
                tabPanel(
                    "EU Countries \u2014 ETS Summary",
                    br(),
                    helpText(
                        "Departing flights June 2024. UK/CH included in sums but not shown.",
                        "Click a column header to sort. The \u201cEEA/CH/UK\u201d total ",
                        "row is pinned at the top and is not affected by sorting."
                    ),
                    reactableOutput("country_summary_reactable"),
                    reactableOutput("country_reactable")
                ),
                tabPanel(
                    "EU Airports \u2014 ETS Detail",
                    br(),
                    helpText(
                        "Departing flights June 2024. UK/CH included in sums but not shown.",
                        "Click a column header to sort. The \u201cEEA/CH/UK\u201d total ",
                        "row is pinned at the top and is not affected by sorting."
                    ),
                    reactableOutput("airport_summary_reactable"),
                    reactableOutput("airport_reactable")
                )
            )
        )
    )
)

###################################################
# Server
###################################################

server <- function(input, output, session) {
    
    observeEvent(input$show_readme, {
        showModal(modalDialog(
            title = "About this app",
            easyClose = TRUE,
            size = "l",
            footer = modalButton("Close"),
            tagList(
                h4("What this app does"),
                p(
                    "Shows airports within a chosen radius of a geographic center ",
                    "of Europe, and estimates how many flights from EU",
                    "airports would newly be exposed to ETS-style compliance costs ",
                    "if coverage extended to the non-EU airports currently selected ",
                    "on the map.
                    This is a personal project and offers no policy prescriptions."
                ),
                h4("Data"),
                tags$ul(
                    tags$li(
                        "Airport reference data: OurAirports."
                    ),
                    tags$li(
                        "Flight data: June 2024 Eurocontrol ADR&R extract ",
                        "“This document/R&D product has been created with or contains elements",
                        "of ATM Datasets made available by EUROCONTROL. ©2020, EUROCONTROL)."
                    ),
                ),
                h4("Key assumptions"),
                tags$ul(
                    tags$li(
                        "\u201cMonthly flights\u201d = Data for June 2024 - the most recent ECTRL ADRR dataset."
                    ),
                    tags$li(
                        "\u201cFlights to EU\u201d = flights landing in an excluded ",
                        "(EU/EEA/UK/CH) country - the current ETS baseline."
                    ),
                    tags$li(
                        "\u201cFlights to selection\u201d = flights from EU airports ",
                        "to the non-EU airports currently inside the map's radius - ",
                        "recalculated whenever you change the center or radius."
                    ),
                ),
                h4("DISCLAIMER"),
                tags$ul(
                    tags$li(
                        "\u201cUse with your own responsibility\u201d = I made this to better understand the choice of radius and centre-point for the ETS aviation scope proposal. There are no guarantees about the accuracy of the data or sums. Absolutely no policy prescriptions should be derived from this app. Have fun & be careful ! - Michael"
                    ),
                )
            )
        ))
    })
    
    center_point <- reactive({
        c <- centers[[input$center_choice]]
        list(lat = unname(c["lat"]), lon = unname(c["lon"]))
    })
    
    # Airports NOT in an excluded (EU/EEA/UK/CH) country, within the
    # chosen radius of the chosen center - i.e. the "user selection".
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
            addProviderTiles(providers$Stadia.Outdoors) %>%
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
    
    #################################################
    # Table: Airport_Flights_to_ETS_nonEU
    # For each EU-country departure airport, flights to airports
    # currently in the map's user selection (non-EU, within radius).
    # Dynamic - recalculated whenever center/radius change.
    #################################################
    
    Airport_Flights_to_ETS_nonEU <- reactive({
        target_idents <- filtered_airports()$ident
        
        flights %>%
            filter(adep_country %in% excluded, ADES %in% target_idents) %>%
            group_by(ADEP, adep_iata, adep_name, adep_country) %>%
            summarise(n_to_nonEU = n(), .groups = "drop")
    })
    
    #################################################
    # Tab 1 data: EU Countries summary table
    #################################################
    
    # Unsorted, one row per EU/EEA/UK/CH country - feeds the sortable table.
    country_summary_base <- reactive({
        nonEU <- Airport_Flights_to_ETS_nonEU()
        
        eu_by_country <- Airport_Flights_to_ETS_EU %>%
            group_by(adep_country) %>%
            summarise(flights_to_EU = sum(n_to_EU), .groups = "drop")
        
        nonEU_by_country <- nonEU %>%
            group_by(adep_country) %>%
            summarise(flights_to_nonEU = sum(n_to_nonEU), .groups = "drop")
        
        tibble(iso_country = ets_countries) %>%
            left_join(eu_by_country, by = c("iso_country" = "adep_country")) %>%
            left_join(nonEU_by_country, by = c("iso_country" = "adep_country")) %>%
            mutate(
                flights_to_EU    = coalesce(flights_to_EU, 0L),
                flights_to_nonEU = coalesce(flights_to_nonEU, 0L),
                pct_increase     = ifelse(
                    flights_to_EU == 0, NA_real_,
                    round(flights_to_nonEU / flights_to_EU * 100, 1)
                )
            ) %>%
            select(
                Country                = iso_country,
                `Flights to EU`        = flights_to_EU,
                `Flights to selection` = flights_to_nonEU,
                `Percent increase in flights subject to ETS compliance costs` =
                    pct_increase
            )
    })
    
    # Single-row "EEA/CH/UK" total - a true aggregate (sum of numerator and
    # denominator, not an average of percentages).
    # Single-row "EEA/CH/UK" total - always based on the FULL 'excluded'
    # (EU/EEA/UK/CH) vector, independent of which countries are shown as
    # rows in country_summary_base(). A true aggregate (sum of numerator
    # and denominator, not an average of percentages).
    country_summary_total <- reactive({
        nonEU <- Airport_Flights_to_ETS_nonEU()
        
        total_EU <- Airport_Flights_to_ETS_EU %>%
            filter(adep_country %in% excluded) %>%
            pull(n_to_EU) %>%
            sum(na.rm = TRUE)
        
        total_nonEU <- nonEU %>%
            filter(adep_country %in% excluded) %>%
            pull(n_to_nonEU) %>%
            sum(na.rm = TRUE)
        
        total_pct <- if (total_EU == 0) {
            NA_real_
        } else {
            round(total_nonEU / total_EU * 100, 1)
        }
        
        tibble(
            Country                = "EEA/CH/UK",
            `Flights to EU`        = total_EU,
            `Flights to selection` = total_nonEU,
            `Percent increase in flights subject to ETS compliance costs` =
                total_pct
        )
    })
    
    #################################################
    # Tab 2 data: EU Airports detail table
    #################################################
    
    # Unsorted, one row per EU airport - feeds the sortable table.
    eu_airport_base <- reactive({
        nonEU <- Airport_Flights_to_ETS_nonEU()
        
        Airport_Flights %>%
            filter(adep_country %in% ets_countries) %>%
            left_join(
                Airport_Flights_to_ETS_EU %>% select(ADEP, n_to_EU),
                by = "ADEP"
            ) %>%
            left_join(
                nonEU %>% select(ADEP, n_to_nonEU),
                by = "ADEP"
            ) %>%
            mutate(
                n_to_EU      = coalesce(n_to_EU, 0L),
                n_to_nonEU   = coalesce(n_to_nonEU, 0L),
                pct_increase = ifelse(
                    n_to_EU == 0, NA_real_,
                    round(n_to_nonEU / n_to_EU * 100, 1)
                )
            ) %>%
            select(
                Airport                = adep_iata,
                `Airport name`         = adep_name,
                Country                = adep_country,
                `Monthly flights`      = n_flights,
                `Flights to EU`        = n_to_EU,
                `Flights to selection` = n_to_nonEU,
                `Percent increase in flights subject to ETS compliance costs` =
                    pct_increase
            )
    })
    
    # Single-row "EEA/CH/UK" total - a true aggregate (sum of numerator and
    # denominator, not an average of percentages).
    # Single-row "EEA/CH/UK" total - always based on the FULL 'excluded'
    # (EU/EEA/UK/CH) vector, independent of which airports are shown as
    # rows in eu_airport_base() (which is trimmed to ets_countries). A true
    # aggregate (sum of numerator and denominator, not an average of %s).
    airport_summary_total <- reactive({
        nonEU <- Airport_Flights_to_ETS_nonEU()
        
        total_flights <- Airport_Flights %>%
            filter(adep_country %in% excluded) %>%
            pull(n_flights) %>%
            sum(na.rm = TRUE)
        
        total_EU <- Airport_Flights_to_ETS_EU %>%
            filter(adep_country %in% excluded) %>%
            pull(n_to_EU) %>%
            sum(na.rm = TRUE)
        
        total_nonEU <- nonEU %>%
            filter(adep_country %in% excluded) %>%
            pull(n_to_nonEU) %>%
            sum(na.rm = TRUE)
        
        total_pct <- if (total_EU == 0) {
            NA_real_
        } else {
            round(total_nonEU / total_EU * 100, 1)
        }
        
        tibble(
            Airport                = "EEA/CH/UK",
            `Airport name`          = "",
            Country                 = "",
            `Monthly flights`       = total_flights,
            `Flights to EU`         = total_EU,
            `Flights to selection`  = total_nonEU,
            `Percent increase in flights subject to ETS compliance costs` =
                total_pct
        )
    })
    
    
    #################################################
    # reactable outputs
    #################################################
    
    output$country_summary_reactable <- renderReactable({
        reactable(
            country_summary_total(),
            columns    = country_col_defs,
            sortable   = FALSE,
            pagination = FALSE,
            bordered   = TRUE,
            rowStyle   = SUMMARY_ROW_STYLE
        )
    })
    
    output$country_reactable <- renderReactable({
        reactable(
            country_summary_base(),
            columns        = country_col_defs,
            sortable       = TRUE,
            defaultSorted  = list(`Flights to EU` = "desc"),
            pagination     = FALSE,
            highlight      = TRUE,
            bordered       = TRUE,
            striped        = TRUE
        )
    })
    
    output$airport_summary_reactable <- renderReactable({
        reactable(
            airport_summary_total(),
            columns    = airport_col_defs,
            sortable   = FALSE,
            pagination = FALSE,
            bordered   = TRUE,
            rowStyle   = SUMMARY_ROW_STYLE
        )
    })
    
    output$airport_reactable <- renderReactable({
        reactable(
            eu_airport_base(),
            columns        = airport_col_defs,
            sortable       = TRUE,
            defaultSorted  = list(`Monthly flights` = "desc"),
            pagination     = FALSE,
            highlight      = TRUE,
            bordered       = TRUE,
            striped        = TRUE
        )
    })
}

shinyApp(ui, server)
