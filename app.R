

library(gifski)
library(fst)
library(tidyverse)
library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(sf)
library(lubridate)
library(gganimate)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(shiny)
library(rsconnect)
library(data.table)
library(pryr)
library(arrow)
library(aws.s3)
library(DBI)
library(renv)
renv::init()
rsconnect::setAccountInfo(name='jrong', token='B5E18944BAB06E37B94A958C4F670D9E', secret='pEvnUFzP0e4peOEF7DT9HDjjJEKwYAnyfJXyZRZo')

mapbox_api_key <- "pk.eyJ1IjoianJvbmdjaG4iLCJhIjoiY2w4YzdpcGtkMDQ0NTNwcHBnajhzdHlzcyJ9.LRe_b1oOFxGWI_vp6zyxKg"
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-1")  # Region of your bucket
Sys.setenv(AWS_ACCESS_KEY_ID = Sys.getenv("AWS_ACCESS_KEY_ID"))
Sys.setenv(AWS_SECRET_ACCESS_KEY = Sys.getenv("AWS_SECRET_ACCESS_KEY"))


# Use your custom Mapbox style
mapbox_style_url <- paste0(
  "https://api.mapbox.com/styles/v1/jrongchn/cl9up1mtj000f14rx67roxhh8/tiles/{z}/{x}/{y}?access_token=",
  mapbox_api_key
)
# load only min/max values globally to use in UI
df_summary <- as.data.table(
  read_parquet("s3://bike-share-data-bucket/bike_data.parquet",
               col_select = c("hour"))
)
min_hour <- min(df_summary$hour, na.rm = TRUE)
max_hour <- max(df_summary$hour, na.rm = TRUE)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Import Google Font */
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@400;700&display=swap');

      /* Apply Roboto Mono font to entire app */
      body, .sliderInput, .leaflet-container, .titlePanel {
        font-family: 'Roboto Mono', monospace !important;
      }

      /* Adjust title position */
      .titlePanel {
        margin-left: 40px !important;  /* Moves title slightly to the right */
      }

      /* Change the Play/Pause button color */
      .irs-grid-text { 
        font-size: 6px !important; 
        font-family: 'Roboto Mono', monospace !important;
        color: #444 !important;  /* Dark gray */
      }

      /* Change the slider label */
      .irs-single {
        background: #6A88FF !important;  /* Blue */
        color: white !important;
        font-weight: bold;
      }

      /* Change slider track */
      .irs-bar {
        background: #6A88FF !important;  /* Blue */
        border-top: 1px solid #0056b3 !important;
        border-bottom: 1px solid #0056b3 !important;
      }

      /* Change the handle (thumb) */
      .irs-slider {
        background: #6A88FF !important;  /* Blue */
        width: 6px !important;
        height: 6px !important;
        border-radius: 50% !important;
      }

      /* Change Play/Pause button color */
      .irs-play, .irs-pause {
        background: #6A88FF !important;  /* Blue */
        color: white !important;
        border-radius: 5px !important;
        padding: 5px !important;
      }

      /* Change Play/Pause button hover effect */
      .irs-play:hover, .irs-pause:hover {
        background: #6A88FF !important;  /* Blue */
      }
    "))
  ),
  
  titlePanel("Bike Movement Over 24 Hours"),  # Title moved slightly to the right
  
  div(
    style = "display: flex; justify-content: center;",
    leafletOutput("bikeMap", width = "90%", height = "600px")  # Bigger map
  ),
  
  div(
    style = "display: flex; justify-content: center; margin-top: 20px;",
    sliderInput("time", "Select Time:", 
                min = min_hour, 
                max = max_hour,  
                value = min_hour,  
                step = 3600,
                animate = animationOptions(interval = 1000, loop = TRUE),
                width = "80%"  # Wider slider
    )
  )
)
# Define Server
server <- function(input, output, session) {
  cat("Checking files...\n")  # Debugging
  
  # Log file existence
  cat("df_24hrs_cleaned.fst exists:", file.exists("df_24hrs_cleaned.fst"), "\n")
  cat("df_24hrs_geometry.rds exists:", file.exists("df_24hrs_geometry.rds"), "\n")
  
  df_24hrs_no_geom <- reactive({
    req(input$time)
    cat("Loading data for hour: ", input$time, "\n")
    tryCatch({
      data <- open_dataset("s3://bike-share-data-bucket/bike_data.parquet", format = "parquet") %>%
        collect()
      cat("Data loaded successfully, rows: ", nrow(data), "\n")
      if(nrow(data) == 0) {
        showNotification("No data available for the selected time", type = "warning")
      }
      data
    }, error = function(e) {
      showNotification("Failed to load data: " + e$message, type = "error")
      cat("Error loading data: ", e$message, "\n")
      NULL
    })
  })
  
  
  df_24hrs_geom <- reactive({
    req(file.exists("df_24hrs_geometry.rds"))
    readRDS("df_24hrs_geometry.rds")
  })
  
  df_24hrs <- reactive({
    req(df_24hrs_no_geom(), df_24hrs_geom())
    st_sf(df_24hrs_no_geom(), geometry = df_24hrs_geom()$geometry)
  })
  
  # Render Leaflet map
  output$bikeMap <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = mapbox_style_url, 
        options = tileOptions(tileSize = 512, zoomOffset = -1),
        attribution = "© OpenStreetMap"
      ) %>%
      setView(lng = mean(df_24hrs()$lon), lat = mean(df_24hrs()$lat), zoom = 13)
  })
  
  observe({
    req(input$time) 
    
    filtered_data <- df_24hrs() %>%   
      filter(hour == input$time) %>%
      slice_sample(n = 300)  
    
    # Assign icons based on vehicle_type_id with smaller size
    vehicle_icons <- iconList(
      bike = makeIcon(iconUrl = "https://img.icons8.com/?size=100&id=257&format=png&color=6A83FF", iconWidth = 15, iconHeight = 15),  
      scooter = makeIcon(iconUrl = "https://img.icons8.com/?size=100&id=90151&format=png&color=FFD4EF", iconWidth = 15, iconHeight = 15)
    )
    
    filtered_data <- filtered_data %>%
      mutate(icon_type = case_when(
        vehicle_type_id == "Bike" ~ "bike",
        vehicle_type_id == "Scooter" ~ "scooter",
        TRUE ~ "bike"
      ))
    
    leafletProxy("bikeMap", data = filtered_data) %>%
      clearMarkers() %>%
      addMarkers(
        lng = ~lon, lat = ~lat,
        icon = ~vehicle_icons[icon_type],
        options = markerOptions(opacity = 0.7)
      )
    invalidateLater(5000)  # Check memory every 5000 milliseconds (5 seconds)
    cat("Current memory usage:", format(pryr::mem_used(), big.mark = ","), "\n")
  })
  
  # Save the Leaflet map as an HTML file
  output$downloadMap <- downloadHandler(
    filename = "bike_map.html",
    content = function(file) {
      map <- leaflet() %>%
        addTiles(
          urlTemplate = mapbox_style_url,
          options = tileOptions(tileSize = 512, zoomOffset = -1),
          attribution = "© OpenStreetMap"
        ) %>%
        setView(lng = mean(df_24hrs()$lon), lat = mean(df_24hrs()$lat), zoom = 13)
      
      saveWidget(map, file, selfcontained = TRUE)
    }
  )
}

shinyApp(ui, server)



print(paste0)
