library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidyr)



#1. Read and preproccessing Data

data <- read.csv("data/20240110-28-yearly-new-registration-of-vehicles-by-make-model-and-body-shape.csv", stringsAsFactors = FALSE)
data <- data %>% rename(
  model = Model_Name,
  year = Year,
  body_shape = Body_Shape_Name,
  registrations = Transactions
)


traffic_data <- read.csv("data/road_nsw_maps.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(wgs84_longitude) & !is.na(wgs84_latitude))%>%
  mutate(daily_total = replace_na(daily_total, 0), state = "NSW") %>%
  sample_n(min(10000, n()))

wa_traffic_data <- read.csv("data/Traffic_Digest.csv", stringsAsFactors = FALSE)
wa_traffic_data <- wa_traffic_data %>%
  mutate(
    MON_SUN = replace_na(MON_SUN, 0),
    MON_FRI = replace_na(MON_FRI, 0),
    SAT_SUN = replace_na(SAT_SUN, 0)
  ) %>%
  rename(
    wgs84_longitude = X,
    wgs84_latitude = Y,
    station_key = SITE_NO,
    road_name = ROAD_NAME,
    lga = LG_NAME
  ) %>%
  mutate(year = as.integer(substr(TRAFFIC_YEAR, 1, 4)), state = "WA") %>%
  mutate(daily_total = ceiling((MON_SUN + MON_FRI + SAT_SUN) / 7)) %>%
  select(station_key, wgs84_longitude, wgs84_latitude, road_name, year, daily_total,state) %>%
  filter(!is.na(wgs84_longitude) & !is.na(wgs84_latitude))%>%
  sample_n(min(10000, n()))


qld_traffic_data_2021 <- read.csv("data/qld-traffic-data-average-by-hour-by-day-2021_updated.csv", stringsAsFactors = FALSE) %>%
  mutate(year = 2021, state = "QLD")
qld_traffic_data_2022 <- read.csv("data/qld-traffic-data-average-by-hour-by-day-2022.csv", stringsAsFactors = FALSE) %>%
  mutate(year = 2022, state = "QLD")
qld_traffic_data_2023 <- read.csv("data/qld-traffic-data-average-by-hour-by-day-2023.csv", stringsAsFactors = FALSE) %>%
  mutate(year = 2023, state = "QLD")

qld_traffic_data <- bind_rows(qld_traffic_data_2021, qld_traffic_data_2022, qld_traffic_data_2023) %>%
  group_by(SITE_ID,year) %>%
  summarise(
    daily_total = ceiling((sum(WEEKDAY_AVERAGE) * 5 + sum(WEEKEND_AVERAGE) * 2)/7) %% 90000,
    wgs84_longitude = first(LONGITUDE),
    wgs84_latitude = first(LATITUDE),
    station_key = first(SITE_ID),
    road_name = first(ROAD_NAME),
    year = first(year),
    state = first(state)) %>%
  ungroup()


combined_traffic_data <- bind_rows(traffic_data, wa_traffic_data, qld_traffic_data)


#write.csv(combined_traffic_data, "data/combined_traffic_data_saved.csv", row.names = FALSE)

combined_traffic_datas <- read.csv("data/combined_traffic_data_saved.csv", stringsAsFactors = FALSE) %>%  mutate(daily_total=ceiling(daily_total%% 90000))


# Shiny UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      .well {\n        border-radius: 15px;\n        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);\n      }\n      .control-label {\n        font-size: 20px;\n      }\n    "))
  ),
  titlePanel("NSW& WA & QLD Traffic Volume Map"),
  fluidRow(
    column(3, wellPanel(
      checkboxGroupInput("state_filter", "Select States:",
                         choices = c("NSW", "QLD", "WA"),
                         selected = c("NSW", "QLD", "WA")),
      sliderInput("year_range", "Select Year Range:",
                  min = min(data$year, na.rm = TRUE),
                  max = max(data$year, na.rm = TRUE),
                  value = range(data$year, na.rm = TRUE), step = 1, 
                  animate = animationOptions(interval = 1000, loop = TRUE)),
      tabsetPanel(
        tabPanel("Line Chart",
                 plotlyOutput("registrationLineChart", height = "800px")),
        tabPanel("TrafficChart",
                 plotlyOutput("combinedTrafficChart", height = "800px")),
        tabPanel("Scatter Plot", 
                 plotlyOutput("scatterPlot", height = "800px")),
    )
    
    )),
    column(8, offset = 1,leafletOutput("worldMap", height = "1000px")),
    
    
  )
)



# Shiny server
server <- function(input, output, session) {
  
  filtered_mapdata <- reactive({
    combined_traffic_datas %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             state %in% input$state_filter) %>%
      sample_n(min(30000, n()))
      
  })
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 4)%>%
      addLegend(
        position = "topright",
        pal = colorBin("YlOrRd", traffic_data$daily_total, bins = 7),
        values = traffic_data$daily_total,
        title = "Traffic Volume",
        opacity = 0.6
      )
    
  })
  
  observe({
    
    currentZoom <- input$worldMap_zoom  
    filteredData <- filtered_mapdata()
    
    sizeFactor <- ifelse(currentZoom < 15, 0.04, currentZoom * 0.005)
    
    leafletProxy("worldMap", data = filtered_mapdata()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~wgs84_longitude, lat = ~wgs84_latitude,
        weight = 1,
        radius = ~sqrt(daily_total) * sizeFactor, 
        color = ~colorBin("YlOrRd", traffic_data$daily_total, bins = 9)(daily_total),
        stroke = TRUE, fillOpacity = 0.3,
        label = ~paste("Station Key:", station_key, "| Year:", year, "| Traffic Volume:", daily_total),
        popup = ~paste(
                       "<div style='font-family: Arial, Helvetica, sans-serif; background: rgba(255, 255, 255, 0.3); padding: 10px; border-radius: 8px;'> ","<br>",
                       "<strong>Station Key:</strong>", station_key, "<br>",
                       "<strong>Year:</strong>", year, "<br>",
                       "<strong>Traffic Volume:</strong>", daily_total,"<br>",
                       "<strong>Name:</strong>", name, "<br>",
                       "<strong>Road Name:</strong>", road_name, "<br>",
                       "<strong>Road Type:</strong>", road_name_type,"<br>",
                       "<strong>Road Functional Hierarchy:</strong>", road_functional_hierarchy, "<br>",
                       "<strong>Road On Type:</strong>", road_on_type, "<br>",
                       "<strong>Lane Count:</strong>", lane_count, "<br>",
                       "<strong>Road Classification Type:</strong>", road_classification_type, "<br>",
                       "<strong>Road Classification Admin:</strong>", road_classification_admin, "<br>",
                       "<strong>RMS Region:</strong>", rms_region, "<br>",
                       "<strong>LGA:</strong>", lga, "<br>",
                       "<strong>Suburb:</strong>", suburb, "<br>",
                       "<strong>Post Code:</strong>", post_code, "<br>",
                       "<strong>Device Type:</strong>", device_type)
        
      )
    
      
  })
  
  # Filter data based on input
  filtered_data <- reactive({
    data %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
   
  })
  
  
  
  # Dynamically generate vehicle registration trend line chart
  output$registrationLineChart <- renderPlotly({
    trend_data <- filtered_data() %>%
      group_by(year) %>%
      summarise(total_registrations = sum(registrations, na.rm = TRUE))
    
    p_trend <- ggplot(trend_data, aes(x = year, y = total_registrations)) +
      geom_line(color = "#2c7fb8") +
      geom_point(color = "#fcaca8") +
      theme_minimal() +
      labs(title = "Yearly Trend of New Vehicle Transactions",
           x = "Year",
           y = "Total Transactions") +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p_trend) %>% layout(legend = list(title = list(text = "Total Transactions")))
  })
  
  output$combinedTrafficChart <- renderPlotly({
    state_data <- combined_traffic_datas %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             state %in% input$state_filter) %>%
      group_by(state) %>%
      summarise(total_volume = sum(daily_total, na.rm = TRUE))
    p_bar <- ggplot(state_data, aes(x = state, y = total_volume, fill = state)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Traffic Volume by State",
           x = "state",
           y = "Traffic_volume",
           ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p_bar)
  })
  
  
  output$scatterPlot <- renderPlotly({
    scatter_data <- combined_traffic_datas %>%
      filter(year >= input$year_range[1], year <= input$year_range[2], state %in% input$state_filter)
    
    p_scatter <- ggplot(scatter_data, aes(x = year, y = daily_total, color = road_name, text = road_name)) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(title = "Traffic Volume by State", x = "yaer", y = "Traffic Volume") +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p_scatter, tooltip = "text")
  })
 
}

# Run Shiny app
shinyApp(ui, server)



