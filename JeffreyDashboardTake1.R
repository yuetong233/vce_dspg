# === LIBRARIES ===
library(shiny)

library(shinyWidgets)

library(bslib)

library(dplyr)

library(readr)

library(leaflet)

library(sf)

library(tigris)

library(tidycensus)

library(readxl)



options(tigris_use_cache = TRUE)



# UI

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "flatly"),
  
  
  
  tags$head(
    
    tags$style(HTML("

      h1, h2, h3, h4 {

        color: #2e7d32;

        font-weight: bold;

      }

    "))
    
  ),
  
  
  
  h1("Volunteer Data Dashboard"),
  
  
  
  tabsetPanel(
    
    tabPanel("Volunteer County Data",
             
             h3("County Data"),
             
             p("Monitor the county data for volunteers"),
             
             
             
             leafletOutput("volunteer_map", height = "600px")
             
    ),
    
    
    
    tabPanel("4-H Data",
             
             p("More tables to come!!"),
             tableOutput("fourh_totals"),
             leafletOutput("fourh_map", height = "600px")
             
    ),
    
    tabPanel("Demographic Data",
             
             h3("Program Participation Demographics"),
             
             p("View demographic distribution across counties"),
             
             leafletOutput("demographic_map", height = "600px")
             
    )
    
  )
  
)



# SERVER

server <- function(input, output, session) {
  # Set Census API Key
  census_api_key("6ee5ecd73ef70e9464ee5509dec0cdd4a3fa86c7", install = TRUE, overwrite = TRUE)
  
  # Clean volunteer data
  volunteer_data <- read.csv("countiesimpact.csv") %>%
    filter(Hours != "did not log hours") %>%
    mutate(
      County = tolower(County),
      County = gsub(" county", "", County),
      County = gsub(" city", "", County),
      County = trimws(County)
    ) %>%
    group_by(County) %>%
    summarise(Volunteers = n(), .groups = "drop")
  
  # Load VA counties
  va_counties <- counties(state = "VA", cb = TRUE, year = 2023) %>%
    st_transform(crs = 4326) %>%
    mutate(
      County = tolower(NAME),
      County = gsub(" county", "", County),
      County = trimws(County)
    )
  
  # Load population data
  va_population <- get_acs(
    geography = "county",
    state = "VA",
    variables = "B01003_001",
    year = 2023,
    survey = "acs5",
    output = "wide"
  ) %>%
    mutate(
      County = tolower(NAME),
      County = gsub(" county, virginia", "", County),
      County = gsub(" city, virginia", "", County),
      County = trimws(County),
      Population = B01003_001E
    ) %>%
    # Fix Fairfax and Franklin County population
    mutate(
      Population = ifelse(County == "fairfax", 1147532, Population),
      Population = ifelse(County == "franklin", 54477, Population)
    ) %>% select(County, Population)
  
  # Merging data
  combined_data <- left_join(volunteer_data, va_population, by = "County") %>%
    mutate(VolunteerRate = round((Volunteers / Population) * 100, 2))
  
  map_data <- left_join(va_counties, combined_data, by = "County") %>%
    st_as_sf()
  
  # Color scale
  max_rate <- max(map_data$VolunteerRate, na.rm = TRUE)
  if (!is.finite(max_rate)) max_rate <- 1
  max_rate <- ceiling(max_rate * 4) / 4
  breaks <- seq(0, max_rate, by = 0.25)
  
  pal <- colorBin("YlGnBu", domain = map_data$VolunteerRate, bins = breaks, na.color = "#f0f0f0")
  
  # Labels
  map_data <- map_data %>%
    mutate(label_content = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Volunteers: ", ifelse(is.na(Volunteers), "N/A", Volunteers), "<br>",
      "Population: ", ifelse(is.na(Population), "N/A", Population), "<br>",
      "Volunteer Rate: ", ifelse(is.na(VolunteerRate), "N/A", paste0(VolunteerRate, "%"))
    ))
  
  # Map
  output$volunteer_map <- renderLeaflet({
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(VolunteerRate),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = lapply(map_data$label_content, htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = map_data$VolunteerRate,
                title = "Volunteers as % of Population",
                opacity = 1
      ) %>%
      setView(lng = -78.6569, lat = 37.4316, zoom = 6)
  })
  
  # --- 4-H Data Map ---
  fourh_raw_data <- read_csv("annualprogreport.csv")
  
  # Calculate the Total column
  fourh_data <- fourh_raw_data %>%
    mutate(
      County = tolower(CountyArea),
      County = gsub(" county", "", County),
      County = gsub(" city", "", County),
      County = trimws(County),
      Total = rowSums(select(., c(
        "eHispanic", "eNotHispanic", "eNotProvided", "ePreferNotToState",
        "rWhite", "rBlack", "rIndianAlaskan", "rHawaiianIslander",
        "rAsian", "rMoreThanOne", "rUndetermined"
      )), na.rm = TRUE)
    )
  
  # Join 4-H data with VA counties shapefile
  fourh_map_data <- left_join(va_counties, fourh_data, by = "County") %>%
    st_as_sf()
  
  # Define color palette for 4-H map
  fourh_pal <- colorBin("Purples", domain = fourh_map_data$Total, bins = 5, na.color = "#f0f0f0")
  
  # Labels for 4-H map
  fourh_map_data <- fourh_map_data %>%
    mutate(fourh_label_content = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Total Participants: ", ifelse(is.na(Total), "N/A", Total)
    ))
  
  # Render 4-H totals table
  
  
  # Render 4-H Leaflet map
  output$fourh_map <- renderLeaflet({
    leaflet(fourh_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~fourh_pal(Total),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = lapply(fourh_map_data$fourh_label_content, htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright",
                pal = fourh_pal,
                values = fourh_map_data$Total,
                title = "4-H Participants",
                opacity = 1
      ) %>%
      setView(lng = -78.6569, lat = 37.4316, zoom = 6)
  })
  
  # --- Demographic Data Map ---
  # Read and process demographic data
  demographic_data <- read_csv("countiesdemographics.csv") %>%
    group_by(site_county) %>%
    summarise(
      total_participants = sum(participants_total, na.rm = TRUE)
    ) %>%
    mutate(
      County = tolower(site_county),
      County = gsub(" county", "", County),
      County = gsub(" city", "", County),
      County = trimws(County)
    )
  
  # Read population data
  population_data <- read_csv("virginia2024population.csv", skip = 2, col_names = c("State_County", "Population")) %>%
    filter(!is.na(Population)) %>%
    mutate(
      County = tolower(State_County),
      County = gsub(" county, virginia", "", County),
      County = gsub(" city, virginia", "", County),
      County = gsub("^.", "", County),  # Remove leading dot
      County = trimws(County),
      Population = as.numeric(Population)
    ) %>%
    select(County, Population)
  
  # Join demographic data with population data
  demographic_data <- left_join(demographic_data, population_data, by = "County") %>%
    mutate(
      participation_rate = (total_participants / Population) * 100
    )
  
  # Join with VA counties shapefile
  demographic_map_data <- left_join(va_counties, demographic_data, by = "County") %>%
    st_as_sf()
  
  # Define color palette for demographic map
  demographic_pal <- colorBin("YlOrRd", domain = demographic_map_data$participation_rate, bins = 5, na.color = "#f0f0f0")
  
  # Labels for demographic map
  demographic_map_data <- demographic_map_data %>%
    mutate(demographic_label_content = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Total Participants: ", ifelse(is.na(total_participants), "N/A", total_participants), "<br>",
      "County Population: ", ifelse(is.na(Population), "N/A", format(Population, big.mark=",")), "<br>",
      "Participation Rate: ", ifelse(is.na(participation_rate), "N/A", paste0(round(participation_rate, 2), "%"))
    ))
  
  # Render demographic Leaflet map
  output$demographic_map <- renderLeaflet({
    leaflet(demographic_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~demographic_pal(participation_rate),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = lapply(demographic_map_data$demographic_label_content, htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright",
                pal = demographic_pal,
                values = demographic_map_data$participation_rate,
                title = "Participation Rate (%)",
                opacity = 1
      ) %>%
      setView(lng = -78.6569, lat = 37.4316, zoom = 6)
  })
}



# Running the Website

shinyApp(ui = ui, server = server)