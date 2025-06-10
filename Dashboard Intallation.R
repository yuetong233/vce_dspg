library(shiny)

library(shinyWidgets)

library(bslib)

library(dplyr)

library(readr)

library(leaflet)

library(sf)

library(tigris)

options(tigris_use_cache = TRUE)

# === UI ===

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
             
             leafletOutput("volunteer_map", height = "600px"),
             
             verbatimTextOutput("debug"),
             
             textOutput("error")
             
    ),
    
    tabPanel("Another Tab",
             
             p("More content here.")
             
    )
    
  )
  
)

# === SERVER ===

server <- function(input, output, session) {
  
  # Load volunteer data with error handling
  
  tryCatch({
    
    volunteer_data <- read.csv("countiesimpact.csv")
    
    # Clean and count volunteers by county
    
    volunteer_count <- volunteer_data %>%
      
      mutate(
        
        County = tolower(County),
        
        County = gsub(" county", "", County),
        
        County = gsub(" city", "", County),
        
        County = trimws(County)
        
      ) %>%
      
      group_by(County) %>%
      
      summarise(Volunteers = n(), .groups = "drop")
    
    # Load VA counties shapefile
    
    va_counties <- counties(state = "VA", cb = TRUE, year = 2023) %>%
      
      st_transform(crs = 4326) %>%
      
      mutate(
        
        County = tolower(NAME),
        
        County = gsub(" county", "", County),
        
        County = trimws(County)
        
      )
    
    # Join data
    
    map_data <- left_join(va_counties, volunteer_count, by = "County") %>%
      
      st_as_sf()
    
    # Define color palette
    
    pal <- colorBin("Blues", domain = map_data$Volunteers, bins = 5, na.color = "#f0f0f0")
    
    # Render Leaflet map
    
    output$volunteer_map <- renderLeaflet({
      
      leaflet(map_data) %>%
        
        addProviderTiles("CartoDB.Positron") %>%
        
        addPolygons(
          
          fillColor = ~pal(Volunteers),
          
          color = "black",
          
          weight = 1,
          
          fillOpacity = 0.7,
          
          label = ~paste0(
            
            "<strong>", toupper(County), "</strong><br>",
            
            "Volunteers: ", ifelse(is.na(Volunteers), "N/A", Volunteers)
            
          ) %>% lapply(htmltools::HTML),
          
          highlightOptions = highlightOptions(
            
            weight = 2,
            
            color = "#666",
            
            fillOpacity = 0.9,
            
            bringToFront = TRUE
            
          )
          
        ) %>%
        
        addLegend("bottomright",
                  
                  pal = pal,
                  
                  values = map_data$Volunteers,
                  
                  title = "Volunteer Count",
                  
                  opacity = 1
                  
        ) %>%
        
        setView(lng = -78.6569, lat = 37.4316, zoom = 6)
      
    })
    
    # Add debugging output
    
    output$debug <- renderPrint({
      
      list(
        
        n_counties = nrow(map_data),
        
        counties_with_data = sum(!is.na(map_data$Volunteers)),
        
        total_volunteers = sum(map_data$Volunteers, na.rm = TRUE)
        
      )
      
    })
    
  }, error = function(e) {
    
    # Handle any errors
    
    output$error <- renderText({
      
      paste("Error loading data:", e$message)
      
    })
    
  })
  
}

# === RUN ===

shinyApp(ui = ui, server = server)


shinyApp(ui = ui, server = server)
