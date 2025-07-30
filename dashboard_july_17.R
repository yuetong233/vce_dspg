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
library(plotly)
library(htmltools)
library(stringr)

# === HELPER FUNCTION ===
clean_county <- function(x) {
  x %>%
    tolower() %>%
    gsub(" county, virginia| city, virginia", "", .) %>%
    gsub(" county| city", "", .) %>%
    trimws()
}
# === UI ===
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  ## CSS styling for headers and outlined button
  tags$head(
    tags$style(HTML("
     h1, h2, h3, h4 { color:#2e7d32; font-weight:bold; }
     .my-outline-button {
       background-color: transparent;
       color: #00AE42;
       border: 2px solid #00AE42;
       border-radius: 6px;
       padding: 8px 18px;
       font-weight: bold;
       font-size: 14px;
     }
     .my-outline-button:hover {
       background-color: #00AE42;
       color: white;
       cursor: pointer;
     }
   "))
  ),
  div(
    style = "display: flex; align-items: center; justify-content: space-between; padding: 10px;",
    # === TITLE ===
    h1("Exploring Engagement in Virginia Cooperative Extension",
       style = "margin: 0;"),
    # === LOGO ===
    conditionalPanel(
      condition = "['4-H Participation County Data', '4-H Participation Trends', '4-H Volunteer County Data', '4-H Volunteers vs Participants'].includes(input.tabs)",
      tags$img(src = "Virginia 4H Logo.png", height = "80px")
    )
  ),
  
  tabsetPanel(id = "tabs",
              tabPanel("Home",
                       h2("Welcome"),
                       p("This dashboard visualizes volunteer engagement and 4-H participation ",
                         "across Virginia counties. Use the tabs above to explore the data."),
                       hr(),
                       h3("Purpose / Background"),
                       p("The goal of this project is to identify potential gaps between the demographics of communities ",
                         "and the populations served by existing programming, as well as potential demographic gaps ",
                         "between volunteers and their communities at large. This project will be driven by American ",
                         "Community Survey data and Virginia Cooperative Extension programming needs."),
                       h3("Methodology"),
                       tags$ul(
                         tags$li(strong("Data Sources:"),
                                 tags$ul(
                                   tags$li("Better Impact volunteer records"),
                                   tags$li("PEARS 4-H Annual Program Reports"),
                                   tags$li("American Community Survey (ACS) population data")
                                 )),
                         tags$li("County names standardized for consistency."),
                         tags$li("Volunteer and participant counts aggregated by county."),
                         tags$li("Rates calculated against county population or program totals."),
                         tags$li(strong("Purpose:"),
                                 "By identifying the areas of Virginia that need support and comparing them to where ",
                                 "Virginia Cooperative Extension’s resources are currently being allocated, this analysis ",
                                 "aims to give VCE stakeholders a clearer picture of community needs and how best to direct ",
                                 "future outreach and support.")
                       ),
                       h3("How to Use This Dashboard"),
                       tags$ol(
                         tags$li(strong("4-H Participation County Data:"), " Explore program reach by viewing the number of 4-H participants by county for the 2023-2024 program year, including breakdowns by race and ethnicity."),
                         tags$li(strong("4-H Participation Trends:"), " View trends in 4-H participation from 2021 to 2024 to observe changes in program reach over time."),
                         tags$li(strong("VCE Volunteer County Data:"), " View the distribution of volunteers in VCE programs for the 2025 program year across different counties, including breakdowns by race and ethnicity, and compare these against the total county populations."),
                         tags$li(strong("VCE Participation Data:"), " View the total number of participants in VCE programs by county for the 2025 program year. "),
                         tags$li(strong("VCE Volunteers vs Participants"), " View 2025 volunteer and participant data across all counties, including the number of volunteers as a percentage of participants. ")
                       ),
                       
                       h3("Insights to Explore"),
                       tags$ul(
                         tags$li("Counties with high volunteer engagement per capita"),
                         tags$li("Areas where participant numbers exceed volunteer capacity"),
                         tags$li("Demographic underrepresentation in 4-H programs")
                       ),
                       h3("Acknowledgments"),
                       p("Developed by Jeffrey Ogle and Diego Cuadra, with support from the Department of Agricultural and Applied Economics, Virginia Tech. ")
              ),
              tabPanel("4-H Participation County Data",
                       h3("Participation Data"),
                       selectInput("selected_participation_group", "Select Group:",
                                   choices = c(
                                     "Hispanic" = "eHispanic",
                                     "Not Hispanic" = "eNotHispanic",
                                     "Ethnicity Not Provided" = "eNotProvided",
                                     "Prefer Not to State Ethnicity" = "ePreferNotToState",
                                     "White" = "rWhite",
                                     "Black or African American" = "rBlack",
                                     "American Indian or Alaskan Native" = "rIndianAlaskan",
                                     "Native Hawaiian or Pacific Islander" = "rHawaiianIslander",
                                     "Asian" = "rAsian",
                                     "Two or More Races" = "rMoreThanOne",
                                     "Race Undetermined" = "rUndetermined"
                                   )),
                       leafletOutput("fourh_map", height = "600px")
              ),
              tabPanel("4-H Volunteer County Data",
                       h3("4-H Volunteer Demographics by County or City"),
                       selectInput("group_filter", "Select Demographic Group:",
                                   choices = c("Hispanic", "Non-Hispanic",
                                               "White", "Black", "Asian",
                                               "Undetermined")),
                       leafletOutput("volunteer_choropleth", height = "600px")
              ),
              
              tabPanel("4-H Volunteers vs Participants",
                       h3("Volunteers as % of Participants by County"),
                       leafletOutput("vol_vs_particip_map", height = "600px")
              ),
              
              tabPanel("4-H Participation Trends",
                       h3("Participation as a Percentage of Population"),
                       plotlyOutput("Rplot01",
                                    width = "100%",
                                    height = "auto"),
                       p("Please reference the map below for the geographic regions
               referenced in the above graph!"),
                       
                       # Add the image here
                       tags$img(src = "VCE_regions_map.png", width = "100%", alt = "VCE Regions Map")
              ),
              tabPanel("VCEE Volunteer County Data",
                       h3("Volunteer Data"),
                       leafletOutput("vce_volunteer_map", height = "600px")
              ), 
              tabPanel("VCE Participation Data",
                       h3("Program Participation Demographics"),
                       leafletOutput("demographic_map", height = "600px")
              ),
              tabPanel("VCE Volunteers vs Participation",
                       h3("Volunteers as % of Participants"),
                       leafletOutput("vce_vol_particip_map", height = "600px")
              )
  )
)
# === SERVER ===
server <- function(input, output, session) {
  census_api_key("6ee5ecd73ef70e9464ee5509dec0cdd4a3fa86c7", install = TRUE, overwrite = TRUE)
  
  va_counties <- counties("VA", cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(County = clean_county(NAME))
  
  # === UPDATED: READ PEARS_BI DATA FROM NEW FILE ===
  pears_bi_df <- read_excel("PEARS_BI_with_NewVolunteers.xlsx") %>%
    mutate(
      County = tolower(County),
      County = gsub(" county", "", County),
      County = gsub(" city", " (city)", County),
      County = trimws(County),
      Volunteers = NewVolunteers,  # <== assign NewVolunteers to Volunteers
      volunteers_pct_participants = ifelse(Participants > 0,
                                           round(Volunteers / Participants * 100, 2),
                                           NA)
    )
  
  # === MERGE WITH VA COUNTIES ===
  vce_vol_particip_map_data <- left_join(va_counties, pears_bi_df, by = "County") %>%
    st_as_sf() %>%
    mutate(label_vce = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Volunteers: ", Volunteers, "<br>",
      "Participants: ", Participants, "<br>",
      "Volunteers as % of Participants: ", volunteers_pct_participants, "%"
    ))
  
  # === RENDER MAP ===
  output$vce_vol_particip_map <- renderLeaflet({
    pal <- colorBin("YlOrRd", domain = vce_vol_particip_map_data$volunteers_pct_participants,
                    bins = 5, na.color = "#d9d9d9")
    
    leaflet(vce_vol_particip_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(volunteers_pct_participants),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(vce_vol_particip_map_data$label_vce, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal,
                values = vce_vol_particip_map_data$volunteers_pct_participants,
                title = "% Volunteers of Participants",
                opacity = 1)
  })
  
  output$volunteer_map <- renderLeaflet({
    data <- volunteer_map_data_reactive()
    if (nrow(data) == 0 || all(is.na(data$VolunteerRate))) {
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
        addPopups(lng = -78.6569, lat = 37.4316, popup = "No data available.")
    } else {
      pal <- colorBin("YlGnBu", domain = data$VolunteerRate,
                      bins = seq(0, ceiling(max(data$VolunteerRate, na.rm=TRUE)*4)/4, 0.25),
                      na.color = "#f0f0f0")
      leaflet(data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = ~pal(VolunteerRate), color = "black", weight = 1,
                    fillOpacity = 0.7, label = lapply(data$label_vol, htmltools::HTML),
                    highlightOptions = highlightOptions(weight=2,color="#666",
                                                        fillOpacity=0.9,bringToFront=TRUE)) %>%
        addLegend("bottomright", pal=pal, values=data$VolunteerRate,
                  title="Volunteers as % of Pop.", opacity=1)
    }
  })
  library(plotly)
  # Inside your server function
  output$Rplot01 <- renderPlotly({
    # Re-create the plot here
    particip_data <- particip_combined %>%
      group_by(Region, Year) %>%
      summarize(
        total_participation = sum(`County Totals`),
        total_population = sum(Population),
        .groups = 'drop'
      ) %>%
      mutate(
        participation_pct = total_participation / total_population * 100,
        Year = as.factor(Year),
        hover_text = paste0(
          "Year: ", Year, "<br>",
          "Region: ", Region, "<br>",
          "Population: ", total_population, "<br>",
          "Participation: ", total_participation, "<br>",
          "Participation %: ", round(participation_pct, 2), "%"
        )
      )
    region_colors <- c(
      "Northeast" = "#FFA07A",
      "Southeast" = "lightblue",
      "Central" = "darkblue",
      "Northwest" = "orange",
      "Southwest" = "maroon"
    )
    plot_ly(
      data = particip_data,
      x = ~Year,
      y = ~total_participation,
      color = ~Region,
      colors = region_colors,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~hover_text,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "4-H Participation trend 2021-2024",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Participation")
      )
  })
  # === PARTICIPATION MAP ===
  particip_combined <- read_xlsx("particip_combined.xlsx")
  fourh_data <- reactive({
    read_csv("annualprogreport.csv") %>%
      mutate(
        County = CountyArea %>%
          tolower() %>%
          trimws(),
        # Specific case corrections if needed
        County = case_when(
          County == "fairfax" & grepl("city", CountyArea, ignore.case = TRUE) ~ "fairfax city",
          TRUE ~ County
        ),
        Total = rowSums(select(., starts_with("e"), starts_with("r")), na.rm = TRUE)
      )
  })
  output$fourh_map <- renderLeaflet({
    req(input$selected_participation_group)
    df <- left_join(va_counties, fourh_data(), by = "County") %>% st_as_sf()
    selected_values <- df[[input$selected_participation_group]]
    selected_values[is.na(selected_values)] <- 0
    total_values <- df$Total
    total_values[is.na(total_values)] <- 0
    pal <- colorBin("Purples", domain = selected_values, bins = 5, na.color = "#f0f0f0")
    df <- df %>%
      mutate(label_4h = paste0(
        "<strong>", toupper(County), "</strong><br>",
        "Selected Group: ", selected_values, "<br>",
        "Total Participants: ", total_values
      ))
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(selected_values),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(df$label_4h, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = selected_values,
                title = "4-H Participants", opacity = 1)
  })
  # === VOLUNTEERS VS PARTICIPATION ===
  # === Load New Volunteers Data ===
  new_vol_df <- read_excel("PEARS_BI_with_NewVolunteers.xlsx") %>%
    transmute(
      County = clean_county(County),
      Volunteers = NewVolunteers
    )
  
  # === Updated Ratio DataFrame Reactive ===
  ratio_df <- reactive({
    full_join(
      new_vol_df,
      fourh_data() %>% select(County, Participants = Total),
      by = "County"
    ) %>%
      mutate(RatioVP = ifelse(Participants > 0,
                              round(Volunteers / Participants * 100, 2), NA))
  })
  
  
  ####VCEE start of Map
  # === NEW: VCE Volunteer Data Map ===
  
  # Get VA population as you already do
  va_population <- get_acs("county", state = "VA", variables = "B01003_001", year = 2023, survey = "acs5", output = "wide") %>%
    transmute(County = clean_county(NAME), Population = B01003_001E) %>%
    mutate(Population = ifelse(County == "fairfax", 1147532, Population),
           Population = ifelse(County == "franklin",  54477, Population))
  
  # UPDATED: Read from the new file and use NewVolunteers
  vce_volunteer_df <- read_excel("PEARS_BI_with_NewVolunteers.xlsx") %>%
    transmute(
      County = clean_county(County),
      Volunteers = NewVolunteers  # Using the updated column
    )
  
  # Merge volunteers data with population and counties shapefile
  vce_volunteer_map_data <- left_join(vce_volunteer_df, va_population, by = "County") %>%
    mutate(VolunteerRate = round(Volunteers / Population * 100, 2)) %>%
    left_join(va_counties, ., by = "County") %>%
    st_as_sf() %>%
    mutate(label_vce_vol = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Volunteers: ", Volunteers, "<br>",
      "Population: ", Population, "<br>",
      "Volunteer Rate: ", VolunteerRate, "%"
    ))
  
  # Render the leaflet map
  output$vce_volunteer_map <- renderLeaflet({
    data <- vce_volunteer_map_data
    pal <- colorBin("YlOrRd", domain = data$VolunteerRate,
                    bins = 5, na.color = "#f0f0f0")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(VolunteerRate),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(data$label_vce_vol, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = data$VolunteerRate,
                title = "Volunteers as % of Pop.", opacity = 1)
  })
  
  ##Start of 4H Map
  # === 4-H Volunteer Demographic Map (By Race or Ethnicity) ===
  
  # Read and process the Excel data
  volunteer_demo_df <- read_excel("4H Volunteers.xlsx") %>%
    mutate(CountyOrCity = str_to_title(trimws(CountyOrCity)))
  
  # Updated: safer parsing function
  expand_demo_data <- function(df, column, pattern_list) {
    for (pattern in pattern_list) {
      colname <- gsub(" ", "_", pattern)
      regex <- paste0(pattern, ":\\s*(\\d+)")
      df[[colname]] <- as.numeric(str_match(df[[column]], regex)[, 2])
      df[[colname]][is.na(df[[colname]])] <- 0
    }
    return(df)
  }
  
  # Ethnicity and Race groups
  ethnicity_list <- c("Hispanic", "Non-Hispanic")
  race_list <- c("White", "Black", "Asian", "Undetermined")
  
  # Parse both fields
  volunteer_demo_df <- expand_demo_data(volunteer_demo_df, "Ethnicity", ethnicity_list)
  volunteer_demo_df <- expand_demo_data(volunteer_demo_df, "Race", race_list)
  
  # Standardize geography names
  volunteer_demo_df <- volunteer_demo_df %>%
    mutate(CountyOrCity = str_to_title(trimws(CountyOrCity)))
  
  # Load shapefile for counties/cities
  va_counties_map <- counties(state = "VA", cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(CountyOrCity = str_to_title(gsub(" County, Virginia| City, Virginia", "", NAME)))
  
  # Join shapefile and data
  volunteer_demo_map_data <- left_join(va_counties_map, volunteer_demo_df, by = "CountyOrCity") %>%
    st_as_sf()
  
  # Update dropdown options
  observe({
    updateSelectInput(session, "group_filter", choices = c(ethnicity_list, race_list))
  })
  
  # Reactive selection based on dropdown
  filtered_demo_data <- reactive({
    req(input$group_filter)
    column <- gsub(" ", "_", input$group_filter)
    map_df <- volunteer_demo_map_data
    map_df$selected_group <- map_df[[column]]
    map_df$selected_group[is.na(map_df$selected_group)] <- 0
    map_df <- map_df %>%
      mutate(label_filtered = paste0(
        "<strong>", toupper(CountyOrCity), "</strong><br>",
        input$group_filter, ": ", selected_group, "<br>",
        "Total Volunteers: ", `4-H Volunteers`
      ))
    return(map_df)
  })
  
  # Render map
  output$volunteer_choropleth <- renderLeaflet({
    data <- filtered_demo_data()
    pal <- colorBin("YlGnBu", domain = data$selected_group, bins = 5, na.color = "#f0f0f0")
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(selected_group),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(data$label_filtered, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = data$selected_group,
                title = input$group_filter, opacity = 1)
  })
  
  ###4-H Volunteer vs Participation tab
  # === Volunteers vs Participants Data ===
  vol_particip_data <- reactive({
    volunteers_df <- volunteer_demo_df %>%
      transmute(
        County = tolower(CountyOrCity),
        Volunteers = `4-H Volunteers`
      )
    
    participants_df <- fourh_data() %>%
      transmute(
        County = tolower(County),
        Participants = Total
      )
    
    combined <- left_join(volunteers_df, participants_df, by = "County") %>%
      mutate(
        RatioVP = ifelse(Participants > 0,
                         round(Volunteers / Participants * 100, 2), NA)
      )
    
    left_join(va_counties, combined, by = "County") %>%
      st_as_sf() %>%
      mutate(label_vp = paste0(
        "<strong>", toupper(County), "</strong><br>",
        "Volunteers: ", Volunteers, "<br>",
        "Participants: ", Participants, "<br>",
        "Volunteers as % of Participants: ", RatioVP, "%"
      ))
  })
  
  # === Map Output ===
  output$vol_vs_particip_map <- renderLeaflet({
    data <- vol_particip_data()
    pal <- colorBin("YlOrRd", domain = data$RatioVP,
                    bins = 5, na.color = "#f0f0f0")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(RatioVP),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(data$label_vp, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal,
                values = data$RatioVP,
                title = "% Volunteers of Participants",
                opacity = 1)
  })
  
  
  
  
  # === DEMOGRAPHIC MAP ===
  demographic_data <- read_csv("countiesdemographics.csv") %>%
    group_by(site_county) %>%
    summarise(total_participants = sum(participants_total, na.rm = TRUE)) %>%
    mutate(
      County = tolower(site_county),
      County = gsub(" county", "", County),
      County = gsub(" city", "", County),
      County = trimws(County)
    )
  population_data <- read_csv("virginia2024population.csv",
                              skip = 2,
                              col_names = c("State_County", "Population")) %>%
    filter(!is.na(Population)) %>%
    mutate(
      County = tolower(State_County),
      County = gsub(" county, virginia", "", County),
      County = gsub(" city, virginia", "", County),
      County = gsub("^\\.", "", County),
      County = trimws(County),
      Population = as.numeric(Population),
      Population = ifelse(County == "fairfax", 1147532, Population),
      Population = ifelse(County == "franklin", 54477, Population)
    ) %>%
    select(County, Population)
  demographic_data <- left_join(demographic_data, population_data, "County") %>%
    mutate(participation_rate = (total_participants / Population) * 100)
  demographic_map_data <- left_join(va_counties, demographic_data, "County") %>% st_as_sf()
  pal_demo <- colorBin("YlOrRd", domain = demographic_map_data$participation_rate,
                       bins = 5, na.color = "#f0f0f0")
  demographic_map_data <- demographic_map_data %>%
    mutate(label_demo = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Total Participants: ", total_participants, "<br>",
      "County Population: ", format(Population, big.mark = ","), "<br>",
      "Participation Rate: ", round(participation_rate, 2), "%"
    ))
  output$demographic_map <- renderLeaflet({
    leaflet(demographic_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_demo(participation_rate),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(demographic_map_data$label_demo, htmltools::HTML),
        highlightOptions = highlightOptions(weight=2,color="#666",
                                            fillOpacity=0.9,bringToFront=TRUE)
      ) %>%
      addLegend("bottomright", pal=pal_demo,
                values=demographic_map_data$participation_rate,
                title="Participation Rate (%)", opacity=1)
  })
}
# === RUN APP ===
shinyApp(ui, server)






