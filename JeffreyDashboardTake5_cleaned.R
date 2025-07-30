# LIBRARIES
library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(tigris)
library(tidycensus)
# Read your R script into R as lines
lines <- readLines("JeffreyDashboardTake5.R")
# Remove empty lines (lines that are completely blank or contain only whitespace)
clean_lines <- lines[grepl("\\S", lines)]
# Write the cleaned lines back to a new file
writeLines(clean_lines, "JeffreyDashboardTake5_cleaned.R")
options(tigris_use_cache = TRUE)
# HELPER
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
  tags$head(tags$style(HTML("
    h1, h2, h3, h4 { color:#2e7d32; font-weight:bold; }
  "))),
  h1("Exploring Engagement in Virginia 4-H Programs"),
  tabsetPanel(
    # Home Tab
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
               tags$li(strong("Volunteer County Data:"), " View distribution of volunteers and comparison against the total population in different counties."),
               tags$li(strong("Participation County Data:"), " Explore program reach by looking at the number of participants by county."),
               tags$li(strong("Volunteers vs Participation:"), " Comparison of volunteers vs participants in each county"),
               tags$li(strong("Demographic Data:"), " Comparison of participants in VCE programs and population across counties. ")
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
    # Tabs
    tabPanel("Volunteer County Data",
             h3("Volunteer Data"),
             p("Monitor the county data for volunteers"),
             leafletOutput("volunteer_map", height = "600px")
    ),
    tabPanel("Participation County Data",
             h3("Participation Data"),
             p("Monitor the county data for participants"),
             leafletOutput("fourh_map", height = "600px")
    ),
    tabPanel("Volunteers vs Participation",
             h3("Volunteers as % of Participants"),
             p("Volunteers divided by participants across Virginia counties"),
             leafletOutput("vol_particip_map", height = "600px")
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
  census_api_key("6ee5ecd73ef70e9464ee5509dec0cdd4a3fa86c7",
                 install = TRUE, overwrite = TRUE)
  # Volunteers
  volunteer_data <- read_csv("countiesimpact.csv") %>%
    filter(Hours != "did not log hours") %>%
    mutate(County = clean_county(County)) %>%
    group_by(County) %>% summarise(Volunteers = n(), .groups = "drop")
  va_counties <- counties("VA", cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(County = clean_county(NAME))
  va_population <- get_acs("county", state = "VA",
                           variables = "B01003_001", year = 2023,
                           survey = "acs5", output = "wide") %>%
    transmute(
      County     = clean_county(NAME),
      Population = B01003_001E
    ) %>%
    mutate(Population = ifelse(County == "fairfax", 1147532, Population),
           Population = ifelse(County == "franklin",  54477,  Population))
  # Volunteer map
  volunteer_map_data <- left_join(volunteer_data, va_population, "County") %>%
    mutate(VolunteerRate = round(Volunteers / Population * 100, 2)) %>%
    left_join(va_counties, ., "County") %>% st_as_sf()
  pal_vol <- colorBin(
    "YlGnBu",
    domain   = volunteer_map_data$VolunteerRate,
    bins     = seq(0, ceiling(max(volunteer_map_data$VolunteerRate, na.rm=TRUE)*4)/4, 0.25),
    na.color = "#f0f0f0"
  )
  volunteer_map_data <- volunteer_map_data %>%
    mutate(label_vol = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Volunteers: ", Volunteers, "<br>",
      "Population: ", Population, "<br>",
      "Volunteer Rate: ", VolunteerRate, "%"
    ))
  output$volunteer_map <- renderLeaflet({
    leaflet(volunteer_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_vol(VolunteerRate),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(volunteer_map_data$label_vol, htmltools::HTML),
        highlightOptions = highlightOptions(weight=2,color="#666",
                                            fillOpacity=0.9,bringToFront=TRUE)
      ) %>%
      addLegend("bottomright", pal=pal_vol,
                values=volunteer_map_data$VolunteerRate,
                title="Volunteers as % of Pop.", opacity=1)
  })
  # 4-H Participation
  fourh_raw <- read_csv("annualprogreport.csv")
  fourh_data <- fourh_raw %>%
    mutate(
      County = clean_county(CountyArea),
      Total  = rowSums(select(., starts_with("e") | starts_with("r")), na.rm = TRUE)
    )
  fourh_map_data <- left_join(va_counties, fourh_data, "County") %>% st_as_sf()
  pal_4h <- colorBin("Purples", domain = fourh_map_data$Total, bins = 5, na.color="#f0f0f0")
  fourh_map_data <- fourh_map_data %>%
    mutate(label_4h = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Total Participants: ", Total
    ))
  output$fourh_map <- renderLeaflet({
    leaflet(fourh_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_4h(Total),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(fourh_map_data$label_4h, htmltools::HTML),
        highlightOptions = highlightOptions(weight=2,color="#666",
                                            fillOpacity=0.9,bringToFront=TRUE)
      ) %>%
      addLegend("bottomright", pal=pal_4h, values=fourh_map_data$Total,
                title="4-H Participants", opacity=1)
  })
  # Volunteers vs Participants
  ratio_df <- full_join(
    volunteer_data,
    fourh_data %>% select(County, Participants = Total),
    by = "County"
  ) %>%
    mutate(RatioVP = ifelse(Participants > 0,
                            round(Volunteers / Participants * 100, 2), NA))
  ratio_map_data <- left_join(va_counties, ratio_df, "County") %>% st_as_sf()
  # Color Scale: change every 100%
  max_ratio  <- max(ratio_map_data$RatioVP, na.rm = TRUE)
  breaks_100 <- seq(0, ceiling(max_ratio / 100) * 100, by = 100)
  pal_ratio <- colorBin(
    "RdYlBu",
    domain   = ratio_map_data$RatioVP,
    bins     = breaks_100,
    na.color = "#f0f0f0"
  )
  # -------------------------------------------------------
  ratio_map_data <- ratio_map_data %>%
    mutate(label_ratio = paste0(
      "<strong>", toupper(County), "</strong><br>",
      "Volunteers: ", Volunteers, "<br>",
      "Participants: ", Participants, "<br>",
      "Volunteers / Participants: ", RatioVP, "%"
    ))
  output$vol_particip_map <- renderLeaflet({
    leaflet(ratio_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_ratio(RatioVP),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(ratio_map_data$label_ratio, htmltools::HTML),
        highlightOptions = highlightOptions(weight=2,color="#666",
                                            fillOpacity=0.9,bringToFront=TRUE)
      ) %>%
      addLegend("bottomright", pal=pal_ratio,
                values=ratio_map_data$RatioVP,
                title="% Volunteers of Participants",
                opacity=1)
  })
  # Demographic Map
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
