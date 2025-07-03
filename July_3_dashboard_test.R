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

getwd()
setwd("C:\\Users\\jeffr\\Desktop\\diego_and_jeffrey_initial_vce_data")
options(tigris_use_cache = TRUE)



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
    
    tags$img(src = "Virginia 4H Logo.png", height = "80px")
    
  ),
  
  
  
  tabsetPanel(
    
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
               
               tags$li(strong("Volunteer County Data:"), " View distribution of 4-H volunteers and comparison against the total population in different counties."),
               
               tags$li(strong("Participation County Data:"), " Explore program reach by looking at the number of 4-H participants by county."),
               
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
    
    
    
    tabPanel("Volunteer County Data",
             
             h3("Volunteer Data"),
             
             selectInput("selected_race", "Select Race or Ethnicity:",
                         
                         choices = c("All" = "All",
                                     
                                     "White" = "05. White",
                                     
                                     "Black or African American" = "03. Black or African American",
                                     
                                     "Asian" = "02. Asian",
                                     
                                     "Two or more races" = "06. Two or more races",
                                     
                                     "Hispanic" = "Hispanic or Latino/a/x",
                                     
                                     "Not Hispanic" = "Not Hispanic or Latino/a/x",
                                     
                                     "Prefer not to answer" = "08. Prefer not to answer",
                                     
                                     "No Response" = "No Response"),
                         
                         selected = "All"),
             
             leafletOutput("volunteer_map", height = "600px")
             
    ),
    
    
    
    tabPanel("Participation County Data",
             
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
    
    
    
    tabPanel("Volunteers vs Participation",
             
             h3("Volunteers as % of Participants"),
             
             leafletOutput("vol_particip_map", height = "600px")
             
    ),
    
    tabPanel("4-H Participation Trends",
             h3("Participation as a Percentage of Population"),
             plotlyOutput("UpdatedParticipationGraph",
                          width = "100%",
                          height = "auto"),
             p("Note: Data for the year 2024-2025 is not complete"),
             
),
    
    tabPanel("Demographic Data",
             
             h3("Program Participation Demographics"),
             
             leafletOutput("demographic_map", height = "600px")
             
    )
    
  )
  
)





# === SERVER ===

server <- function(input, output, session) {
  
  
  
  census_api_key("6ee5ecd73ef70e9464ee5509dec0cdd4a3fa86c7", install = TRUE, overwrite = TRUE)
  
  
  
  va_counties <- counties("VA", cb = TRUE, year = 2023) %>%
    
    st_transform(4326) %>%
    
    mutate(County = clean_county(NAME))
  
  
  
  va_population <- get_acs("county", state = "VA", variables = "B01003_001", year = 2023, survey = "acs5", output = "wide") %>%
    
    transmute(County = clean_county(NAME), Population = B01003_001E) %>%
    
    mutate(Population = ifelse(County == "fairfax", 1147532, Population),
           
           Population = ifelse(County == "franklin",  54477, Population))
  
  
  
  full_volunteer_data <- read_csv("countiesimpact.csv") %>%
    
    filter(Hours != "did not log hours") %>%
    
    mutate(
      
      County = clean_county(County),
      
      Race = `CF - Demographic Information - Race`,
      
      Ethnicity = `CF - Demographic Information - Ethnicity`
      
    )
  
  
  
  filtered_volunteer_data <- reactive({
    
    if (input$selected_race == "All") {
      
      full_volunteer_data
      
    } else if (input$selected_race == "No Response") {
      
      full_volunteer_data %>% filter(is.na(Race) | trimws(Race) == "")
      
    } else if (input$selected_race %in% c("Hispanic or Latino/a/x", "Not Hispanic or Latino/a/x")) {
      
      full_volunteer_data %>% filter(Ethnicity == input$selected_race)
      
    } else {
      
      full_volunteer_data %>% filter(Race == input$selected_race)
      
    }
    
  })
  
  
  
  volunteer_map_data_reactive <- reactive({
    
    volunteer_data <- filtered_volunteer_data() %>%
      
      count(County, name = "Volunteers")
    
    
    
    left_join(volunteer_data, va_population, "County") %>%
      
      mutate(VolunteerRate = round(Volunteers / Population * 100, 2)) %>%
      
      left_join(va_counties, ., "County") %>%
      
      st_as_sf() %>%
      
      mutate(label_vol = paste0("<strong>", toupper(County), "</strong><br>",
                                
                                "Volunteers: ", Volunteers, "<br>",
                                
                                "Population: ", Population, "<br>",
                                
                                "Volunteer Rate: ", VolunteerRate, "%"))
    
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
  

  output$UpdatedParticipationGraph <- renderPlotly({
    aggregated_data <- particip_combined %>%
      group_by(Year, Region) %>%
      summarise(Total = sum(`County Totals`, na.rm = TRUE))
      # ungroup()
    
    # Define custom colors for regions
    region_colors <- c(
      "Northwest" = "#FF8C00",
      "Northeast" = "#B8860B",   # Light orange
      "Central" = "#00008B",
      "Southeast" = "#006994",
      "Southwest" = "#8B0000"
    )
    
    # Plot grouped bar chart
    library(ggplot2)
    # Assuming region_summary is your data frame
    
    p <- ggplot(region_summary, aes(
      x = Year,
      y = Total_Participation,
      fill = Region,
      text = paste0(
        "Region: ", Region, "<br>",
        "Year: ", Year, "<br>",
        "Total Participation: ", Total_Participation, "<br>",
        "Total Population: ", Total_Population, "<br>",
        "Participation %: ", round(Participation_Percentage, 2), "%"
      )
    )) +
      geom_bar(stat = "identity", position = "dodge")+ # or your preferred geom
      scale_fill_manual(values = region_colors) +
      labs(
       title = "4-H Participation Across Time",
        x = "Year",
       y = "Total Participation",
      fill = "Region"
      ) + 

      theme_minimal() +
      theme(
        plot.title = element_text(family = "Helvetica", size = 18, face = "bold"),
        axis.title = element_text(family = "Georgia", size = 14),
        axis.text = element_text(family = "Open Sans", size = 12),
        

       
      ) 

      ggplotly(p, tooltip = "text")
      
  })
  
  
  
  # === PARTICIPATION MAP ===
  
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
  
  ratio_df <- reactive({
    
    full_join(
      
      full_volunteer_data %>% count(County, name = "Volunteers"),
      
      fourh_data() %>% select(County, Participants = Total),
      
      by = "County"
      
    ) %>%
      
      mutate(RatioVP = ifelse(Participants > 0,
                              
                              round(Volunteers / Participants * 100, 2), NA))
    
  })
  
  
  
  output$vol_particip_map <- renderLeaflet({
    
    data <- left_join(va_counties, ratio_df(), "County") %>% st_as_sf()
    
    pal <- colorBin("RdYlBu", domain = data$RatioVP,
                    
                    bins = seq(0, ceiling(max(data$RatioVP, na.rm=TRUE)/100)*100, 100),
                    
                    na.color="#f0f0f0")
    
    data <- data %>%
      
      mutate(label_ratio = paste0("<strong>", toupper(County), "</strong><br>",
                                  
                                  "Volunteers: ", Volunteers, "<br>",
                                  
                                  "Participants: ", Participants, "<br>",
                                  
                                  "Ratio: ", RatioVP, "%"))
    
    leaflet(data) %>%
      
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(fillColor = ~pal(RatioVP), color = "black", weight = 1,
                  
                  fillOpacity = 0.7, label = lapply(data$label_ratio, htmltools::HTML),
                  
                  highlightOptions = highlightOptions(weight=2,color="#666",
                                                      
                                                      fillOpacity=0.9,bringToFront=TRUE)) %>%
      
      addLegend("bottomright", pal=pal, values=data$RatioVP,
                
                title="% Volunteers of Participants", opacity=1)
    
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