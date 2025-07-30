install.packages("sf")
library(sf)
library(tigris)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
install.packages("tidygeocoder")
library(tidygeocoder)
library(leaflet)
library(readxl)
install.packages("read.csv")

setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data")

names_string <- 
            "Accomack
             Albemarle
             Alexandria (city)
             Alleghany
             Amelia
             Amherst
             Appomattox
             Arlington
             Augusta
             Bath
             Bedford
             Bland
             Botetourt
             Brunswick
             Buchanan
             Buckingham
             Campbell
             Caroline
             Carroll
             Charles City
             Charlotte
             Chesapeake (city)
             Chesterfield
             Clarke
             Craig
             Culpeper
             Cumberland
             Danville (city)
             Dickenson
             Dinwiddie
             Essex
             Fairfax
             Fauquier
             Floyd
             Fluvanna
             Franklin
             Frederick
             Giles
             Gloucester
             Goochland
             Grayson
             Greene
             Greensville/Emporia
             Halifax
             Hampton (city)
             Hanover
             Henrico
             Henry/Martinsville
             Highland
             Isle of Wight
             James City
             King and Queen
             King George
             King William
             Lancaster
             Lee
             Loudoun
             Louisa
             Lunenburg
             Lynchburg (city)
             Madison
             Mathews
             Mecklenburg
             Middlesex
             Montgomery
             Nelson
             New Kent
             Newport News (city)
             Norfolk (city)
             Northampton
             Northumberland
             Nottoway
             Orange
             Page
             Patrick
             Petersburg (city)
             Pittsylvania
             Portsmouth (city)
             Powhatan
             Prince Edward
             Prince George
             Prince William
             Pulaski
             Rappahannock
             Richmond
             Richmond (city)
             Roanoke
             Rockbridge
             Rockingham
             Russell
             Scott
             Shenandoah
             Smyth
             Southampton
             Spotsylvania
             Stafford
             State Office
             Suffolk (city)
             Surry
             Sussex
             Tazewell
             Virginia Beach (city)
             Warren
             Washington
             Westmoreland
             Wise
             Wythe
             York/City of Poquoson
             Accomack
             Albemarle
             Alexandria (city)
             Alleghany
             Amelia
             Amherst
             Appomattox
             Arlington
             Augusta
             Bath
             Bedford
             Bland
             Botetourt
             Brunswick
             Buchanan
             Buckingham
             Campbell
             Caroline
             Carroll
             Charles City
             Charlotte
             Chesapeake (city)
             Chesterfield
             Clarke
             Craig
             Culpeper
             Cumberland
             Danville (city)
             Dickenson
             Dinwiddie
             Essex
             Fairfax
             Fauquier
             Floyd
             Fluvanna
             Franklin
             Frederick
             Giles
             Gloucester
             Goochland
             Grayson
             Greene
             Greensville/Emporia
             Halifax
             Hampton (city)
             Hanover
             Henrico
             Henry/Martinsville
             Highland
             Isle of Wight
             James City
             King and Queen
             King George
             King William
             Lancaster
             Lee
             Loudoun
             Louisa
             Lunenburg
             Lynchburg (city)
             Madison
             Mathews
             Mecklenburg
             Middlesex
             Montgomery
             Nelson
             New Kent
             Newport News (city)
             Norfolk (city)
             Northampton
             Northumberland
             Nottoway
             Orange
             Page
             Patrick
             Petersburg (city)
             Pittsylvania
             Portsmouth (city)
             Powhatan
             Prince Edward
             Prince George
             Prince William
             Pulaski
             Rappahannock
             Richmond
             Richmond (city)
             Roanoke
             Rockbridge
             Rockingham
             Russell
             Scott
             Shenandoah
             Smyth
             Southampton
             Spotsylvania
             Stafford
             State Office
             Suffolk (city)
             Surry
             Sussex
             Tazewell
             Virginia Beach (city)
             Warren
             Washington
             Westmoreland
             Wise
             Wythe
             York/City of Poquoson
             Accomack
             Albemarle
             Alexandria (city)
             Alleghany
             Amelia
             Amherst
             Appomattox
             Arlington
             Augusta
             Bath
             Bedford
             Bland
             Botetourt
             Brunswick
             Buchanan
             Buckingham
             Campbell
             Caroline
             Carroll
             Charles City
             Charlotte
             Chesapeake (city)
             Chesterfield
             Clarke
             Craig
             Culpeper
             Cumberland
             Danville (city)
             Dickenson
             Dinwiddie
             Essex
             Fairfax
             Fauquier
             Floyd
             Fluvanna
             Franklin
             Frederick
             Giles
             Gloucester
             Goochland
             Grayson
             Greene
             Greensville/Emporia
             Halifax
             Hampton (city)
             Hanover
             Henrico
             Henry/Martinsville
             Highland
             Isle of Wight
             James City
             King and Queen
             King George
             King William
             Lancaster
             Lee
             Loudoun
             Louisa
             Lunenburg
             Lynchburg (city)
             Madison
             Mathews
             Mecklenburg
             Middlesex
             Montgomery
             Nelson
             New Kent
             Newport News (city)
             Norfolk (city)
             Northampton
             Northumberland
             Nottoway
             Orange
             Page
             Patrick
             Petersburg (city)
             Pittsylvania
             Portsmouth (city)
             Powhatan
             Prince Edward
             Prince George
             Prince William
             Pulaski
             Rappahannock
             Richmond
             Richmond (city)
             Roanoke
             Rockbridge
             Rockingham
             Russell
             Scott
             Shenandoah
             Smyth
             Southampton
             Spotsylvania
             Stafford
             State Office
             Suffolk (city)
             Surry
             Sussex
             Tazewell
             Virginia Beach (city)
             Warren
             Washington
             Westmoreland
             Wise
             Wythe
             York/City of Poquoson
             Accomack
             Albemarle
             Alexandria (city)
             Alleghany
             Amelia
             Amherst
             Appomattox
             Arlington
             Augusta
             Bath
             Bedford
             Bland
             Botetourt
             Brunswick
             Buchanan
             Buckingham
             Campbell
             Caroline
             Carroll
             Charles City
             Charlotte
             Chesapeake (city)
             Chesterfield
             Clarke
             Craig
             Culpeper
             Cumberland
             Danville (city)
             Dickenson
             Dinwiddie
             Essex
             Fairfax
             Fauquier
             Floyd
             Fluvanna
             Franklin
             Frederick
             Giles
             Gloucester
             Goochland
             Grayson
             Greene
             Greensville/Emporia
             Halifax
             Hampton (city)
             Hanover
             Harrisonburg (city)
             Henrico
             Henry/Martinsville
             Highland
             Isle of Wight
             James City
             King and Queen
             King George
             King William
             Lancaster
             Lee
             Loudoun
             Louisa
             Lunenburg
             Lynchburg (city)
             Madison
             Mathews
             Mecklenburg
             Middlesex
             Montgomery
             Nelson
             New Kent
             Newport News (city)
             Norfolk (city)
             Northampton
             Northumberland
             Nottoway
             Orange
             Page
             Patrick
             Petersburg (city)
             Pittsylvania
             Portsmouth (city)
             Powhatan
             Prince Edward
             Prince George
             Prince William
             Pulaski
             Rappahannock
             Richmond
             Richmond (city)
             Roanoke
             Rockbridge
             Rockingham
             Russell
             Scott
             Shenandoah
             Smyth
             Southampton
             Spotsylvania
             Stafford
             State Office
             Suffolk (city)
             Surry
             Sussex
             Tazewell
             Virginia Beach (city)
             Warren
             Washington
             Westmoreland
             Wise
             Wythe
             York/City of Poquoson
             Z. Charlottesville (city) (INACTIVE)" 

# replace with your pasted block

names_vector <- strsplit(names_string, "\n")[[1]]
names_vector <- trimws(names_vector)
names_vector <- names_vector[names_vector != ""]

formatted_names <- paste0("\"", names_vector, "\"")
formatted_names_with_commas <- paste0(formatted_names, ",")

cat(paste(formatted_names_with_commas, collapse = "\n"))



# June 24 -----------------------------------------------------------------

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)


# --- Step 2: Your existing list of counties in a data frame ---
# Example: replace this with your actual data frame
# It should contain at least a column with county names, e.g., 'County'
# For example:
# county_names_df <- data.frame(County = c("CountyA", "CountyB", "CountyC", ...))
# If you already have such a data frame, skip this step
# For illustration, here's a dummy example:
county_names_df <- names_string
print(county_names_df)



# --- Step 3: Create a 'Region' column based on county names ---
region_map <- county_names_df %>%
  mutate(Region = case_when(
    # Replace these with your actual counties and regions
    County %in% c("Lee", "Buchanan", "Wise", "Scott", "Dickenson", "Russell", "Tazewell", "Washington",
                  "Smyth", "Grayson", "Wythe", "Bland", "Giles", "Pulaski", "Carroll", "Floyd",
                  "Montgomery", "Craig", "Roanoke", "Botetourt", "Alleghany") ~ "Southwest",
    
    County %in% c("Patrick", "Henry/Martinsville", "Franklin", "Pittsylvania", "Danville (city)",
                  "Bedford", "Lynchburg (city)", "Campbell", "Amherst", "Appomattox",
                  "Charlotte", "Halifax", "Mecklenburg", "Brunswick", "Lunenburg",
                  "Prince Edward", "Nottoway", "Amelia", "Cumberland", "Buckingham") ~ "Central",
    
    County %in% c("Chesapeake (city)", "Hampton (city)", "Newport News (city)", 
                  "Norfolk (city)", "Petersburg (city)", "Portsmouth (city)", 
                  "Suffolk (city)", "Virginia Beach (city)", "Dinwiddie", 
                  "Sussex", "Greensville/Emporia", "Southampton", "Prince George", 
                  "Isle of Wight", "Suffolk", "Surry", "James City", "New Kent",
                  "Charles City", "York/City of Poquoson", "Accomack", "Northampton"
                  ) ~ "Southeast",
    
    County %in% c("Alexandria (city)", "Richmond (city)", "Arlington", "Fairfax", "Loudoun",
                  "Fairfax", "Stafford", "Spotsylvania", "King George", "Caroline",
                  "Westmoreland", "Northumberland", "Lancaster", "Essex",
                  "Middlesex", "Mathews", "Gloucester", "King and Queen", "King William",
                  "Hanover", "Henrico", "Goochland", "Powhatan", "Chesterfield") ~ "Northeast",
    
    County %in% c("Harrisonburg (city)", "Rockbridge", "Bath", "Highland", "Augusta", 
                  "Nelson", "Albemarle", "Fluvanna", "Louisa", "Orange", "Greene", "Rockingham",
                  "Shenandoah", "Page", "Madison", "Culpeper", "Rappahannock",
                  "Fauquier", "Warren", "Frederick", "Clarke") ~ "Northwest",
    
    TRUE ~ NA_character_ # default if no match
  ))


class(county_names_df)


county_names_df <- data.frame(County = county_names_df)


str(county_names_df)

# Merge the region info into your participation data
Combined_Participation_Counts <- Combined_Participation_Counts %>%
  left_join(region_map, by = "County")


test_df <- head(Combined_Participation_Counts)
test_join <- test_df %>%
  left_join(region_map, by = "County")
print(test_join)


# Count how many non-NA regions
sum(!is.na(Combined_Participation_Counts$Region))


# Unique counties in your main data
unique(Combined_Participation_Counts$County)

# Unique counties in your region_map
unique(region_map$County)




library(dplyr)
library(stringr)

# Remove newline characters from 'County' column
Combined_Participation_Counts$County <- str_replace_all(Combined_Participation_Counts$County, "\\n", "")

# (Optional) trim whitespace just in case
Combined_Participation_Counts$County <- str_trim(Combined_Participation_Counts$County)

# Perform the join to add the 'Region' column
Combined_Participation_Counts <- Combined_Participation_Counts %>%
  left_join(region_map, by = "County")


table(is.na(Combined_Participation_Counts$Region))
# If FALSE, then no more NAs, and your regions are assigned!


str(region_map)
view(region_map)

region_map <- county_names_df %>%
  mutate(Region = case_when(
    County %in% c("Lee", "Buchanan", "Wise", "Scott", "Dickenson", "Russell", "Tazewell", "Washington",
                                          "Smyth", "Grayson", "Wythe", "Bland", "Giles", "Pulaski", "Carroll", "Floyd",
                                          "Montgomery", "Craig", "Roanoke", "Botetourt", "Alleghany") ~ "Southwest",
                            
                            County %in% c("Patrick", "Henry/Martinsville", "Franklin", "Pittsylvania", "Danville (city)",
                                          "Bedford", "Lynchburg (city)", "Campbell", "Amherst", "Appomattox",
                                          "Charlotte", "Halifax", "Mecklenburg", "Brunswick", "Lunenburg",
                                          "Prince Edward", "Nottoway", "Amelia", "Cumberland", "Buckingham") ~ "Central",
                            
                            County %in% c("Chesapeake (city)", "Hampton (city)", "Newport News (city)", 
                                          "Norfolk (city)", "Petersburg (city)", "Portsmouth (city)", 
                                          "Suffolk (city)", "Virginia Beach (city)", "Dinwiddie", 
                                          "Sussex", "Greensville/Emporia", "Southampton", "Prince George", 
                                          "Isle of Wight", "Suffolk", "Surry", "James City", "New Kent",
                                          "Charles City", "York/City of Poquoson", "Accomack", "Northampton") ~ "Southeast",
                            
                            County %in% c("Alexandria (city)", "Richmond (city)", "Arlington", "Fairfax", "Loudon",
                                          "Fairfax", "Stafford", "Spotsylvania", "King George", "Caroline",
                                          "Westmoreland", "Northumberland", "Lancaster", "Essex",
                                          "Middlesex", "Mathews", "Gloucester", "King and Queen", "King William",
                                          "Hanover", "Henrico", "Goochland", "Powhatan", "Chesterfield") ~ "Northeast",
                            
                            County %in% c("Harrisonburg (city)", "Rockbridge", "Bath", "Highland", "Augusta", 
                                          "Nelson", "Albemarle", "Fluvanna", "Louisa", "Orange", "Greene", "Rockingham",
                                          "Shenandoah", "Page", "Madison", "Culpeper", "Rappahannock",
                                          "Fauquier", "Warren", "Frederick", "Clarke") ~ "Northwest"
    
  ))

head(region_map)

colnames(region_map)
colnames(Combined_Participation_Counts)


names(Combined_Participation_Counts)[names(Combined_Participation_Counts) == "Region.x"] <- "Region"



Combined_Participation_Counts <- Combined_Participation_Counts %>%
  select(-Region.y)


Combined_Participation_Counts <- Combined_Participation_Counts %>%
  left_join(region_map, by = "County")

view(Combined_Participation_Counts)


# Rename 'Region' in region_map
region_map <- region_map %>%
  rename(Region = Region) # optional if you want to keep the same name

# When joining, if both have 'Region', R will create 'Region.x' and 'Region.y'
# To control which to keep, you can select one after join


# Rename 'Region' in region_map to 'Region_map' before merging
region_map <- region_map %>%
  rename(Region = Region_map)


# Left join main data with the region map
Combined_Participation_Counts <- Combined_Participation_Counts %>%
  left_join(region_map, by = "County")


# June 25 Retrying Data ---------------------------------------------------


rm(region_map, test_df, test_join)


# Example: Define counties for each region
southwest_counties <- c("Lee", "Buchanan", "Wise", "Scott", "Dickenson", "Russell", "Tazewell", "Washington",
                        "Smyth", "Grayson", "Wythe", "Bland", "Giles", "Pulaski", "Carroll", "Floyd",
                        "Montgomery", "Craig", "Roanoke", "Botetourt", "Alleghany")

central_counties <- c("Patrick", "Henry/Martinsville", "Franklin", "Pittsylvania", "Danville (city)",
                      "Bedford", "Lynchburg (city)", "Campbell", "Amherst", "Appomattox",
                      "Charlotte", "Halifax", "Mecklenburg", "Brunswick", "Lunenburg",
                      "Prince Edward", "Nottoway", "Amelia", "Cumberland", "Buckingham")

northeast_counties <- c("Alexandria (city)", "Richmond (city)", "Arlington", "Fairfax", "Loudoun",
                        "Fairfax", "Stafford", "Spotsylvania", "King George", "Caroline",
                        "Westmoreland", "Northumberland", "Lancaster", "Essex",
                        "Middlesex", "Mathews", "Gloucester", "King and Queen", "Richmond", "King William",
                        "Hanover", "Henrico", "Goochland", "Powhatan", "Chesterfield", "Prince William")

southeast_counties <- c("Chesapeake (city)", "Hampton (city)", "Newport News (city)", 
                        "Norfolk (city)", "Petersburg (city)", "Portsmouth (city)", 
                        "Suffolk (city)", "Virginia Beach (city)", "Dinwiddie", 
                        "Sussex", "Greensville/Emporia", "Southampton", "Prince George", 
                        "Isle of Wight", "Suffolk", "Surry", "James City", "New Kent",
                        "Charles City", "York/City of Poquoson", "Accomack", "Northampton")

northwest_counties <- c("Harrisonburg (city)", "Rockbridge", "Bath", "Highland", "Augusta", 
                        "Nelson", "Albemarle", "Fluvanna", "Louisa", "Orange", "Greene", "Rockingham",
                        "Shenandoah", "Page", "Madison", "Culpeper", "Rappahannock",
                        "Fauquier", "Warren", "Frederick", "Clarke")

rm(formatted_names, formatted_names_with_commas, names_string, names_vector)


# Combine into a data frame
county_region <- data.frame(
  County = c(southwest_counties, northwest_counties, northeast_counties, southeast_counties, central_counties),
  Region = c(
    rep("Southwest", length(southwest_counties)),
    rep("Northwest", length(northwest_counties)),
    rep("Northeast", length(northeast_counties)),
    rep("Southeast", length(southeast_counties)),
    rep("Central", length(central_counties))
  ),
  stringsAsFactors = FALSE
)


Combined_Participation_Counts <- Combined_Participation_Counts %>%
  left_join(county_region, by = "County")



# Remove columns
Combined_Participation_Counts <- Combined_Participation_Counts %>%
  select(-Region.x, -Region.y)



# Rename the column
Combined_Participation_Counts <- Combined_Participation_Counts %>%
  rename(Region = Region.y)


library(writexl)

write_xlsx(Combined_Participation_Counts, path = "C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")

write_xlsx(Combined_Participation_Counts, path = "C:/Users/jeffr/Desktop/Combined_Participation_Counts.xlsx")



# Keep only rows where Region is not NA
Combined_Participation_Counts <- Combined_Participation_Counts %>%
  filter(!is.na(Region))



#MAKING THE BAR PLOT


# Load libraries
library(ggplot2)
library(dplyr)
install.packages("plotly")
library(plotly)

# Optional: Convert 'Year' to factor for ordering
Combined_Participation_Counts$Year <- factor(Combined_Participation_Counts$Year,
                                             levels = c("2021-2022", "2022-2023", "2023-2024", "2024-2025"))

# Aggregate total participation by Year and Region
aggregated_data <- Combined_Participation_Counts %>%
  group_by(Year, Region) %>%
  summarise(Total = sum(`County Totals`, na.rm = TRUE)) %>%
  ungroup()

# Define custom colors for regions
region_colors <- c(
  "Northwest" = "orange",
  "Northeast" = "lightgoldenrod",   # Light orange
  "Central" = "blue",
  "Southeast" = "lightblue",
  "Southwest" = "maroon"
)

# Plot grouped bar chart
p <- ggplot(aggregated_data, aes(x = Year, y = Total, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = region_colors, name = "Region") +
  labs(
    x = "Year",
    y = "Total Participation",
    title = "Total Participation in 4-H Programs"
  ) +
  theme_bw() +
   theme(
    plot.title = element_text(family = "Helvetica", size = 18, face = "bold"),
    axis.title = element_text(family = "Georgia", size = 14),
    axis.text = element_text(family = "Open Sans", size = 12)
  )
ggplotly(p)

install.packages("ggthemes")
library(ggthemes)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("ggsci")
library(ggsci)
