# Load necessary package
library(dplyr)

# Read the CSV file (replace 'yourfile.csv' with your actual filename)
county_demographics_name_change <- read.csv("countiesdemographics.csv", stringsAsFactors = FALSE)

# Replace "Lexington (city)" with "Lexington" in the entire dataframe
# Assuming the relevant data is in a specific column, e.g., 'CityColumn'
# If you want to replace in all character columns, you can do that too

# Example for a specific column:
county_demographics_name_change$site_county <- gsub("Lexington \\(city\\)", "Lexington", county_demographics_name_change$site_county)

county_demographics_name_change$site_county <- gsub("Salem \\(city\\)", "Salem", county_demographics_name_change$site_county)

# If you haven't already loaded the data, do so
# data <- read.csv("yourfile.csv", stringsAsFactors = FALSE)
# If you haven't already loaded the data, do so
# data <- read.csv("yourfile.csv", stringsAsFactors = FALSE)

# Remove "(city)" from the relevant column
county_demographics_name_change$site_county <- gsub(" \\(city\\)", "", county_demographics_name_change$site_county)

# Save the updated data if needed
# write.csv(data, "yourfile_updated.csv", row.names = FALSE)
write.csv(county_demographics_name_change, "newcountiesdemographics.csv")
# Remove "(city)" from the relevant column
# Save the updated data if needed
# write.csv(data, "yourfile_updated.csv", row.names = FALSE)
