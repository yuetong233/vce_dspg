#SPLITTING THE COMBINED_PARTICIPATION DATA FRAME INTO FOUR DIFFERENT
#DATA FRAMES BY YEAR

library(dplyr)


df_2021_2022 <- Combined_Participation_Counts_updated %>%
  filter(Year == "2021-2022")

df_2022_2023 <- Combined_Participation_Counts_updated %>%
  filter(Year == "2022-2023")

df_2023_2024 <- Combined_Participation_Counts_updated %>%
  filter(Year == "2023-2024")

df_2024_2025 <- Combined_Participation_Counts_updated %>%
  filter(Year == "2024-2025")




# Extract the County columns
county_df1 <- df_2021_2022$County
county_df2 <- virginia_2020_2024_population$County

# 1. Counties in virginia_2020_2024_population not in df_2021_2022
not_in_df1 <- setdiff(county_df2, county_df1)

# 2. Counties in df_2021_2022 not in virginia_2020_2024_population
not_in_df2 <- setdiff(county_df1, county_df2)


# Print results
print("Counties in virginia_2020_2024_population not in df_2021_2022:")
print(not_in_df1)

print("Counties in df_2021_2022 not in virginia_2020_2024_population:")
print(not_in_df2)

# Replace "Danville" with "Danville (city)" in the "County" column
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Danville"] <- "Danville (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Newport News"] <- "Newport News (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Chesapeake"] <- "Chesapeake (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Portsmouth"] <- "Portsmouth (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Suffolk"] <- "Suffolk (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Hampton"] <- "Hampton (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Petersburg"] <- "Petersburg (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Richmond City"] <- "Richmond (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Alexandria"] <- "Alexandria (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Lynchburg*"] <- "Lynchburg (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Norfolk*"] <- "Norfolk (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Prince Edward*"] <- "Prince Edward"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Montgomery*"] <- "Montgomery"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Virginia Beach"] <- "Virginia Beach (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Lexington*"] <- "Lexington"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Radford*"] <- "Radford"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Charlottesville*"] <- "Charlottesville"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Roanoke City"] <- "Roanoke (city)"
virginia_2020_2024_population$County[virginia_2020_2024_population$County == "Williamsburg*"] <- "Williamsburg"


#WORKING TO COMBINE THE ENTRIES THAT NEED TO BE COMBINED


# York/City of Poquoson  --------------------------------------------------



library(dplyr)

# Define the population columns
pop_cols_y_p <- c("2020 Census", "July 1, 2020", "July 1, 2021", "July 1, 2022", "July 1, 2023", "July 1, 2024")

# Step 1: Filter for "York" and "Poquoson"
rows_to_combine_y_p <- virginia_2020_2024_population %>%
  filter(County %in% c("York", "Poquoson"))

# Step 2: Sum the population columns
combined_pop_y_p <- colSums(rows_to_combine_y_p[, pop_cols_y_p], na.rm = TRUE)

# Step 3: Remove original "York" and "Poquoson" entries
virginia_2020_2024_population <- virginia_2020_2024_population %>%
  filter(!County %in% c("York", "Poquoson"))

# Step 4: Add the new combined entry
virginia_2020_2024_population <- virginia_2020_2024_population %>%
  add_row(
    County = "York/City of Poquoson",
    !!!setNames(as.list(combined_pop_y_p), pop_cols_y_p)
  )


# Greensville/Emporia -----------------------------------------------------

library(dplyr)

# Define the population columns
pop_cols_g_e <- c("2020 Census", "July 1, 2020", "July 1, 2021", "July 1, 2022", "July 1, 2023", "July 1, 2024")

# Filter for "Greensville" and "Emporia"
rows_to_combine_g_e <- virginia_2020_2024_population %>%
  filter(County %in% c("Greensville", "Emporia"))

# Sum the population columns
combined_pop_g_e <- colSums(rows_to_combine_g_e[, pop_cols_g_e], na.rm = TRUE)

# Remove the original entries
virginia_2020_2024_population <- virginia_2020_2024_population %>%
  filter(!County %in% c("Greensville", "Emporia"))

# Add the new combined entry
virginia_2020_2024_population <- virginia_2020_2024_population %>%
  add_row(
    County = "Greensville/Emporia",
    !!!setNames(as.list(combined_pop_g_e), pop_cols_g_e)
  )


# Henry/Martinsville ------------------------------------------------------

# Define the population columns
pop_cols_h_m <- c("2020 Census", "July 1, 2020", "July 1, 2021", "July 1, 2022", "July 1, 2023", "July 1, 2024")

# Filter for "Greensville" and "Emporia"
rows_to_combine_h_m <- virginia_2020_2024_population %>%
  filter(County %in% c("Henry", "Martinsville"))

# Sum the population columns
combined_pop_h_m <- colSums(rows_to_combine_h_m[, pop_cols_h_m], na.rm = TRUE)

# Remove the original entries
virginia_2020_2024_population <- virginia_2020_2024_population %>%
  filter(!County %in% c("Henry", "Martinsville"))

# Add the new combined entry
virginia_2020_2024_population <- virginia_2020_2024_population %>%
  add_row(
    County = "Henry/Martinsville",
    !!!setNames(as.list(combined_pop_h_m), pop_cols_h_m)
  )


# Removing unnecessary entries --------------------------------------------

library(dplyr)

# Remove row where County is "Danville"
# Remove row 5
virginia_2020_2024_population <- virginia_2020_2024_population[-1, ]

virginia_2020_2024_population <- virginia_2020_2024_population[-c(98, 103, 108, 115, 122, 94, 99, 104, 109, 118, 125, 95, 101, 105, 111, 112),]
virginia_2020_2024_population <- virginia_2020_2024_population[-c(99, 97, 94, 106, 107),]
virginia_2020_2024_population <- virginia_2020_2024_population[-c(105,106),]

Combined_Participation_Counts_updated <- Combined_Participation_Counts_updated[-c(364),]

# Ensure the 'County' column exists in both data frames
# Reorder 'virginia_2020_2024_population' to match the order in 'df_2021_2022'
virginia_2020_2024_population <- virginia_2020_2024_population[
  match(df_2021_2022$County, virginia_2020_2024_population$County),
]



# Merging Data Sets -------------------------------------------------------


library(dplyr)

# List of data frames to update
dfs <- list(df_2021_2022, df_2022_2023, df_2023_2024, df_2024_2025)

# Column names to join
pop_columns <- c("2020 Census", "July 1, 2020", "July 1, 2021", "July 1, 2022", "July 1, 2023", "July 1, 2024")

# Loop through each data frame and join the population columns
for (i in seq_along(dfs)) {
  dfs[[i]] <- dfs[[i]] %>%
    left_join(virginia_2020_2024_population %>% select(County, all_of(pop_columns)), by = "County")
}

# Assign back to your original data frames
df_2021_2022 <- dfs[[1]]
df_2022_2023 <- dfs[[2]]
df_2023_2024 <- dfs[[3]]
df_2024_2025 <- dfs[[4]]



# July 2nd, saving new excel sheets and so on -----------------------------
library(dplyr)
library(openxlsx)

setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")

write.xlsx(df_2021_2022, "va_pop_particip_2020_2024.xlsx")

virginia_pop_particip_2021_2025 <- bind_rows(df_2021_2022, df_2022_2023, df_2023_2024, df_2024_2025)

# Find duplicate rows
duplicates <- df_2024_2025[duplicated(df_2024_2025), ]

# View duplicate rows
print(duplicates)

# Count total number of duplicate rows
num_duplicates <- sum(duplicated(df_2024_2025))
cat("Number of duplicate rows:", num_duplicates, "\n")


#FIND THE OUTLIER AMONG THE FOUR DATA FRAMES, BECAUSE THERE IS ONE MORE ROW IN
#THE FINAL DATA FRAME

# Combine the first three data frames' 'County' columns
counties_first_three <- unique(c(
  df_2021_2022$County,
  df_2022_2023$County,
  df_2023_2024$County
))

# Find the county(s) in the fourth data frame not in the first three
extra_county <- setdiff(df_2024_2025$County, counties_first_three)

# Print the extra county or counties
print(extra_county)

#OUTLIER FOUND: HARRISONBURG (CITY)

df_2024_2025 <- df_2024_2025[-c(46),]


library(dplyr)

# Find counties in the second data frame that are NOT in the first
discrepancies <- setdiff(Annual_Program_Report_Participation_Count_21_22_$CountyArea,
                         df_2021_2022$County)

# Print the discrepancies
print("Counties in the second data frame not in the first:")
print(discrepancies)

# To see full rows in the second data frame that correspond to these discrepancies:
extra_rows_2 <- Annual_Program_Report_Participation_Count_21_22_ %>%
  filter(CountyArea %in% discrepancies)

# View the extra rows
print(extra_rows_2)

library(dplyr)

# Remove a column by name
va_pop_particip_2021_2025 <- va_pop_particip_2021_2025 %>% select(-CountyAreaId)

write.xlsx(va_pop_particip_2021_2025, "va_pop_particip_2021_2025.xlsx")

#EXTRA ROW IN RAW DATA SET WAS THE STATE OFFICE, IRRELEVANT

# Now, actually merging ---------------------------------------------------

library(dplyr)

# Combine data frames row-wise
va_pop_particip_2021_2025 <- bind_rows(df_2021_2022, df_2022_2023, df_2023_2024, df_2024_2025)

getwd()
setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")
write.xlsx(particip_combined, "particip_combined.xlsx")

