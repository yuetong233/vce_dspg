# June 12 2025 ------------------------------------------------------------

library(tidyverse)
library(dplyr)

#ATTEMPTING TO FURTHER CLEAN AND SUMMARIZE THE DATA

rm(summary_df)

ultra_cleaned_demographic_sheet2 <- counties_and_demographics_june_11

# Summarize numeric columns by sum
ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2 %>%
  group_by(`Event Site County`) %>%
  summarize(
    total_volunteers = sum(`Number of Volunteers`, na.rm = FALSE),
    total_hours = sum(`Volunteering Hours`, na.rm = FALSE),
    total_participants = sum(`Total Participants`, na.rm = FALSE),
    number_sessions = sum(`Number of Sessions`, na.rm = FALSE),
    male_participants = sum(`Male Participants`, na.rm = FALSE),
    female_participants = sum(`Female Participants`, na.rm = FALSE),
    non_binary_participants = sum(`Non-Binary Participants`, na.rm = FALSE),
    gender_not_respond = sum(`Gender Prefer Not to Respond`, na.rm = FALSE),
    age_1to5 = sum(`Aged 1-5`, na.rm = FALSE),
    age_5to7 = sum(`Aged 5-7`, na.rm = FALSE),
    age_8to10 = sum(`Aged 8-10`, na.rm = FALSE),
    age_11to13 = sum(`Aged 11-13`, na.rm = FALSE),
    age_14to17 = sum(`Aged 14-17`, na.rm = FALSE),
    age_18to29 = sum(`Aged 18-29`, na.rm = FALSE),
    age_30to59 = sum(`Aged 30-59`, na.rm = FALSE),
    age_60to75 = sum(`Aged 60-75`, na.rm = FALSE),
    age_76over = sum(`Aged 76+`, na.rm = FALSE),
    hispanic_particip = sum(`Total Hispanic Population`, na.rm = FALSE),
    non_hispanic_particip = sum(`Total Non-Hispanic Population`, na.rm = FALSE),
    ethnicity_no_response = sum(`Ethnicity Prefer Not to Respond`, na.rm = FALSE),
    unknown_ethnicity = sum(`Total Participation - Unknown Ethnicity`, na.rm = FALSE),
    total_natam_one_particip = sum(`Total Native American Participation - One Race`, na.rm = FALSE),
    total_natam_multi_particip = sum(`Total Native American Participation - Multirace`, na.rm = FALSE),
    total_asian_one_particip = sum(`Total Asian Participation - One Race`, na.rm = FALSE),
    total_asain_multi_particip = sum(`Total Asian Participation - Multirace`, na.rm = FALSE),
    total_afram_one_particip = sum(`Total African American Participation - One Race`, na.rm = FALSE),
    total_afram_multi_particip = sum(`Total African American Participation - Multirace`, na.rm = FALSE),
    total_hawpac_one_particip = sum(`Total Hawaiian/Pacific Islander Participation - One Race`, na.rm = FALSE),
    total_hawpac_multi_particip = sum(`Total Hawaiian/Pacific Islander Participation - Multirace`, na.rm = FALSE),
    total_white_one_particip = sum(`Total White Participation - One Race`, na.rm = FALSE),
    total_white_multi_particip = sum(`Total White Participation - Multirace`, na.rm = FALSE),
  )


#IN THE ABOVE DATA FRAME, THE CODE IGNORED "NA" VALUES, AND DID NOT TAKE THEM
#INTO CONSIDERATION FOR CALCULATION

setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")
write.xlsx(summary_df, "cleaned_demographic_sheet_6.12.25.xlsx")

cleaned_demographic_sheet_6_16_25 <- cleaned_demographic_sheet_6_12_25

cleaned_demographic_sheet_6_16_25 <- cleaned_demographic_sheet_6_16_25 %>%
  rename("Total Volunteers" = "total_volunteers",
         "Total Volunteer Hours" = "total_hours",
         "Total Participants" = "total_participants",
         "Number of Sessions" = "number_sessions",
         "Total Male Participants" = "male_participants",
         "Total Female Participants" = "female_participants",
         "Total Non-Binary Participants" = "non_binary_participants",
         "Gender - Prefer Not to Respond" = "gender_not_respond",
         "Aged 1-5" = "age_1to5",
         "Aged 5-7" = "age_5to7",
         "Aged 8-10" = "age_8to10",
         "Aged 11-13" = "age_11to13",
         "Aged 14-17" = "age_14to17",
         "Aged 18-29" = "age_18to29",
         "Aged 30-59" = "age_30to59",
         "Aged 60-75" = "age_60to75",
         "Aged 76+" = "age_76over",
         "Total Hispanic Participation " = "hispanic_particip",
         "Total Non-Hispanic Participation" = "non_hispanic_particip",
         "Ethnicity - Prefer Not to Respond" = "ethnicity_no_response",
         "Unknown Ethnicity" = "unknown_ethnicity",
         "Total Native American Participation - One Race" = "total_natam_one_particip",
         "Total Native American Participation - Multi-Race" = "total_natam_multi_particip",
         "Total Asian Participation - One Race" = "total_asian_one_particip",
         "Total Asian Participation - Multi-Race" = "total_asain_multi_particip",
         "Total African American Participation - One Race" = "total_afram_one_particip",
         "Total African American Participation - Multi-Race" = "total_afram_multi_particip",
         "Total Hawaiian/Pacific Islander Participation - One Race" = "total_hawpac_one_particip",
         "Total Hawaiin/Pacific Islander Participation - Multi-Race" = "total_hawpac_multi_particip",
         "Total White Participation - One Race" = "total_white_one_particip",
         "Total White Participation - Muti-Race" = "total_white_multi_particip")


# Notes From Dr. Cary - June 12 2025 --------------------------------------


#Some of the rows are small towns in Virginia for which there will not be any 
#population data, etc. You should figure out which county these towns are located
#in and combine the data at the county level. The easiest way to do this is to
#see if the location has a FIPS code or not (). Places without a FIPS code will 
#be the small towns that you need to lump in with the county – 
#based on your data set there will be ~20 of these

#If you can, create one more column for volunteers who logged hours. 
#This will let you calculate the average number of hours volunteered 
#per volunteer. The new column is necessary because, e.g., one of the first 
#lines has one volunteer and no hours. Adding this column lets you determine 
#average hours volunteered in an unbiased way.

#Are the data for age groups for volunteers or participants? Just make this clear
#in your notes (which I’m sure you already have).

#Merge population data from the ACS so you can calculate volunteer and participation 
#rates.


# -------------------------------------------------------------------------


# June 13 2025 ------------------------------------------------------------


ultra_cleaned_demographic_sheet2 <- counties_and_demographics_june_11
rm(counties_and_demographics_june_11)


# Create a new column for volunteers who reported hours
ultra_cleaned_demographic_sheet2$Reported_Hours <- ifelse(!is.na(ultra_cleaned_demographic_sheet2$`Volunteering Hours`), 
                                                                     ultra_cleaned_demographic_sheet2$`Total Volunteer Hours`, NA)

# Create a new column for volunteers who did not report hours
ultra_cleaned_demographic_sheet2$No_Reported_Hours <- ifelse(is.na(ultra_cleaned_demographic_sheet2$`Volunteering Hours`), "Did Not Report", "Reported")

# Alternatively, if you just want TRUE/FALSE
# df$Reported <- !is.na(df$VolunteeringHours)
rm(ultra_cleaned_demographic_sheet)

names(ultra_cleaned_demographic_sheet2)


# Reorder to put 'NoReportedHours' first
ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2[c("Volunteers_Reported_Hours", setdiff(names(ultra_cleaned_demographic_sheet2), "Volunteers_Reported_Hours"))]


# Verify change
names(df)

ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2[ , -1]
ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2[ , -47]

# Create a new column 'LoggedHours' that is TRUE if hours are reported, FALSE if not
ultra_cleaned_demographic_sheet2$LoggedHours <- !is.na(ultra_cleaned_demographic_sheet2$`Volunteering Hours`)

# Calculate average hours for volunteers who reported hours
avg_hours <- mean(ultra_cleaned_demographic_sheet2$`Volunteering Hours`, na.rm = TRUE)


# Specify the column name
column_name <- "LoggedHours"

# Get all other columns
other_columns <- setdiff(names(ultra_cleaned_demographic_sheet2), column_name)

# Create new order: first 8 columns, then the target column, then the rest
new_order <- append(other_columns, column_name, after = 8)

# Reorder the data frame
ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2[new_order]


ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2 %>%
  rename("Logged Hours" = "LoggedHours")

checking_columns <- sapply(ultra_cleaned_demographic_sheet2, is.numeric)
print(checking_columns)

# Apply to all numeric columns
ultra_cleaned_demographic_sheet2[checking_columns] <- lapply(ultra_cleaned_demographic_sheet2[checking_columns], function(col) {
  col <- as.character(col)          # Convert to character
  col[is.na(col)] <- "No Data"       # Replace NA with "No Data"
  return(col)
})

rm(summary_df, cleaned_demographic_sheet, counties_and_demographics_june_11,)



# June 16th, 2025 ---------------------------------------------------------

#TRYING AGAIN TO CREATE THE SECTIONS FOR LOGGED AND UNLOGGED HOURS

library(dplyr)

ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2 %>%
  mutate(
    Hours_Logged = if_else(!is.na(`Total Volunteer Hours`), `Total Volunteer Hours`, NA_real_),
    No_Hours_Logged = if_else(is.na(`Total Volunteer Hours`), "No", NA_character_)
  )


# Get the current column names
col_names <- colnames(ultra_cleaned_demographic_sheet2)

# Reorder columns: put the new columns in positions 4 and 5
new_order <- c(
  col_names[1:3],             # columns 1 to 3
  "Hours_Logged",             # 4th position
  "No_Hours_Logged",          # 5th position
  col_names[(4:length(col_names))]  # remaining columns
)

# Reorder the data frame
ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2 %>%
  select(all_of(new_order))

ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2 %>%
  rename("Volunteer Hours Logged" = "Hours_Logged",
         "Volunteer No Hours Logged" = "No_Hours_Logged")

# Calculate average hours for volunteers who reported hours
avg_hours <- mean(ultra_cleaned_demographic_sheet2$`Volunteering Hours`, na.rm = TRUE)

library(dplyr)

# Replace NA with blank ("")
ultra_cleaned_demographic_sheet2[is.na(ultra_cleaned_demographic_sheet2)] <- "No Data"

# Or replace NA with zero
library(readxl)
library(writexl)


# Loop through each column and replace NA with "No Data" only if the column is character
ultra_cleaned_demographic_sheet2 <- ultra_cleaned_demographic_sheet2 %>%
  mutate(across(where(is.character), ~replace(., is.na(.), "No Data")))


# Save the modified data
library(writexl)
write_xlsx(ultra_cleaned_demographic_sheet2, "june16_cleaned_data.xlsx")

setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")

merged_data_june16_2025 <- bind_rows(june16_cleaned_data, county_populations_virginia_2020_2024)

write_xlsx(cleaned_demographic_sheet_6_16_25, "cleaned_demographic_sheet_6_16_25.xlsx")
getwd()


# Create the "VolunteersReported" column
counties_and_demographics_june_11$volunteers_reported_hours <- !is.na(counties_and_demographics_june_11$total_volunteer_hours)

# Create the "VolunteersNotReported" column
counties_and_demographics_june_11$volunteers_no_report_hours <- is.na(counties_and_demographics_june_11$total_volunteer_hours)

# Create the "NoVolunteerData" column
counties_and_demographics_june_11$no_volunteer_data <- is.na(counties_and_demographics_june_11$num_volunteers)



# Specify the column names you want to move:
cols_to_move <- c("volunteers_reported_hours", "volunteers_no_report_hours", "no_volunteer_data")

# Get current column names
current_cols <- colnames(counties_and_demographics_june_11)
current_cols

# Remove the columns to move from current columns
remaining_cols <- setdiff(current_cols, cols_to_move)

# Create the new column order:
# Place the three columns in positions 9, 10, 11
new_order <- c(remaining_cols[1:8], cols_to_move, remaining_cols[9:length(remaining_cols)])

# Reorder the data frame
counties_and_demographics_june_11 <- counties_and_demographics_june_11[, new_order]
is.data.frame(counties_and_demographics_june_11)
print(new_order)

getwd()
write_xlsx(counties_and_demographics_june_11, "June16_cleaned_Data.xlsx")

library(purrr)
Merge_test3 <- bind_rows(counties_and_demographics_june_11, va_demographics_updated)


library(dplyr)

df_2024 <- county_populations_virginia_2020_2024 %>%
  select(`table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)`, ...7)


df_2024 <- df_2024 %>%
  rename( "State, County" = "table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)",
         "Population" = "...7")

write_xlsx(df_2024, "Virginia_Population_2024.xlsx")
