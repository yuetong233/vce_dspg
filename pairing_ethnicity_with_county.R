#TESTING TESTING TESTING


# June 11 2025 ------------------------------------------------------------


#IMPORTING THE PEARS RAW DATA, AND SELECTING THE COUNTY, SCHOOL, GENDER, AND AGE COLUMNS
install.packages("readxl")
library(readxl)

#INSTALLING RELEVANT PACKAGES, THEN MAKING DATA FRAME

install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)

counties_and_participation <- read_excel("PEARS Raw Data 5.23.25.xlsx", sheet = "Program Activities")
rm(countyies_and_participation)

selected_data <- counties_and_participation %>%
  select(site_name, site_city, site_county, site_zip, site_setting, number_sessions, 
         num_volunteers, total_volunteer_hours, participants_total, participants_gender_male, participants_gender_female,
         participants_gender_non_binary, participants_gender_prefer_not_to_respond,participants_age_youth,
         participants_age_lt5, participants_age_5to7, participants_age_8to10, participants_age_11to13,
         participants_age_14to17, participants_age_18to29, participants_age_30to59, participants_age_60to75,
         participants_age_76plus, participants_age_adult, participants_ethnicity_hispanic,
         participants_ethnicity_non_hispanic,
         participants_ethnicity_prefer_not_to_respond,
         participants_ethnicity_unknown,
         participants_amerind_onerace_total,
         participants_amerind_multirace_total,
         participants_asian_onerace_total,
         participants_asian_multirace_total,
         participants_black_onerace_total,
         participants_black_multirace_total,
         participants_hawpac_onerace_total,
         participants_hawpac_multirace_total,
         participants_white_onerace_total,
         participants_white_multirace_total,
         participants_race_amerind,
         participants_race_asian,
         participants_race_black,
         participants_race_hawpac,
         participants_race_white,
         participants_race_two_or_more,
         participants_race_prefer_not_to_respond,
         participants_race_unknown)



#EDITING THE ETHNICITY TOTALS TABLE
rm(ethnicity_totals_statewide, ethnicity_totals_virginia)

ethnicity_totals_virginia <- read_excel("ethnicity_totals_statewide.xlsx", col_names = FALSE)
colnames(ethnicity_totals_virginia) <- c("Ethnicity/Race", "Totals")

print(colnames(ethnicity_totals_virginia))

library(writexl)

write_xlsx(ethnicity_totals_virginia, "ethnicity_totals_virginia_PEARS.xlsx")

#SUCCESS ^^^

#WHAT I WANT TO DO NOW IS TO COMBINE THE DATA SHEETS TO SHOW THEM BOTH SIDE BY SIDE

write_xlsx(selected_data, "counties_and_demographics_june_11.xlsx")

combined_df <- bind_cols(ethnicity_totals_virginia_PEARS, counties_and_demographics_june_11)

#COMBINING THEM DOESN'T WORK, SINCE THEY HAVE DIFFERING NUMBERS OF ROWS