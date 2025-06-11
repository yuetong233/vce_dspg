#READING IN THE PEARS RAW DATA, ACTION PLAN, AND OUTCOME INDICATORS

install.packages("readxl")

library(readxl)
library(dplyr)

setwd("C:/Users/jeffr/Desktop/diego_and_jeffrey_initial_vce_data")

pears_action_plan <- read_excel("PEARS Raw Data 5.23.25.xlsx", sheet = "Program Activities")

pears_action_plan_data <- read_excel("PEARS Action-Plans-Export 5.30.25.xlsx", sheet = "Action Plan Data")

pears_outcome_indicators <- read_excel("PEARS Action-Plans-Export 5.30.25.xlsx", sheet = "Outcome Indicators")

pears_raw_data_codebook <- read_excel("PEARS Raw Data 5.23.25.xlsx", sheet = "Program Activities Codebook")

pears_action_plan_codebook <- read_excel("PEARS Action-Plans-Export 5.30.25.xlsx", sheet = "Codebook")



# June 9 2025 -------------------------------------------------------------

#IDEA: COMPARE THE PARTICIPANTS LISTED IN THE PEARS DATA WITH THE DEMOGRAPHIC DATA
#CREATED A NEW R SCRIPT FOR THIS

#SUMMING UP PARTICIPATION RATES BY ETHNICITY

library(dplyr)
library(tidycensus)
library(tidyverse)
library(readxl)

getwd()
list.files()

pears_raw_ethnic_data <- read_excel("PEARS Raw Data 5.23.25.xlsx", sheet = "Program Activities")

program_activities_ethnicity_count <- pears_raw_ethnic_data

head(program_activities_ethnicity_count)

total_hispanic <- sum(program_activities_ethnicity_count$participants_ethnicity_hispanic, na.rm = TRUE)
total_non_hispanic <- sum(program_activities_ethnicity_count$participants_ethnicity_non_hispanic, na.rm = TRUE)
total_unknown_ethnicity <- sum(program_activities_ethnicity_count$participants_ethnicity_unknown, na.rm = TRUE)
test_do_not_reference <- sum(program_activities_ethnicity_count$participants_ethnicity_prefer_not_to_respond, na.rm = TRUE)
total_onerace_native_american <- sum(program_activities_ethnicity_count$participants_amerind_onerace_total, na.rm = TRUE)
total_multirace_native_american <- sum(program_activities_ethnicity_count$participants_amerind_multirace_total, na.rm = TRUE)
total_asian_onerace <- sum(program_activities_ethnicity_count$participants_asian_onerace_total, na.rm = TRUE)
total_asian_multirace <- sum(program_activities_ethnicity_count$participants_asian_multirace_total, na.rm = TRUE)
total_black_onerace <- sum(program_activities_ethnicity_count$participants_black_onerace_total, na.rm = TRUE)
total_black_multirace <- sum(program_activities_ethnicity_count$participants_black_multirace_total, na.rm = TRUE)
total_hawpac_onerace <- sum(program_activities_ethnicity_count$participants_hawpac_onerace_total, na.rm = TRUE)
total_hawpac_multirace_updated <- sum(program_activities_ethnicity_count$participants_hawpac_multirace_total, na.rm = TRUE)
total_white_onerace <- sum(program_activities_ethnicity_count$participants_white_onerace_total, na.rm = TRUE)
total_white_multirace <- sum(program_activities_ethnicity_count$participants_white_multirace_total, na.rm = TRUE)

participants_race_nativeamerican <- sum(program_activities_ethnicity_count$participants_race_amerind, na.rm = TRUE)
participants_race_asian <- sum(program_activities_ethnicity_count$participants_race_asian, na.rm = TRUE)
participants_race_black <- sum(program_activities_ethnicity_count$participants_race_black, na.rm = TRUE)
participants_race_hawpac <- sum(program_activities_ethnicity_count$participants_race_hawpac, na.rm = TRUE)
participants_race_white <- sum(program_activities_ethnicity_count$participants_race_white, na.rm = TRUE)
participants_race_two_or_more <- sum(program_activities_ethnicity_count$participants_race_two_or_more, na.rm = TRUE)
participants_race_no_response <- sum(program_activities_ethnicity_count$participants_race_prefer_not_to_respond, na.rm = TRUE)
participants_race_unknown <- sum(program_activities_ethnicity_count$participants_race_unknown, na.rm = TRUE)

total_participation <- sum(ethnicity_totals_statewide)

ethnicity_totals <- rbind(total_hispanic,total_non_hispanic,total_unknown_ethnicity,total_onerace_native_american,

      total_multirace_native_american,total_asian_onerace,total_asian_multirace,
      total_black_onerace,total_black_multirace,total_hawpac_onerace, total_hawpac_multirace_updated,
      total_white_onerace,total_white_multirace,participants_race_nativeamerican,
    participants_race_asian, participants_race_black, participants_race_hawpac,
    participants_race_white,participants_race_two_or_more, participants_race_no_response,
    participants_race_unknown)

                          total_multirace_native_american,total_asian_onerace,total_asian_multirace
                          total_black_onerace,total_black_multirace,total_hawpac_onerace, total_hawpac_multirace_updated
                          total_white_onerace,total_white_multirace,participants_race_nativeamerican
                          participants_race_asian, participants_race_black, participants_race_hawpac
                          participants_race_white,participants_race_two_or_more, participants_race_no_response
                          participants_race_unknown


ethnicity_totals_statewide <- ethnicity_totals

#CREATING AN INTERACTIVE DATA SET

install.packages("DataExplorer")
library(DataExplorer)
interactive_data_table <- data.frame(list("Total Hispanic Participation" = total_hispanic, "Total Non-Hispanic Participants" = total_non_hispanic,

               "Total Participants of Unknown Ethnicity" = total_unknown_ethnicity,
               "Participants Ethnicity - Prefer Not To Respond" = test_do_not_reference,
               "Total Participants of Native American Descent - One Race" = total_onerace_native_american,
               "Total Participants of Native American Descent - Multirace" = total_multirace_native_american,
               "Total Participants Asian Descent - One Race" = total_asian_onerace,
               "Total Participants of Asian Descent - Multirace" = total_asian_multirace,
               "Total Participants - African American, One Race" = total_black_onerace,
               "Total Participants - African American, Multirace" = total_black_multirace,
               "Total Participants - Hawaiian/Pacific Islander - One Race" = total_hawpac_onerace,
               "Total Participants - Hawaiian/Pacific Islander - Multirace" = total_hawpac_multirace_updated,
               "Total Participants - White, One Race" = total_white_onerace,
               "Total Participants - White, Multirace" = total_white_multirace,
               "Participants Race - Native American" = participants_race_nativeamerican,
               "Participants Race - Asian" = participants_race_asian,
               "Participants Race - Black" = participants_race_black,
               "Participants Race - Hawaiian/Pacific Islander" = participants_race_hawpac,
               "Participants Race - White" = participants_race_white,
               "Participants Race - Two or More" = participants_race_two_or_more,
               "Participants Race - No Response" = participants_race_no_response,
               "Participants Race - Unknown" = participants_race_unknown))

                                          "Total Participants of Unknown Ethnicity" = total_unknown_ethnicity
                                          "Participants Ethnicity - Prefer Not To Respond" = test_do_not_reference
                                          "Total Participants of Native American Descent - One Race" = total_onerace_native_american
                                          "Total Participants of Native American Descent - Multirace" = total_multirace_native_american
                                          "Total Participants Asian Descent - One Race" = total_asian_onerace
                                          "Total Participants of Asian Descent - Multirace" = total_asian_multirace
                                          "Total Participants - African American, One Race" = total_black_onerace
                                          "Total Participants - African American, Multirace" = total_black_multirace
                                          "Total Participants - Hawaiian/Pacific Islander - One Race" = total_hawpac_onerace
                                          "Total Participants - Hawaiian/Pacific Islander - Multirace" = total_hawpac_multirace_updated
                                          "Total Participants - White, One Race" = total_white_onerace
                                          "Total Participants - White, Multirace" = total_white_multirace
                                          "Participants Race - Native American" = participants_race_nativeamerican
                                          "Participants Race - Asian" = participants_race_asian
                                          "Participants Race - Black" = participants_race_black
                                          "Participants Race - Hawaiian/Pacific Islander" = participants_race_hawpac
                                          "Participants Race - White" = participants_race_white
                                          "Participants Race - Two or More" = participants_race_two_or_more
                                          "Participants Race - No Response" = participants_race_no_response
                                          "Participants Race - Unknown" = participants_race_unknown


row.names(ethnicity_totals_statewide) <- c("Total Hispanic Participation", "Total Non-Hispanic Participation",
                                           "Total Participation - Unknown Ethnicity", "Total Native American Participation - One Race",
                                           "Total Native American Participation - Multirace", "Total Asian Participation - One Race",
                                           "Total Asian Participation - Multirace", "Total African American Participation - One Race",
                                           "Total African American Participation - Multirace", "Total Hawaiian/Pacific Islander Participation - One Race",
                                           "Total Hawaiian/Pacific Islander Participation - Multirace", "Total White Participation - One Race",
                                           "Total White Participation - Multirace", "Attendees Reported as Native American", "Attendees Reported as Asian", "Attendees Reported as African American", "Attendees Reported as Hawaiian/Pacific Islander",
                                           "Attendees Reported as White", "Attendees Reported as having Two or More Races", "No Response", "Unknown")

# Change specific column name
print(colnames(ethnicity_totals_statewide))
colnames(ethnicity_totals_statewide) <- c("Ethnicities","Totals")
ncol(ethnicity_totals_statewide)

#GOT IT TO WORK BELOW

colnames(ethnicity_totals_statewide) <- "Totals"

new_column_values <- c("Total Hispanic Participation", "Total Non-Hispanic Participation",
                       "Total Participation - Unknown Ethnicity", "Total Native American Participation - One Race",
                       "Total Native American Participation - Multirace", "Total Asian Participation - One Race",
                       "Total Asian Participation - Multirace", "Total African American Participation - One Race",
                       "Total African American Participation - Multirace", "Total Hawaiian/Pacific Islander Participation - One Race",
                       "Total Hawaiian/Pacific Islander Participation - Multirace", "Total White Participation - One Race",
                       "Total White Participation - Multirace", "Attendees Reported as Native American", "Attendees Reported as Asian", "Attendees Reported as African American", "Attendees Reported as Hawaiian/Pacific Islander",
                       "Attendees Reported as White", "Attendees Reported as having Two or More Races", "No Response", "Unknown")

new_sheet <- cbind.data.frame(new_column_values, ethnicity_totals_statewide)

new_sheet2 <- rbind(new_sheet, ethnicity_totals_statewide)


# June 10 2025 ------------------------------------------------------------

#WILL BE WORKING TO DRAW IN MORE CENSUS VARIABLES, AND OVERLAY
#OVERLAY THEM WITH 4-H ATTENDANCE, AND ACTION PLAN ATTENDANCE
#BY ETHNICITY

#CHANGING THE YEARS COLUMN IN THIS EXCEL SHEET

# Define mapping vector
date_mapping <- c("4/1/2020", "7/1/2020", "7/1/2021", "7/1/2022", "7/1/2023")


# Assuming your data frame is called df and the column is 'years'
va_demographics$YEAR <- date_mapping[va_demographics$YEAR]

install.packages("openxlsx")
library(openxlsx)

setwd("C:\Users\jeffr\Desktop\diego_and_jeffrey_initial_vce_data")

getwd()
write.xlsx(va_demographics, "va_demographics_updated.xlsx")

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