# June 12 2025 ------------------------------------------------------------

#ATTEMPTING TO FURTHER CLEAN AND SUMMARIZE THE DATA

rm(summary_df)


# Summarize numeric columns by sum
summary_df <- counties_and_demographics_june_11 %>%
  group_by(`Event Site County`) %>%
  summarize(
    total_volunteers = sum(`Number of Volunteers`, na.rm = TRUE),
    total_hours = sum(`Volunteering Hours`, na.rm = TRUE),
    total_participants = sum(`Total Participants`, na.rm = TRUE),
    number_sessions = sum(`Number of Sessions`, na.rm = TRUE),
    male_participants = sum(`Male Participants`, na.rm = TRUE),
    female_participants = sum(`Female Participants`, na.rm = TRUE),
    non_binary_participants = sum(`Non-Binary Participants`, na.rm = TRUE),
    gender_not_respond = sum(`Gender Prefer Not to Respond`, na.rm = TRUE),
    age_1to5 = sum(`Aged 1-5`, na.rm = TRUE),
    age_5to7 = sum(`Aged 5-7`, na.rm = TRUE),
    age_8to10 = sum(`Aged 8-10`, na.rm = TRUE),
    age_11to13 = sum(`Aged 11-13`, na.rm = TRUE),
    age_14to17 = sum(`Aged 14-17`, na.rm = TRUE),
    age_18to29 = sum(`Aged 18-29`, na.rm = TRUE),
    age_30to59 = sum(`Aged 30-59`, na.rm = TRUE),
    age_60to75 = sum(`Aged 60-75`, na.rm = TRUE),
    age_76over = sum(`Aged 76+`, na.rm = TRUE),
    hispanic_particip = sum(`Total Hispanic Population`, na.rm = TRUE),
    non_hispanic_particip = sum(`Total Non-Hispanic Population`, na.rm = TRUE),
    ethnicity_no_response = sum(`Ethnicity Prefer Not to Respond`, na.rm = TRUE),
    unknown_ethnicity = sum(`Total Participation - Unknown Ethnicity`, na.rm = TRUE),
    total_natam_one_particip = sum(`Total Native American Participation - One Race`, na.rm = TRUE),
    total_natam_multi_particip = sum(`Total Native American Participation - Multirace`, na.rm = TRUE),
    total_asian_one_particip = sum(`Total Asian Participation - One Race`, na.rm = TRUE),
    total_asain_multi_particip = sum(`Total Asian Participation - Multirace`, na.rm = TRUE),
    total_afram_one_particip = sum(`Total African American Participation - One Race`, na.rm = TRUE),
    total_afram_multi_particip = sum(`Total African American Participation - Multirace`, na.rm = TRUE),
    total_hawpac_one_particip = sum(`Total Hawaiian/Pacific Islander Participation - One Race`, na.rm = TRUE),
    total_hawpac_multi_particip = sum(`Total Hawaiian/Pacific Islander Participation - Multirace`, na.rm = TRUE),
    total_white_one_particip = sum(`Total White Participation - One Race`, na.rm = TRUE),
    total_white_multi_particip = sum(`Total White Participation - Multirace`, na.rm = TRUE),
  )


#IN THE ABOVE DATA FRAME, THE CODE IGNORED "NA" VALUES, AND DID NOT TAKE THEM
#INTO CONSIDERATION FOR CALCULATION

setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")
write.xlsx(summary_df, "cleaned_demographic_sheet_6.12.25.xlsx")

cleaned_demographic_sheet <- summary_df

cleaned_demographic_sheet <- cleaned_demographic_sheet %>%
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
#per volunteer. The new column is necessary because, e.g., none of the first 
#lines has one volunteer and no hours. Adding this column lets you determine 
#average hours volunteered in an unbiased way.

#Are the data for age groups for volunteers or participants? Just make this clear
#in your notes (which I’m sure you already have).

#Merge population data from the ACS so you can calculate volunteer and participation 
#rates.


# -------------------------------------------------------------------------
