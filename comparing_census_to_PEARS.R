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



