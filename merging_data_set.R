#RENAME THE COLUMN HEADERS TAKE TWO BECAUSE APPARENTLY I CANNOT PROPERLY SAVE MY FILES

#2024 - 2025
Participation_2024_2025_thru_June_2 <- Participation_2024_2025_thru_June_2 %>%
  rename(
    "Youth Members of Organized 4-H Community Clubs" = "a",
    "Youth Members of Organized 4-H In-School Clubs" = "b",
    "Youth Members of Organized 4-H After School Clubs" = "c",
    "Youth Members of Military 4-H Clubs" = "d",
    "Total 4-H Club Membership" = "e",
    "Youth Participating in 4-H Special Interest/Short-Term Programs" = "f",
    "Youth Participating in 4-H Overnight Camping Programs" = "g",
    "Youth Participating in 4-H Day Camping Programs" = "h",
    "Total Youth Participating in 4-H Camping Programs" = "i",
    "Youth Participating in School Enrichment Programs" = "j",
    "Youth Participating in Individual Study/Mentoring/Family Learning Programs" = "k",
    "Youth Participating in After-School Programs Using 4-H Curricula/Staff Training" = "l",
    "Youth Participating in Instructional TV/Video/Web Programs" = "m",
    "County Totals" = "total",
    "County" = "CountyArea")

#2021 - 2022

Participation_2021_2022 <- Participation_2021_2022 %>%
  rename(
    "Youth Members of Organized 4-H Community Clubs" = "a",
    "Youth Members of Organized 4-H In-School Clubs" = "b",
    "Youth Members of Organized 4-H After School Clubs" = "c",
    "Youth Members of Military 4-H Clubs" = "d",
    "Total 4-H Club Membership" = "e",
    "Youth Participating in 4-H Special Interest/Short-Term Programs" = "f",
    "Youth Participating in 4-H Overnight Camping Programs" = "g",
    "Youth Participating in 4-H Day Camping Programs" = "h",
    "Total Youth Participating in 4-H Camping Programs" = "i",
    "Youth Participating in School Enrichment Programs" = "j",
    "Youth Participating in Individual Study/Mentoring/Family Learning Programs" = "k",
    "Youth Participating in After-School Programs Using 4-H Curricula/Staff Training" = "l",
    "Youth Participating in Instructional TV/Video/Web Programs" = "m",
    "County Totals" = "total",
    "County" = "CountyArea")

#2022 - 2023
Participation_2022_2023 <- Participation_2022_2023 %>%
  rename(
    "Youth Members of Organized 4-H Community Clubs" = "a",
    "Youth Members of Organized 4-H In-School Clubs" = "b",
    "Youth Members of Organized 4-H After School Clubs" = "c",
    "Youth Members of Military 4-H Clubs" = "d",
    "Total 4-H Club Membership" = "e",
    "Youth Participating in 4-H Special Interest/Short-Term Programs" = "f",
    "Youth Participating in 4-H Overnight Camping Programs" = "g",
    "Youth Participating in 4-H Day Camping Programs" = "h",
    "Total Youth Participating in 4-H Camping Programs" = "i",
    "Youth Participating in School Enrichment Programs" = "j",
    "Youth Participating in Individual Study/Mentoring/Family Learning Programs" = "k",
    "Youth Participating in After-School Programs Using 4-H Curricula/Staff Training" = "l",
    "Youth Participating in Instructional TV/Video/Web Programs" = "m",
    "County Totals" = "total",
    "County" = "CountyArea")

#2023 - 2024

Participation_2023_2024 <- Participation_2023_2024 %>%
  rename(
    "Youth Members of Organized 4-H Community Clubs" = "a",
    "Youth Members of Organized 4-H In-School Clubs" = "b",
    "Youth Members of Organized 4-H After School Clubs" = "c",
    "Youth Members of Military 4-H Clubs" = "d",
    "Total 4-H Club Membership" = "e",
    "Youth Participating in 4-H Special Interest/Short-Term Programs" = "f",
    "Youth Participating in 4-H Overnight Camping Programs" = "g",
    "Youth Participating in 4-H Day Camping Programs" = "h",
    "Total Youth Participating in 4-H Camping Programs" = "i",
    "Youth Participating in School Enrichment Programs" = "j",
    "Youth Participating in Individual Study/Mentoring/Family Learning Programs" = "k",
    "Youth Participating in After-School Programs Using 4-H Curricula/Staff Training" = "l",
    "Youth Participating in Instructional TV/Video/Web Programs" = "m",
    "County Totals" = "total",
    "County" = "CountyArea")

#NOW ATTEMPTING TO MERGE ALL FOUR EXCEL SHEETS INTO ONE VERTICAL SHEET
#THE LAYOUT WILL BE (LEFT - RIGHT) COUNTY, PROGRAMS YEAR

#OPENING AND INSTALLING NECESSARY PACKAGES

library(tidyverse)
library(dplyr)
library(readxl)
install.packages("purrr")
library(purrr)

#BEGINNING MERGE
#SETTING WORKING DIRECTORY TO WHERE THE EXCEL FILES ARE LOCATED (DOWNLOADS)

setwd("/Users/jeffreyogle/Participation_2021_2022.xlsx")

#FIXING ERROR BY HAVING IT JUST READ ALL OF MY DOWNLOADS BY USING THE LIST.FILES COMMAND

setwd("\\Users\\JeffreyOgle\\Downloads")
list.files()

#SELECTING FILES NOW

df_21_22 <- read_excel("Participation_2021_2022.xlsx", sheet = "Participation Count") %>%
  mutate(Year = "2021-2022")
df_22_23 <- read_excel("Participation_2022_2023.xlsx", sheet = "Participation Count") %>%
  mutate(Year = "2022-2023")
df_23_24 <- read_excel("Participation_2023_2024.xlsx", sheet = "Participation Count") %>%
  mutate(Year = "2023-2024")
df_24_25 <- read_excel("Participation_2024_2025_thru_June_2.xlsx", sheet = "Participation Count") %>%
  mutate(Year = "2024-2025")

#MERGING DATA

merged_df <- bind_rows(df_21_22, df_22_23, df_23_24, df_24_25)

#RENAMING THE COLUMN HEADERS IN THE NEW MERGED_DF SHEET

merged_df <- merged_df %>%
  rename(
    "Youth Members of Organized 4-H Community Clubs" = "a",
    "Youth Members of Organized 4-H In-School Clubs" = "b",
    "Youth Members of Organized 4-H After School Clubs" = "c",
    "Youth Members of Military 4-H Clubs" = "d",
    "Total 4-H Club Membership" = "e",
    "Youth Participating in 4-H Special Interest/Short-Term Programs" = "f",
    "Youth Participating in 4-H Overnight Camping Programs" = "g",
    "Youth Participating in 4-H Day Camping Programs" = "h",
    "Total Youth Participating in 4-H Camping Programs" = "i",
    "Youth Participating in School Enrichment Programs" = "j",
    "Youth Participating in Individual Study/Mentoring/Family Learning Programs" = "k",
    "Youth Participating in After-School Programs Using 4-H Curricula/Staff Training" = "l",
    "Youth Participating in Instructional TV/Video/Web Programs" = "m",
    "County Totals" = "total",
    "County" = "CountyArea")

#INSTALLING WRITEXL

install.packages("writexl")

#SAVING MERGED_DF SHEET INTO DOWNLOADS

library(writexl)
write_xlsx(merged_df, "Combined_Participation_Counts.xlsx")

getwd()
