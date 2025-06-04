"Hi Diego"

"This is a test"

#Uploading Excel Files and Editing the Header Names
library(dplyr)
library(tidyverse)

install.packages("readxl")
library(readxl)

install.packages("openxlsx")
library(openxlsx)

data <- read.xlsx("Participation_2021_2022.xlsx")

print(colnames(`Participation 2021 - 2022`))

#Renaming all of the column headers in accordance with the PDF VCE sent us

library(dplyr)
`Participation 2021 - 2022` <- `Participation 2021 - 2022` %>%
  rename(
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
    "Youth Participating in Instructional TV/Video/Web Programs" = "m")

`Participation 2021 - 2022` <- `Participation 2021 - 2022` %>%
  rename("County Totals" = "total")
  

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
    "County Totals" = "total")

Participation_2022_2023 <- Participation_2022_2023 %>%
  rename(
    "County" = "CountyArea")
  
`Participation 2021 - 2022` <- `Participation 2021 - 2022` %>%
  rename(
    "County" = "CountyArea")

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

#NOW, GROUP COUNTIES AND INDEPENDENT CITIES TOGETHER BY REGION

library(readxl)
library(dplyr)

install.packages("writexl")

library(writexl)







  

