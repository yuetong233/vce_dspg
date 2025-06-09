install.packages("dplyr") 
install.packages("readxl")

library(dplyr)
library(readxl)

#Reading the excel file
df <- read_excel("~/Downloads/Better_Impact_Volunteer_Data_2024.xlsx")

getwd()
list.files()
# Standardize and categorize ethnicity responses
df_cleaned <- df %>%
  mutate(Ethnicity_Grouped = case_when(
    `CF - Demographic Information - Ethnicity` %in% c("Hispanic or Latino/a/x") ~ "Hispanic or Latino/a/x",
    `CF - Demographic Information - Ethnicity` %in% c("Not Hispanic or Latino/a/x") ~ "Not Hispanic or Latino/a/x",
    `CF - Demographic Information - Ethnicity` %in% c("Other", "Prefer not to answer", "Unknown", "Decline to Answer") ~ "Other/Prefer not to answer",
    TRUE ~ "Other/Prefer not to answer" # fallback for unexpected entries
  ))

# View summarized count by ethnicity category
ethnicity_summary <- df_cleaned %>%
  count(Ethnicity_Grouped, sort = TRUE)

print(ethnicity_summary)
#Up to here

