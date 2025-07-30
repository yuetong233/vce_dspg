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

