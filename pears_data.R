#READING IN THE PEARS RAW DATA, ACTION PLAN, AND OUTCOME INDICATORS

install.packages("readxl")

library(readxl)

pears_action_plan <- read_excel("PEARS Raw Data 5.23.25.xlsx", sheet = "Program Activities")

pears_action_plan_data <- read_excel("PEARS Action-Plans-Export 5.30.25.xlsx", sheet = "Action Plan Data")

pears_outcome_indicators <- read_excel("PEARS Action-Plans-Export 5.30.25.xlsx", sheet = "Outcome Indicators")

pears_raw_data_codebook <- read_excel("PEARS Raw Data 5.23.25.xlsx", sheet = "Program Activities Codebook")

pears_action_plan_codebook <- read_excel("PEARS Action-Plans-Export 5.30.25.xlsx", sheet = "Codebook")
