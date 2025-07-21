install.packages("lme4")
install.packages("car")

rm(merged_data)
library(lme4)
library(car)
library(openxlsx)
getwd()
setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Intermediate Data")

library(dplyr)

volunteers_participants_2025 <- volunteers_participants_2025 %>%
  mutate(Counties = ifelse(Counties == "James", "James City", Counties))

# === STANDARDIZING THE COUNTY COLUMNS
write.xlsx(volunteers_participants_2025, "volunteers_participants_2025.xlsx")
write.xlsx(independent_variables, "independent_variables.xlsx")

volunteers_participants_2025 <- volunteers_participants_2025 %>%
  rename("Counties" = "County")

# === SEEING WHICH COUNTIES ARE IN ONE DATA FRAME BUT NOT THE OTHER
# Find County entries in 'independent_variables' not present in 'volunteers_participants_2025'
missing_counties <- independent_variables %>%
  filter(!(County %in% volunteers_participants_2025$Counties)) %>%
  select(County)

# View the missing entries
print(missing_counties)


duplicate_names <- merged_data %>%
  group_by(Counties) %>%
  filter(n() > 1) %>%
  summarise(count = n()) %>%
  ungroup()

print(duplicate_names)

# Remove duplicates
rows_to_remove <- c(37, 38, 45, 46, 104, 105, 108, 109)
merged_data <- merged_data[-rows_to_remove, ]



merged_data <- subset(merged_data, select = -Volunteers)


write.xlsx(merged_data, "regression_data_july_16.xlsx")


# === PUTTING EVERYTHING TOGETHER ===

library(dplyr)
library(stringr)

#STEP 1: STANDARDIZE THE NAMES
independent_variables_copy <- independent_variables_copy %>%
  mutate(County = str_replace(County, "\\s*,\\s*Virginia$", "")) %>%
  mutate(County = str_replace(County, "\\s*(County|city)\\s*$", "")) %>%
  mutate(County = str_trim(County))



independent_variables <- independent_variables %>%
  mutate(County = str_replace(County, "\\s*,\\s*Virginia$", "")) %>%
  mutate(County = str_replace(County, "\\s*(County|city)\\s*$", "")) %>%
  mutate(County = str_trim(County))



# Merge on the 'County' and 'Counties' columns
merged_data <- volunteers_participants_2025 %>%
  inner_join(independent_variables, by = c("Counties" = "County"))



# === RUNNING THE INITIAL REGRESSION ===

# Loop through predictor variables
for (col in names(merged_data)[3:30]) {
  # Enclose in backticks if necessary
  var_name <- if (grepl(" ", col)) paste0("`", col, "`") else col
  formula <- as.formula(paste("Participants ~", var_name))
  model <- lm(formula, data = merged_data)
  cat("Regression of Participants on", col, ":\n")
  print(summary(model))
  cat("\n-------------------------\n\n")
}

merged_data <- merged_data %>%
  rename("Percent Got A Bachelors Degree" = "Percent Got a Bachelor's Degree")

# === REVISING THE REGRESSION ===

model <- lm(log(Participants +1) ~ log(`Average Household Size`) + 
              log(`Median Household Income` +1) + 
              log(`Average Household Size of Owned Dwellings` +1) +
              log(`Number of Dwellings That are Owned` +1) +
              log(`Average Household Size of Rented Dwellings` +1) +
              `Percent Got A Bachelors Degree` +
              `Percent Did Not Graduate College` +
              `Percent of People who Have Monthly Costs That Are At Least 35% of Income` +
              `Percent of Families With One or More Children Under 18 y/o` +
              `Percent Only Graduated High School` +
              `Percent Not Educated Past Middle School` +
              `Percent of Families in Poverty who Have Children` +
              `Percent of Married Couples Who Have Children` +
              `Percent Did Not Graduate High School` +
              `Percent of People 16 and Older Who Are Employed` +
              `Percent of People 16 and Older Who Are Unemployed` +
              `Percent of Families Where Both Parents Work` +
              `Percentage of Homes with a Mortgage Between $1500 and $1999` +
              `Percentage of Homes with a Mortgage $3000 or more` +
              `Percentage of Homes with a Mortgage Under $500` +
              `Percent of Divorced Men` +
              `Percent of Divorced Women` +
              `Percent of Dwellings That are Rented` +
              `Percent of Single Fathers` +
              `Percent of Single Mothers` +
              `Percentage of Asian/Pacific Islander Language Speakers` +
              `Percentage of Spanish Speakers` 
            , data = merged_data)

summary(model)

stargazer(model, type = "text")
