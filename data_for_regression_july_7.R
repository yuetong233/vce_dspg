library(tidycensus)
library(dplyr)

#TEST
data <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0047PE'))


#PULLING ACS DATA

household_size <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0016E'))
household_one_or_more_under_18 <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0014PE'))
percent_owner_occupied_units <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0090PE'))
percent_mortgage_under_500 <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0094PE'))
percent_mortgage_3000_or_more <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0100PE'))
percent_mortgage_1500_1999 <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0097PE'))
percent_renter_occupied_unit <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0047PE'))
owner_occupied_average_hh_size <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0049E'))
renter_occupied_average_hh_size <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0049E'))
median_household_income <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP03_0062E'))
percent_16_and_over_employed <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP03_0004PE'))
percent_16_and_over_unemployed <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP03_0005PE'))
percent_families_both_parents_work <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP03_0017PE'))
less_than_9th_grade <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0060PE'))
no_hs_diploma <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0061PE'))
hs_diploma <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0062PE'))
college_dropout <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0063PE'))
bachelors_degree <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0065PE'))
cost_burden_35_percent_or_more <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP04_0124PE'))
married_families_with_children_under_18_below_poverty_line <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP03_0123PE'))
single_mothers_with_children <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0011PE'))
single_fathers_with_children <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0007PE'))
married_with_children_under_18 <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0003PE'))
speaks_spanish_at_home <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0115PE'))
speaks_indo_european_language_at_home <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0117PE'))
speaks_asian_pacisl_language_at_home <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0119PE'))
percent_of_women_divorced <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0036PE'))
percent_of_men_divorced <- get_acs(geography = 'county', year = 2023, state = "VA", variables = c('DP02_0030PE'))



# -------------------------------------------------------------------------
# === REMOVING UNNECESSARY COLUMNS ===


household_size <- household_size[ , !(names(household_size) %in% c("GEOID", "moe", "variable"))]
household_one_or_more_under_18 <- household_one_or_more_under_18[ , !(names(household_one_or_more_under_18) %in% c("GEOID", "moe", "variable"))]
percent_owner_occupied_units <- percent_owner_occupied_units[ , !(names(percent_owner_occupied_units) %in% c("GEOID", "moe", "variable"))]
percent_mortgage_under_500 <- percent_mortgage_under_500[ , !(names(percent_mortgage_under_500) %in% c("GEOID", "moe", "variable"))]
percent_mortgage_3000_or_more <- percent_mortgage_3000_or_more[ , !(names(percent_mortgage_3000_or_more) %in% c("GEOID", "moe", "variable"))]
percent_mortgage_1500_1999 <- percent_mortgage_1500_1999[ , !(names(percent_mortgage_1500_1999) %in% c("GEOID", "moe", "variable"))]
percent_renter_occupied_unit <- percent_renter_occupied_unit[ , !(names(percent_renter_occupied_unit) %in% c("GEOID", "moe", "variable"))]
owner_occupied_average_hh_size <- owner_occupied_average_hh_size[ , !(names(owner_occupied_average_hh_size) %in% c("GEOID", "moe", "variable"))]
renter_occupied_average_hh_size <- renter_occupied_average_hh_size[ , !(names(renter_occupied_average_hh_size) %in% c("GEOID", "moe", "variable"))]
median_household_income <- median_household_income[ , !(names(median_household_income) %in% c("GEOID", "moe", "variable"))]
percent_16_and_over_employed <- percent_16_and_over_employed[ , !(names(percent_16_and_over_employed) %in% c("GEOID", "moe", "variable"))]
percent_16_and_over_unemployed <- percent_16_and_over_unemployed[ , !(names(percent_16_and_over_unemployed) %in% c("GEOID", "moe", "variable"))]
percent_families_both_parents_work <- percent_families_both_parents_work[ , !(names(percent_families_both_parents_work) %in% c("GEOID", "moe", "variable"))]
less_than_9th_grade <- less_than_9th_grade[ , !(names(less_than_9th_grade) %in% c("GEOID", "moe", "variable"))]
no_hs_diploma <- no_hs_diploma[ , !(names(no_hs_diploma) %in% c("GEOID", "moe", "variable"))]
bachelors_degree <- bachelors_degree[ , !(names(bachelors_degree) %in% c("GEOID", "moe", "variable"))]
college_dropout <- college_dropout[ , !(names(college_dropout) %in% c("GEOID", "moe", "variable"))]
hs_diploma <- hs_diploma[ , !(names(hs_diploma) %in% c("GEOID", "moe", "variable"))]
cost_burden_35_percent_or_more <- cost_burden_35_percent_or_more[ , !(names(cost_burden_35_percent_or_more) %in% c("GEOID", "moe", "variable"))]
married_families_with_children_under_18_below_poverty_line <- married_families_with_children_under_18_below_poverty_line[ , !(names(married_families_with_children_under_18_below_poverty_line) %in% c("GEOID", "moe", "variable"))]
married_with_children_under_18 <- married_with_children_under_18[ , !(names(married_with_children_under_18) %in% c("GEOID", "moe", "variable"))]
single_fathers_with_children <- single_fathers_with_children[ , !(names(single_fathers_with_children) %in% c("GEOID", "moe", "variable"))]
single_mothers_with_children <- single_mothers_with_children[ , !(names(single_mothers_with_children) %in% c("GEOID", "moe", "variable"))]
speaks_asian_pacisl_language_at_home <- speaks_asian_pacisl_language_at_home[ , !(names(speaks_asian_pacisl_language_at_home) %in% c("GEOID", "moe", "variable"))]
speaks_indo_european_language_at_home <- speaks_indo_european_language_at_home[ , !(names(speaks_indo_european_language_at_home) %in% c("GEOID", "moe", "variable"))]
speaks_spanish_at_home <- speaks_spanish_at_home[ , !(names(speaks_spanish_at_home) %in% c("GEOID", "moe", "variable"))]
percent_of_men_divorced <- percent_of_men_divorced[ , !(names(percent_of_men_divorced) %in% c("GEOID", "moe", "variable"))]
percent_of_women_divorced <- percent_of_women_divorced[ , !(names(percent_of_women_divorced) %in% c("GEOID", "moe", "variable"))]

rm(df, speaks_filtered, combined_df)

# === RENAMING VARIABLE COLUMN(S) ===


household_size <- household_size %>%
  rename("Average Household Size" = "estimate")
household_one_or_more_under_18 <- household_one_or_more_under_18 %>%
  rename("Percent of Families With One or More Children Under 18 y/o" = "estimate")
percent_16_and_over_employed <- percent_16_and_over_employed %>%
  rename("Percent of People 16 and Older Who Are Employed" = "estimate")
percent_16_and_over_unemployed <- percent_16_and_over_unemployed %>%
  rename("Percent of People 16 and Older Who Are Unemployed" = "estimate")
percent_families_both_parents_work <- percent_families_both_parents_work %>%
  rename("Percent of Families Where Both Parents Work" = "estimate")
percent_mortgage_1500_1999 <- percent_mortgage_1500_1999 %>%
  rename("Percentage of Homes with a Mortgage Between $1500 and $1999" = "estimate")
percent_mortgage_3000_or_more <- percent_mortgage_3000_or_more %>%
  rename("Percentage of Homes with a Mortgage $3000 or more" = "estimate")
percent_mortgage_under_500 <- percent_mortgage_under_500 %>%
  rename("Percentage of Homes with a Mortgage Under $500" = "estimate")
percent_of_men_divorced <- percent_of_men_divorced %>%
  rename("Percent of Divorced Men" = "estimate")
percent_of_women_divorced <- percent_of_women_divorced %>%
  rename("Percent of Divorced Women" = "estimate")
percent_owner_occupied_units <- percent_owner_occupied_units %>%
  rename("Number of Dwellings That are Owned" = "Percent of Dwellings That are Owned")
percent_renter_occupied_unit <- percent_renter_occupied_unit %>%
  rename("Percent of Dwellings That are Rented" = "estimate")
median_household_income <- median_household_income %>%
  rename("Median Household Income" = "estimate")
owner_occupied_average_hh_size <- owner_occupied_average_hh_size %>%
  rename("Average Household Size of Owned Dwellings" = "estimate")
renter_occupied_average_hh_size <- renter_occupied_average_hh_size %>%
  rename("Average Household Size of Rented Dwellings" = "estimate")
less_than_9th_grade <- less_than_9th_grade %>%
  rename("Percent Not Educated Past Middle School" = "Not Educated Past Middle School")
no_hs_diploma <- no_hs_diploma %>%
  rename("Percent Did Not Graduate High School" = "Did Not Graduate High School")
college_dropout <- college_dropout %>%
  rename("Percent Did Not Graduate College" = "Did Not Graduate College")
bachelors_degree <- bachelors_degree %>%
  rename("Percent Got a Bachelor's Degree" = "Got a Bachelor's Degree")
hs_diploma <- hs_diploma %>%
  rename("Percent Only Graduated High School" = "Only Graduated High School")
cost_burden_35_percent_or_more <- cost_burden_35_percent_or_more %>%
  rename("Percent of People who Have Monthly Costs That Are At Least 35% of Income" = "Monthly Costs Are At Least 35% of Income")
married_families_with_children_under_18_below_poverty_line <- married_families_with_children_under_18_below_poverty_line %>%
  rename("Percent of Families in Poverty who Have Children" = "estimate")
married_with_children_under_18 <- married_with_children_under_18 %>%
  rename("Percent of Married Couples Who Have Children" = "estimate")
single_fathers_with_children <- single_fathers_with_children %>%
  rename("Percent of Single Fathers" = "estimate")
single_mothers_with_children <- single_mothers_with_children %>%
  rename("Percent of Single Mothers" = "estimate")
speaks_asian_pacisl_language_at_home <- speaks_asian_pacisl_language_at_home %>%
  rename("Percentage of Asian/Pacific Islander Language Speakers" = "Percentage of Asian/Pacific Islander Languages")
speaks_indo_european_language_at_home <- speaks_indo_european_language_at_home %>%
  rename("Percentage of Indo/European Languages" = "estimate")
speaks_spanish_at_home <- speaks_spanish_at_home %>%
  rename("Percentage of Spanish Speakers" = "estimate")


# === REMOVE UNNECESSARY COLUMNS FROM ONE DATA FRAME ===

library(dplyr)

# Remove columns using select() with negative selection
speaks_asian_pacisl_language_at_home <- speaks_asian_pacisl_language_at_home %>% select(-Clean_NAME, -NAME_std)


# === JOINING DATA FRAMES ===

library(dplyr)
library(purrr)

all_dfs <- list(bachelors_degree, college_dropout, cost_burden_35_percent_or_more,
                household_one_or_more_under_18, household_size, 
                hs_diploma, less_than_9th_grade, married_families_with_children_under_18_below_poverty_line,
                married_with_children_under_18, median_household_income, no_hs_diploma, 
                owner_occupied_average_hh_size, percent_16_and_over_employed, percent_16_and_over_unemployed,
                percent_families_both_parents_work, percent_mortgage_1500_1999, 
                percent_mortgage_3000_or_more, percent_mortgage_under_500,
                percent_of_men_divorced, percent_of_women_divorced, percent_owner_occupied_units,
                percent_renter_occupied_unit, renter_occupied_average_hh_size, single_fathers_with_children,
                single_mothers_with_children, speaks_asian_pacisl_language_at_home, 
                speaks_indo_european_language_at_home, speaks_spanish_at_home)

# Merge all data frames by "NAME"
combined_df <- reduce(all_dfs, full_join, by = "NAME")

# Rename "NAME" column to "County"
colnames(combined_df)[colnames(combined_df) == "NAME"] <- "County"

# View the combined data frame
print(combined_df)


library(openxlsx)
setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\Clean Data")

write.xlsx(combined_df, "independent_variables.xlsx")

independent_variables <- combined_df
rm(combined_df)
