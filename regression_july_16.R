install.packages("lme4")
install.packages("car")
install.packages("kableExtra")
install.packages("broom")

library(lme4)
library(car)
library(openxlsx)
library(broom)
library(kableExtra)
getwd()
setwd("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\VCEDashboard")

library(dplyr)

merged_data <- read.xlsx("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\VCEDashboard\\merged_data.xlsx")
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

model <- lm(log(Participants +1) ~ log(`Average.Household.Size`) + 
              log(`Median.Household.Income` +1) + 
              log(`Average.Household.Size.of.Owned.Dwellings` +1) +
              log(`Number.of.Dwellings.That.are.Owned` +1) +
              log(`Average.Household.Size.of.Rented.Dwellings` +1) +
              `Percent.Got.A.Bachelors.Degree` +
              `Percent.Did.Not.Graduate.College` +
              `Percent.of.People.who.Have.Monthly.Costs.That.Are.At.Least.35%.of.Income` +
              `Percent.of.Families.With.One.or.More.Children.Under.18.y/o` +
              `Percent.Only.Graduated.High.School` +
              `Percent.Not.Educated.Past.Middle.School` +
              `Percent.of.Families.in.Poverty.who.Have.Children` +
              `Percent.of.Married.Couples.Who.Have.Children` +
              `Percent.Did.Not.Graduate.High.School` +
              `Percent.of.People.16.and.Older.Who.Are.Employed` +
              `Percent.of.People.16.and.Older.Who.Are.Unemployed` +
              `Percent.of.Families.Where.Both.Parents.Work` +
              `Percentage.of.Homes.with.a.Mortgage.Between.$1500.and.$1999` +
              `Percentage.of.Homes.with.a.Mortgage.$3000.or.more` +
              `Percentage.of.Homes.with.a.Mortgage.Under.$500` +
              `Percent.of.Divorced.Men` +
              `Percent.of.Divorced.Women` +
              `Percent.of.Dwellings.That.are.Rented` +
              `Percent.of.Single.Fathers` +
              `Percent.of.Single.Mothers` +
              `Percentage.of.Asian/Pacific.Islander.Language.Speakers` +
              `Percentage.of.Spanish.Speakers` 
            , data = merged_data)

summary(model)

stargazer(model, type = "text")


# === MAKING IT LOOK PRETTIER ===



  model <- lm(log(Participants + 1) ~ 
                log(`Average.Household.Size`) + 
                log(`Median.Household.Income` + 1) + 
                log(`Average.Household.Size.of.Owned.Dwellings`) +
                log(`Number.of.Dwellings.That.are.Owned`) +
                log(`Average.Household.Size.of.Rented.Dwellings`) +
                `Percent.Got.A.Bachelors.Degree` +
                `Percent.Did.Not.Graduate.College` +
                `Percent.of.People.who.Have.Monthly.Costs.That.Are.At.Least.35%.of.Income` +
                `Percent.of.Families.With.One.or.More.Children.Under.18.y/o` +
                `Percent.Only.Graduated.High.School` +
                `Percent.Not.Educated.Past.Middle.School` +
                `Percent.of.Families.in.Poverty.who.Have.Children` +
                `Percent.of.Married.Couples.Who.Have.Children` +
                `Percent.Did.Not.Graduate.High.School` +
                `Percent.of.People.16.and.Older.Who.Are.Employed` +
                `Percent.of.People.16.and.Older.Who.Are.Unemployed` +
                `Percent.of.Families.Where.Both.Parents.Work` +
                `Percentage.of.Homes.with.a.Mortgage.Between.$1500.and.$1999` +
                `Percentage.of.Homes.with.a.Mortgage.$3000.or.more` +
                `Percentage.of.Homes.with.a.Mortgage.Under.$500` +
                `Percent.of.Divorced.Men` +
                `Percent.of.Divorced.Women` +
                `Percent.of.Dwellings.That.are.Rented` +
                `Percent.of.Single.Fathers` +
                `Percent.of.Single.Mothers` +
                `Percentage.of.Asian/Pacific.Islander.Language.Speakers` +
                `Percentage.of.Spanish.Speakers`,
              data = merged_data)
  
  stargazer(model, type = "text")

  # Get tidy output
  tidy_results <- tidy(model)
  
  # Add significance markers based on p-value thresholds
  tidy_results <- tidy_results %>%
    mutate(
      significance = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      ),
      # Append significance symbols to term labels
      term_display = paste0(term, significance),
      # Round estimates and standard errors for clarity
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 2),
      p.value_display = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
    )
  
  # Create a formatted table with highlights for significant variables
  kable_table <- tidy_results %>%
    select(term_display, estimate, std.error, statistic, p.value_display) %>%
    kable(caption = "Regression Results") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) 

# === HIGHLIGHTING STATISTICALLY SIGNIFICANT ROWS ===
  
  positively_significant <- c(5, 12 )
  
  kable_table <- kable_table %>%
    row_spec(positively_significant, background = "green") 

  negatively_significant <- c(20, 28)
  
  kable_table <- kable_table %>%
    row_spec(negatively_significant, background = "red")

  kable_table


