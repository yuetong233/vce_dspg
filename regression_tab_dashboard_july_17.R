library(broom)


# === SAVING THE MODEL AS TEXT ===

regression_results <- capture.output(summary(model))

# === TAB FOR DASHBOARD

tabPanel("Correlation Between Homelife and VCE Program Participation",
         h3("Regression Results"),
         verbatimTextOutput("regression_results")
)

# === SERVER OUTPUT ===


output$regression_results <- renderPrint({

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
  
  stargazer(model, type = "text")
})


install.packages("writexl")
library(openxlsx)

write.xlsx(merged_data, "merged_data.xlsx")
