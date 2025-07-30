#JUNE 17, 2025 - LOOKING FOR DISCRPENCIES IN THE DATA TO SEE WHY
#THERE IS NO PARTICIPATION DATA IN SO MANY COUNTIES

setwd("C:\\Users\\jeffr\\Desktop\\Data For Dashboard")

getwd()

DF1_county_demographics <- read.csv("countiesdemographics.csv")
DF2_VCE_particip_ethnicities_county <- read.csv("annualprogreport.csv")
DF3_volunteer_demographics <- read.csv("countiesimpact.csv")
DF4_virginia_population <- read.csv("virginia2024population.csv")

rm(DF1, DF2, DF3, DF4)

#GOING TO WHITTLE IT DOWN TO JUST PARTICIPANTS WHO ARE IN THE AGE RANGE
#TO BE PARTICIPATING IN VCE PROGRAMS

rm(swiss, ui, mtcars, model_multi, model, clean_lines, lines, clean_county, server)
rm(countiesdemographics.csv)
