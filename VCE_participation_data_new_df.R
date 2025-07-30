library(readr)
rm(annualprogreport, particip_combined, PEARS_Raw_Data_5_23_25, rows_to_remove)
vce_particip_demographics <- read.csv("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\VCEDashboard\\newcountiesdemographics.csv")

vce_particip_demographics <- vce_particip_demographics[, !(names(vce_particip_demographics) %in% c("X", "site_name", "site_city", "site_zip", "site_setting",
                                                                                                   "number_sessions", "num_volunteers", "total_volunteer_hours",
                                                                                                   "participants_gender_male", "participants_gender_female",
                                                                                                   "participants_gender_non_binary", "participants_gender_prefer_not_to_respond",
                                                                                                   "participants_age_youth", "participants_age_1to5", "participants_age_5to7",
                                                                                                   "participants_age_8to10", "participants_age_11to13", "participants_age_14to17",
                                                                                                   "participants_age_18to29", "participants_age_30to59",
                                                                                                   "participants_age_60to75", "participants_age_76plus",
                                                                                                   "participants_age_adult", "participants_ethnicity_non_hispanic",
                                                                                                   "participants_ethnicity_prefer_not_to_respond",
                                                                                                   "participants_ethnicity_unknown", "participants_amerind_onerace_total",
                                                                                                   "participants_amerind_multirace_total", "participants_hawpac_onerace_total",
                                                                                                   "participants_hawpac_multirace_total", "participants_asian_onerace_total",
                                                                                                   "participants_asian_multirace_total", "participants_black_onerace_total",
                                                                                                   "participants_black_multirace_total", "participants_white_onerace_total",
                                                                                                   "participants_white_multirace_total", "participants_race_amerind",
                                                                                                   "participants_race_hawpac", "participants_race_two_or_more",
                                                                                                   "participants_race_prefer_not_to_respond", "participants_race_unknown"))]

vce_particip_demographics <- vce_particip_demographics[, ! (names(vce_particip_demographics) %in% c("participants_age_lt5"))]


library(openxlsx)

write.xlsx(vce_particip_demographics, "vce_particip_demographics.xlsx")
getwd()


vce_particip_demographics[is.na(vce_particip_demographics)] <- 0

print(vce_particip_demographics)
