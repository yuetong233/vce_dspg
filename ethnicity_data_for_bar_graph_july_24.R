library(dplyr)
library(tidycensus)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(readxl)
library(readr)

hispanic <- get_acs(
  geography = 'state',
  year = 2023,
  state = 'VA',
  variables = c('DP05_0070E')
)

black <- get_acs(
  geography = 'state',
  year = 2023,
  state = 'VA',
  variables = c('DP05_0038E')
)

white <- get_acs(
  geography = 'state',
  year = 2023,
  state = 'VA',
  variables = c('DP05_0037E')
)

asian <- get_acs(
  geography = 'state',
  year = 2023,
  state = 'VA',
  variables = c('DP05_0045E', 'DP05_0046E',
                'DP05_0047E', 'DP05_0048E', 'DP05_0049E',
                'DP05_0050E', 'DP05_0051E')
)


asian <- sum(asian$estimate, na.rm = TRUE)

asian <- data.frame(Total = asian)


rm(asian1)


asian_participants <- sum(newcountiesdemographics$participants_race_asian, na.rm = TRUE)
asian_participants <- data.frame(Total = asian_participants)

white_participants <- sum(newcountiesdemographics$participants_race_white, na.rm = TRUE)
white_participants <- data.frame(Total = white_participants)

black_participants <- sum(newcountiesdemographics$participants_race_black, na.rm = TRUE)
black_participants <- data.frame(Total = black_participants)

hispanic_participants <- sum(newcountiesdemographics$participants_ethnicity_hispanic, na.rm = TRUE)
hispanic_participants <- data.frame(Total = hispanic_participants)


white <- white %>% select(-1, -2, -3, -5)
black <- black %>% select(-1, -2, -3, -5)
hispanic <- hispanic %>% select(-1, -2, -3, -5)

white <- white %>%
  rename("Total" = "estimate")

black <- black %>%
  rename("Total" = "estimate")

hispanic <- hispanic %>%
  rename("Total" = "estimate")


# === FIRST DRAFT OF BAR GRAPH ===

population_participation_VCE_july_24 <- read_excel("population_participation_VCE_july_24.xlsx", sheet = "Sheet1")  # Adjust sheet name if needed

long_data <- data %>%
  gather(key = "Type", value = "Count", Population, Participants)

ggplot(long_data, aes(x = Group, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Population and Participants by Group",
       x = "Group",
       y = "Count",
       fill = "Type") +
  theme_minimal()


# === SECOND DRAFT OF BAR GRAPH ===


data <- read_excel("population_participation_VCE_july_24.xlsx", sheet = "Sheet1")  # Adjust sheet name if needed

long_data <- data %>%
  gather(key = "Type", value = "Count", Population, Participants)

ggplot(long_data, aes(x = Group, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_y_log10() +  # Set y-axis to logarithmic scale
  labs(title = "Population and Participants by Group",
       x = "Group",
       y = "Count (log scale)",
       fill = "Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Participants" = "red", "Population" = "cyan")) +
  theme(panel.grid.major = element_line(color = "grey80"))


# === THIRD DRAFT OF BAR GRAPH ===


df <- read_excel("population_participation_VCE_july_24.xlsx")
df <- df %>%
  mutate(ParticipantPercent = (Participants / Population) * 100)

ggplot(df, aes(x = Group, y = ParticipantPercent, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "VCE Participants as % of Racial/Ethnic Group in Virginia",    
    x = "Group",
    y = "Participation Percentage (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))