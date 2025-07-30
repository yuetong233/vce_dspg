rm(df_2021_2022, df_2022_2023, df_2023_2024, df_2024_2025, particip_2021_2022, particip_2022_2023, particip_2023_2024)

# Assuming your original data frame is named 'df'

# Filter rows based on the 'Year' column
df_2021_2022 <- subset(df, Year == "2021-2022")
df_2022_2023 <- subset(df, Year == "2022-2023")
df_2023_2024 <- subset(df, Year == "2023-2024")

# Select the common columns
common_cols <- c("County", "County Totals", "Region")

# Create the first data frame and add the specific column
particip_2021_2022 <- df_2021_2022[, c(common_cols, "Year")]
particip_2021_2022[["July 1, 2022"]] <- NA  # Initialize with NA or actual data if available

# Similarly for the second data frame
particip_2022_2023 <- df_2022_2023[, c(common_cols, "Year")]
particip_2022_2023[["July 1, 2023"]] <- NA

# And for the third data frame
particip_2023_2024 <- df_2023_2024[, c(common_cols, "Year")]
particip_2023_2024[["July 1, 2024"]] <- NA


# First, create the three data frames as before
particip_2021_2022 <- subset(df, Year == "2021-2022")
particip_2022_2023 <- subset(df, Year == "2022-2023")
particip_2023_2024 <- subset(df, Year == "2023-2024")

# For each data frame, add the new column
# Assuming 'Population' is the column with the data you want to assign

# For 2021-2022 data frame
particip_2021_2022[["July 1, 2022"]] <- participation_data_for_2021_2022

# For 2022-2023 data frame
particip_2022_2023[["July 1, 2023"]] <- participation_data_for_2022_2023

# For 2023-2024 data frame
particip_2023_2024[["July 1, 2024"]] <- participation_data_for_2023_2024

#REMOVING UNNECESSARY COLUMNS

# List of data frames
dfs <- list(particip_2021_2022, particip_2022_2023, particip_2023_2024)

# Columns to remove
cols_to_remove <- c("Youth Members of Organized 4-H Community Clubs", 
                    "Youth Members of Organized 4-H In-School Clubs",
                    "Youth Members of Organized 4-H After School Clubs",
                    "Youth Members of Military 4-H Clubs",
                    "Total 4-H Club Membership",
                    "Youth Participating in 4-H Special Interest/Short-Term Programs",
                    "Youth Participating in 4-H Overnight Camping Programs",
                    "Youth Participating in 4-H Day Camping Programs",
                    "Total Youth Participating in 4-H Camping Programs",
                    "Youth Participating in School Enrichment Programs",
                    "Youth Participating in Individual Study/Mentoring/Family Learning Programs",
                    "Youth Participating in After-School Programs Using 4-H Curricula/Staff Training",
                    "Youth Participating in Instructional TV/Video/Web Programs")

# Remove columns from all data frames
dfs <- lapply(dfs, function(df) {
  df[, !names(df) %in% cols_to_remove]
})

# Assign them back to your original variables
particip_2021_2022 <- dfs[[1]]
particip_2022_2023 <- dfs[[2]]
particip_2023_2024 <- dfs[[3]]

# Remove columns from particip_2021_2022
particip_2021_2022 <- particip_2021_2022[, 
                                         !names(particip_2021_2022) %in% c(
                                           "2020 Census", 
                                           "July 1, 2020", 
                                           "July 1, 2021", 
                                           "July 1, 2023", 
                                           "July 1, 2024"
                                         )
]

# Remove columns from particip_2022_2023
particip_2022_2023 <- particip_2022_2023[, 
                                         !names(particip_2022_2023) %in% c(
                                           "2020 Census", 
                                           "July 1, 2020", 
                                           "July 1, 2021", 
                                           "July 1, 2022", 
                                           "July 1, 2024"
                                         )
]

# Remove columns from particip_2023_2024
particip_2023_2024 <- particip_2023_2024[, 
                                         !names(particip_2023_2024) %in% c(
                                           "2020 Census", 
                                           "July 1, 2020", 
                                           "July 1, 2021", 
                                           "July 1, 2022", 
                                           "July 1, 2023"
                                         )
]

getwd()

write.xlsx(particip_2021_2022, "particip_2021_2022.xlsx")
write.xlsx(particip_2022_2023, "particip_2022_2023.xlsx")
write.xlsx(particip_2023_2024, "particip_2023_2024.xlsx")

particip_2021_2022 <- particip_2021_2022 %>%
  rename("Population" = "July 1, 2022")

particip_2022_2023 <- particip_2022_2023 %>%
  rename("Population" = "July 1, 2023")

particip_2023_2024 <- particip_2023_2024 %>%
  rename("Population" = "July 1, 2024")



# Making Graph ------------------------------------------------------------



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(plotly)

# Combine your three data frames into one
particip_combined <- bind_rows(
  particip_2021_2022,
  particip_2022_2023,
  particip_2023_2024
)

# Ensure 'Year' is a character for proper labeling
particip_combined$Year <- as.character(particip_combined$Year)

# Summarize data: aggregate by Year and Region
region_summary <- particip_combined %>%
  group_by(Year, Region) %>%
  summarise(
    Total_Participation = sum(`County Totals`),
    Total_Population = sum(Population),
    .groups = 'drop'
  ) %>%
  mutate(
    Participation_Percentage = ifelse(Total_Population == 0, 0, (Total_Participation / Total_Population) * 100)
  )

# Use darker, richer colors for each region
region_colors <- c(
  "Southeast" = "#006994",  # Darker Blue
  "Northeast" = "#B8860B",  # Dark Goldenrod (darker yellow)
  "Northwest" = "#FF8C00",  # Dark Orange
  "Central" = "#00008B",    # Dark Blue
  "Southwest" = "#8B0000"   # Dark Red (Maroon)
)

# Create ggplot object with a more engaging theme
p <- ggplot(region_summary, aes(
  x = Year,
  y = Total_Participation,
  fill = Region,
  text = paste0(
    "Region: ", Region, "<br>",
    "Year: ", Year, "<br>",
    "Total Participation: ", Total_Participation, "<br>",
    "Total Population: ", Total_Population, "<br>",
    "Participation %: ", round(Participation_Percentage, 2), "%"
  )
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = region_colors) +
  labs(
    title = "4-H Participation Across Time",
    x = "Year",
    y = "Total Participation",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +  # Light theme with increased base size
  theme(
    plot.title = element_text(size=20, face="bold", hjust=0.5),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    panel.border = element_rect(color="gray", fill=NA, size=0.8),
    panel.grid.major = element_line(color="gray80"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="gray95", color=NA)
  )

# Convert to interactive plotly plot
interactive_plot <- ggplotly(p, tooltip = "text") %>%
  layout(legend = list(title = list(text = "<b>Region</b>")))

# Display the interactive plot
interactive_plot

