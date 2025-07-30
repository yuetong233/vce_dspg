library(dplyr)
library(plotly)

particip_combined <- read.xlsx("C:\\Users\\jeffr\\Desktop\\VCE Excel Data\\VCEDashboard\\particip_combined.xlsx")

particip_combined <- particip_combined %>%
  rename("County Totals" = "County.Totals")


# Prepare data: aggregate totals per Region and Year
particip_data <- particip_combined %>%
  group_by(Region, Year) %>%
  summarize(
    total_participation = sum(`County Totals`),
    total_population = sum(Population),
    .groups = 'drop'
  ) %>%
  mutate(
    participation_pct = total_participation / total_population * 100,
    Year = as.factor(Year),  # ensure Year is a factor for proper plotting
    hover_text = paste0(
      "Year: ", Year, "<br>",
      "Region: ", Region, "<br>",
      "Population: ", total_population, "<br>",
      "Participation: ", total_participation, "<br>",
      "Participation %: ", round(participation_pct, 2), "%"
    )
  )

# Define colors for each region
region_colors <- c(
  "Northeast" = "#FFA07A",    # Light Salmon (light orange)
  "Southeast" = "lightblue",
  "Central" = "darkblue",
  "Northwest" = "orange",
  "Southwest" = "maroon"
)

# Plot
p <- plot_ly(
  data = particip_data,
  x = ~Year,
  y = ~total_participation,
  color = ~Region,
  colors = region_colors,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~hover_text,
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "4-H Participation Trend 2021–2024",  # You can also fix capitalization here
      font = list(size = 24)
    ),
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Participation"),
    legend = list(title = list(text = "Region")),
    margin = list(t = 100)  # ✅ Top margin to prevent title from being cut off
  )

p
