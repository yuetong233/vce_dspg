# === DROPDOWN MENU CODE ===
# === TAB PANEL SECTION ===

selectInput("vce_race_filter", "Select Group:",
            choices = c(
              "All Volunteers" = "all",
              "Hispanic" = "eth_hispanic",
              "White" = "race_white",
              "Black" = "race_black",
              "Asian" = "race_asian"