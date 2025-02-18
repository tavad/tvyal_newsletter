library(tidyverse)
library(gganimate)
library(scales)
library(WDI)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../initial_setup.R")

transfers_to_gdp_plot <- read_rds("plots/transfers_to_gdp_plot.rds")

remittances_gdp_db <- 
  WDI(indicator = "BX.TRF.PWKR.DT.GD.ZS") |> 
  as_tibble() |> 
  rename(remittances_gdp = BX.TRF.PWKR.DT.GD.ZS)

remittances_gdp_armenia <- 
  remittances_gdp_db |> 
  filter(iso3c == "ARM", !is.na(remittances_gdp), year >= 2013) |> 
  mutate(
    remittances_gdp = remittances_gdp / 100,
    date = ymd(paste(year, "12 31"))
  )

transfers_to_gdp_plot +
  geom_line(
    data = remittances_gdp_armenia,
    aes(date, remittances_gdp), size = 2, linetype = 2,
    color = new_palette_colors[1], alpha = 1
  ) +
  labs(
    caption = caption_f(suffix_text = "Կետագծերը ներկայացնում են պաշտոնական տրանսֆերտներ / ՀՆԱ հարաբերությունը")
  )

# 
# countries <- c(
#   # Neighbors
#   "GEO", "TUR", "IRN", "AZE", "ARM",
#   
#   # BRICS+
#   "BRA", "RUS", "IND", "CHN", "ZAF", "SAU", "IRN", "ETH", "EGY",
#   
#   # Key Post-Soviet
#   "KAZ", "KGZ", "TJK", "UZB", "MDA", "UKR", "BLR",
#   
#   # Others with high remittance/GDP
#   "NPL", "PHL", "LBN", "TJK", "KGZ"
# )

countries <- c(
  "ARM", "AZE", "BLR", "EGY", "GEO", "IND", "KGZ", "MDA", "NPL", "PHL", "TJK", "UKR", "UZB"
)

iso_to_unicode_flag <- function(iso2c) {
  sapply(iso2c, function(code) {
    if (is.na(code)) return(NA)
    paste0(
      intToUtf8(127462L + which(LETTERS == substr(code, 1, 1)) - 1L),
      intToUtf8(127462L + which(LETTERS == substr(code, 2, 2)) - 1L)
    )
  })
}

remittances_gdp_db |> 
  # filter(
  #   !is.na(remittances_gdp), 
  #   remittances_gdp >= 0.5
  # ) |> 
  filter(
    iso3c %in% countries, 
    year == 2023
  ) |> 
  mutate(
    remittances_gdp = remittances_gdp / 100,
    country_arm = countrycode::countrycode(iso3c, origin = 'iso3c', destination = 'cldr.name.hy'),
    continent = countrycode::countrycode(country, origin = 'country.name', destination = 'continent'),
    country = fct_reorder(country, remittances_gdp),
    country_arm = fct_reorder(country_arm, remittances_gdp),
    pct_txt = percent(remittances_gdp, accuracy = 0.1),
    flag_unicode = iso_to_unicode_flag(iso2c)
  ) |> 
  ggplot(aes(country_arm, remittances_gdp)) +
  coord_flip() +
  geom_col(width = 0.7, fill = new_palette_colors[3], alpha = 1) +
  geom_text(aes(label = pct_txt), hjust = -0.05, color = "black", size = 5) +
  geom_text(aes(label = flag_unicode, y = -0.02), size = 6, family = "Noto Color Emoji") +
  labs(
    x = NULL,
    y = NULL,
    title =  "Personal remittances, received (% of GDP)",
    subtitle = "Selected countries, 2023",
    caption = caption_f(language = "eng")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(angle = 0, size = 12),
    axis.text.x = element_blank()
  )


# Prepare the data for all years
animated_data <- remittances_gdp_db %>%
  filter(!is.na(remittances_gdp)) |> 
  filter(
    iso3c %in% countries,
    year >= 2000,
    year <= max(year)
  ) %>%
  mutate(
    remittances_gdp = remittances_gdp / 100,
    country_arm = countrycode::countrycode(iso3c, origin = 'iso3c', destination = 'cldr.name.hy'),
    continent = countrycode::countrycode(country, origin = 'country.name', destination = 'continent'),
    flag_unicode = iso_to_unicode_flag(iso2c)
  ) %>%
  group_by(year) %>%
  mutate(
    country = fct_reorder(country, remittances_gdp),
    country_arm = fct_reorder(country_arm, remittances_gdp)
  ) %>%
  ungroup()

# Create the animated plot
p <- ggplot(animated_data, aes(country_arm, remittances_gdp)) +
  coord_flip() +
  geom_col(width = 0.7, fill = new_palette_colors[2], alpha = 1) +
  # Format the percentage directly in geom_text
  geom_text(aes(label = paste0(format(100 * remittances_gdp, digits = 1, nsmall = 1), "%")), 
            hjust = -0.05, color = "black", size = 5) +
  geom_text(aes(label = flag_unicode, y = -0.02), size = 6, family = "Noto Color Emoji") +
  labs(
    x = NULL,
    y = NULL,
    title = "Personal remittances, received (% of GDP)",
    subtitle = "Selected countries, {frame_time}",
    caption = caption_f(language = "eng")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(angle = 0, size = 12),
    axis.text.x = element_blank()
  ) +
  # Add animation specific elements
  transition_time(year) +
  ease_aes('linear')

# Render the animation
animate(p,
        nframes = 240,  # 10 frames per year
        fps = 10,
        width = 800,
        height = 600,
        renderer = gifski_renderer())

# Save the animation
anim_save("plots/remittances_animation.gif")


####################################


# 1. For total transfers and market share analysis
transfers_total_analysis <- transfers_total_chartdata %>%
  group_by(year) %>%
  summarise(
    total_transfers = sum(K_USD),
    russia_share = K_USD[country == "Russian Federation"] / total_transfers,
    usa_share = K_USD[country == "USA"] / total_transfers
  ) %>%
  mutate(
    yoy_growth = (total_transfers / lag(total_transfers) - 1) * 100
  )

# 2. For inflow/outflow dynamics
transfers_flow_analysis <- transfers_date %>%
  group_by(year = year(date), direction) %>%
  summarise(
    total_amount = sum(K_USD_YOY) / 1e6,  # Convert to billions
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = direction,
    values_from = total_amount
  ) %>%
  mutate(
    net_flow = Inflow - Outflow,
    inflow_yoy = (Inflow / lag(Inflow) - 1) * 100,
    outflow_yoy = (Outflow / lag(Outflow) - 1) * 100
  )

# 3. For Russia-specific analysis
russia_transfers_analysis <- transfers_clean %>%
  filter(
    country == "Russian Federation",
    direction == "Inflow"
  ) %>%
  group_by(year, type) %>%
  summarise(
    total_amount = sum(K_USD) / 1e6,  # Convert to billions
    .groups = "drop"
  )


# For H1 analysis
h1_analysis <- transfers_clean %>%
  filter(month <= 6) %>%
  group_by(year) %>%
  summarise(
    h1_total = sum(K_USD),
    russia_h1 = sum(K_USD[country == "Russian Federation"])
  ) %>%
  mutate(
    h1_yoy = (h1_total / lag(h1_total) - 1) * 100,
    russia_h1_yoy = (russia_h1 / lag(russia_h1) - 1) * 100
  )


# Monthly pattern analysis for Russian transfers
russia_monthly_analysis <- transfers_clean %>%
  filter(
    country == "Russian Federation",
    direction == "Inflow",
    year %in% c("2021", "2022", "2023", "2024")
  ) %>%
  group_by(year, month) %>%
  summarise(
    monthly_total = sum(K_USD) / 1e3 , # Convert to millions,
  ) |> 
  ungroup() |> 
  mutate(month_change = monthly_total - lag(monthly_total)) |> 
  filter(year %in% c("2022", "2023", "2024"))

# Let's look at the last few months of 2023
russia_monthly_2023_q4 <- russia_monthly_analysis %>%
  filter(year == "2022", month >= 10)

# Analysis of the monthly dynamics
print(russia_monthly_analysis, n = 36)
