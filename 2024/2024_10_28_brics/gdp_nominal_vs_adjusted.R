library(tidyverse)
library(scales)
library(WDI)
library(tidytext)
library(countrycode)


iso_to_unicode_flag <- function(iso2c) {
  sapply(iso2c, function(code) {
    if (is.na(code)) return(NA)
    paste0(
      intToUtf8(127462L + which(LETTERS == substr(code, 1, 1)) - 1L),
      intToUtf8(127462L + which(LETTERS == substr(code, 2, 2)) - 1L)
    )
  })
}


valid_countries <- 
  countrycode::codelist |> 
  filter(!is.na(iso2c)) |> 
  pull(iso2c)


gdp <-  WDI(indicator = c("NY.GDP.MKTP.PP.CD", "NY.GDP.MKTP.CD"), start = 1990) |>
  as_tibble() |>
  rename(gdp_nominal = NY.GDP.MKTP.CD, gdp_ppp = NY.GDP.MKTP.PP.CD)


facet_labels <- c("GDP, PPP (current international $)", "GDP, nominal (current US$)")

gdp |> 
  filter(iso2c %in% valid_countries) |> 
  pivot_longer(contains("gdp"), names_to = "indicator") |> 
  arrange(country, indicator) |> 
  group_by(indicator, iso3c) |> 
  mutate(
    estimated = ifelse(is.na(value), "estimated", "")
  ) |> 
  fill(value, .direction = "down") |> 
  mutate(
    estimated = ifelse(is.na(value), "", estimated)
  ) |> 
  group_by(year, indicator) |> 
  mutate(
    pct = value / sum(value, na.rm = TRUE),
    pct_text = percent(pct, accuracy = 0.1)
  ) |> 
  ungroup() |> 
  filter(year == max(year)) |> 
  group_by(indicator) |> 
  slice_max(order_by = pct, n = 7) |> 
  mutate(row_number = as.character(row_number())) |> 
  ungroup() |> 
  mutate(
    country = case_when(
      country == "United Kingdom" ~ "UK", 
      country == "Russian Federation" ~ "Russia",
      country == "United States" ~ "USA",
      TRUE ~ country
    ),
    country = reorder_within(country, pct, indicator) |> fct_rev(),
    flag = iso_to_unicode_flag(iso2c),
    indicator = case_when(
      indicator == "gdp_ppp" ~ facet_labels[1],
      indicator == "gdp_nominal" ~ facet_labels[2]
    )
  ) |> 
  ggplot(aes(country, pct)) +
  facet_wrap(~indicator, scales = "free_x") +
  geom_col(aes(fill = row_number), alpha = 1) +
  geom_text(aes(y = -0.015, label = flag), size = 8) +
  geom_text(aes(label = pct_text), vjust = -0.3, size = 5) +
  geom_text(
    aes(y = 0.007, label = dollar(value / 1e12, accuracy = 0.1, suffix = "B")),
    color = "white"
  ) +
  geom_text(
    data = tibble(
      x = 2.8, y = 0.13, 
      label = c(
        "Better reflects:\n➡️ Industrial output\n➡️ Innovation potential\n➡️ Resource efficiency\n➡️ Living standards\n➡️ Real production capacity\n➡️ Domestic consumption power",
        "Better reflects:\n➡️ International purchasing power\n➡️ Global financial influence\n➡️ Trade capabilities\n➡️ Geopolitical economic leverage\n➡️ Currency market power\n➡️ International debt capacity"
      ),
      indicator = facet_labels
    ),
    aes(x, y, label = label),
    vjust = 0, hjust = 0,
    lineheight = 1.7
  ) +
  scale_x_reordered() +
  scale_fill_manual(values = colfunc2(11)[2:8]) +
  # scale_fill_gradientn(colors = rev(colfunc2(100)[15:75])) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Nominal GDP vs GDP PPP: Which is Better?",
    subtitle = "The differences between nominal and adjusted GDP in 2023",
    caption = caption_f(language = "eng", source = "World Bank")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    # axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(size = 14)
  )

ggsave("plots/gdp_nominal_ppp_diff.png", ggplot2::last_plot(), width = 8, height = 7)


#################################################

gdp |> 
  filter(iso2c %in% c("RU", "KZ", "AM", "IN", "UZ", "CN")) |> 
  mutate(
    ppp_convertion = gdp_ppp / gdp_nominal,
    country = case_when(
      country == "United Kingdom" ~ "UK", 
      country == "Russian Federation" ~ "Russia",
      country == "United States" ~ "USA",
      TRUE ~ country
    ),
    text = paste0(country, ", ", number(ppp_convertion, accuracy = 0.1)),
    text = ifelse(year == max(year), text, NA)
  ) |> 
  ggplot(aes(year, ppp_convertion, color = country)) +
  geom_line(alpha = 1, linewidth = 1.5) +
  geom_text(aes(x = year + 0.5, label = text), hjust = 0, alpha = 1) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(breaks = 1:10) +
  scale_color_manual(values = colfunc3(7)) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "PPP conversion ratio",
    subtitle = "for select countries",
    caption = "calculated based on data from World Bank"
  ) +
  theme(
    plot.margin = margin(10, 60, 10, 10),
    legend.position = "drop"
  )

