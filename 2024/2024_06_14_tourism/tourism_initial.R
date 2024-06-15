library(tidyverse)
library(RcppRoll)
library(scales)


# rm(list = ls()); gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../../initial_setup.R")


tourism_monthly <- read_csv("data_monthly.csv")


tourism_monthly <- 
  tourism_monthly |> 
  pivot_longer(-year, names_to = "month") |> 
  mutate(
    date = ym(paste(year, month)) + months(1) - days(1),
    value_yoy = roll_sumr(value, 12),
    month = as.numeric(month)
  ) |> 
  relocate(date) |> 
  filter(!is.na(value))

max_month <- 
  tourism_monthly |> 
  filter(date == max(date)) |> 
  pull(month)


tourism_monthly |> 
  ggplot(aes(date, value_yoy)) +
  geom_line()

tourism_monthly |> 
  mutate(
    period = ifelse(month <= max_month, "start_of_the_year", "rest_of_the_year")
  ) |> 
  group_by(year, period) |> 
  summarise(value = sum(value), .groups = "drop") |> 
  group_by(year) |> 
  mutate(
    text = sum(value),
    text = ifelse(period == "start_of_the_year", value, text),
    text = ifelse(
      text >= 1e6,
      number(text/1e6, accuracy = 0.01, suffix = " M"),
      number(text/1000, accuracy = 1, suffix = " K")
    ),
    text_start = ifelse(
      period == "start_of_the_year",
      paste0(text, "*"),
      NA
    ),
    text_rest = ifelse(period == "rest_of_the_year", text, NA),
  ) |> 
  ungroup() |> 
  ggplot() +
  geom_col(aes(year, value, fill = period), width = 0.3) +
  geom_text(
    aes(year + 0.45, y = 0, fill = period, label = text_start), 
    position = position_dodge(),
    # size = 2,
    vjust = - 0.1, color = new_palette_colors[6]
  ) +
  geom_text(
    aes(year, value, fill = period, label = text_rest), 
    position = "stack", vjust = -0.3, color = new_palette_colors[2]
  ) +
  geom_segment(
    mapping = aes(x = year - 0.15, xend = year + 0.75, y = 0, yend = 0),
    linetype = 3, color = new_palette_colors[6]
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(
    values = new_palette_colors[c(2,6)],
    labels = c("", "* հունվարից մինչև մարտ")
    ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Զբոսաշրջային այցելություններ դեպի Հայաստան",
    subtitle = "ըստ տարիների",
    captions = paste0(caption_arm, "    |    Տվյալների աղբյուր՝ mineconomy.am")
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = c(0, 1)))) +
  theme(
    panel.grid.major.x = element_blank()
  )
  


###############################################

library(rnaturalearth)
library(ggplot2)

armenia_map <- ne_countries(country = "Armenia", returnclass = "sf", scale = 10)

ggplot() +
  geom_sf(data = armenia_map, fill = "orange") +
  theme_void()


data <- read_csv("transport_map_data.csv")

ggplot() +
  geom_sf(data = armenia_map, fill = new_palette_colors[7]) +
  geom_rect(
    data = tibble(xmin = c(44.9, 45.15), xmax = c(45.05, 45.25), ymin = c(41, 40), ymax = c(41.1, 41)),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = new_palette_colors[7], alpha = 1
  ) +
  geom_rect(
    data = tibble(
      xmin = c(44, 44.5), xmax = c(44, 44.5), 
      ymin = c(39, 39), ymax = c(39.1, 39.1),
      fill = c("2023 հունվարից մարտ", "2024 հունվարից մարտ")
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
  ) +
  geom_point(data = data, aes(x = longitude, y = latitude), color = "black", size = 2) +
  geom_segment(
    data = data, 
    aes(x = longitude, xend = long_line_1, y = latitude, yend = lat_line_1),
    linetype = 3
  ) +
  geom_segment(
    data = data, 
    aes(x = long_line_1, xend = long_line_2, y = lat_line_1, yend = lat_line_2),
    linetype = 3
  ) +
  geom_text(
    data = data, 
    aes(
      x = long_line_2, y = lat_line_2,
      label = paste0(
        X2023, "%", " | ", X2024, "%", "\n",
        transportation_emoji, " ", place
      ),
      hjust = ifelse(long_line_2 == min(long_line_2), 0, 1)
    ),
  ) +
  geom_rect(
    data = data,
    aes(
      xmin = long_line_2 + ifelse(long_line_2 == min(long_line_2), 0 , -0.32), 
      xmax = long_line_2 + ifelse(long_line_2 == min(long_line_2), 0.12, -0.2), 
      ymin = lat_line_2 + 0.085, 
      ymax = lat_line_2 + 0.085 + X2023 / 200
    ),
    fill = new_palette_colors[2], alpha = 1
  ) +
  geom_rect(
    data = data,
    aes(
      xmin = long_line_2 + ifelse(long_line_2 == min(long_line_2), 0.2, 0), 
      xmax = long_line_2 + ifelse(long_line_2 == min(long_line_2), 0.32, -0.12), 
      ymin = lat_line_2 + 0.085, 
      ymax = lat_line_2 + 0.085 + X2024 / 200
    ),
    fill = new_palette_colors[6], alpha = 1
  ) +
  scale_fill_manual(values =  new_palette_colors[c(2, 6)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Զբոսաշրջային այցելություններ",
    subtitle = "ըստ պետական սահմանի",
    captions = paste0(caption_arm, "    |    Տվյալների աղբյուր՝ mineconomy.am")
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
  )




library(ggrepel)

ggplot() +
  geom_sf(data = armenia_map, fill = new_palette_colors[7]) +
  geom_point(data = data, aes(x = longitude, y = latitude), color = "black", size = 2) +
  geom_text_repel(
    data = data,
    aes(x = longitude, y = latitude,
        label = paste0(
          transportation_emoji,
          " ", place, "\n",
          X2023, "%", " | ", X2024, "%"
        )
    ),
    segment.color = "gray70", segment.curvature = -0.1,
    box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines"),
    fill = "white", color = "black", size = 4, nudge_x = 0.1
  ) +
  theme_void()


data |> 
  select(place, matches("\\d{4}")) |> 
  pivot_longer(-place, names_to = "year") |> 
  mutate(year = parse_number(year) |> as.factor()) |> 
  filter(place == "Զվարթնոց") |> 
  ggplot(aes(year, value, fill = year, label = value)) +
  geom_col() +
  geom_text(vjust = -0.3) +
  scale_y_continuous(limits = c(0, 80), breaks = NULL)

  