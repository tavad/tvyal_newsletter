
library(tidyverse)
library(RcppRoll)
library(scales)
library(sf)
library(scatterpie)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("~/R/newsletter/initial_setup.R")

expand_type <- 
  function(tbl) {
    tbl <- tbl |> 
      mutate(
        type = case_when(
          type == "ՋԷԿ" ~ "Ջերմաէլեկտրակենտրոն (ՋԷԿ)",
          type == "ՀԱԷԿ" ~ "Հայկական ատոմային էլեկտրակայան (ՀԱԷԿ)",
          type == "ՀԷԿ" ~ "Հիդրոէլեկտրակայան (ՀԷԿ)",
          TRUE ~ type
        )
      )
    return(tbl)
  }

electricity_data <- read_csv("electricity_production_arm.csv") |> expand_type()
electricity_forecast <- read_csv("electricity_forecast.csv") |> expand_type()

#########################

electricity_plot <-
  electricity_data |> 
  filter(
    !grepl("ընդամենը|Այլ|ԳՋ|հողմային", type),
    !is.na(YoY_value)
  ) |> 
  arrange(date) |> 
  group_by(date) |> 
  mutate(
    pct_YoY = YoY_value/sum(YoY_value),
    year = year(date),
    date = date + months(1) - days(1)
  ) |> 
  group_by(year) |> 
  mutate(
    text = ifelse(date == max(date) & date != as.Date("2024-03-31"), pct_YoY, NA),
    text = ifelse(text <= 0.01, NA, text),
    text = percent(text, accuracy = 0.1)
  ) |> 
  ungroup() |> 
  mutate(
    type = case_when(
      type == "Ջերմաէլեկտրակենտրոն (ՋԷԿ)" ~ "Thermal Power Plant (TPP)",
      type == "Հայկական ատոմային էլեկտրակայան (ՀԱԷԿ)" ~ "Armenian Nuclear Power Plant (ANPP)",
      type == "Հիդրոէլեկտրակայան (ՀԷԿ)" ~ "Hydroelectric Power Plant (HPP)",
      type == "արևային էլեկտրակայաններ" ~ "Solar Power Plants",
      TRUE ~ as.character(type)  
    ),
    type = fct_relevel(type, "Thermal Power Plant (TPP)", "Armenian Nuclear Power Plant (ANPP)", "Hydroelectric Power Plant (HPP)"),
    type = fct_rev(type)
  ) |> 
  ggplot(aes(date, pct_YoY, fill = type, label = text)) +
  geom_area(alpha = 0.6) +
  geom_text(
    position = position_stack(vjust = 0.5)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = percent_format()) +
  scale_fill_manual(values = new_palette_colors[c(2,4,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Armenia's Electricity Production Share by Source",
    subtitle = "2016-2024, Year over Year*",
    caption = "* Latest data presented as of July 2024\n\nAuthor: Aghasi Tavadyan    |    Forecast performed by the author, data source: pcrc.am"
  )

ggsave(filename = "../plots/1_electricity_share_plot.png", plot = electricity_plot, width = 10, height = 7)
ggsave(filename = "../plots/1_electricity_share_plot.svg", plot = electricity_plot, width = 10, height = 7)

###############################

electricity_forecast_plot1 <-
  electricity_forecast |>
  mutate(
    month = month(date),
    year = year(date),
    model_name = str_remove(model_name, "Forecast ")
  ) |> 
  filter(
    !grepl("ընդամենը|Հողմային", type),
    !model_name %in% c("ets", "stlm"),
    grepl("year", period),
    month == 12
  ) |> 
  filter(
    !c(year == 2023 & model_name != "Actual Data")
  ) |> 
  mutate(
    year = ifelse(
      model_name == "Actual Data",
      year, 
      paste0(year, "\n", model_name)
    ),
    type = fct_reorder(type, value),
    type = fct_rev(type),
  ) |> 
  arrange(year) |> 
  group_by(year) |>
  mutate(
    pct = value / sum(value),
    pct_text = number(pct * 100, accuracy = 1),
    pct_text2 = pct_text,
    type = case_when(
      type == "Ջերմաէլեկտրակենտրոն (ՋԷԿ)" ~ "Thermal Power Plant (TPP)",
      type == "Հայկական ատոմային էլեկտրակայան (ՀԱԷԿ)" ~ "Armenian Nuclear Power Plant (ANPP)",
      type == "Հիդրոէլեկտրակայան (ՀԷԿ)" ~ "Hydroelectric Power Plant (HPP)",
      type == "արևային էլեկտրակայաններ" ~ "Solar Power Plants",
      TRUE ~ as.character(type)  
    ),
    type = fct_relevel(type, "Thermal Power Plant (TPP)", "Armenian Nuclear Power Plant (ANPP)", "Hydroelectric Power Plant (HPP)"),
    pct_text = ifelse(!grepl("TPP|HPP", type), "", pct_text),
    pct_text2 = ifelse(grepl("TPP|HPP", type), "", pct_text2)
  ) |> 
  ungroup() |> 
  ggplot(aes(as.character(year), value, fill = type)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = ribbon_low , ymax = ribbon_high), position = "dodge"
  ) +
  geom_text(
    aes(y = 100, label = pct_text), 
    vjust = 0.6,
    position = position_dodge(width = .9)
  ) +
  geom_text(
    aes(y = 100, label = pct_text2), 
    vjust = 2.3,
    position = position_dodge(width = .9)
  ) +
  scale_y_continuous(labels = number_format()) +
  scale_fill_manual(values = new_palette_colors[c(2,4,6,8)]) +
  # scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Armenia's Electricity Generation: Annual Trends and Projections by Source",
    subtitle = "2015-2026, million kWh, percentage shares are given at the bottom of the chart",
    caption = "Author: Aghasi Tavadyan    |    Forecast performed by the author, data source: pcrc.am"
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )


electricity_forecast_plot2_data <- 
  electricity_forecast %>% 
  filter(
    !grepl("ets", model_name),
    grepl("Monthly", period),
    !grepl("ընդամենը|Հողմային", type),
  ) |> 
  mutate(
    type = fct_reorder(type, value),
    type = fct_rev(type)
  ) |> 
  mutate(
    # type = case_when(
    #   type == "Ջերմաէլեկտրակենտրոն (ՋԷԿ)" ~ "Теплоэлектроцентраль (ТЭЦ)",
    #   type == "Հայկական ատոմային էլեկտրակայան (ՀԱԷԿ)" ~ "Армянская атомная электростанция (ААЭС)",
    #   type == "Հիդրոէլեկտրակայան (ՀԷԿ)" ~ "Гидроэлектростанция (ГЭС)",
    #   type == "արևային էլեկտրակայաններ" ~ "Солнечные электростанции",
    #   TRUE ~ as.character(type)  
    # ),
    type = case_when(
      type == "Ջերմաէլեկտրակենտրոն (ՋԷԿ)" ~ "Thermal Power Plant (TPP)",
      type == "Հայկական ատոմային էլեկտրակայան (ՀԱԷԿ)" ~ "Armenian Nuclear Power Plant (ANPP)",
      type == "Հիդրոէլեկտրակայան (ՀԷԿ)" ~ "Hydroelectric Power Plant (HPP)",
      type == "արևային էլեկտրակայաններ" ~ "Solar Power Plants",
      TRUE ~ as.character(type)  
    ),
    # model_name  = case_when(
    #   model_name == "Actual Data" ~ "Фактические данные",
    #   model_name == "Forecast auto.arima" ~ "Прогноз AUTO.ARIMA",
    #   model_name == "Forecast bats" ~ "Прогноз BATS",
    #   model_name == "Forecast ets" ~ "Прогноз ETS",
    #   model_name == "Forecast stlm" ~ "Прогноз STLM"
    # ),
    # model_name = fct_inorder(model_name),
    type = fct_inorder(type)
  )

electricity_forecast_plot2 <- 
  electricity_forecast_plot2_data |> 
  ggplot(aes(date, value, color = model_name, lty = model_name)) +
  facet_wrap(~type, scales = "free_y") +
  geom_smooth(
    data = filter(electricity_forecast_plot2_data, model_name == "Actual Data"),
    color = "gray",
    method = 'loess', formula = 'y ~ x'
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low , ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = new_palette_colors[c(2,4,1,5,8)]) +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2)) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "Monthly Electricity Generation in Armenia: Trends and Projections by Source",
    subtitle = "million kWh, by types of power plants",
    caption = "Author: Aghasi Tavadyan    |    Forecast performed by the author, data source: pcrc.am"
  )

ggsave(filename = "../plots/2_electricity_forecast_plot1.png", plot = electricity_forecast_plot1, width = 10, height = 7)
ggsave(filename = "../plots/2_electricity_forecast_plot1.svg", plot = electricity_forecast_plot1, width = 10, height = 7)

ggsave(filename = "../plots/3_electricity_forecast_plot2.png", plot = electricity_forecast_plot2, width = 10, height = 7)
ggsave(filename = "../plots/3_electricity_forecast_plot2.svg", plot = electricity_forecast_plot2, width = 10, height = 7)

###################################################

damage <-
  read_csv("electrical_network_damage.scv")

yerevan_boundaries <- st_read("media/yerevan_boundary_polygon.gpkg")

yerevan_map <- yerevan_boundaries %>%
  left_join(damage %>% filter(district != "Ընդամենը"), by = c("NAME" = "district"))

# Prepare data for pie charts
yerevan_centroids <- yerevan_map %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>% 
  select(NAME, x, y, hec_damaged_pct, other_org_damaged_pct, total_damaged_cables) %>%
  filter(!is.na(hec_damaged_pct)) %>%  # Remove rows with NA values
  pivot_longer(cols = c(hec_damaged_pct, other_org_damaged_pct), 
               names_to = "damage_type", 
               values_to = "value") %>%
  st_drop_geometry() |> 
  pivot_wider(names_from = damage_type, values_from = value)

total_row <- 
  damage |> 
  filter(district == "Ընդամենը") |> 
  rename(NAME = district) |> 
  mutate(
    x = 44.4,
    y = 40.1,
  ) |> 
  select(NAME, total_damaged_cables, x, y, hec_damaged_pct, other_org_damaged_pct)

yerevan_centroids <- bind_rows(yerevan_centroids, total_row) |> 
  mutate(
    NAME = case_when(
      NAME == "Արաբկիր" ~ "Arabkir",
      NAME == "Էրեբունի" ~ "Erebuni",
      NAME == "Կենտրոն" ~ "Center",
      NAME == "Նոր Նորք" ~ "Nor Nork",
      NAME == "Շենգավիթ" ~ "Shengavit",
      NAME == "Ընդամենը" ~ "Total",
      TRUE ~ NAME
    )
  )

yerevan_map_plot <- 
  ggplot() +
  geom_sf(data = yerevan_map) +
  geom_scatterpie(
    data = yerevan_centroids,
    aes(x = x, y = y, group = NAME),
    cols = c("hec_damaged_pct", "other_org_damaged_pct"),
    alpha = 0.6,
    pie_scale = 4
  ) +
  geom_text(
    data = yerevan_centroids,
    aes(x = x, y = y, label = paste0(round(hec_damaged_pct * 100), "%")),
    nudge_x =  0.005,
    nudge_y = -0.005,
    size = 2.5
  ) +
  geom_text(
    data = yerevan_centroids,
    aes(x = x, y = y, label = paste0(round(other_org_damaged_pct * 100), "%")),
    nudge_x = -0.005,
    nudge_y =  0.007,
    size = 2.5
  ) +
  geom_text(
    data = yerevan_centroids,
    aes(x = x, y = y, label = paste0(NAME, " (", total_damaged_cables, ")")),
    vjust = -7,
    size = 3
  ) +
  geom_text(
    aes(x = 44.4, y = 40.1, label = "The number in parentheses indicates\nthe number of damaged cables"),
    vjust = 4,
    size = 2.5
  ) +
  scale_fill_manual(
    values = new_palette_colors[c(6,2)],
    labels = c(
      "Cables damaged by ENA CJSC",
      "Cables damaged by other organizations"
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Distribution of Cable Damage Across Yerevan Districts",
    subtitle = "Number of damaged cables for the period from January to August 2024",
    caption = "Author: Aghasi Tavadyan   |   Data from: ENA CJSC"
  ) + 
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )

ggsave(filename = "../plots/4_Yerevan_map.png", plot = yerevan_map_plot, width = 7, height = 7)
ggsave(filename = "../plots/4_Yerevan_map.svg", plot = yerevan_map_plot, width = 7, height = 7)


