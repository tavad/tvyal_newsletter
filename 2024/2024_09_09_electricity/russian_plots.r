library(tidyverse)
library(RcppRoll)
library(scales)
library(sf)
library(scatterpie)

source("../../initial_setup.R")

# Предполагается, что начальные настройки доступны в initial_setup.R,
# а также доступны все требуемые данные в текущей директории

# Установка рабочей директории для сохранения графиков
output_dir <- "~/proj/2025_RAU_grant/2025_04_03_energy_short_article/plots"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Функция для перевода типов электроэнергии
expand_type <- function(tbl) {
  tbl <- tbl |>
    mutate(
      type = case_when(
        type == "ՋԷԿ" ~ "ТЭС",
        type == "ՀԱԷԿ" ~ "АЭС",
        type == "ՀԷԿ" ~ "ГЭС",
        type == "արևային էլեկտրակայաններ" ~ "Солнечные электростанции",
        TRUE ~ type
      )
    )
  return(tbl)
}

# Функция caption для русского языка
caption_f <- function(source = NULL, suffix_text = NULL) {
  prefix <- "Источник: Агаси Тавадян    |    tvyal.com    |    tavadyan.com"

  if(!is.null(source)) {
    prefix <- paste0(prefix, "    |    Данные: ", source)
  }

  if(!is.null(suffix_text)) {
    prefix <- paste0(prefix, "    |    ", suffix_text)
  }

  return(prefix)
}

# Загрузка данных
electricity_data <- read_csv("electricity_production_arm.csv") |> expand_type()
electricity_forecast <- read_csv("electricity_forecast.csv") |> expand_type()
damage <- read_csv("electrical_network_damage.scv")
electricity_in_NACE <- read_csv("electricity_in_NACE.csv")
yerevan_boundaries <- st_read("media/yerevan_boundary_polygon.gpkg")

# Предполагается, что caption_arm и new_palette_colors определены в initial_setup.R
# Если нет, можно определить их здесь

# График 1: Карта повреждений электросетей в Ереване
yerevan_map <- yerevan_boundaries %>%
  left_join(damage %>% filter(district != "Ընդամենը"), by = c("NAME" = "district"))

# Подготовка данных для круговых диаграмм
yerevan_centroids <- yerevan_map %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>%
  select(NAME, x, y, hec_damaged_pct, other_org_damaged_pct, total_damaged_cables) %>%
  filter(!is.na(hec_damaged_pct)) %>%
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

yerevan_centroids <- bind_rows(yerevan_centroids, total_row)

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
    vjust = -5,
    size = 3
  ) +
  geom_text(
    aes(x = 44.4, y = 40.1, label = "Число в скобках - количество\nповрежденных кабелей"),
    vjust = 4,
    size = 2.5
  ) +
  scale_fill_manual(
    values = new_palette_colors[c(6,2)],
    labels = c(
      "Кабели, поврежденные ЭСА",
      "Кабели, поврежденные другими организациями"
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Поврежденные кабели в Ереване по административным районам",
    subtitle = "Количество поврежденных кабелей за январь-август 2024г.",
    caption = caption_f()
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )

ggsave(paste0(output_dir, "/yerevan_map_plot.png"), plot = yerevan_map_plot, width = 10, height = 8, dpi = 300)

# График 2: Структура производства электроэнергии по источникам
electricity_plot_2 <-
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
    title = "Структура производства электроэнергии в Армении по источникам*",
    subtitle = "Годовой разрез*",
    caption = caption_f("psrc.am", "Последние представленные данные по состоянию на июль 2024г.")
  )

ggsave(paste0(output_dir, "/electricity_plot_2.png"), plot = electricity_plot_2, width = 10, height = 8, dpi = 300)

# График 3: Прогноз годовых объемов производства электроэнергии
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
    # Перевод названия модели
    model_name = case_when(
      model_name == "Actual Data" ~ "Фактические данные",
      model_name == "auto.arima" ~ "auto.arima",
      model_name == "bats" ~ "bats",
      TRUE ~ model_name
    )
  ) |>
  arrange(year) |>
  group_by(year) |>
  mutate(
    pct = value / sum(value),
    pct_text = number(pct * 100, accuracy = 1),
    pct_text2 = pct_text,
    pct_text = ifelse(!grepl("ТЭС|ГЭС", type), "", pct_text),
    pct_text2 = ifelse(grepl("ТЭС|ГЭС", type), "", pct_text2)
  ) |>
  ungroup() |>
  ggplot(aes(as.character(year), value, fill = type)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = ribbon_low, ymax = ribbon_high), position = "dodge"
  ) +
  geom_text(
    aes(y = 100, label = pct_text),
    vjust = 0.5,
    position = position_dodge(width = .9)
  ) +
  geom_text(
    aes(y = 100, label = pct_text2),
    vjust = 2,
    position = position_dodge(width = .9)
  ) +
  scale_y_continuous(labels = number_format()) +
  scale_fill_manual(values = new_palette_colors[c(2,4,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Годовые объемы производства электроэнергии и прогноз",
    subtitle = "2015-2024, млн. кВт·ч, процентные доли указаны внизу графика",
    caption = paste0(caption_f("pcrc.am", "Прогноз выполнен автором"))
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(paste0(output_dir, "/electricity_forecast_plot1.png"), plot = electricity_forecast_plot1, width = 10, height = 8, dpi = 300)

# График 4: Месячная динамика и прогноз производства электроэнергии
electricity_forecast_plot2_data <-
  electricity_forecast %>%
  filter(
    !grepl("ets", model_name),
    grepl("Monthly", period),
    !grepl("ընդամենը|Հողմային", type)
  ) |>
  mutate(
    type = fct_reorder(type, value),
    type = fct_rev(type),
    # Перевод названия модели
    model_name = case_when(
      model_name == "Actual Data" ~ "Фактические данные",
      model_name == "Forecast auto.arima" ~ "Прогноз auto.arima",
      model_name == "Forecast bats" ~ "Прогноз bats",
      model_name == "Forecast stlm" ~ "Прогноз stlm",
      TRUE ~ model_name
    )
  )

electricity_forecast_plot2 <-
  electricity_forecast_plot2_data |>
  ggplot(aes(date, value, color = model_name, lty = model_name)) +
  facet_wrap(~type, scales = "free_y") +
  geom_smooth(
    data = filter(electricity_forecast_plot2_data, model_name == "Фактические данные"),
    color = "gray",
    method = 'loess', formula = 'y ~ x'
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low, ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = new_palette_colors[c(2,4,1,5,8)]) +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2)) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "Месячная динамика и прогноз производства электроэнергии",
    subtitle = "млн. кВт·ч, по типам электростанций",
    caption = paste0(caption_f("pcrc.am", "Прогноз выполнен автором"))
  )

ggsave(paste0(output_dir, "/electricity_forecast_plot2.png"), plot = electricity_forecast_plot2, width = 10, height = 8, dpi = 300)

# График 5: Динамика производства электроэнергии в секторе NACE
electricity_in_NACE_plot <-
  electricity_in_NACE |>
  mutate(
    yoy_value = roll_sumr(month_value, 12),
    code_text = paste0(code, ", ", name_clean)
  ) |>
  na.omit() |>
  ggplot(aes(date, yoy_value / 1000)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = number_format(), n.breaks = 8) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Производство, передача и распределение электроэнергии",
    subtitle = "млрд драмов, в годовом разрезе, по состоянию на март 2024г.",
    caption = caption_f()
  )

ggsave(paste0(output_dir, "/electricity_in_NACE_plot.png"), plot = electricity_in_NACE_plot, width = 10, height = 8, dpi = 300)

# Вывод сообщения о завершении
cat("Все графики успешно сохранены в директории:", output_dir, "\n")
