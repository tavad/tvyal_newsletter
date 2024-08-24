library(tidyverse)
library(RcppRoll)
library(sweep)
library(timetk)
library(forecast)
library(zoo)

# this code makes a time series forecast on fuel price in Armenia 
# based on the past Data.

#### geting data


transfers_total <- 
  read_csv("transfers_by_countries_clean.csv") |> 
  # mutate(date = ym(paste(year(date), month(date)))) |>
  filter(country != "Total") |> 
  group_by(date, direction, type) |> 
  summarise(K_USD = sum(K_USD, na.rm = TRUE), .groups = "drop")
  # arrange(direction, type, date) |> 
  # group_by(direction, type) |> 
  # mutate(K_USD = roll_sumr(K_USD, 12)) |> 
  # ungroup() |> 
  # na.omit() |> 
  # filter(date >= ym("2014-01"))

last_date_value <- 
  transfers_total |> 
  filter(date == max(date)) |> 
  pull(date) |> 
  unique()


#### forecasting

arm_transfers_ts <-
  transfers_total |> 
  nest(data = c(date, K_USD)) |> 
  mutate(ts = map(data, tk_ts, start = 2013, freq = 12))

arm_transfers_forecasts <- 
  arm_transfers_ts |> 
  crossing(model_name = c("auto.arima", "ets", "bats", "stlm")) |> 
  mutate(
    model = map2(model_name, ts, ~ exec(.x, .y)),
    forecast = map(model, forecast, h = 24, level = c(50, 80, 95)),
    sweep = map(forecast, sw_sweep)
  )

arm_transfers_forecasts_clean <- 
  arm_transfers_forecasts %>% 
  select(-data, -ts, -model, -forecast) %>%
  unnest(sweep) %>% 
  mutate(
    K_USD = ifelse(is.na(K_USD), value, K_USD),
    date = as.Date(as.yearmon(index), frac = 1)
  ) |> 
  relocate(date) |> 
  select(-index, -value)



#### preparing forecast data for the plot

# data_select <- 
#   arm_transfers_forecasts_clean %>% 
#   arrange(direction, type, model_name, date) %>% 
#   pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") %>% 
#   pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") %>% 
#   mutate(high = parse_number(high),
#          low = parse_number(low)) %>%
#   filter(high == low) %>% 
#   rename(interval = high) %>% 
#   select(-low) %>%
#   filter(interval == 0.8)
# 
# 
# forecast_first_rows <- 
#   data_select %>%  
#   filter(key == "actual") |> 
#   filter(date == max(date)) %>%
#   mutate(
#     key = "forecast",
#     ribbon_low = K_USD,
#     ribbon_high = K_USD
#   )
# 
# data_select1 <- 
#   data_select %>% 
#   bind_rows(forecast_first_rows)
# 
# data_select2 <- 
#   data_select1 %>% 
#   mutate(model_name = paste0("Forecast ", model_name)) %>% 
#   mutate(model_name = ifelse(key == "actual", "Actual Data", model_name)) %>% 
#   select(-key) %>% 
#   unique()


#############################


data_select <-
  arm_transfers_forecasts_clean %>%
  crossing(period = c("month", "YoY")) %>%
  arrange(period, direction, type, model_name, date) %>%
  group_by(period, direction, type, model_name) %>%
  mutate(
    across(
      contains(c("lo.", "hi.")),
      ~ ifelse(is.na(.x) & period == "YoY", K_USD, .x)),
    across(
      contains(c("lo.", "hi.", "K_USD")),
      ~ ifelse(
        period == "YoY",
        roll_sum(.x, n = 12, fill = NA, align = c("right")),
        .x
      )
    ),
    across(contains(c("lo.", "hi.")), ~ ifelse(key == "actual", NA, .x))
  ) %>%
  ungroup() %>%
  pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") %>%
  pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") %>%
  mutate(
    high = parse_number(high),
    low = parse_number(low)
  ) %>%
  filter(high == low) %>%
  rename(interval = high) %>%
  select(-low) %>%
  filter(interval == 0.5)


forecast_first_rows <-
  data_select %>%
  filter(key == "actual") |>
  filter(date == max(date)) %>%
  mutate(
    key = "forecast",
    ribbon_low = K_USD,
    ribbon_high = K_USD
  )

data_select1 <-
  data_select %>%
  bind_rows(forecast_first_rows)


data_select1 <- data_select %>%
  bind_rows(forecast_first_rows)


data_select2 <-
  data_select1 %>%
  mutate(model_name = paste0("Forecast ", model_name)) %>%
  mutate(model_name = ifelse(key == "actual", "Actual Data", model_name)) %>%
  select(-key) %>%
  unique()

data_select2 |> write_csv("transfers_forecast.csv")

#### plotting

data_select2 %>%
  filter(
    type == "total", 
    # direction == "Net inflow",
    !grepl("arima|bats", model_name),
    date >= ym("2019-01"),
    # period == "YoY"
  ) |> 
  mutate(
    across(where(is.numeric), ~.x / 1e6)
  ) |> 
  ggplot(aes(date, K_USD, color = model_name, lty = model_name)) +
  facet_grid(period~direction, scales = "free_y") +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low, ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = number_format(), n.breaks = 7) +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2))



data_select2 |> 
  filter(
    month(date) == 12,
    period == "YoY",
    type == "total",
    grepl("stlm|Data", model_name),
    !c(!grepl("Data", model_name) & date == ymd(last_date_value))
  ) |> 
  filter(date != max(date)) |> 
  arrange(type, direction, date) |> 
  group_by(type, direction) |> 
  mutate(
    year = year(date),
    across(matches("ribbon"), ~ifelse(is.na(.x), K_USD, .x)),
    pct_change = ifelse(
      ribbon_low == K_USD,
      ifelse(
        K_USD / lag(K_USD) < 2,
        percent(K_USD / lag(K_USD) - 1, accuracy = 0.1),
        paste(number(K_USD / lag(K_USD), accuracy = 0.01), "անգամ")
      ),
      paste0(
        percent(K_USD / lag(K_USD) - 1, accuracy = 0.1), 
        "\n(", percent(ribbon_low / lag(ribbon_low) - 1, accuracy = 0.1), "-ից մինչև ",
        percent(ribbon_high / lag(ribbon_high) - 1, accuracy = 0.1), ")"
      )
    ),
    across(matches("K_USD|ribbon"), ~number(.x/1000, accuracy = 0.1)),
    K_USD = ifelse(
      ribbon_low == K_USD,
      K_USD,
      paste0(
        K_USD, "\n(", ribbon_low, "-ից մինչև ", ribbon_high, ")"
      )
    ),
  ) |> 
  ungroup() |> 
  filter(
    date >= ym("2018-01")
  ) |> 
  select(year, direction, K_USD, pct_change) |> 
  mutate(
    direction = factor(direction, levels = c("Inflow", "Outflow", "Net inflow"))
  ) |> 
  arrange(direction) |> 
  pivot_wider(
    names_from = direction, values_from = c(K_USD, pct_change), 
    names_vary = "slowest"
  ) |> 
  
  view()
  



# data_select2 %>%
#   filter(
#     type == "total", 
#     !direction == "Net inflow",
#     !grepl("arima|bats", model_name),
#     date >= ym("2019-01"),
#     period == "YoY"
#   ) |> 
#   mutate(
#     across(matches("K_USD|ribbon"), ~.x / 1e6)
#   ) |> 
#   pivot_wider(names_from = direction, values_from = matches("K_USD|ribbon")) |> 
#   mutate(
#     net_inflow = K_USD_Inflow - K_USD_Outflow,
#     net_inflow_ribbon_low  = ribbon_low_Inflow - ribbon_low_Outflow,
#     net_inflow_ribbon_high = ribbon_high_Inflow - ribbon_high_Outflow
#   ) |> 
#   select(-matches("Inflow|Outflow", ignore.case = FALSE)) |> 
#   ggplot(aes(date, net_inflow, color = model_name, lty = model_name)) +
#   # facet_wrap(scales = "free_y") +
#   geom_line(linewidth = 1) +
#   geom_ribbon(aes(ymin = net_inflow_ribbon_low, ymax = net_inflow_ribbon_high), alpha = .2) +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   scale_y_continuous(labels = number_format(), n.breaks = 7) +
#   scale_linetype_manual(values = c(1, 2, 2, 2, 2))

