library(tidyverse)
library(sweep)
library(timetk)
library(forecast)
library(zoo)

# this code makes a time series forecast on fuel price in Armenia 
# based on the past Data.

#### geting data

usd_amd <- 
  read_csv("exchanges.csv") |> 
  filter(name == "USD_AMD") |> 
  transmute(date, usd_amd = exchange)

arm_fuel_price <- 
  read_csv("../2023_11_20_petrol_price/fuel_price_2023_11_17.csv") |> 
  add_row(
    tibble(date = as.Date("2023-11-18") + 0:9)
  ) |> 
  fill(-date, .direction = "down") |> 
  pivot_longer(-date, names_to = "description") |> 
  left_join(usd_amd, join_by(date)) |> 
  fill(usd_amd, .direction = "down") |> 
  mutate(value = value / usd_amd)

#### forecasting

arm_fuelprice_ts <-
  crossing(
    date = as.Date(ymd("2022-01-19"):today()),
    description = c("disel", "liquefied_gas_price", "petrol_premium", "petrol_regular")
  ) |> 
  left_join(arm_fuel_price, by = join_by(date, description)) |> 
  arrange(description, date) |> 
  group_by(description) |> 
  fill(value, .direction = "down") |> 
  ungroup() |> 
  select(-usd_amd) |> 
  nest(data = c(date, value)) |> 
  mutate(ts = map(data, tk_ts, start = as.Date("2022-01-19")))

arm_fuelprice_forecasts <- 
  arm_fuelprice_ts |> 
  crossing(model_name = c("auto.arima", "ets", "bats")) |> 
  mutate(
    model = map2(model_name, ts, ~ exec(.x, .y)),
    forecast = map(model, forecast, h = 365, level = c(50, 80, 95)),
    sweep = map(forecast, sw_sweep)
  )

arm_fuelprice_forecasts_clean <- 
  arm_fuelprice_forecasts %>% 
  select(-data, -ts, -model, -forecast) %>%
  unnest(sweep) %>% 
  mutate(
    across(contains(c("lo.", "hi.")), ~ if_else(.x <= 0, 0, .x)),
    date = as.Date(as.Date(index), frac = 1)
  ) |> 
  relocate(date) |> 
  select(-index)

#### preparing forecast data for the plot

data_select <- 
  arm_fuelprice_forecasts_clean %>% 
  arrange(description, model_name, date) %>% 
  pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") %>% 
  pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") %>% 
  mutate(high = parse_number(high),
         low = parse_number(low)) %>%
  filter(high == low) %>% 
  rename(interval = high) %>% 
  select(-low) %>%
  filter(interval == 0.8)


forecast_first_rows <- 
  data_select %>%  
  filter(key == "actual") |> 
  filter(date == max(date)) %>%
  mutate(
    key = "forecast",
    ribbon_low = value,
    ribbon_high = value
  )

data_select1 <- 
  data_select %>% 
  bind_rows(forecast_first_rows)

data_select2 <- 
  data_select1 %>% 
  mutate(model_name = paste0("Forecast ", model_name)) %>% 
  mutate(model_name = ifelse(key == "actual", "Actual Data", model_name)) %>% 
  select(-key) %>% 
  unique()

#### plotting

forecast_plot <- 
  data_select2 %>%
  ggplot(aes(date, value, color = model_name, lty = model_name)) +
  facet_wrap(~description, scales = "free_y") +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low , ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  scale_y_continuous(labels = dollar_format()) +
  ggthemes::scale_color_wsj() +
  scale_linetype_manual(values = c(1, 2, 2, 2))

forecast_plot
