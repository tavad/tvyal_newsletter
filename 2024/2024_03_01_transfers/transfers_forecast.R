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
  mutate(date = ym(paste(year(date), month(date)))) |> 
  group_by(date, direction, type) |> 
  summarise(K_USD = sum(K_USD, na.rm = TRUE), .groups = "drop")

first_date_values <- 
  transfers_total |> 
  filter(date == min(date)) |> 
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

data_select <- 
  arm_transfers_forecasts_clean %>% 
  arrange(direction, type, model_name, date) %>% 
  pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") %>% 
  pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") %>% 
  mutate(high = parse_number(high),
         low = parse_number(low)) %>%
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

data_select2 <- 
  data_select1 %>% 
  mutate(model_name = paste0("Forecast ", model_name)) %>% 
  mutate(model_name = ifelse(key == "actual", "Actual Data", model_name)) %>% 
  select(-key) %>% 
  unique()

#### plotting

forecast_plot <- 
  data_select2 %>%
  filter(type == "total") |> 
  ggplot(aes(date, K_USD, color = model_name, lty = model_name)) +
  facet_wrap(~direction, scales = "free_y") +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low , ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # scale_y_continuous(labels = dollar_format()) +
  ggthemes::scale_color_wsj() +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2))

forecast_plot



