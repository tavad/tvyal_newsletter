library(tidyverse)
library(RcppRoll)
library(sweep)
library(timetk)
library(forecast)
library(zoo)

# this code makes a time series forecast on electricity production in Armenia 
# based on the past Data.

#### getting data



electricity_production <- 
  read_csv("electricity_production_arm.csv") |> 
  select(date, type, month_value) |> 
  mutate(month_value = ifelse(is.na(month_value), 0, month_value)) |> 
  filter(
    date >= as.Date("2015-01-01"),
    !type %in% c("???????????????? ??????????????, ??????. ????", "?????? ????????????????????")
  )

#### forecasting

electricity_ts <-
  electricity_production |> 
  nest(data = c(date, month_value)) |> 
  mutate(ts = map(data, tk_ts, start = 2015, freq = 12))

electricity_forecasts <- 
  electricity_ts |> 
  crossing(model_name = c("auto.arima", "ets", "bats", "stlm")) |> 
  mutate(
    model = map2(model_name, ts, ~ exec(.x, .y)),
    forecast = map(model, forecast, h = 24, level = c(50, 80, 95)),
    sweep = map(forecast, sw_sweep)
  )

electricity_forecasts_clean <- 
  electricity_forecasts |> 
  select(-data, -ts, -model, -forecast) |> 
  unnest(sweep) |> 
  mutate(
    index = as.Date(as.yearmon(index), frac = 1)
  ) |> 
  relocate(index) |> 
  rename(date = index) |> 
  mutate(month_value = ifelse(is.na(month_value), value, month_value)) |> 
  select(-value)

#### preparing forecast data for the plot

data_select <- 
  electricity_forecasts_clean |> 
  crossing(period = c("month", "YoY")) |>
  rename(value = month_value) |> 
  arrange(type, model_name, period) |> 
  group_by(type, model_name, period)  |>  
  mutate(
    across(contains(c("lo.", "hi.")),
           ~ ifelse(is.na(.x) & period == "YoY", value , .x)),
    across(contains(c("lo.", "hi.", "value")),
           ~ ifelse(
             period == "YoY",
             roll_sum(.x, n = 12, fill = NA, align = c("right")),
             .x
           )
    ),
    period = ifelse(
      period == "YoY",
      "Year-over-year (YoY) production values and forecast",
      "Monthly production values and forecast"
    ),
    across(contains(c("lo.", "hi.")), ~ ifelse(key == "actual", NA, .x))
  ) |> 
  ungroup() |> 
  pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") |> 
  pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") |> 
  mutate(
    high = parse_number(high),
    low = parse_number(low)
  ) |>
  filter(high == low) |> 
  mutate(interval = high) |> 
  select(-low, -high) |> 
  filter(interval == 0.5)

forecast_first_rows <- 
  data_select |>  
  filter(key == "actual") |> 
  filter(date == max(date)) |>
  mutate(
    key = "forecast",
    ribbon_low = value,
    ribbon_high = value
  )

data_select1 <- 
  data_select |> 
  bind_rows(forecast_first_rows)

data_select2 <- 
  data_select1 |> 
  mutate(model_name = paste0("Forecast ", model_name)) |> 
  mutate(model_name = ifelse(key == "actual", "Actual Data", model_name)) |> 
  select(-key) |> 
  unique()

data_select2 |> 
  write_excel_csv("electricity_forecast.csv")


#### plotting

# see the main Rmd file in this directory for plots