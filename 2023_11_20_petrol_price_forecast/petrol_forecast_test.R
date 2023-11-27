library(tidyverse)
library(RcppRoll)
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

last_date_values <- 
  arm_fuel_price |> 
  group_by(description) |> 
  filter(date == max(date))

arm_fuel_price_delta <- 
  crossing(
    date = as.Date(ymd("2022-01-19"):today()),
    description = c("disel", "liquefied_gas_price", "petrol_premium", "petrol_regular")
  ) |> 
  left_join(arm_fuel_price, by = join_by(date, description)) |> 
  arrange(description, date) |> 
  group_by(description) |> 
  fill(value, .direction = "down") |> 
  mutate(
    value = roll_meanr(value, 30),
    delta = value / lag(value)
  ) |> 
  ungroup() |> 
  # select(-usd_amd) |> 
  select(date, description, delta) |>
  na.omit()


#### forecasting

arm_fuelprice_ts <-
  arm_fuel_price_delta |> 
  nest(data = c(date, delta)) |> 
  mutate(ts = map(data, tk_ts, start = as.Date("2022-02-19")))

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
    ribbon_low = delta,
    ribbon_high = delta
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
  ggplot(aes(date, delta, color = model_name, lty = model_name)) +
  facet_wrap(~description, scales = "free_y") +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low , ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  # scale_y_continuous(labels = dollar_format()) +
  ggthemes::scale_color_wsj() +
  scale_linetype_manual(values = c(1, 2, 2, 2))

forecast_plot




##################################################################

#### second approach using crude oil price for the forecast

library(tidymodels)
library(modeltime)

crude_oil_opec <- 
  read_csv("opec_crude_oil_price.csv") |> 
  rename(crude_oil_price = value)

fuel_data_for_forecast <- 
  arm_fuel_price |> 
  filter(description == "petrol_regular") |> 
  select(date, value) |> 
  left_join(crude_oil_opec, by = join_by(date)) |> 
  fill(crude_oil_price, .direction = "down")


set.seed(123)

# activity_time_split <- initial_time_split(activity_data_combined_model, prop = 0.75)

activity_time_split <-
  time_series_split(
    fuel_data_for_forecast, asses = "2 months", 
    date_var = date, cumulative = TRUE, skip = TRUE
  )

activity_time_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value)


activity_time_train <- training(activity_time_split)
activity_time_test <- testing(activity_time_split)


set.seed(456)
activity_time_roll <-
  rolling_origin(
    data = activity_time_train,
    initial = round(nrow(activity_time_train) * 0.75),
    assess = 18,
    cumulative = TRUE,
    skip = TRUE
  )

rec_activity_time_train <- 
  recipe(value ~ ., data = activity_time_train) %>%
  step_log(!contains(c("date", "value")), offset = 30) %>% 
  step_normalize(-c("date", "value"))


final_model <- function(model_name, time_series_model = TRUE){
  wf <- workflow() %>%
    add_model(model_name)
  
  if (time_series_model) {
    wf <- wf %>% add_recipe(rec_activity_time_train)
  } else {
    wf <- wf %>%
      add_recipe(rec_activity_time_train %>% step_mutate(date = as.numeric(date)))
  }
  
  wf %>% fit(activity_time_train)
}


arima_spec <-
  modeltime::arima_reg(
    seasonal_period = 12
  ) %>%
  set_engine('arima')

auto_arima_spec <-
  modeltime::arima_reg(
    seasonal_period = 12
  ) %>%
  set_engine('auto_arima')

adam_reg_adam_spec <-
  modeltime::adam_reg(
    # non_seasonal_ar = 1,
    # use_constant = TRUE,
    # regressors_treatment = "use",
    # distribution = "dinvgauss",
    seasonal_period = 12
  ) %>%
  set_engine('adam')

adam_reg_auto_adam_spec <-
  modeltime::adam_reg(
    non_seasonal_ar = 1,
    use_constant = TRUE,
    regressors_treatment = "use",
    distribution = "dinvgauss",
    seasonal_period = 12
  ) %>%
  set_engine('auto_adam')

arima_boost_arima_xgboost_spec <-
  modeltime::arima_boost(
    seasonal_period = 12
  ) %>%
  set_engine('arima_xgboost')

arima_boost_auto_arima_xgboost_spec <-
  modeltime::arima_boost() %>%
  set_engine('auto_arima_xgboost')

prophet_reg <- modeltime::prophet_reg(
  seasonality_yearly = TRUE
) %>%
  set_engine("prophet")

linear_reg <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet")

exp_smoothing_ets_spec <-
  modeltime::exp_smoothing() %>%
  set_engine('ets')

exp_smoothing_smooth_es_spec <-
  modeltime::exp_smoothing() %>%
  set_engine('smooth_es')

exp_smoothing_theta_spec <-
  modeltime::exp_smoothing() %>%
  set_engine('theta')

naive_reg_snaive_spec <-
  modeltime::naive_reg() %>%
  set_engine('snaive')

seasonal_reg_stlm_arima_spec <-
  modeltime::seasonal_reg(
    seasonal_period_1 = 12,
    # seasonal_period_2 = 16,
    # seasonal_period_3 = 24
  ) %>%
  set_engine('stlm_arima')

# final_model(linear_reg, time_series_model = FALSE),

linear_reg_spec <-
  linear_reg() %>%
  set_engine('glm')


seasonal_reg_stlm_ets_spec <-
  modeltime::seasonal_reg(seasonal_period_1 = 12) %>%
  set_engine('stlm_ets')

seasonal_reg_tbats_spec <-
  modeltime::seasonal_reg(seasonal_period_1 = 12) %>%
  set_engine('tbats')

temporal_hierarchy_thief_spec_3 <-
  modeltime::temporal_hierarchy(seasonal_period = 12, use_model = "theta") %>%
  set_engine('thief')




model_tbl <- modeltime_table(
  # final_model(linear_reg_spec, time_series_model = FALSE),
  final_model(arima_spec),
  # final_model(auto_arima_spec),
  final_model(adam_reg_adam_spec),
  # final_model(adam_reg_auto_adam_spec),
  # final_model(arima_boost_arima_xgboost_spec),
  # final_model(arima_boost_auto_arima_xgboost_spec),
  final_model(prophet_reg),
  # final_model(exp_smoothing_ets_spec),
  # final_model(exp_smoothing_smooth_es_spec),
  # final_model(exp_smoothing_theta_spec),
  # final_model(naive_reg_snaive_spec),
  final_model(seasonal_reg_stlm_arima_spec)
)


