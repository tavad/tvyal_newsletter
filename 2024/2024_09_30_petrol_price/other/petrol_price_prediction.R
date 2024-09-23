library(tidyverse)
library(tidymodels)

library(modeltime)
library(modeltime.ensemble)
library(timetk)

doParallel::registerDoParallel(cores = 8)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


prices_data <- read_csv("fuel_price_2024_03_20.csv")

usa_fuel_price <- read_csv("usa_fuel_price.csv")

usa_fuel_price |> 
  rename(date = Date) |> 
  mutate(date = ymd(paste(year(date), month(date), day(date))))


usa_fuel_price_filtered <- 
  usa_fuel_price |> 
  left_join(usa_fuel_price_dict, join_by(worksheet_name)) |> 
  filter(!is.na(value)) |> 
  filter(
    grepl("Gasoline|Diesel", description),
    Date >= as.Date("2022-01-01")
  ) |> 
  mutate(
    value = value / 3.78541178, # gallon to liter conversion
    place = case_when(
      grepl("New York", place) ~ "New York Harbor",
      grepl("Gulf Coast", place) ~ "U.S. Gulf Coast",
      grepl("Los Angeles", place) ~ "Los Angeles"
    )
  ) |> 
  filter(
    round(value, digits = 5) != 0.26417,
    !grepl("Diesel", description)
  ) |> 
  rename(date = Date) |> 
  mutate(date = ymd(paste(year(date), month(date), day(date)))) |> 
  mutate(
    name = case_when(
      grepl("New York", place) ~ "petrol_new_york",
      grepl("Gulf", place) ~ "petrol_gulf",
      grepl("Angeles", place) ~ "petrol_los_angeles"
    )
  ) |> 
  select(date, name, value) |> 
  pivot_wider()

prices_data <- 
  prices_data |> 
  select(date, petrol_regular) |> 
  full_join(usa_fuel_price_filtered) |> 
  arrange(date) |> 
  fill(-date, .direction = "down") |> 
  filter(!is.na(petrol_regular))


# prices_data |>
#   view()


prices_data |> 
  ggplot(aes(date, petrol_regular)) +
  geom_line()

max_date <- prices_data$date |> max()

# prices_data |>
#   mutate(symbol = "PAXGUSD") |>
#   group_by(symbol) |>
#   plot_time_series(
#     date, price,
#     .title = "trs", .facet_ncol = 1,
#     .trelliscope = TRUE
#   )

# prices_data |>
#   ggplot(aes(date, close)) +
#   geom_line()


prices_data |>
  psych::pairs.panels(
    # smooth = TRUE,
    # scale = TRUE,
    # density = FALSE,
    # ellipses = TRUE,
    # method = "pearson",
    # pch = 14,
    # lm = FALSE,
    # car = TRUE,
    # jiggle = FALSE,
    # factor = 2,
    # hist.col = 4,
    # starts = TRUE,
    # ci = TRUE
  )


# assess = 30
# lookback = 150
# lags = c(1, 2, 3, 6, 15)



assess = 90
lookback = 360
rolls = c(10,20,30,45,60)
lags = c(assess, assess * 1.5)


# prices_data |> 
#   summarise(
#     across(c(price, open, high, low), ~mean(.x))
#   ) |> 
#   view()





prices_data_time_extended_monthly_tbl <- 
  prices_data |> 
  arrange(date) |> 
  future_frame(.date_var = date, .length_out = assess, .bind_data = TRUE) |> 
  tk_augment_slidify(
    -date, .period = rolls, .f = mean, 
    .partial = TRUE,
    .align = "right"
  ) |> 
  tk_augment_lags(-date, .lags = lags) |>
  drop_na(
    paste0("petrol_regular_roll_", max(rolls), "_lag",  max(lags))
  ) |>
  # drop_na(
  #   paste0("price_lag",  assess*1.5)
  # ) |>
  ungroup()



  # filter(date <= min(date) + minutes(assess*2))


prices_data_time_extended_monthly_tbl <- 
  prices_data_time_extended_monthly_tbl |> 
  select(date, petrol_regular) |> 
  left_join(
    prices_data_time_extended_monthly_tbl |> 
      select_if(~ !any(is.na(.))),
    by = "date"
  )

# prices_data_time_extended_monthly_tbl |> colnames()

prices_data_time_extended_monthly_tbl$date |> min()

prices_data_time_extended_monthly_tbl |> view()




############################




# prices_data_time_extended_monthly_tbl

prices_data_time_extended_monthly_tbl |> select(contains("petrol_regular")) |>  colnames()

# price_quarter_time_extended_monthly_tbl |> view()

prices_data_time_extended_monthly_tbl |> 
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  t(.)

prices_data_time_extended_monthly_tbl |> glimpse()

## 4. Data split

price_future <- prices_data_time_extended_monthly_tbl |> filter(is.na(petrol_regular))
price_future

price_actual <- prices_data_time_extended_monthly_tbl |> filter(!is.na(petrol_regular))
price_actual

# prices_data_time_extended_monthly_tbl |> view()



### train test split

splits <- 
  timetk::time_series_split(
    price_actual, date_var = date,
    assess = assess, cumulative = TRUE
  )



# splits %>%
#   tk_time_series_cv_plan() %>%
#   plot_time_series_cv_plan(date, price)



set.seed(456)
time_cv <- 
  sliding_period(
    training(splits),
    # lookback = 3600 - 1,
    lookback = lookback/2,
    assess_stop = assess,
    index = date,
    period = "day",
    step = lookback / 30
  )



# TODO needs a cross validation

## 5. global models




rec_spec_date <-
  recipe(petrol_regular ~ ., data = training(splits))


rec_spec_date |> prep() |> juice() |> glimpse()

# rec_spec |> prep() |> juice() |> 
#   summarise(across(everything(), ~mean(is.na(.)))) %>%
#   t(.)
  
  
rec_spec_date |> prep() |> juice() |> select(contains("petrol_regular"))
  # colnames() 




# Seasonal decomposition


model_seasonal_reg_stlm_ets <-
  modeltime::seasonal_reg(
    ) %>%
  set_engine('stlm_ets')

wflw_seasonal_reg_stlm_ets <- workflow() |>
  add_model(model_seasonal_reg_stlm_ets) |>
  add_recipe(rec_spec_date) |>
  fit(training(splits))


## Median models

# step_log(matches("price_roll|price_lag")) |> 
# step_log(price, trained = FALSE)

### model median 4

model_median_4 <- window_reg(window_size = 30) |> 
  set_engine(
    engine = "window_function", window_function = median, na.rm = TRUE
  )

wflw_win_median_4 <- workflow() |> 
  add_model(model_median_4) |> 
  add_recipe(rec_spec_date) |> 
  fit(training(splits))

### model median 6

model_median_6 <- window_reg(window_size = 45) |> 
  set_engine(
    engine = "window_function", window_function = median, na.rm = TRUE
  )

wflw_win_median_6 <- workflow() |> 
  add_model(model_median_6) |> 
  add_recipe(rec_spec_date) |> 
  fit(training(splits))


## 6. ModelTime workflow

modeltime_calib_tbl <- 
  modeltime_table(
    wflw_exp_smoothing_ets,
    wflw_seasonal_reg_stlm_ets,
    wflw_win_median_4,
    wflw_win_median_6
  ) |> 
  modeltime_calibrate(testing(splits))

# modeltime_calib_tbl |> write_rds("modeltime_calib_tbl_models.RDS")


### Accuracy
modeltime_calib_tbl |> 
  modeltime_accuracy(acc_by_id = FALSE) |> 
  table_modeltime_accuracy()
# 
# modeltime_calib_tbl |> 
#   modeltime_accuracy(acc_by_id = TRUE) |> 
#   group_by(region) |> 
#   table_modeltime_accuracy()

### Test Forecast Visualization

test_forecast_tbl <- modeltime_calib_tbl |> 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = price_actual,
    # conf_by_id = TRUE,
    conf_method = "conformal_default",
    keep_data = TRUE
  )



test_forecast_tbl |> 
  mutate(symbol = "price_actual") |> 
  group_by(symbol) |> 
  plot_time_series(
    date, petrol_regular, 
    .title = NULL,
    .trelliscope = TRUE
  )

## 7 Performance Evaluation for the next year

total_clv_prediction <- 
  test_forecast_tbl |> 
  filter(!is.na(.model_id)) |> 
  group_by(.model_id, .model_desc) |> 
  transmute(
    prediction = .value, actual = petrol_regular
  ) |> 
  ungroup()
  
total_clv_prediction |> 
  group_by(.model_id, .model_desc) |> 
  mae(actual, prediction)

### Prediction Quality

g <- total_clv_prediction |> 
  mutate(.model_desc = paste(.model_id, .model_desc)) |> 
  ggplot(aes(actual, prediction, color = .model_desc)) +
  geom_point() +
  geom_smooth(aes(group = .model_desc), method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed()

plotly::ggplotly(g)

## 8 Ensemble Model


## 9. FUTURE FORECAST

# refit
refit_table <- modeltime_calib_tbl |> 
  dplyr::slice(2) |>                          #the best model is 1
  modeltime_refit(
    data = filter(price_actual, date >= max(date) - minutes(lookback * 2)) 
  )

refit_table |> 
  pull(.calibration_data) %>% .[[1]] |> 
  ggplot(aes(date,  .residuals)) +
  geom_line()

###########################################     ????????????????

future_forecast_tbl <- refit_table |> 
  modeltime_forecast(
    new_data = price_future,
    actual_data = price_actual,
    # conf_by_id = TRUE,
    conf_method = "conformal_default",
    keep_data = TRUE
  )

future_forecast_tbl |> 
  filter(.index >= max(.index) - minutes(lookback * 2)) |> 
  ggplot(aes(.index, .value, color = .model_desc)) +
  geom_ribbon(aes(ymin = .conf_lo, ymax = .conf_hi), alpha = 0.2) +
  geom_line()

future_forecast_tbl |> 
  filter(grepl("Կենտրոն միջին", region)) |> 
  select(date, .model_desc, .value, matches("conf")) |> 
  mutate(
    price_growth = .value / lag(.value, 4),
    date = ymd(date)
  ) |> 
  view()
  

future_forecast_tbl |> 
  write_csv("future_price_forecast.csv")


future_forecast_tbl |> 
  mutate(.index = .index + months(1) - days(1)) |> 
  filter(grepl("Կենտրոն|Աջափնյակ|Արաբկիր|Ծաղկաձոր|Հրազդան", region)) |>
  # filter(region %in% sample(unique(region), 20)) |> 
  # filter(region %in% unique(region)[1:10]) |> 
  group_by(region) |> 
  plot_modeltime_forecast(
    .trelliscope = TRUE
  )

## 11  Feature importance

wflw_xgb |> 
  extract_fit_engine() %>%
  xgboost::xgb.importance(model = .) |> 
  as_tibble() |> view()

wflw_xgb |> view()

## 10 business impact

ranked_future_churn <- 
  future_forecast_tbl |> 
  filter(!is.na(.model_id)) |> 
  group_by(region) |> 
  summarise(sum_value = sum(.value)) |> 
  arrange(desc(sum_value))

ranked_future_churn |> head(10)

ranked_future_churn |> tail(10)


  