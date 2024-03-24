library(tidyverse)
library(tidymodels)

library(modeltime)
library(modeltime.ensemble)
library(timetk)

doParallel::registerDoParallel(cores = 8)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

GDP_quarter <- read_csv("GDP_quarter_tidy.csv")

GDP_quarter_model_data <- 
  GDP_quarter |> 
  # transmute(date = yq(date), eng, volume =vol_YoY_pct/100) |> 
  transmute(date = yq(date), eng, volume = production) |> 
  na.omit()

max_date <- GDP_quarter_model_data$date |> max()

GDP_quarter_model_data |> 
  # filter(eng %in% unique(eng)[1:10]) |> 
  group_by(eng) |> 
  plot_time_series(
    date, volume, 
    .title = "trs", .facet_ncol = 1, 
    .trelliscope = TRUE
  )

GDP_quarter_time_extended_monthly_tbl <- 
  GDP_quarter_model_data |> 
  group_by(eng) |> 
  future_frame(.date_var = date, .length_out = 4, .bind_data = TRUE) |>
  tk_augment_lags(volume, .lags = c(4, 6)) |> 
  tk_augment_slidify(volume_lag4, .period = c(2,4,6), .f = prod, .partial = TRUE) |> 
  ungroup() |> 
  drop_na(volume_lag6, volume_lag4_roll_6)

# GDP_quarter_time_extended_monthly_tbl |> view()

GDP_quarter_time_extended_monthly_tbl |> 
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  t(.)

GDP_quarter_time_extended_monthly_tbl |> glimpse()

## 4. Data split

GDP_future <- GDP_quarter_time_extended_monthly_tbl |> filter(is.na(volume))
GDP_future

GDP_actual <- GDP_quarter_time_extended_monthly_tbl |> filter(!is.na(volume))
GDP_actual


### train test split

splits <- 
  timetk::time_series_split(
    GDP_actual, date_var = date,
    assess = 4, cumulative = TRUE
  )

# splits %>%
#   tk_time_series_cv_plan() %>%
#   plot_time_series_cv_plan(date, volume)


set.seed(456)
traning_time_roll <-
  rolling_origin(
    data = training(splits),
    initial = round(nrow(training(splits)) * 0.8),
    assess = 18,
    cumulative = TRUE,
    skip = TRUE
  )


# TODO needs a cross validation

## 5. global models

### recipe preprossesor

rec_spec <- recipe(volume ~ ., data = training(splits)) |> 
  step_timeseries_signature(date) |> 
  step_rm(date) |> 
  step_nzv(all_numeric_predictors()) |> 
  step_mutate(eng = as.factor(eng)) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_normalize(all_numeric_predictors())


rec_spec |> prep() |> juice()


set.seed(456)
time_cv <- 
  sliding_period(
    training(splits),
    # lookback = 3600 - 1,
    lookback = 20,
    assess_stop = 4,
    index = date,
    period = "minute",
    step = 1
  )


### model xgboost

model_xgb_tune <-
  boost_tree(
    tree_depth = tune(),
    trees = tune(),
    min_n = tune(),
    learn_rate = 0.01
  ) |>
  set_engine("xgboost") |>
  set_mode("regression")


wflw_xgb_tune <- workflow() |>
  add_model(model_xgb_tune) |>
  add_recipe(rec_spec)

start = Sys.time()

xgb_rs_tune <-
  wflw_xgb_tune |>
  tune_grid(
    resamples = time_cv,
    grid = 10,
    metrics = metric_set(mae, mape, rsq, mase), # other metrics: mase, smape, rmse, rsq
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
  )

xgb_rs_tune[1,]$.notes[[1]]$note[1]
beepr::beep()

(xgb_tune_time = Sys.time() - start)
# Time difference of 2-4 mins

show_best(xgb_rs_tune)

autoplot(xgb_rs_tune)

select_best(xgb_rs_tune, metric = "mase")

wflw_xgb <- wflw_xgb_tune %>%
  finalize_workflow(select_best(xgb_rs_tune, metric = "mase")) %>%
  fit(training(splits))



# model_xgb <- 
#   boost_tree(mode = "regression", learn_rate = 0.1) |>  # tuneable parameter with CV
#   set_engine("xgboost")
# 
# wflw_xgb <- workflow() |> 
#   add_model(model_xgb) |> 
#   add_recipe(rec_spec) |> 
#   fit(training(splits))

### model glmnet

model_glmnet <- linear_reg(penalty = 1.5) |>   # tuneable parameter with CV
  set_engine("glmnet")

wflw_glmnet <- workflow() |> 
  add_model(model_glmnet) |> 
  add_recipe(rec_spec) |> 
  fit(training(splits))

### model median 4

model_median_4 <- window_reg(id = "eng", window_size = 4) |> 
  set_engine(
    engine = "window_function", window_function = median, na.rm = TRUE
  )

wflw_win_median_4 <- workflow() |> 
  add_model(model_median_4) |> 
  add_recipe(recipe = recipe(volume ~ ., data = training(splits))) |> 
  fit(training(splits))

### model median 6

model_median_6 <- window_reg(id = "eng", window_size = 6) |> 
  set_engine(
    engine = "window_function", window_function = median, na.rm = TRUE
  )

wflw_win_median_6 <- workflow() |> 
  add_model(model_median_6) |> 
  add_recipe(recipe = recipe(volume ~ ., data = training(splits))) |> 
  fit(training(splits))


## 6. ModelTime workflow

modeltime_calib_tbl <- 
  modeltime_table(
    wflw_xgb,
    wflw_glmnet,
    wflw_win_median_4,
    wflw_win_median_6
  ) |> 
  modeltime_calibrate(testing(splits), id = "eng")

modeltime_calib_tbl

### Accuracy
modeltime_calib_tbl |> 
  modeltime_accuracy(acc_by_id = FALSE) |> 
  table_modeltime_accuracy()

modeltime_calib_tbl |> 
  modeltime_accuracy(acc_by_id = TRUE) |> 
  group_by(eng) |> 
  table_modeltime_accuracy()

### Test Forecast Visualization

test_forecast_tbl <- modeltime_calib_tbl |> 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = GDP_actual,
    conf_by_id = TRUE,
    conf_method = "conformal_default",
    keep_data = TRUE
  )

test_forecast_tbl


test_forecast_tbl |> 
  filter(eng %in% unique(eng)[1:10]) |> 
  group_by(eng) |> 
  plot_time_series(
    date, volume, 
    .title = NULL,
    .trelliscope = TRUE
  )

##7 Performance Evaluation for the next year

total_clv_prediction <- 
  test_forecast_tbl |> 
  filter(!is.na(.model_id)) |> 
  group_by(.model_id, .model_desc, eng) |> 
  summarise(
    prediction = mean(.value),
    actual = mean(volume)
  ) |> 
  ungroup()
  
total_clv_prediction |> 
  group_by(.model_id, .model_desc) |> 
  mae(actual, prediction)

### Prediction Quality

g <- total_clv_prediction |> 
  ggplot(aes(actual, prediction, color = .model_desc)) +
  geom_point() +
  geom_smooth(aes(group = .model_desc), method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed()

plotly::ggplotly(g)

##8 Ensemble Model

### creating model
model_ensemble <- modeltime_calib_tbl |> 
  modeltime.ensemble::ensemble_weighted(loadings = c(0,0.5,1,0))

### Calibrating model
modeltime_calib_2_tbl <- 
  modeltime_calib_tbl |> 
  add_modeltime_model(model_ensemble) |> 
  modeltime_calibrate(testing(splits), id = "eng")

### Global accuracy
modeltime_calib_2_tbl |> 
  modeltime_accuracy(acc_by_id = FALSE) |> 
  table_modeltime_accuracy()

modeltime_calib_2_tbl |> 
  modeltime_accuracy(acc_by_id = TRUE) |> 
  group_by(eng) |> 
  table_modeltime_accuracy()

# test Forecast

test_forecast_2_tbl <- modeltime_calib_2_tbl |> 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = GDP_actual,
    conf_by_id = TRUE,
    conf_method = "conformal_default",
    keep_data = TRUE
  )

total_clv_prediction_2 <- 
  test_forecast_2_tbl |> 
  filter(!is.na(.model_id)) |> 
  group_by(.model_id, .model_desc, eng) |> 
  summarise(
    prediction = mean(.value),
    actual = mean(volume)
  ) |> 
  ungroup()

total_clv_prediction_2 |> 
  group_by(.model_id, .model_desc) |> 
  mae(actual, prediction)

total_clv_prediction_2 |> 
  group_by(.model_id, .model_desc) |> 
  rmse(actual, prediction)

### prediction Quolity

g_2 <- total_clv_prediction_2 |> 
  ggplot(aes(actual, prediction, color = .model_desc)) +
  geom_point() +
  geom_smooth(aes(group = .model_desc), method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10() +
  scale_y_log10() +
  coord_fixed()

plotly::ggplotly(g_2)

## 9. FUTURE FORECAST

# refit
refit_table <- modeltime_calib_2_tbl |> 
  dplyr::slice(5) |>                          #the best model is 3
  modeltime_refit(data = GDP_actual)

future_forecast_tbl <- refit_table |> 
  modeltime_forecast(
    new_data = GDP_future,
    actual_data = GDP_actual,
    conf_by_id = TRUE,
    conf_method = "conformal_default",
    keep_data = TRUE
  )

future_forecast_tbl |> 
  filter(grepl("Domestic p", eng)) |> 
  ggplot(aes(date, .value, color = .model_desc)) +
  geom_ribbon(aes(ymin = .conf_lo, ymax = .conf_hi), alpha = 0.2) +
  geom_line()

future_forecast_tbl |> 
  filter(grepl("Domestic p", eng)) |> 
  select(date, .model_desc, .value, matches("conf")) |> 
  mutate(
    gdp_growth = .value / lag(.value, 4),
    date = ymd(date)
  ) |> 
  view()
  
future_forecast_tbl |> 
  filter(grepl("Domestic p", eng)) |> 
  mutate(
    gdp_growth = (.value / lag(.value, 4) ),
    date = ymd(date)
  ) |> 
  left_join(
    GDP_quarter |> 
      filter(grepl("Domestic p", eng)) |> 
      transmute(date = yq(date), vol_YoY_pct = vol_YoY_pct / 100),
    by = "date"
  ) |> 
  ggplot(aes(date, gdp_growth, color =  .model_desc)) +
  geom_line() +
  # geom_ribbon(aes(ymin = .conf_lo, ymax = .conf_hi), alpha = 0.2) +
  geom_line(aes(date, vol_YoY_pct), color = "blue")
  
future_forecast_tbl |> 
  write_csv("future_gdp_forecast.csv")


future_forecast_tbl |> 
  filter(grepl("Domestic p", eng)) |> 
  # filter(eng %in% unique(eng)[1:10]) |> 
  group_by(eng) |> 
  plot_modeltime_forecast(
    .trelliscope = TRUE
  )

## 11  Feature importance

wflw_xgb |> 
  extract_fit_engine() %>%
  xgboost::xgb.importance(model = .) |> 
  as_tibble()

## 10 business impact

ranked_future_churn <- 
  future_forecast_tbl |> 
  filter(!is.na(.model_id)) |> 
  group_by(eng) |> 
  summarise(sum_value = sum(.value)) |> 
  arrange(desc(sum_value))

ranked_future_churn |> head(10)

ranked_future_churn |> tail(10)


  