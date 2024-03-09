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
  transmute(date = yq(date), eng, volume =vol_YoY_pct/100) |> 
  na.omit()

max_date <- GDP_quarter_model_data$date |> max()

GDP_quarter_model_data |> 
  filter(eng %in% unique(eng)[1:10]) |> 
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
  tk_augment_slidify(volume_lag4, .period = c(2,4,6), .f = mean, .partial = TRUE) |> 
  ungroup() |> 
  drop_na(volume_lag6, volume_lag4_roll_6)

# GDP_quarter_time_extended_monthly_tbl |> view()

GDP_quarter_time_extended_monthly_tbl |> 
    summarise(across(everything(), ~sum(is.na(.))))

GDP_quarter_time_extended_monthly_tbl |> glimpse()

## 4. Data splot

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

# TODO needs a cross validation

## 5. global models

### recipe preprossesor

rec_spec <- recipe(volume ~ ., data = training(splits)) |> 
  step_timeseries_signature(date) |> 
  step_rm(date) |> 
  step_nzv(all_numeric_predictors()) |> 
  step_mutate(eng = as.factor(eng)) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)


rec_spec |> prep() |> juice()

### model xgboost

model_xgb <- 
  boost_tree(mode = "regression", learn_rate = 0.1) |>  # tuneable parameter with CV
  set_engine("xgboost")

wflw_xgb <- workflow() |> 
  add_model(model_xgb) |> 
  add_recipe(rec_spec) |> 
  fit(training(splits))

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
