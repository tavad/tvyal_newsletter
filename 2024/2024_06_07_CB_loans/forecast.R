library(tidyverse)
library(RcppRoll)
library(scales)
library(readxl)
library(janitor)

# rm(list = ls()); gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../../initial_setup.R")

system("curl --insecure 'https://www.cba.am/stat/stat_data_eng/2_Money%20Base_eng.xlsx' -o '2_Money_Base_eng.xlsx'")
# system("curl --insecure 'https://www.cba.am/stat/stat_data_eng/Internet-Baza_2024%20ENGnew.xlsx' -o '2_Money_Base_dayly_2024.xlsx'")
system("curl 'https://www.cba.am/stat/stat_data_eng/Internet-Baza_Daily_ENGnew.xlsx' -o '2_Money_Base_dayly_2025.xlsx'")


money_base_raw <- read_excel("2_Money_Base_eng.xlsx", skip = 3)
money_base_daily_raw_2024 <- read_excel("2_Money_Base_dayly_2024.xlsx", skip = 3)
money_base_daily_raw_2025 <- read_excel("2_Money_Base_dayly_2025.xlsx", skip = 3)

indicator_names_dict <- 
  tibble(
    indicator_arm = c(
      "Զուտ միջազգային պահուստներ", "Զուտ ներքին ակտիվներ",
      "Կառավարություն", "Բանկեր", "այդ թվում՝ ՌԵՊՈ համաձայնագրեր",
      "ԿԲ կողմից արտարժ. ներգր. ՍՎՈՓ", "Ավանդներ (-)", "Հակադարձ ՌԵՊՈ (-)",
      "ԿԲ կողմից արտարժ. տրամ. ՍՎՈՓ (-)", "ԿԲ կողմից թողարկած արժեթղթեր (-)",
      "Զուտ այլ ակտիվներ", "Փողի բազա", 
      "ԿԲ-ից դուրս կանխիկ դրամ", "Թղթակցային հաշիվներ դրամով",
      "Թղթակցային հաշիվներ արտարժութով",
      "Այլ հաշիվներ", "Այլ հաշիվներ դրամով", "Այլ հաշիվներ արտարժույթով"
    ),
    indicator = c(
      "NET INTERNATIONAL RESERVES 1", "NET DOMESTIC ASSETS",
      "Government", "Banks", "including Repo agreements",
      "CBA foreign currency swap (FX attraction)", "Deposits (-)", "Reverse repo (-)", 
      "CBA foreign currency swap (FX allocation) (-)", "Securities issued by the CBA (-)",
      "Other assets, net", "MONETARY BASE", 
      "Currency outside the CBA", "Correspondent accounts in dram", 
      "Correspondent accounts in FX", 
      "Other accounts", "Other accounts in drams", "Other accounts in FX"
    )
  )


money_base <- 
  money_base_raw |> 
  rename(indicator = 1) |> 
  pivot_longer(-indicator, names_to = "date") |> 
  filter(!is.na(value), !is.na(indicator)) |> 
  mutate(
    date = str_trim(date),
    date_temp = ifelse(grepl("-", date), date, NA),
    date_temp = str_remove_all(date_temp, " ?\\d$"),
    date_temp = my(date_temp),
    date = as.integer(date),
    date = as.Date(date, origin = "1899-12-30"),
    date = if_else(is.na(date), date_temp, date),
    year = year(date),
    month = month(date),
    date = ym(paste(year, month)),
    date = date + months(1) - days(1)
  ) |>
  select(-date_temp) |> 
  relocate(date, year, month) |> 
  left_join(indicator_names_dict, by = "indicator")

clean_dayly_indicators <- function(data){
  
  data_return <- 
    data |> 
    slice_head(n = 25) |> 
    rename(indicator = 1) |> 
    pivot_longer(-indicator, names_to = "date") |> 
    filter(!is.na(value), !is.na(indicator)) |> 
    mutate(
      date = str_trim(date),
      date_temp = ifelse(grepl("-|\\.", date), date, NA),
      date_temp = str_remove_all(date_temp, " \\d$"),
      date_temp = dmy(date_temp),
      date = as.integer(date),
      date = as.Date(date, origin = "1899-12-30"),
      date = if_else(is.na(date), date_temp, date),
      year = year(date),
      month = month(date),
      indicator = str_trim(indicator),
      indicator = fct_inorder(indicator)
    ) |> 
    select(-date_temp) |> 
    relocate(date, year, month) |> 
    left_join(indicator_names_dict, by = "indicator") 
  
  return(data_return)
  
}

money_base_daily_2024 <- clean_dayly_indicators(money_base_daily_raw_2024)
money_base_daily_2025 <- 
  clean_dayly_indicators(money_base_daily_raw_2025) |> 
  filter(year >= 2025, !is.na(date))

money_base_daily <- 
  bind_rows(money_base_daily_2024, money_base_daily_2025)

money_base_daily |> 
  count(indicator, indicator_arm) |>
  pull(indicator)

money_base_daily |> 
  filter(indicator == "Net international reserves (NIR)") |> 
  ggplot(aes(date, value / 1000)) +
  geom_line()

max_date <- money_base_daily$date |> max()
max_date

money_base_daily |> 
  filter(indicator == "Correspondent accounts (in FX)") |> 
  ggplot(aes(date, value / 1000)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    x = NULL, y = NULL,
    title = "Correspondent accounts (in FX)",
    subtitle = paste("billion AMD, max date:", max_date)
  )

money_base_daily |> 
  filter(value != 0) |> 
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y")






money_base |> 
  filter(
    year >= 2000,
    !indicator %in% c(
      "Government",
      "CBA foreign currency swap (FX attraction)", "Deposits (-)", "Reverse repo (-)", 
      "CBA foreign currency swap (FX allocation) (-)", "Securities issued by the CBA (-)",
      "Other assets, net"
    )
  ) |> 
  mutate(
    indicator = fct_inorder(indicator)
  ) |> 
  ggplot(aes(date, value, color = indicator_arm)) +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y")

money_base |> 
  filter(
    year >= 2000,
    !indicator %in% c(
      "Government",
      "CBA foreign currency swap (FX attraction)", "Deposits (-)", "Reverse repo (-)", 
      "CBA foreign currency swap (FX allocation) (-)", "Securities issued by the CBA (-)",
      "Other assets, net"
    )
  ) |> 
  filter(indicator == "Correspondent accounts in FX") |> 
  
  mutate(
    indicator = fct_inorder(indicator)
  ) |> 
  ggplot(aes(date, value, color = indicator_arm)) +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_log10()


money_base |> 
  group_by(year) |> 
  filter(
    year >= 2006,
    month == max(month),
    indicator_arm %in% c(
      "ԿԲ-ից դուրս կանխիկ դրամ", "Թղթակցային հաշիվներ դրամով",
      "Թղթակցային հաշիվներ արտարժութով",
      "Այլ հաշիվներ"
    )
  ) |> 
  mutate(
    date = date + months(1) - days(1),
    value_txt = number(value / 1000, accuracy = 1),
    value_txt = ifelse(value / 1000 >= 50, value_txt, NA)
  ) |> 
  ungroup() |> 
  mutate(
    indicator_arm = fct_inorder(indicator_arm)
  ) |> 
  ggplot(aes(year, value /1000, fill = indicator_arm, label = value_txt)) +
  geom_col() +
  geom_text(position = position_stack(vjust = .5)) +
  scale_x_continuous(breaks = seq(2006, 2026, 2)) +
  scale_y_continuous(labels = number_format()) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Փողի բազան Հայաստանում (Monetary Base)",
    subtitle = "Մլրդ դրամ, 2024-ը ապրիլի դրությամբ",
    caption = paste0(caption_arm, "   |   տվյալների աղբյուր՝ cba.am")
  )

data1 <- 
  money_base |> 
  filter(
    year >= 2012,
    indicator_arm %in% c(
      "Թղթակցային հաշիվներ արտարժութով"
    ),
    date < ymd("2023-12-31")
  ) |> 
  mutate(
    # value = log10(value)
  )

data1_1 <- 
  money_base_daily |> 
  filter(indicator == "Correspondent accounts (in FX)") |> 
  mutate(
    indicator_arm = "Թղթակցային հաշիվներ արտարժութով",
    indicator = "Correspondent accounts in FX"
  )

data1 |> 
  bind_rows(data1_1) |> 
  view()
  ggplot(aes(date, value)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%y")


FX <- read_csv("~/R/Gcapatker/2024_03_24_CBA_FX/CBA_FX_data_cleaned.csv")

data2 <- FX |>
  filter(FX_ISO == "USD") |> 
  filter(year >= 2012) |> 
  rename(value = AMD) |> 
  mutate(indicator_arm = "ԱՄՆ դոլար / ՀՀ դրամ փոխարժեք")


date_decimal_converter <- 
  function(date){
    year(date) + (yday(date) - 1)/366
  }

rect_data <- 
  tibble(
    xmin=ymd("2013-12-01", "2022-12-01"), 
    xmax=ymd("2014-12-25", "2024-06-01"), 
    ymin=c(0, 0), 
    ymax=c(800, 800)
  )

rect_data_decimal <- 
  rect_data |> 
  mutate(
    xmin = date_decimal_converter(xmin),
    xmax = date_decimal_converter(xmax)
  )
  


bind_rows(
  data1, data1_1, data2
) |> 
  bind_rows(
    tibble(
      date = seq.Date(ymd("2012-01-31"), today(), by = "days")
    )
  ) |> 
  complete(
    date, nesting(indicator, indicator_arm, FX_ISO)
  ) |> 
  filter(!is.na(indicator_arm)) |> 
  arrange(indicator, indicator_arm, date) |> 
  group_by(indicator, indicator_arm) |> 
  mutate(value = zoo::na.approx.default(value, x = date, na.rm = FALSE)) |> 
  filter(date >= ymd("2012-01-31"), !is.na(value)) |> 
  mutate(
    date_decimal = date_decimal_converter(date)
  ) |> 
  mutate(
    value = ifelse(grepl("դոլար", indicator_arm), value, value / 1000),
    indicator_arm = fct_rev(indicator_arm)
  ) |> 
  ggplot() +
  geom_rect(
    data = rect_data_decimal,
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = new_palette_colors[4],
    alpha = 0.3
  ) +
  geom_line(aes(date_decimal, value, color = indicator_arm), size = 1.2) +
  geom_hline(yintercept = 363.9, color = "gray80") +
  scale_x_continuous(breaks = seq(2013, 2025, 2)) +
  scale_y_continuous(
    name = "մլրդ դրամ",
    sec.axis = sec_axis(
      ~ .,
      name = "USD / AMD",  
    )
  ) +
  scale_color_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Արտարժույթով թղթակցային հաշիվների\nև ԱՄՆ դոլար / ՀՀ դրամ փոխարժեքի համադրություն",
    # subtitle = "Մլրդ դրամ",
    caption = paste0(caption_arm, "   |   տվյալների աղբյուր՝ cba.am")
  )


bind_rows(
  data1, data1_1
) |> 
  bind_rows(
    tibble(
      date = seq.Date(ymd("2012-01-31"), today(), by = "days")
    )
  ) |> 
  complete(
    date, nesting(indicator, indicator_arm)
  ) |> 
  filter(!is.na(indicator_arm)) |> 
  arrange(indicator, indicator_arm, date) |> 
  group_by(indicator, indicator_arm) |> 
  mutate(value = zoo::na.approx.default(value, x = date, na.rm = FALSE)) |> 
  filter(date >= ymd("2012-01-31"), !is.na(value)) |> 
  mutate(
    date_decimal = date_decimal_converter(date)
  ) |> 
  mutate(
    value = log10(value)
  ) |> 
  ggplot() +
  # geom_rect(
  #   data = rect_data_decimal,
  #   mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #   fill = new_palette_colors[4],
  #   alpha = 0.3
  # ) +
  geom_line(aes(date_decimal, value, color = indicator_arm), size = 1.2) +
  # forecast::geom_forecast(aes(date_decimal, value, color = indicator_arm)) +
  geom_smooth(aes(date_decimal, value, color = indicator_arm)) +
  geom_hline(yintercept = log10(363900), color = "gray80") +
  scale_x_continuous(breaks = seq(2013, 2025, 2)) +
  scale_y_continuous(
    name = "մլրդ դրամ",
    sec.axis = sec_axis(
      ~ .,
      name = "USD / AMD",  
    )
  ) +
  scale_color_manual(values = new_palette_colors[c(2,6)]) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  )

#########################################################################

money_base_forecast <- 
  money_base |> 
  filter(
    year >= 2009,
    !indicator %in% c(
      "Government",
      "CBA foreign currency swap (FX attraction)", "Deposits (-)", "Reverse repo (-)", 
      "CBA foreign currency swap (FX allocation) (-)", "Securities issued by the CBA (-)",
      "Other assets, net", "NET DOMESTIC ASSETS"
    ),
    !grepl("Other accounts", indicator)
  ) |> 
  select(-c(year, month, indicator_arm)) 
  # bind_rows(
  #   tibble(
  #     date = ymd("2024-05-31"), value = 428756.6
  #   )
  # )

money_base_forecast |> 
  filter(value < 0)

# money_base_daily |> 
#   filter(
#     date == max(date), 
#     indicator == "Correspondent accounts (in FX)"
#   ) |> 
#   view()

# money_base_forecast |> view()


library(tidyverse)
library(tidymodels)

library(modeltime)
library(modeltime.ensemble)
library(timetk)

doParallel::registerDoParallel(cores = 8)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# system("scp root@10.11.11.11:~/projects/gold/PAXGUSD.csv ~/proj/2024_GOLD/PAXGUSD.csv")

# prices_data |>
#   view()



# prices_data <-
#   prices_data |>
#   filter(timestamp <= max(timestamp) - hours(0) - minutes(48))

max_date <- money_base_forecast$date |> max()

assess = 12
lookback = 148
rolls = c(2,4,6,12)
lags = c(assess, assess * 1.5)



prices_data_time_extended_monthly_tbl <- 
  money_base_forecast |> 
  group_by(indicator) |> 
  arrange(indicator) |> 
  arrange(date) |> 
  future_frame(.date_var = date, .length_out = assess, .bind_data = TRUE) |> 
  tk_augment_slidify(
    value, .period = rolls, .f = mean, 
    # .partial = TRUE, 
    .align = "right"
  ) |> 
  tk_augment_lags(contains("value"), .lags = lags) |>
  drop_na(
    paste0("value_roll_", max(rolls), "_lag",  max(lags))
  ) |>
  ungroup()




prices_data_time_extended_monthly_tbl <- 
  prices_data_time_extended_monthly_tbl |> 
  select(date, value, indicator) |> 
  left_join(
    prices_data_time_extended_monthly_tbl |> 
      select_if(~ !any(is.na(.))),
    by = c("date", "indicator")
  )

# prices_data_time_extended_monthly_tbl |> colnames()

prices_data_time_extended_monthly_tbl$date |> min()

money_base_forecast$date |> min()




############################




# prices_data_time_extended_monthly_tbl

prices_data_time_extended_monthly_tbl |> select(contains("value")) |>  colnames()

# price_quarter_time_extended_monthly_tbl |> view()

prices_data_time_extended_monthly_tbl |> 
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  t(.)

prices_data_time_extended_monthly_tbl |> glimpse()

## 4. Data split

price_future <- prices_data_time_extended_monthly_tbl |> filter(is.na(value))
price_future

price_actual <- prices_data_time_extended_monthly_tbl |> filter(!is.na(value))
price_actual

# prices_data_time_extended_monthly_tbl |> view()



### train test split

splits <- 
  timetk::time_series_split(
    price_actual, date_var = date,
    assess = assess, cumulative = TRUE
  )








set.seed(456)
time_cv <- sliding_period(
  training(splits),
  index = date,
  period = "month",
  lookback = 24,
  assess_stop = 12,
  step = 3
)

testing(time_cv[[1]][[1]])



rec_spec <-
  recipe(value ~ ., data = training(splits)) |>
  step_log(all_numeric_predictors(), offset = 1) |> 
  # step_log(value, trained = TRUE) |>
  step_timeseries_signature(date) |>
  step_rm(date) |> 
  step_mutate(across(all_nominal_predictors(), ~factor(.x))) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  # step_zv(all_numeric_predictors()) |> 
  step_interact(terms = ~ value_lag12:date_index.num) |>
  step_normalize(all_numeric_predictors())



# price_lag24, price_roll_2_lag24
# 
rec_spec_date <-
  recipe(value ~ ., data = training(splits)) |>
  step_log(all_numeric_predictors(), offset = 1) |> 
  step_timeseries_signature(date) |>
  step_mutate(across(all_nominal_predictors(), ~factor(.x))) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  # step_zv(all_numeric_predictors()) |>
  step_interact(terms = ~ value_lag12:date_index.num) |>
  # step_select(c(value, timestamp, cols_to_select)) |>
  step_normalize(all_numeric_predictors())


rec_spec |> prep() |> juice() |> glimpse()


rec_spec |> prep() |> juice() |> select(contains("date"))
# colnames() 

# ### model xgboost

# trees min_n tree_depth .config              
# 1357     4         13 


model_xgb_tune <-
  boost_tree(
    tree_depth = tune(),
    trees = tune(),
    min_n = tune(),
    learn_rate = tune(),
    # tree_depth = tune(),
    # trees = tune(),
    # min_n = tune(),
    # learn_rate = tune(),
    # loss_reduction = tune(),
    # sample_size = tune(),
    # stop_iter = tune()
  ) |>
  set_engine("xgboost") |>
  set_mode("regression")

wflw_xgb_tune <- workflow() |>
  add_model(model_xgb_tune) |>
  add_recipe(rec_spec)


# library(finetune)
#
# start = Sys.time()
# set.seed(1651545)
# xgb_rs_tune <- tune_race_anova(
#   object = model_xgb_tune,
#   preprocessor = rec_spec,
#   resamples = time_cv,
#   grid = 10,
#   metrics = metric_set(mae, mape, rsq), #
#   control = control_race(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
# )

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

show_best(xgb_rs_tune) |> view()

autoplot(xgb_rs_tune)

select_best(xgb_rs_tune, metric = "mase")

wflw_xgb <- wflw_xgb_tune %>%
  finalize_workflow(select_best(xgb_rs_tune, metric = "mase")) %>%
  fit(training(splits))

wflw_xgb |> write_rds("wflw_xgb_model_2024_04_02.RDS")

# 
# 
# wflw_xgb_2 <- workflow() |>
#   add_model(
#     boost_tree(
#       tree_depth = 7,
#       trees = 1984,
#       min_n = 3,
#       learn_rate = 0.01
#     ) |>
#       set_engine("xgboost") |>
#       set_mode("regression")
#   ) |>
#   add_recipe(rec_spec) |>
#   fit(training(splits))
# 
 
# ## Phtopet boost
#  
# model_prophet_xg_boost <-
#   modeltime::prophet_boost(
#     growth = "linear",
#     changepoint_num = tune(),
#     changepoint_range = tune(),
#     seasonality_yearly = tune(),
#     seasonality_weekly = tune(),
#     seasonality_daily = tune(),
#     season = "multiplicative",
#     prior_scale_changepoints = tune(),
#     prior_scale_seasonality = tune(),
#     prior_scale_holidays = tune(),
#     tree_depth = tune(),
#     trees = tune(),
#     min_n = tune(),
#     learn_rate = tune()
#   ) %>%
#   set_engine('prophet_xgboost') |>
#   set_mode("regression")
# 
# 
# wflw_prophet_xg_boost_tune <- workflow() |>
#   add_model(model_prophet_xg_boost) |>
#   add_recipe(rec_spec_date)
# 
# 
# # library(finetune)
# #
# # start = Sys.time()
# # set.seed(1651545)
# # xgb_rs_tune <- tune_race_anova(
# #   object = model_xgb_tune,
# #   preprocessor = rec_spec,
# #   resamples = time_cv,
# #   grid = 10,
# #   metrics = metric_set(mae, mape, rsq), #
# #   control = control_race(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
# # )
# 
# start = Sys.time()
# 
# prophet_xg_boost_tune <-
#   wflw_prophet_xg_boost_tune |>
#   tune_grid(
#     resamples = time_cv,
#     grid = 10,
#     metrics = metric_set(mae, mape, rsq, mase), # other metrics: mase, smape, rmse, rsq
#     control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
#   )
# 
# prophet_xg_boost_tune[1,]$.notes[[1]]$note[1]
# beepr::beep()
# 
# (prophet_xg_boost_tune_time = Sys.time() - start)
# # Time difference of 2-4 mins
# 
# show_best(prophet_xg_boost_tune)
# 
# autoplot(prophet_xg_boost_tune)
# 
# select_best(prophet_xg_boost_tune, metric = "mase") |>
#   t()
# 
# wflw_prophet_xg_boost <- wflw_prophet_xg_boost_tune %>%
#   finalize_workflow(select_best(prophet_xg_boost_tune, metric = "mase")) %>%
#   fit(training(splits))

# wflw_prophet_xg_boost |> write_rds("wflw_prophet_xgb_model_2024_04_02.RDS")






model_glmnet <- linear_reg(penalty = tune(), mixture = tune()) |>   # tuneable parameter with CV
  set_engine("glmnet")


wflw_glmnet <- workflow() |> 
  add_model(model_glmnet) |> 
  add_recipe(rec_spec)


glmnet_tune <-
  wflw_glmnet |>
  tune_grid(
    resamples = time_cv,
    grid = 30,
    metrics = metric_set(mae, mape, rsq), # other metrics: mase, smape, rmse
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
  )

glmnet_tune[1,]$.notes[[1]]$note[1]


show_best(glmnet_tune)

autoplot(glmnet_tune)

select_best(glmnet_tune, metric = "mae")
# 
wflw_glmnet <- wflw_glmnet %>%
  finalize_workflow(select_best(glmnet_tune, metric = "rsq")) %>%
  fit(training(splits))

### model bag tree

library("baguette")

model_bag_tree_rpart <-
  bag_tree() |> 
  set_engine("rpart") |> 
  set_mode("regression")

wflw_bag_tree <- workflow() |> 
  add_model(model_bag_tree_rpart) |> 
  add_recipe(rec_spec) |> 
  fit(training(splits))


### model exp smoothing




model_exp_smoothing_ets <-
  modeltime::exp_smoothing() |>
  set_engine('ets')

wflw_exp_smoothing_ets <- workflow() |>
  add_model(model_exp_smoothing_ets) |>
  add_recipe(rec_spec_date) |>
  fit(training(splits))


## MODEL poisson_reg_glmnet

library(poissonreg)

model_poisson_reg_glmnet <-
  poisson_reg(penalty = tune(), mixture = tune()) |>
  set_engine('glmnet')


wflw_poisson_reg_glmnet <- workflow() |>
  add_model(model_poisson_reg_glmnet) |>
  add_recipe(rec_spec)


poisson_reg_glmnet_tune <-
  wflw_poisson_reg_glmnet |>
  tune_grid(
    resamples = time_cv,
    grid = 30,
    metrics = metric_set(mae, mape, rsq), # other metrics: mase, smape, rmse, rsq
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
  )

poisson_reg_glmnet_tune[1,]$.notes[[1]]$note[1]


show_best(poisson_reg_glmnet_tune)

autoplot(poisson_reg_glmnet_tune)

select_best(poisson_reg_glmnet_tune, metric = "mae")
# 
wflw_poisson_reg_glmnet <- wflw_poisson_reg_glmnet %>%
  finalize_workflow(select_best(poisson_reg_glmnet_tune, metric = "rsq")) %>%
  fit(training(splits))


## model_prophet

model_prophet_reg_prophet_spec <-
  modeltime::prophet_reg(
    growth = "linear",
    changepoint_num = tune(),
    changepoint_range = tune(), 
    seasonality_weekly = tune(),
    seasonality_daily = tune(), 
    season = tune(), 
    prior_scale_changepoints = tune(),
    prior_scale_seasonality = tune()
  ) %>%
  set_engine('prophet')

# 1] "\033[1m\033[33mError\033[39m in `glubort()`
# :\033[22m\n\033[33m!\033[39m Capacities must be supplied
# for `growth = 'logistic'`. Try specifying at least one of
# 'logistic_cap' or 'logistic_floor'"

wflw_prophet_reg_prophet <- workflow() |>
  add_model(model_prophet_reg_prophet_spec) |>
  add_recipe(rec_spec_date)

start = Sys.time()

wflw_prophet_reg_tune <-
  wflw_prophet_reg_prophet |>
  tune_grid(
    resamples = time_cv,
    grid = 10,
    metrics = metric_set(mae, mape, rsq), # other metrics: mase, smape, rmse, rsq
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
  )

wflw_prophet_reg_tune[1,]$.notes[[1]]$note[1]
beepr::beep()

(prophet_reg_time = Sys.time() - start)

select_best(wflw_prophet_reg_tune, metric = "mae")
# 
model_prophet_reg_prophet <- wflw_prophet_reg_prophet %>%
  finalize_workflow(select_best(wflw_prophet_reg_tune, metric = "rsq")) %>%
  fit(training(splits))



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

median_recipe <- 
  recipe(value ~ ., data = training(splits))
# step_log(matches("price_roll|price_lag")) |> 
# step_log(price, trained = FALSE)

### model median 4

model_median_4 <- window_reg(window_size = 4) |> 
  set_engine(
    engine = "window_function", window_function = median, na.rm = TRUE
  )

wflw_win_median_4 <- workflow() |> 
  add_model(model_median_4) |> 
  add_recipe(median_recipe) |> 
  fit(training(splits))

### model median 6

model_median_6 <- window_reg(window_size = 6) |> 
  set_engine(
    engine = "window_function", window_function = median, na.rm = TRUE
  )

wflw_win_median_6 <- workflow() |> 
  add_model(model_median_6) |> 
  add_recipe(median_recipe) |> 
  fit(training(splits))


## 6. ModelTime workflow

modeltime_calib_tbl <- 
  modeltime_table(
    wflw_xgb
    # # wflw_xgb_h2o,
    # wflw_glmnet,
    # wflw_poisson_reg_glmnet,
    # wflw_bag_tree,
    # # model_prophet_reg_prophet,
    # wflw_exp_smoothing_ets,
    # # wflw_seasonal_reg_stlm_ets,
    # wflw_win_median_4,
    # wflw_win_median_6
  ) |> 
  modeltime_calibrate(testing(splits))








## 6. ModelTime workflow
# 
# modeltime_calib_tbl <- 
#   modeltime_table(
#     wflw_xgb
#     # wflw_prophet_xg_boost
#   ) |> 
#   modeltime_calibrate(testing(splits))

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

# test_forecast_tbl |> view()

## 7 Performance Evaluation for the next year

total_clv_prediction <- 
  test_forecast_tbl |> 
  filter(!is.na(.model_id)) |> 
  group_by(.model_id, .model_desc, indicator) |> 
  transmute(
    prediction = .value, actual = value, indicator
  ) |> 
  ungroup()

total_clv_prediction |> 
  group_by(.model_id, .model_desc, indicator) |> 
  rsq(actual, prediction) |> 
  view()

### Prediction Quality

g <- total_clv_prediction |> 
  mutate(.model_desc = paste(.model_id, .model_desc)) |> 
  ggplot(aes(actual, prediction, color = .model_desc)) +
  geom_point() +
  geom_smooth(aes(group = .model_desc), method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~indicator, scales = "free")
  coord_fixed()

plotly::ggplotly(g)

total_clv_prediction |> 
  filter( indicator == "Correspondent accounts in dram") |>
  rsq(prediction, actual)



modeltime_calib_tbl |> 
  dplyr::slice(1) |> 
  pull(.calibration_data) %>% .[[1]] |> 
  ggplot(aes(date,  .residuals)) +
  geom_line()

###########################################     ????????????????

price_future$date |> max()

future_forecast_tbl <- modeltime_calib_tbl |> 
  # filter(.model_id %in% c(1,4)) |> 
  modeltime_forecast(
    new_data = price_future,
    actual_data = price_actual,
    conf_by_id = TRUE,
    conf_method = "conformal_default",
    id = "indicator", # Specify the appropriate column name if 'id' column exists in calibration data
    keep_data = TRUE
  )


future_forecast_tbl |> view()

future_forecast_tbl |> write_rds("~/R/newsletter/2024/2024_05_31_CB_loans/2_Money_Base_forecast_rds")


future_forecast_tbl <- read_rds("~/R/newsletter/2024/2024_05_31_CB_loans/2_Money_Base_forecast_rds")


future_forecast_tbl

future_forecast_tbl |>
  filter(
    indicator == "Correspondent accounts in FX"
  ) |>  
  bind_rows(
    transmute(
      data1, .value = value, .index = date, 
      indicator = indicator,
      .model_id = NA, .model_desc = "ACTUAL", .key = "actual"
    )
  ) |> 
  unique() |> 
  mutate(
    across(matches("conf|value"), ~.x/1000),
    across(matches("conf|value"), ~ifelse(grepl("FX|NET", indicator) & .key == "prediction", .x - 130, .x)),
    indicator = factor(indicator, levels = indicator_names_dict$indicator),
    .model_desc = ifelse(.model_desc == "ACTUAL", "Թղթակցային հաշիվներ արտարժույթով", "կանխատեսում (XG-BOOST)")
  ) |> view()
  ggplot() +
  geom_rect(
    data = rect_data,
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = new_palette_colors[4],
    alpha = 0.3
  ) +
  geom_line(aes(.index, .value, color = .model_desc), size = 1.2) +
  geom_ribbon(
    aes(.index, .value, ymin = .conf_lo, ymax = .conf_hi), alpha = 0.1,
    color = new_palette_colors[3], fill = new_palette_colors[3]
  ) +
  geom_line(
    data = data2,
    aes(date, value, color = "ԱՄՆ դոլար / ՀՀ դրամ փոխարժեք"),  size = 1.2
  ) +
  geom_hline(yintercept = 363.9, color = new_palette_colors[4]) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(
    name = "մլրդ դրամ",
    sec.axis = sec_axis(
      ~ .,
      name = "USD / AMD",  
    )
  ) +
  scale_color_manual(values = new_palette_colors[c(6,2,3)]) +
  labs(
    x = NULL, y = NULL, color = NULL, lty = NULL,
    title = "Արտարժույթով թղթակցային հաշիվների\nև ԱՄՆ դոլար / ՀՀ դրամ փոխարժեքի համադրություն",
    # subtitle = "մլրդ դրամ",
    caption = paste0(caption_arm, "   |   տվյալների աղբյուր՝ cba.am")
  )





