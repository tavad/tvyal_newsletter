library(tidyverse)
library(RcppRoll)
library(sweep)
library(timetk)
library(forecast)
library(zoo)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# this code makes a time series forecast on exports by Partner Regions in Armenia 
# based on the past Data.

#### getting data


arm_trade_country <- 
  read_csv("arm_trade_country.csv") 


EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Rep.",
       "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
       "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
       "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

EAEU = c("Russian Federation", "Kazakhstan", "Belarus", "Armenia", "Kyrgyzstan")

Middle_East = c("Bahrain", "Egypt", "Iran", "Iraq", "Israel", "Jordan", "Kuwait",
                "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Syria", "Turkey", 
                "United Arab Emirates", "Yemen")

China = c("China", "Hong Kong")

arm_trade_country <- 
  arm_trade_country %>% 
  mutate(country_region = case_when(
    country %in% EU ~ "EU",
    country %in% EAEU ~ "EAEU",
    country %in% Middle_East ~ "Middle East",
    country %in% China ~ "Greater China",
    TRUE ~ "Other countries"
  ))

rm(EU, EAEU, Middle_East, China)

arm_trade_country_region <- 
  arm_trade_country |> 
  group_by(date, country_region) |> 
  summarise(
    export = sum(export),
    import = sum(import_origin),
    .groups = "drop"
  ) |> 
  bind_rows(
    arm_trade_country |> 
      group_by(date) |> 
      summarise(
        country_region = "Total",
        export = sum(export),
        import = sum(import_origin),
        .groups = "drop"
      )
  ) |> 
  pivot_longer(c(import, export), names_to = "direction") |> 
  arrange(country_region, direction, date)



#### forecasting

arm_trade_ts <-
  arm_trade_country_region |> 
  nest(data = c(date, value)) |> 
  mutate(ts = map(data, ~ ts(.x$value, start = c(2007, 1), frequency = 12))) 

arm_trase_forecasts <- 
  arm_trade_ts |> 
  crossing(model_name = c("auto.arima", "ets", "bats", "stlm")) |> 
  mutate(
    model = map2(model_name, ts, ~ {
      if (.x == "stlm") {
        .y <- msts(.y, seasonal.periods = c(12))
      }
      exec(.x, .y)
    }),
    forecast = map(model, forecast, h = 24, level = c(50, 80, 95)),
    sweep = map(forecast, sw_sweep)
  )



arm_trade_forecasts_clean <- 
  arm_trase_forecasts %>% 
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
  arm_trade_forecasts_clean |> 
  arrange(country_region, direction, model_name, date) |> 
  pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") |> 
  pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") |> 
  mutate(high = parse_number(high),
         low = parse_number(low)) |> 
  filter(high == low) |> 
  rename(interval = high) |> 
  select(-low) |> 
  filter(interval == 0.8)


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

#### plotting

forecast_plot <- 
  data_select2 |> 
  filter(
    direction == "export",
    country_region == "Total"
  ) |> 
  ggplot(aes(date, value, color = model_name, lty = model_name)) +
  facet_wrap(~country_region, scales = "free_y") +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low , ymax = ribbon_high), alpha = .2) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  # scale_y_continuous(labels = dollar_format()) +
  ggthemes::scale_color_wsj() +
  scale_linetype_manual(values = c(1, 2, 2, 2, 2))

forecast_plot

# I do not like this. Lets go by all hs2 commodities. But first of all let's check gold.

##################################################################

# hs2 commodities

arm_trade_commodity <- 
  read_csv("arm_trade_commodity.csv")

arm_trade_commodity_groupped <- 
  arm_trade_commodity |> 
  group_by(date, hs2) |> 
  summarise(
    import = sum(import),
    export = sum(export),
    .groups = "drop"
  ) |> 
  pivot_longer(c(import, export), names_to = "direction") |> 
  arrange(hs2, direction, date)




#### forecasting

arm_commodity_ts <-
  arm_trade_commodity_groupped |> 
  filter(hs2 != 71) |> 
  group_by(date,  direction) |> 
  summarise(
    value = sum(value),
    .groups = "drop"
  ) |> 
  nest(data = c(date, value)) |> 
  mutate(ts = map(data, ~ ts(.x$value, start = c(2007, 1), frequency = 12)))

arm_commodity_forecasts <- 
  arm_commodity_ts |> 
  crossing(model_name = c("auto.arima", "ets", "bats", "stlm")) |> 
  mutate(
    model = map2(model_name, ts, ~ {
      if (.x == "stlm") {
        .y <- msts(.y, seasonal.periods = c(12))
      }
      exec(.x, .y)
    }),
    forecast = map(model, forecast, h = 24, level = c(50, 80, 95)),
    sweep = map(forecast, sw_sweep)
  )



arm_commodity_forecasts_clean <- 
  arm_commodity_forecasts %>% 
  select(-data, -ts, -model, -forecast) %>%
  unnest(sweep) %>% 
  mutate(
    across(contains(c("lo.", "hi.")), ~ if_else(.x <= 0, 0, .x)),
    date = as.Date(as.Date(index), frac = 1)
  ) |> 
  relocate(date) |> 
  select(-index)

# arm_commodity_forecasts_clean <-  readRDS("commodity_clean")

#### preparing forecast data for the plot

data_select <- 
  arm_commodity_forecasts_clean %>% 
  crossing(period = c("month", "YoY")) %>% 
  group_by(direction, model_name, period) %>% 
  arrange(direction, model_name, period) %>% 
  mutate(across(contains(c("lo.", "hi.")),
                ~ ifelse(is.na(.x) & period == "YoY", value, .x))) %>%
  mutate(across(contains(c("lo.", "hi.", "value")),
                ~ ifelse(period == "YoY",
                         roll_sum(.x, n = 12, fill = NA, align = c("right")),
                         .x
                )
  )
  ) %>% 
  mutate(across(contains(c("lo.", "hi.")), ~ ifelse(key == "actual", NA, .x))) %>% 
  ungroup() %>% 
  pivot_longer(contains("lo."), names_to = "low", values_to = "ribbon_low") %>% 
  pivot_longer(contains("hi."), names_to = "high", values_to = "ribbon_high") %>% 
  mutate(high = parse_number(high),
         low = parse_number(low)) %>%
  filter(high == low) %>% 
  rename(interval = high) %>% 
  select(-low) %>%
  filter(interval == 0.5) #input3

forecast_first_rows <- data_select %>%  
  filter(key == "actual") %>% 
  filter(date == max(date)) %>%
  mutate(key = "forecast",
         ribbon_low = value,
         ribbon_high = value)

data_select1 <- data_select %>% 
  bind_rows(forecast_first_rows)

data_select2 <- data_select1 %>% 
  mutate(model_name = paste0("Forecast ", model_name)) %>% 
  mutate(model_name = ifelse(key == "actual", "Actual Data", model_name)) %>% 
  select(-key) %>% 
  unique()

#### plotting

# arm_trade_commodity_groupped |> 
#   filter(hs2 == 71, direction == "export") |> 
#   filter(
#     date >= ymd("2024-05-01"), # date <= ymd("2023-09-01")
#   ) |> 
#   summarise(mean(value))

arm_trade_71_add <- 
  arm_trade_commodity_groupped |> 
  filter(hs2 == 71, direction == "export") |> 
  mutate(
    value_71 = value,
    ribbon_high_71 = value,
    ribbon_low_71 = value,
  ) |> 
  select(-value) |> 
  bind_rows(
    tibble(
      date = seq.Date(as.Date("2024-07-01"), as.Date("2026-08-01"), "months"),
      value_71       = 320466,
      ribbon_high_71 = 592583,
      ribbon_low_71  = 121821
    )
  ) |> 
  mutate(
    across(matches("value|ribbon"), ~roll_sumr(.x, 12))
  ) |> 
  filter(date >= ymd("2019-12-01")) |> 
  mutate(
    across(matches("High|low"), ~ifelse(date <= ymd("2024-05-01"), NA, .x))
  ) |> 
  select(-c(hs2, direction))


forecast_plot
data_select2 |> 
  filter(
    direction == "export",
    period == "YoY",
    date >= ymd("2019-12-01"),
    grepl("Data|stlm|ets", model_name)
  ) |> 
  left_join(arm_trade_71_add, by = join_by(date)) |> 
  crossing(info = c("+ 71. gold", "NO 71. gold")) |> 
  mutate(
    date = date + months(1) - days(1),
    value = ifelse(grepl("NO", info), value, value + value_71), 
    ribbon_low = ifelse(grepl("NO", info), ribbon_low, ribbon_low + ribbon_low_71),
    ribbon_high = ifelse(grepl("NO", info), ribbon_high, ribbon_high + ribbon_high_71),
  ) |>
  filter(date <= ymd("2026-01-01")) |> 
  select(-contains("_71"), -interval) |> 
  arrange(direction, info, period, model_name, date) |> 
  # write_csv("exports_forecast.csv")
  mutate(across(matches("ribbon|value"), ~.x/1e6)) |> 
  ggplot(aes(date, value, color = info, lty = model_name)) +
  geom_segment(
    aes(y = 8.5, x = ymd("2020-01-01"), yend = 8.5, xend = ymd("2024-06-30")),
    arrow = arrow(length = unit(0.02, "npc")),
    color = new_palette_colors[3],
    lineend = "round", linejoin = "mitre"
  ) +
  geom_segment(
    aes(y = 0, x = ymd("2024-06-30"), yend = 15, xend = ymd("2024-06-30")),
    color = "gray", linetype = 2,
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ribbon_low , ymax = ribbon_high), alpha = .2) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_text(
    aes(
      x = ymd("2022-01-01"), y = 10.3, fontface = "plain",
      label = "2024թ. հունիսի դրությամբ 12-ամսյա\nարտահանումը կազմել է 13.1 մլրդ դոլար,\nորից 62.4%-ը «Թանկարժեք քարեր և մետաղներ» (ԱՏԳ ԱԱ 71):\n2022թ. արտահանումը 3 մլրդ դոլար էր,\nորից այս ապրանքախումբը՝ 10.8%:"
    ),
    size = 3.2,
    color = "gray40",
    family = "DejaVu Sans Light"  # Change to a font that supports Armenian
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,14,2), labels = dollar_format(), limits = c(0, 15)) +
  scale_color_manual(
    values = new_palette_colors[c(6,2)],
    labels = c("Ամբողջական արտահանումը", "Արտահանումը, բացառությամբ 71. թանկարժեք քարեր և մետաղներ")
  ) +
  scale_linetype_manual(
    values = c(1, 2, 3),
    labels = c("Իրական տվյալներ", "ETS կանխատեսում", "STLM կանխատեսում")
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "ՀՀ արտահանման կանխատեսում",
    subtitle = "մլրդ դոլար, 12-ամսյա կտրվածքով",
    caption = caption_f()
  )


arm_trade_commodity_groupped |> 
  filter(direction == "export") |> 
  mutate(is71 = ifelse(hs2 == 71, "71", "not71")) |> 
  group_by(is71, date) |> 
  summarise(value = sum(value), .groups = "drop") |>
  mutate(value = roll_sumr(value, 12)) |> 
  filter(date >= ymd("2019-12-01")) |> 
  group_by(date) |> 
  mutate(
    total = sum(value),
    pct = value / total
    ) |> 
  view()


arm_trade_commodity_groupped |> 
  filter(
    # direction == "export",
    hs2 == 71, 
    date >= ymd("2022-01-01")
  ) |> 
  mutate(date = date + days(16)) |> 
  ggplot(aes(date, value/1e3)) +
  geom_col() +
  facet_wrap(~direction, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    x = NULL,
    y = NULL,
    title = "Թանկարժեք քարերի և մետաղների արտահանում (ԱՏԳ ԱԱ 71)",
    subtitle = "մլն դոլար, ամսական",
    caption = caption_f()
  )
  

######################################################


data_select2


data_select2 |> 
  filter(
    direction == "export",
    # hs2 == 71,
    # key == "actual",
    grepl("Data", model_name),
    date >= ymd("2024-01-01"),
    # date <= ymd("2023-11-01"),    
  ) |> 
  summarise(value = mean(value))



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
  recipe(value ~ ., data = fuel_data_for_forecast) %>%
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
  final_model(auto_arima_spec),
  # final_model(adam_reg_adam_spec),
  # final_model(adam_reg_auto_adam_spec),
  final_model(arima_boost_arima_xgboost_spec),
  # final_model(arima_boost_auto_arima_xgboost_spec),
  final_model(prophet_reg),
  # final_model(exp_smoothing_ets_spec),
  # final_model(exp_smoothing_smooth_es_spec),
  # final_model(exp_smoothing_theta_spec),
  # final_model(naive_reg_snaive_spec),
  final_model(seasonal_reg_stlm_arima_spec)
)


#################

calib_tbl <- model_tbl %>%
  modeltime_calibrate(activity_time_test)

# * Accuracy ----
calib_tbl %>% modeltime_accuracy() |> 
  write_csv("model_accuracy.scv")
# arrange(rmse)
# mutate(rsq = percent(rsq, accuracy = 0.1))

# write_excel_csv("Aghasi_Tavadyan_Report3/autoreg_models")



# * Test Set Visualization ----
calib_tbl %>%
  modeltime_forecast(
    new_data = 
      fuel_data_for_forecast |> 
      mutate(date = date + days(30)) |> 
      slice_tail(n = 30) |> 
      select(-value),
    actual_data = fuel_data_for_forecast
  ) %>%
  plot_modeltime_forecast() |> 
  saveRDS("model_plot.RDS")



  
