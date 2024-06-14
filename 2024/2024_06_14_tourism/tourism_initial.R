library(tidyverse)
library(RcppRoll)
library(scales)


# rm(list = ls()); gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../../initial_setup.R")


tourism_monthly <- read_csv("data_monthly.csv")

tourism_monthly |> 
  pivot_longer(-year, names_to = "month") |> 
  mutate(
    date = ym(paste(year, month)) + months(1) - days(1),
    value_yoy = roll_sumr(value, 12)
  ) |> 
  relocate(date) |> 
  filter(!is.na(value)) |> 
  
  ggplot(aes(date, value_yoy)) +
  geom_line()
