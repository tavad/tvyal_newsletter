library(tidyverse)
library(readxl)




fuel_price_1 <- read_csv("~/R/newsletter/2023/2023_11_20_petrol_price/fuel_price_2023_11_17.csv")
fuel_price_2 <- read_excel("~/Բենզինի գներ.xlsx")

petrol_name_dict <- 
  tibble(
    name_arm = c("Բենզին ԱԻ 92 Կ-5", "Բենզին ԱԻ 95 Կ5", "Դիզելային վառելիք Կ5"),
    name_code = c( "petrol_regular", "petrol_premium", "disel")
  )

fuel_price_1 |> view()


fuel_price_2 <- 
  fuel_price_2 |> 
  rename(date = 1, name_arm = 2, value = 3) |> 
  left_join(petrol_name_dict, by = join_by(name_arm)) |> 
  mutate(date = ymd(paste(year(date), month(date), day(date)))) |> 
  select(-name_arm) |> 
  group_by(date, name_code) |> 
  filter(value == max(value)) |> 
  ungroup() |> 
  unique() |> 
  pivot_wider(names_from = name_code) |> 
  fill(petrol_regular, petrol_premium, disel, .direction = "down")


fuel_price_1 |> 
  filter(date < ym("2023-11")) |> 
  bind_rows(fuel_price_2) |>
  write_csv("~/R/newsletter/2024/2024_03_22_petrol_price/fuel_price_2024_03_20.csv")
  
