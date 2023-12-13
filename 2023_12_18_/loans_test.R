# file from: https://www.cba.am/en/sitepages/statmonetaryfinancial.aspx

library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

loans_url1 <- "https://www.cba.am/stat/stat_data_eng/6_loans%20by%20sectors_eng.xlsx"
loans_url2 <- "https://www.cba.am/stat/stat_data_eng/7_loans%20by%20branches_eng.xlsx"


system(
  paste0("curl \"", loans_url1, "\" -o \"loans_6_eng.xlsx\"")
)

system(
  paste0("curl \"", loans_url2, "\" -o \"loans_7_eng.xlsx\"")
)



data1 <- read_excel("loans_6_eng.xlsx", sheet = 2, skip = 4)


data1 |>
  rename(info = 1) |> 
  mutate(
    across(-info, ~as.character(.x)),
    across(-info, ~parse_number(.x))
  ) |> 
  pivot_longer(-info, names_to = "date", values_to = "mln_amd") |> 
  mutate(
    info = str_replace(info, "^([//D]+)//d$", "//1"),
    date = as.numeric(date),
    date = as.Date(date, origin = "1899-12-30")
  ) |> 
  filter(
    !info %in% c("AMD", "FX", "Private", "Public", "including"),
    !is.na(info),
    !is.na(mln_amd)
  ) |> 
  ggplot(aes(date, mln_amd, color = info)) +
  geom_line(size = 1.5, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_log10(breaks = 10^(0:10), labels = number_format()) +
  scale_colour_brewer(type = "qual", palette = 3) +
  ggthemes::theme_fivethirtyeight()

################

data2_1 <- read_excel("loans_7_eng.xlsx", sheet = 1, col_names = FALSE)
  
data2_1 |> 
  janitor::row_to_names(row_number = 4, remove_rows_above = FALSE) |> 
  rename(industry = 1) |> 
  filter(!is.na(industry)) |> 
  mutate(
    across(-industry, ~as.character(.x)),
    across(-industry, ~parse_number(.x)),
    type = ifelse(grepl("commercial", tolower(industry)), industry, NA)
  ) |> 
  fill(type, .direction = "down") |> 
  relocate(industry, type) |> 
  pivot_longer(-c(industry, type), names_to = "date", values_to = "mln_amd") |> 
  filter(!is.na(mln_amd)) |> 
  mutate(
    industry = str_replace(industry, "^([//D]+)//d+$", "//1"),
    date = as.numeric(date),
    date = as.Date(date, origin = "1899-12-30")
  ) |> 
  # count(industry)
  ggplot(aes(date, mln_amd, color = industry)) +
  geom_line(size = 1.5, alpha = 0.8) +
  facet_wrap(~type) +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_log10(breaks = 10^(0:10), labels = number_format()) +
  scale_colour_brewer(type = "qual", palette = 3) +
  ggthemes::theme_fivethirtyeight()


################################################


data2_2 <- read_excel("loans_7_eng.xlsx", sheet = 2, skip = 3)

data2_2_clean <- 
  data2_2 |> 
  rename(indicator = 1) |> 
  filter(!is.na(indicator)) |> 
  extract(
    indicator, into = c("code", "indicator"),
    regex = ("([A-Z]\\.|\\([\\d\\.\\+\\-/ ]+\\))? ?(.+)")
  ) |> 
  mutate(
    code = str_remove(code, "\\.$"),
    main_code = case_when(
      code %in% LETTERS  ~ code,
      grepl("including", indicator) & code == "" ~ indicator
    ),
    code = ifelse(is.na(main_code), code, "Total")
  ) |> 
  relocate(main_code) |> 
  fill(main_code, .direction = "down") |> 
  pivot_longer(
    -c(main_code, code, indicator),
    names_to = "date", values_to = "K_AMD"
  ) |> 
  filter(!is.na(K_AMD)) |> 
  mutate(
    type = ifelse(grepl("\\.{3}", date), "FX", "AMD"),
    date = ifelse(grepl("\\.{3}", date), NA, date),
    date = as.numeric(date),
    date = as.Date(date, origin = "1899-12-30"),
    K_AMD = parse_number(K_AMD)
  ) |> 
  fill(date, .direction = "down")

data2_2_clean |> 
  filter(
    code == "Total",
    type == "AMD",
    main_code %in% LETTERS
  ) |> 
  ggplot(aes(date, K_AMD, color = main_code)) +
  geom_line() +
  facet_wrap(~main_code, scales = "free_y") +
  scale_y_log10()
  
