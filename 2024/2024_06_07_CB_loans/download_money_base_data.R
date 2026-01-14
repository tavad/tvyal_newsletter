library(tidyverse)
library(RcppRoll)
library(readxl)
library(janitor)

script_path = "/home/tavad/R/newsletter/2024/2024_06_07_CB_loans"

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

# URL and destination file mapping
urls <- c(
  "https://www.cba.am/stat/stat_data_eng/2_Money%20Base_eng.xlsx",
  "https://www.cba.am/stat/stat_data_eng/Internet-Baza_Daily_ENGnew.xlsx"
)
dest_files <- c(
  "2_Money_Base_eng.xlsx",
  "2_Money_Base_dayly_2025.xlsx"
)

# Download each file using base R
for (i in seq_along(urls)) {
  download.file(
    url = urls[i],
    destfile = dest_files[i],
    method = "libcurl",
    extra = paste0("--user-agent='", ua, "'")
  )
}


# system("curl --insecure 'https://www.cba.am/stat/stat_data_eng/2_Money%20Base_eng.xlsx' -o '2_Money_Base_eng.xlsx'")
# # system("curl --insecure 'https://www.cba.am/stat/stat_data_eng/Internet-Baza_2024%20ENGnew.xlsx' -o '2_Money_Base_dayly_2024.xlsx'")
# system("curl 'https://www.cba.am/stat/stat_data_eng/Internet-Baza_Daily_ENGnew.xlsx' -o '2_Money_Base_dayly_2025.xlsx'")


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

money_base |> write_excel_csv(file.path(script_path, "money_base.csv"))

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


money_base_daily |> write_excel_csv(file.path(script_path, "money_base_daily.csv"))





