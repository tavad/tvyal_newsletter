# folder = "~/R/Gcapatker/2023_12_08_armstat_publications_scrape/7z/"
folder = "C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/"


files_dictionary <- 
  tibble(
    path = list.files(paste0(folder, "extracted/"), recursive = TRUE, full.names = TRUE)
  ) |> 
  extract(
    path, into = c("language", "date", "code", "filetype"),
    regex = ".+extracted/([a-z]+)_([\\d_]+)/(.+)\\.([a-z]+)$",
    remove = FALSE
  )

files_dictionary <- 
  files_dictionary |> 
  mutate(
    date = ym(date)
  ) |> 
  extract(
   code, "code", regex = ".+_(\\d+)$" 
  ) |> 
  mutate(code = as.numeric(code))


socio_economic_tables_info <- 
  read_csv("C:/Users/Lenovo/Desktop/R/newsletter/2023_12_11_/socio_economic_tables_info.csv")

joind_dictionary <- 
  socio_economic_tables_info |> 
  filter(!is.na(code)) |> 
  mutate(code = ifelse(grepl("^5", code), code*10, code)) |> 
  inner_join(files_dictionary, join_by(date, code))


ardyunaberutyun <- 
  joind_dictionary |> 
  filter(
    # grepl("միջազգային զբոսաշրջություն", tolower(info)),
    # grepl("իրավախախտումներ", tolower(info)),
    # grepl("անշարժ", tolower(info)),
    # grepl("հաշվ", tolower(info)),
    grepl("արդյունաբերություն", tolower(info))
  )


ardyunaberutyun <- 
  ardyunaberutyun |> 
  filter(date != as.Date("2015-09-01")) |> 
  mutate(sheets = map(path, excel_sheets)) |> 
  unnest(sheets)

ardyunaberutyun2 <- 
  ardyunaberutyun |> 
  mutate(database = map2(path, sheets, ~read_excel(.x, sheet = .y, col_names = FALSE)))

ardyunaberutyun2 |> 
  saveRDS("C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/ardyunaberutyun.RDS")



ardyunaberutyun2 <-  
  readRDS("C:/Users/Lenovo/Desktop/R/Gcapatker/2023_12_08_armstat_publications_scrape/ardyunaberutyun.RDS")

library(tidyverse)
library(readxl)

test <- 
  ardyunaberutyun2 |> 
  # slice_sample(n = 100) |> 
  select(date, sheets, database) |> 
  mutate(
    database = map(database, ~mutate(.x, across(everything(), ~as.character(.x))))
  ) |> 
  unnest(database)

test |> 
  rename(x1 = 3) |> 
  filter(
    !if_all(-c(date, sheets), ~is.na(.x)),
    !grepl("___", x1)
  ) |> 
  mutate(
    info = ifelse(
      if_all(-c(date, sheets, x1), ~is.na(.x)),
      x1,
      NA
    )
  ) 

electricity <- 
  test %>%
  rename(x1 = 3) |> 
  filter(
    !if_all(-c(date, sheets), ~is.na(.x)),
    !grepl("___", x1)
    ) %>%
  mutate(
    info = ifelse(
      rowSums(!is.na(select(., -c(date, sheets)))) == 1,
      coalesce(!!!select(., -c(date, sheets))),
      NA_character_
    )
  ) |> 
  filter(!grepl("այդ թվում|որից|ընթացիկ գներով|%-ներ|եռամսյակ", info)) |> 
  fill(info, .direction = "down") |> 
  mutate(
    check = ifelse(info == lag(info), TRUE, FALSE)
  ) |> 
  relocate(date, sheets, info, check) |> 
  filter(check) |> 
  filter(grepl("Էլեկտրաէներգիայի", info))
  
  
  
count(info) |> 
  view()


# արտադրության ծավալներն ըստ տնտեսական գործունեության տեսակների
# արտադրությունն ըստ ՀՀ մարզերի և ք. Երևանի
# արտադրանքի ծավալը և արտադրության ինդեքսները
# Արդյունաբերությամբ զբաղվող տնտեսավարող սուբյեկտների արտադրանքի  արտադրության և իրացման ծավալներն ըստ աշխատողների թվաքանակով որոշվող չափերի
# Էլեկտրաէներգիայի և ջերմաէներգիայի
# Հանքագործական արդյունաբերութ
# Հիմնային  մետաղների  արտադրությամբ
# Հիմնային քիմիական նյութերի, քիմիական արտադրանքների,
# Մանածագործական արտադրատեսակների 
# Մանածագործական և հագուստի  արտադրությամբ զբաղվող կազմակերպությունների
# Մետաղական արտադրատեսակների 
# Պատրաստի մետաղե արտադրատեսակների, համակարգիչների, էլեկտրոնային

compact_data <- function(data) {
  for (row in 1:nrow(data)) {
    non_na_indices <- which(!is.na(data[row, ]))
    data[row, ] <- c(data[row, non_na_indices], rep(NA, ncol(data) - length(non_na_indices)))
  }
  return(data)
}


electricity <- 
  electricity |> 
  compact_data() |> 
  select_if(~any(!is.na(.)))

library(RcppRoll)

electricity_cleaned <- 
  electricity %>%
  select(-c(sheets, info, check)) |> 
  rename(
    year_cumsum = 3, year_prev_cumsum = 4, pct_change = 5, na = 6
  ) |> 
  mutate(
    across(-c(date), ~str_trim(.x))
  ) |> 
  filter(
    !grepl("\\d{4}", x1)
  ) |> 
  select(-na, -pct_change) |> 
  mutate(
    year_cumsum = parse_number(year_cumsum),
    year_prev_cumsum = parse_number(year_prev_cumsum),
    x1 = case_when(
      grepl("արևային", tolower(x1)) ~ "Արևային էլեկտրակայաններ",
      grepl("հողմային", tolower(x1)) ~ "Հողմային էլեկտրակայաններ",
      TRUE ~ x1
    )
  ) %>%
  full_join(
    select(., -year_cumsum) |> 
      rename(year_prev_cumsum2 = year_prev_cumsum) |> 
      mutate(date = date - years(1)),
    by = join_by(date, x1)
  ) |> 
  select(-year_prev_cumsum) |> 
  mutate(year_cumsum = ifelse(is.na(year_cumsum), year_prev_cumsum2, year_cumsum)) |> 
  select(-year_prev_cumsum2) |> 
  complete(date, x1) |>
  arrange(x1, date) |> 
  group_by(x1) |> 
  mutate(
    year_cumsum = zoo::na.approx(year_cumsum, x = date, na.rm = FALSE),
    month_value = ifelse(month(date) == 1, year_cumsum, year_cumsum - lag(year_cumsum)),
    YoY_value = roll_sumr(month_value, 12)
  ) |> 
  ungroup()

electricity_cleaned |> 
  filter(!x1 %in% c("Ջերմային էներգիա, հազ. ԳՋ", "Այլ աղբյուրներ")) |> 
  ggplot(aes(date, month_value)) +
  geom_line() +
  facet_wrap(~x1, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggthemes::theme_fivethirtyeight() +
  labs(
    title = "Ամսական արտադրություն"
  )

library(scales)

electricity_cleaned |> 
  mutate(year = year(date)) |> 
  filter(
    year >= 2022,
    !grepl("ընդամենը|Այլ|ԳՋ|Հողմային", x1),
  ) |> 
  arrange(date) |> 
  group_by(date) |> 
  mutate(
    pct_month = month_value/sum(month_value),
    text = ifelse(pct_month <= 0.01, NA, pct_month),
    text = percent(text, accuracy = 0.1)
  ) |> 
  ggplot(aes(date, month_value, fill = x1, label = text)) +
  geom_col(alpha = 0.7) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%Y") +
  # scale_y_continuous(breaks = seq(0, 1, 0.1), labels = percent_format()) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ggthemes::theme_fivethirtyeight()
  

electricity_cleaned |> 
  filter(
    !grepl("ընդամենը|Այլ|ԳՋ|Հողմային", x1),
    !is.na(YoY_value)
    ) |> 
  arrange(date) |> 
  group_by(date) |> 
  mutate(
    pct_YoY = YoY_value/sum(YoY_value),
    year = year(date)
  ) |> 
  group_by(year) |> 
  mutate(
    text = ifelse(date == max(date) | date == as.Date("2016-02-01"), pct_YoY, NA),
    text = ifelse(text <= 0.01, NA, text),
    text = percent(text, accuracy = 0.1)
  ) |> 
  ungroup() |> 
  ggplot(aes(date, pct_YoY, fill = x1, label = text)) +
  geom_area(alpha = 0.6) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = percent_format()) +
  scale_fill_brewer(type = "qual", palette = 3) +
  ggthemes::theme_fivethirtyeight()
  
  
  # summarise(across(everything(), ~mean(is.na(.)) * 100, .names = "{.col}"))
  



# 
#   your_data <- data.frame(
#     x1 = c(NA, NA, NA, NA, NA, NA, 1, 1, 1),
#     x2 = c(1, 1, 1, NA, NA, NA, 2, 2, 2),
#     x3 = c(NA, NA, NA, NA, NA, NA, 3, 3, 3),
#     x4 = c(2, 2, 2, 1, 1, 1, NA, NA, NA),
#     x5 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#     x6 = c(3, 3, 3, 2, 2, 2, NA, NA, NA),
#     x7 = c(NA, NA, NA, 3, 3, NA, NA, NA, NA),
#     x8 = c(NA, NA, NA, NA, NA, 3, NA, NA, NA)
#   )
#   
#   # Compact the data
#   compact_data_result <- compact_data(your_data)
#   print(compact_data_result)
#   