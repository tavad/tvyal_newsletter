# library(rvest)

national_account_urls <- 
  read_html("https://www.armstat.am/am/?nid=202") |> 
  html_elements("a")

national_account_urls <- 
  tibble(
    url =  national_account_urls |>
      html_attr("href") |> 
      str_replace("../", "https://www.armstat.am/"),
    text = national_account_urls |>
      html_text()
  )

economic_activity_url <- 
  national_account_urls |> 
  filter(grepl("ակտիվության ցուցանիշը", text)) |> 
  pull(url)




economic_activity <- 
  rio::import(economic_activity_url, format = "xls", sheet = 4, skip = 6) %>%
  as_tibble()


activity_colnames <- c("year", "month", "iea", "iea_adjusted")

economic_activity <- 
  economic_activity[,c(1,3,5,6)] %>% #Chain-link indices with seasonal adjustment (2019=100%)
  set_names(activity_colnames) %>% 
  fill(year, .direction = "down") %>% 
  filter(!is.na(year)) %>% 
  mutate(date = ym(paste(year, month))) %>% 
  hablar::retype() |> 
  mutate(
    growth_month = iea / lag(iea),
    growth_adjusted_month = iea_adjusted / lag(iea_adjusted),
    growth_year = iea / lag(iea, n = 12),
    growth_adjusted_year = iea_adjusted / lag(iea_adjusted, n = 12)
  )

economic_activity %>% 
  mutate(
    growth_month = iea / lag(iea),
    growth_adjusted_month = iea_adjusted / lag(iea_adjusted),
    growth_year = iea / lag(iea, n = 12),
    growth_adjusted_year = iea_adjusted / lag(iea_adjusted, n = 12)
  ) %>% 
  ggplot() +
  geom_line(aes(date, growth_adjusted_year)) +
  geom_line(aes(date, growth_year), color = "red") +
  # geom_line(aes(date, growth_adjusted_month)) +
  # geom_line(aes(date, growth_month), color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format())

economic_activity %>% 
  ggplot() +
  geom_line(aes(date, iea / 100)) +
  geom_line(aes(date, iea_adjusted / 100), color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(), breaks = seq(0.4, 1.6, 0.2)) +
  labs(
    x = NULL, y = "2019 = 100%",
    title = "Տնտեսական ակտիվության ցուցանիշ"
  )



economic_activity |> 
  filter(year >= 2020) |> 
  na.omit() |> 
  ggplot(aes(date, growth_adjusted_year - 1)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = seq(-0.2,0.2,0.02), labels = percent_format())

##########################################


economic_activity |> 
  filter(year >= max(year) - 1) |> 
  group_by(year) |> 
  mutate(
    month = month(date),
    cum_growth = cumprod(growth_adjusted_month)
  ) |> 
  ggplot(aes(
    month,
    growth_adjusted_year - 1, 
    fill = as.character(year),
    label = percent(growth_adjusted_year - 1, accuracy = 0.1),
  )) +
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_text(position = position_dodge(width = 1)) +
  geom_line(aes(y = cum_growth - 1, color = as.character(year))) +
  scale_fill_brewer(type = "qual", palette = 3)


