library(tidyverse)
library(rvest)
library(RcppRoll)
library(scales)
library(ggstream)

caption_eng <-  "Author: Aghasi Tavadyan   |   tvyal.com   |   tavadyan.com"

national_account_html_elements <- 
  read_html("https://www.armstat.am/am/?nid=202") |> 
  html_elements("a")

national_account_urls <- 
  tibble(
    url = html_attr(national_account_html_elements, "href"),
    text = html_text(national_account_html_elements)
  ) |> 
  filter(grepl("^\\.\\./", url)) |> 
  mutate(
    text = str_trim(text),
    url = str_replace(url, "^\\.\\.", "https://www.armstat.am")
  ) |> 
  filter(text != "")

togharkum_urls <- 
  national_account_urls |> 
  filter(grepl("???????????????????????????????? ??????????????????", text)) |> 
  view()
pull(url)

data <-   
  rio::import("https://www.armstat.am/file/doc/99541893.xls", skip = 4) |> 
  as_tibble()




main_data <- 
  data |> 
  rename(code = 1, arm = 2, rus = 3, eng = 4) |> 
  pivot_longer(matches("\\d{4}"), names_to = "date") |> 
  filter(!is.na(code)) |>
  mutate(
    date = yq(date) + months(3) - days(1),
    year = year(date),
    eng = str_trunc(eng, 20),
    eng = paste0(code, ". ", eng),
    eng = fct_lump(eng, n = 7, w = value)
  ) |> 
  group_by(date, year, eng) |> 
  summarise(value = sum(value), .groups = "drop") |> 
  group_by(eng) |> 
  mutate(value_yoy = roll_sumr(value, 4)) |> 
  ungroup() |> 
  group_by(date) |>
  na.omit() |>
  mutate(
    pct = value_yoy/ sum(value_yoy),
  ) |> 
  ungroup() |>
  group_by(year) |> 
  mutate(
    pct_text = ifelse(
      eng != "Other" & date == max(date) & year %in% c(2013, 2023), 
      paste0(
        "?? ", number(value_yoy/1e6, accuracy = 0.01), "T (",
        percent(pct, accuracy = 0.1), ")", "???"
      ),
      NA
    ),
    pct_text = ifelse(
      year == 2023 & eng != "Other",
      paste0("???", eng,  " ", pct_text),
      pct_text
    ),
    value_yoy = value_yoy / 1e6
  ) |> 
  ungroup() |> 
  mutate(
    eng = fct_reorder(eng, value_yoy),
    eng = fct_relevel(eng, "Other", after = 0),
    eng = fct_rev(eng)
  )

main_data |> count(eng)


segments <- 
  main_data |> 
  # filter(year %in% c(2013, 2017, 2020, 2023)) |> 
  group_by(year) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  group_by(date) |> 
  summarise(
    value_yoy = sum(value_yoy),
    value_yoy_text = number(value_yoy, accuracy = 0.01),
    value_yoy = value_yoy + 1
  ) |>  
  ungroup()





main_data |> 
  ggplot(aes(date, value_yoy)) +
  geom_segment(
    data = segments,
    aes(x = date, y = value_yoy - 1, xend = date, yend = value_yoy),color="black"
  ) +
  geom_point(
    data = segments,
    aes(x = date, y = value_yoy),color="black"
  ) +
  geom_text(
    data = segments,
    aes(x = date, y = value_yoy + 0.5, label = value_yoy_text)
  ) +
  geom_area(aes(fill = eng)) +
  # geom_stream(type = "ridge", bw=1) +
  geom_text(
    data = main_data |> filter(date == min(date)),
    aes(label = pct_text, color = eng),
    position = position_stack(vjust = 0.5), hjust = 1
  ) +
  geom_text(
    data = main_data |> filter(date == max(date)),
    aes(label = pct_text, color = eng),
    position = position_stack(vjust = 0.5), hjust = -0
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 14, 2), labels = number_format()) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  # guides(fill = guide_legend(nrow = 5)) +
  ggthemes::theme_fivethirtyeight() +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "The Change from Agriculture to Manufacturing",
    subtitle = "Output of Goods and Service in Armenia",
    captions = paste0(caption_eng, "    |    Data Source: armstat.am")
  ) +
  theme(
    axis.line.x = element_line(linewidth = .75),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18),
    panel.grid = element_blank(),
    axis.text.y=element_blank(),
    legend.position = "none",
    # axis.text.x = element_text(color=txt_col, size=10,margin = margin(5,0,0,0)),
    plot.margin = margin(10,210,10,100),
    # plot.caption = element_markdown(hjust=0, margin=margin(10,0,0,0), size=8, color=txt_col, lineheight = 1.2),
  )


data |> 
  rename(code = 1, arm = 2, rus = 3, eng = 4) |> 
  pivot_longer(matches("\\d{4}"), names_to = "date") |> 
  filter(!is.na(code)) |>
  mutate(
    date = yq(date) + months(3) - days(1),
    year = year(date),
    eng = str_trunc(eng, 40),
    eng = paste0(code, ". ", eng),
  ) |> 
  group_by(code) |> 
  mutate(value_yoy = roll_sumr(value, 4) / 1e3) |> 
  na.omit() |> 
  filter(date %in% c(max(date), min(date))) |>
  # filter(date %in% c(max(date), as.Date("2018-03-31"))) |> 
  mutate(year = ifelse(year == min(year), "min_year", "max_year")) |> 
  select(code, eng, year, value_yoy) |> 
  pivot_wider(names_from = year, values_from = value_yoy) |> 
  mutate(
    pct_10y_change = number(max_year / min_year, accuracy = 0.01),
    middle_year = exp((log(min_year) + log(max_year))/2)  # geomean
  ) |>
  filter(code != "T") |> 
  ggplot() +
  geom_segment(
    aes(x = min_year, xend = max_year, y = fct_reorder(eng, max_year), yend = eng),
    color=pal[1]
  ) +
  geom_point( aes(x=min_year, y=eng), color=pal[3], size=3 ) +
  geom_point( aes(x=max_year, y=eng), color=pal[4], size=3 ) +
  geom_text(aes(x = middle_year, y = eng, label = pct_10y_change), vjust = 0) +
  scale_x_log10(labels = number_format()) +
  ggthemes::theme_fivethirtyeight() +
  labs(
    x = NULL,
    y = NULL,
    title = "From 2013 to 2023, what area had the biggest growth in GDP?",
    subtitle = "The numbers show the growth rate",
    captions = paste0(caption_eng, "    |    Data Source: armstat.am")
  )



select_pal = 
  
  
  library(treemapify)

data |> 
  rename(code = 1, arm = 2, rus = 3, eng = 4) |> 
  pivot_longer(matches("\\d{4}"), names_to = "date") |> 
  filter(!is.na(code)) |>
  mutate(
    date = yq(date) + months(3) - days(1),
    year = year(date),
    eng = str_trunc(eng, 40),
    eng = paste0(code, ". ", eng),
  ) |> 
  group_by(code) |> 
  mutate(value_yoy = roll_sumr(value, 4) / 1e3) |> 
  na.omit() |> 
  filter(date %in% c(max(date), min(date))) |> 
  select(code, eng, year, value_yoy) |> 
  pivot_wider(names_from = year, values_from = value_yoy) |> 
  bind_cols(
    tibble(
      colors_selct = sample(pal, 20, replace = TRUE)
    )
  ) |> 
  pivot_longer(matches("\\d{4}"), names_to = "year", values_to = "value_yoy") |> 
  ggplot(aes(area = value_yoy, fill = I(colors_selct), label = eng)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  facet_wrap(~year)



