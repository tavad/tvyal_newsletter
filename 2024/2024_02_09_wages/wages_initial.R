library(tidyverse)
library(scales)
library(readxl)
library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

wages_raw <- read_excel("wages_in_armenia_2022.xlsx", skip = 2) |> 
  rename(indicator = 1)

# system(
#   paste0(
#     "curl \"", "https://www.cba.am/stat/stat_data_eng/6_CPI_eng.xls", "\"",
#     " -o ", "\"cpi_armenia_eng.xls\""
#   )
# )

inflation_row <- read_excel("cpi_armenia_eng.xls", skip = 3)


#####################

inf_cumprod <- 
  inflation_row |> 
  rename(date = 1, cpi_m = 2) |> 
  select(date, cpi_m) |> 
  mutate(
    date = ym(date),
    cpi_m = cpi_m / 100,
    year = year(date)
  ) |> 
  filter(year <= 2022) |>
  mutate(
    cpi_yoy = roll_prodr(cpi_m, 12)
  ) |>
  arrange(desc(date)) |> 
  mutate(
    adj_price = cumprod(cpi_m),
    adj_price = lag(adj_price),
    adj_price = ifelse(is.na(adj_price), 1, adj_price)
  ) |> 
  group_by(year) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  select(year, cpi_yoy, adj_price)


wages_clean <- 
  wages_raw |>
  filter(!is.na(`2020`))  |>  
  janitor::clean_names() |> 
  mutate(
    across(matches("\\d{4}"), ~ifelse(.x == "-", NA, .x)),
    across(matches("\\d{4}"), ~ as.numeric(.x))
  ) |> 
  mutate(
    indicator = str_replace(indicator, "աշխատավարձ, դրամ", "աշխատավարձ ՀՀ ընդհանուր, դրամ"),
    indicator = str_remove(indicator, "Միջին ամսական անվանական աշխատավարձը? ? ?-? ?\\(?"),
    indicator = str_remove(indicator, ", դրամ$")
  ) |>
  extract(indicator, into = c("ind0", "ind3"),
          regex = "(ըստ .*) *- *(.*)", remove = FALSE) |> 
  # extract(ind0, into = c("ind1", "ind2"),
  #         regex = "(ըստ .*) *- *(.*)", remove = FALSE) |> 
  # mutate(
  #   ind1 = ifelse(is.na(ind1), ind0, ind1),
  #   
  # ) |> view()
  mutate(
    ind0 = str_replace_all(ind0, " *- *", " - "),
    correction_2017 = x2017_2 / x2017,
    correction_2012 = x2012_2 / x2012,
  ) |> 
  select(-c(x2017_2, x2012_2)) |>
  pivot_longer( 
    cols = -c(indicator, ind0, ind3, correction_2017, correction_2012),
    names_to = "year", values_to = "wages"
  ) |> 
  mutate(
    across(contains("ind"), ~str_trim(.x)),
    year = parse_number(year),
    wages_corrected = case_when(
      year <= 2012 & !is.na(correction_2017) ~ correction_2012 * correction_2017 * wages,
      year <= 2017 & year > 2012 & !is.na(correction_2017) ~ correction_2017 * wages,
      TRUE ~ wages
    )
  ) |> 
  left_join(inf_cumprod, by = "year") |> 
  mutate(wages_real = wages_corrected * adj_price) |> 
  select(-contains("correction"))


##############
# plots

wages_clean |> 
  filter(grepl("ըստ ՀՀ մարզերի և ք. Երևանի", indicator)) |>
  mutate(
    ind0 = case_when(
      grepl("ոչ պետական", ind0) ~ "ոչ պետական",
      grepl("պետական", ind0) ~ "պետական",
      TRUE ~ "ընդհանուր"
    )
  ) |> 
  # filter(ind1 == "ըստ ՀՀ մարզերի և ք. Երևանի ") |>
  ggplot(aes(year, wages_real / 1000, fill = year)) +
  geom_col() +
  facet_grid(ind0 ~ ind3) +
  scale_y_continuous(breaks = seq(0, 1000, 100), labels = comma_format()) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_fill_gradient(breaks = 2008:2020,  high = "#132B43", low = "#56B1F7") +
  labs(
    x = NULL,
    y = NULL,
    title = "Ամսական իրական միջին աշխատավարձը ըստ մարզերի Հայաստանում",
    subtitle = "հազար դրամ, գնաճով ճշգրտված"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, 'cm'),
    legend.box = 
  )
# scale_fill_brewer(type = "seq", palette = 1)
# facet_wrap(~ c(ind1))



################################################



years_select = c(2021, 2022) # provide 2 consecutive years that are in household_income_db

date_legend <- 
  tibble(
    x = 215, y = c(1.5, 2.5), 
    color = c("#2f4b7c", "#f95d6a"), 
    text = rev(years_select),
    ind0 = "պետական"
  )

# household_change_in_2_years_plot
wages_clean |> 
  filter(grepl("ըստ ՀՀ մարզերի և ք. Երևանի", indicator)) |>
  mutate(
    ind0 = case_when(
      grepl("ոչ պետական", ind0) ~ "ոչ պետական",
      grepl("պետական", ind0) ~ "պետական",
      TRUE ~ "ընդհանուր"
    )
  ) |> 
  filter(year %in% years_select) |> 
  select(ind0, ind3, year, wages_real) |> 
  mutate(
    wages_real = wages_real / 1000,
    year = ifelse(year == min(year), "begining", "end")
  ) |> 
  rename(marz_arm = ind3) |> 
  pivot_wider(names_from = year, values_from = wages_real) |> 
  mutate(
    marz_arm = fct_reorder(marz_arm, (begining + end)/2, .fun = mean),
    # marz_eng = fct_reorder(marz_eng, (begining + end)/2, .fun = mean),
    color = ifelse(begining > end, "#f95d6a", "#2f4b7c")
  ) |> 
  ggplot() +
  facet_grid(~ind0, scales = "free_x") +
  geom_segment(
    aes(x = begining, xend= end,  y= marz_arm, 
        yend=marz_arm, group = ind0, color = I(color)),
    linewidth = 1.2,
    lineend = 'round', linejoin = 'round',
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x=begining, y=marz_arm, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x=end, y=marz_arm, color=I("#2f4b7c")), size = 3) +
  geom_point(data = date_legend, aes(x, y, color = I(color)), size = 3) +
  geom_text(data = date_legend, aes(x + 15, y, label = text)) +
  scale_y_discrete(expand = c(0.05,0,0.2,0)) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Միջին ամսական անվանական աշխատավարձը ըստ մարզերի և տնտեսության հատվածների",
    subtitle = "Ներկայացված են իրական աշխատավարձերը, 2022 թվականի 8.3 տոկոս գնաճով ճշգրտված\nհազար ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )

# քանի որ գնաճը կումուլատիվ 2021-22 թվականներին կազմել է մոտ 17 տոկոս, մինիմալ աշխատավարձը պիտի բարձրացվի

wages_clean |> 
  filter(grepl("ըստ տնտեսական գործունեության տեսակների", indicator)) |> 
  mutate(
    ind3 = str_trunc(ind3, 30)
  ) |> 
  filter(year %in% years_select) |> 
  select(ind0, ind3, year, wages_real) |> 
  mutate(
    wages_real = wages_real / 1000,
    year = ifelse(year == min(year), "begining", "end")
  ) |> 
  pivot_wider(names_from = year, values_from = wages_real) |> 
  mutate(
    ind3 = fct_reorder(ind3, (begining + end)/2, .fun = mean),
    # marz_eng = fct_reorder(marz_eng, (begining + end)/2, .fun = mean),
    color = ifelse(begining > end, "#f95d6a", "#2f4b7c")
  ) |> 
  ggplot() +
  geom_segment(
    aes(x = begining, xend= end,  y= ind3, 
        yend=ind3, group = ind0, color = I(color)),
    linewidth = 1.2,
    lineend = 'round', linejoin = 'round',
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x=begining, y=ind3, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x=end, y=ind3, color=I("#2f4b7c")), size = 3) +
  geom_point(data = date_legend, aes(x + 400, y, color = I(color)), size = 3) +
  geom_text(data = date_legend, aes(x + 480, y, label = text)) +
  scale_x_log10(breaks = seq(0, 800, 100), limits =c(100, 800)) +
  scale_y_discrete(expand = c(0.05,0,0.2,0)) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Միջին ամսական աշխատավարձը ըստ տնտեսական գործունեության",
    subtitle = "Ներկայացված են իրական աշխատավարձերը, 2022 թվականի 8.3 տոկոս գնաճով ճշգրտված\nանվանական, հազար ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )



wages_clean |> 
  filter(grepl("ըստ կազմակերպության չափի", indicator)) |>
  mutate(
    ind0 = case_when(
      grepl("ոչ պետական", ind0) ~ "ոչ պետական",
      grepl("պետական", ind0) ~ "պետական",
      TRUE ~ "ընդհանուր"
    ),
    ind0 = fct_reorder(ind0, wages_real, .desc = TRUE),
    ind3 = str_remove(ind3, "ում$")
  ) |> 
  ggplot(aes(year, wages_real / 1000, color = ind0)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(~ind3, scale = "free_y") +
  scale_x_continuous(breaks = seq(2008, 2030, 2)) +
  scale_color_manual(values = new_palette_colors[c(2,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Միջին ամսական անվանական աշխատավարձը ըստ կազմակերպության չափի",
    subtitle = "Ներկայացված եsն իրական աշխատավարձերը, 2022 թվականի 8.3 տոկոս գնաճով ճշգրտված\nհազար ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )


wages_clean |> 
  filter(
    grepl("ՀՀ ընդհանուր|ըստ սեռի", indicator),
    !grepl( "նվազագույն", indicator)
  ) |> 
  mutate(
    indicator = ifelse(is.na(ind0), indicator, ind3),
    indicator = fct_reorder(indicator, wages_real, .desc = TRUE),
  ) |> 
  ggplot(aes(year, wages_real / 1000, color = indicator)) +
  geom_line(linewidth = 1.5) +
  scale_x_continuous(breaks = seq(2008, 2030, 2)) +
  scale_color_manual(values = new_palette_colors[c(2,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Միջին ամսական անվանական աշխատավարձը ըստ սեռի",
    subtitle = "Ներկայացված են իրական աշխատավարձերը, 2022 թվականի 8.3 տոկոս գնաճով ճշգրտված\nհազար ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )

