library(tidyverse)
library(scales)
library(readxl)
library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

new_palette_colors <- c(
  "#003f5c", "#2f4b7c", "#665191", "#a05195",
  "#d45087", "#f95d6a", "#ff7c43", "#ffa600"
)

colfunc <- colorRampPalette(c("#2f4b7c", "#fffcf5", "#f95d6a"))
colfunc2 <- colorRampPalette(new_palette_colors)
colfunc3 <- colorRampPalette(c(new_palette_colors, "#FFD17A", "#FFFCF5"))

##########################

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


#######################################################


public_wages <- read_excel("wages_more_info.xlsx", sheet = 3)

public_wages |> 
  # rename(public = 3, non_public = 4) |> 
  select(year, total, public, non_public) |>
  pivot_longer(-year) |> 
  pivot_wider(names_from = year, names_prefix = "x") |> 
  mutate(
    correction_2017 = x2017_2 / x2017,
    correction_2012 = x2012_2 / x2012,
  ) |> 
  select(-c(x2017_2, x2012_2)) |>
  pivot_longer( 
    cols = -c(name, correction_2012, correction_2017),
    names_to = "year", values_to = "wages"
  ) |> 
  mutate(
    year = parse_number(year),
    wages_corrected = case_when(
      year <= 2012 & !is.na(correction_2017) ~ correction_2012 * correction_2017 * wages,
      year <= 2017 & year > 2012 & !is.na(correction_2017) ~ correction_2017 * wages,
      TRUE ~ wages
    )
  ) |> 
  select(name, year, wages_corrected) |> 
  pivot_wider(names_from = name, values_from = wages_corrected) |> 
  mutate(
    pct_public = (total - non_public) / (public - non_public),
    pct_non_public = (total - public) / (non_public - public)
  ) |> 
  ggplot(aes(year, pct_public)) +
  geom_col() +
  scale_x_continuous(breaks = seq(2000, 2030, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = percent_format()) +
  labs(
    x = NULL,
    y = NULL,
    title = "Պետական ոլորտի աշխատողների տեսակարար կշիռը",
    # subtitle = ""
    caption = caption_arm
  )


###################################

workers_n <- read_excel("wages_more_info.xlsx", sheet = "workers_n")

workers_n |> 
  mutate(
    # is_yerevan = ifelse(marz_eng == "Yerevan city", "Yerevan city", "Marzez")
  ) |>
  group_by(marz_eng, type, year) |> 
  summarise(workers_n = sum(workers_n)) |> 
  group_by(type, year) |> 
  # mutate(
  #   workers_pct = workers_n / sum(workers_n),
  # ) |>
  filter(year %in% c(2019, 2022), type == "public") |> 
  pivot_wider(names_from = year, values_from = workers_n, names_prefix = "x") |> 
  mutate(gwt = x2022/x2019) |> 
  view()
  
  

workers_n |> 
  filter(year == max(year)) |>
  # filter(year %in% c(2019, 2022)) |>
  group_by(type, year) |> 
  mutate(
    workers_pct = workers_n / sum(workers_n),
    workers_pct_txt = percent(workers_pct, accuracy = 0.1),
    workers_pct_txt = ifelse(workers_pct >= 0.025, workers_pct_txt, NA),
    marz_arm = fct_reorder(marz_arm, workers_pct, .desc = TRUE),
    type = case_match(
      type,
      "non_public" ~ "Ոչ պետական",
      "public" ~ "Պետական",
      "total" ~ "Ընդամենը"
    )
  ) |> 
  ggplot(aes(y = "1", x = workers_pct, fill = marz_arm, label = workers_pct_txt)) +
  geom_col() +
  geom_text(aes(y = 1.25), position = position_stack(vjust = .5)) +
  facet_grid(year~type, switch = "y") +
  coord_polar() +
  # ggthemes::scale_fill_stata() +
  scale_fill_manual(values = colfunc3(11)) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Աշխատողները ըստ մարզերի",
    subtitle = "Ոչ պետական և պետական աշխատատեղեր․․․",
    caption = paste0(caption_arm, "   |   source:armstat.am")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank()
  )

workers_n |> 
  select(-mean_wage) |> 
  pivot_wider(names_from = type, values_from = workers_n) |> 
  mutate(
    non_public_to_public = non_public / public,
    non_public_to_public_txt = number(non_public_to_public, accuracy = 0.01),
    text_possition = ifelse(
      non_public_to_public > 1,
      non_public_to_public * 1.1,
      non_public_to_public / 1.1
    )
  ) |> 
  arrange(non_public_to_public) |> 
  filter(year == 2022) |> 
  mutate(
    marz_arm = fct_inorder(marz_arm),
    marz_eng = fct_inorder(marz_eng), 
  ) |> 
  ggplot(aes(non_public_to_public, marz_arm,
             fill = marz_arm, label = non_public_to_public_txt)) +
  geom_col() +
  geom_text(aes(x = text_possition)) +
  scale_x_log10() +
  scale_fill_manual(values = colfunc2(11)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Ոչ պետական և պետական աշխատատեղերի հարաբերությունը",
    subtitle = "2022թ․, ըստ մարզերի, լոգարիթմիկ առանցք",
    caption = paste0(caption_arm, "   |   source:armstat.am")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  )


###############################################



library(Hmisc)

wages_by_sector <- read_excel("wages_more_info.xlsx", sheet = "wages_secors")

wages_by_sector <- 
  wages_by_sector |> 
  pivot_longer(matches("\\d{4}"), names_to = "year") |>
  pivot_wider(names_from = version) |> 
  arrange(year, type, sector_code) |> 
  relocate(year, type, sector_code, wage)

wages_by_sector |> 
  filter(sector_code != "total") |> 
  group_by(year, type) |> 
  summarise(
    mean_wage = sum(wage * person) / sum(person),
    median = wtd.quantile(wage, person, probs = 0.5),
    q1 = wtd.quantile(wage, person, probs = 0.25),
    q3 = wtd.quantile(wage, person, probs = 0.75)
  ) |> 
  filter(type == "non_public")



