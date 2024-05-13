library(geomtextpath)

arm_trade_commodity |> 
  filter(year >= 2018) |> 
  write_csv("C:/Users/Administrator/Desktop/newsletter/2024/2024_05_11_reexports_gold/arm_trade_commodity.csv")



chart_data <- 
  arm_trade_commodity |> 
  filter(year == max(year)) |>
  mutate(
    hs2 = as.character(hs2),
    hs2 = fct_lump_prop(hs2, prop = 0.015, w = export)
  ) |> 
  group_by(hs2) |>
  mutate(
    # commodity = str_trunc(commodity, 30),
    commodity = paste0(hs4, ". ", commodity),
    commodity = fct_lump_prop(commodity, prop = 0.05, w = export)
  ) |> 
  ungroup() |> 
  group_by(hs2, commodity) |> 
  summarise(export = sum(export)) |> 
  ungroup() |> 
  mutate(
    commodity = as.character(commodity),
    hs2 = as.character(hs2),
    commodity = ifelse(
      commodity == "Other", 
      paste0(hs2, ". ", commodity), 
      commodity
    ),
    commodity = fct_inorder(commodity),
    hs2 = fct_inorder(hs2)
  )

chart_data_2 <- 
  chart_data |> 
  group_by(hs2) |> 
  summarise(export = sum(export)) |> 
  ungroup() |> 
  mutate(
    hs2 = fct_inorder(hs2),
    pct_hs2 = export / sum(export)
  )

library(ggrepel)
library(scales)


ggplot() +
  geom_col(
    data = chart_data_2,
    aes(x = "1", y = export, color = hs2)
  ) +
  geom_text(
    data = chart_data_2,
    aes(x = "1", y = export,  group = hs2, label = hs2),
    position = position_stack(vjust = .5)
  ) +
  geom_col(
    data = chart_data, 
    aes(x = "2", y = export, fill = commodity)
  ) +
  geom_text(
    data = chart_data, 
    aes(x = "2", y = export, group = commodity, label = commodity),
    position = position_stack(vjust = .5)
  ) +
  theme(
    legend.position = "none"
  ) +
  coord_polar("y")


# ungroup() |> 
#   mutate(pct = export / sum(export))

hs2_arranged <- 
  chart_data_2 |> 
  mutate(
    hs2 = fct_reorder(hs2, export, .desc = TRUE),
    hs2 = fct_relevel(hs2, "Other", after = Inf)
  ) |> 
  arrange(hs2) |> 
  pull(hs2)


chart_data |> 
  left_join(chart_data_2 |> select(-export), by = "hs2") |> 
  relocate(hs2, pct_hs2) |> 
  mutate(
    hs2 = factor(hs2, levels = hs2_arranged)
  ) |> 
  arrange(hs2, desc(export)) |> 
  group_by(hs2) |> 
  mutate(
    hs2 = as.character(hs2),
    hs2 = paste0(hs2, ". ", percent(pct_hs2, accuracy = 0.01))
    # hs2 = ifelse(row_number(hs2) == 1, hs2, ""),
    # pct_hs2 = ifelse(row_number(hs2) == 1, "", NA)
  ) |> 
  select(-pct_hs2) |> 
  ungroup() |> 
  mutate(
    pct = export / sum(export)
  ) |> 
  group_by(hs2) |> 
  gt() |> 
  fmt_number(
    columns = c("export"),
    decimals = 0, 
    use_seps = TRUE,
  ) |> 
  fmt_percent(
    columns = c("pct"),
  ) 


#####################################


names_dic <- 
  read_csv("C:/Users/Administrator/Desktop/2021 CBA grant/data_forecast/names_en_am_ru_short.csv") |> 
  rename(hs2 = hc2) |> 
  mutate(hs2 = as.character(hs2))

names_dic |> 
  write_csv("C:/Users/Administrator/Desktop/newsletter/2024/2024_05_11_reexports_gold/names_en_am_ru_short.csv")



arm_trade_commodity |> 
  filter(year >= 2021) |> 
  group_by(year) |> 
  mutate(
    hs2 = as.character(hs2),
    hs2 = fct_lump_prop(hs2, prop = 0.03, w = export)
  ) |> 
  ungroup() |> 
  mutate(
    year = ifelse(year == max(year), paste(year, "առաջին 3 ամիսը"), year)
  ) |> 
  group_by(year, hs2) |> 
  summarise(export = sum(export)) |> 
  ungroup() |> 
  group_by(year) |> 
  mutate(
    pct = export / sum(export),
    pct_text = percent(pct, accuracy = 0.1)
  ) |> 
  ungroup() |> 
  mutate(hs2 = as.character(hs2)) |> 
  arrange(hs2) |> 
  left_join(names_dic) |> 
  mutate(
    name_short_arm = str_trunc(name_short_arm, 30),
    name_short_arm = paste0(hs2, ". ", name_short_arm),
    name_short_arm = ifelse(grepl("Other", name_short_arm), "Այլ ապրանքային խմբեր", name_short_arm)
  ) |> 
  arrange(as.numeric(hs2)) |> 
  mutate(name_short_arm = fct_inorder(name_short_arm) |> fct_rev()) |> 
  ggplot(
    aes("1", pct, fill = name_short_arm, label = paste0(hs2, ". ", pct_text))
  ) +
  facet_wrap(~year, nrow = 2) +
  geom_col() +
  geom_text(aes(x = 1.3), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  ggthemes::scale_fill_stata() +
  guides(fill = guide_legend(reverse = TRUE, ncol = 1)) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Արտահանման 75 տոկոսը ոսկի և ադամանդ է",
    subtitle = "Հայաստանի արտահանման տեսակարար կշիռը ըստ ԱՏԳ ԱԱ երկնիշ ապրանքանիշերի",
    caption = paste0(caption_arm, "    |    Տվյալների աղբյուր` armstat.am")
  ) +
  theme(
    axis.line = element_blank(), # Remove axis lines
    axis.text = element_blank(), # Remove axis text
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    panel.border = element_blank(), # Remove panel border
    legend.position = "right"
  )



plot1 <- 
  arm_trade_commodity |> 
  filter(year >= max(year)) |> 
  group_by(year) |> 
  mutate(
    hs2 = as.character(hs2),
    hs2 = fct_lump_prop(hs2, prop = 0.015, w = export)
  ) |> 
  ungroup() |> 
  mutate(
    year = ifelse(year == max(year), paste(year, "առաջին 3 ամիսը"), year)
  ) |> 
  group_by(year, hs2) |> 
  summarise(export = sum(export)) |> 
  ungroup() |> 
  group_by(year) |> 
  mutate(
    pct = export / sum(export),
    pct_text = percent(pct, accuracy = 0.1)
  ) |> 
  ungroup() |> 
  mutate(hs2 = as.character(hs2)) |> 
  arrange(hs2) |> 
  left_join(names_dic) |> 
  mutate(
    name_short_arm = str_trunc(name_short_arm, 30),
    name_short_arm = paste0(hs2, ". ", name_short_arm),
    name_short_arm = ifelse(grepl("Other", name_short_arm), "Այլ ապրանքային խմբեր", name_short_arm)
  ) |> 
  arrange(as.numeric(hs2)) |> 
  mutate(name_short_arm = fct_inorder(name_short_arm) |> fct_rev()) |> 
  ggplot(
    aes("1", pct, fill = name_short_arm, label = paste0(hs2, ". ", pct_text))
  ) +
  geom_col() +
  geom_text(aes(x = 1.3), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = -1.2) +
  ggthemes::scale_fill_stata() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.line = element_blank(), # Remove axis lines
    axis.text = element_blank(), # Remove axis text
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    panel.border = element_blank(), # Remove panel border
  )

plot2 <- 
  arm_trade_commodity |> 
  filter(
    year >= max(year),
    hs2 == "71"
  ) |> 
  group_by(year) |> 
  mutate(
    commodity = fct_lump_prop(commodity, prop = 0.01, w = export)
  ) |> 
  ungroup() |> 
  group_by(year, commodity) |> 
  summarise(export = sum(export)) |> 
  ungroup() |>
  group_by(year) |> 
  mutate(
    pct = export / sum(export),
    pct_text = percent(pct, accuracy = 0.1),
    commodity = fct_reorder(commodity, export, .desc = TRUE)
  ) |> 
  ungroup() |> 
  ggplot(
    aes("1", pct, fill = commodity, label = paste0(commodity, ". ", pct_text))
  ) +
  geom_col() +
  geom_text(aes(x = 1), position = position_stack(vjust = 0.5)) +
  ggthemes::scale_fill_stata() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.line = element_blank(), # Remove axis lines
    axis.text = element_blank(), # Remove axis text
    axis.title = element_blank(), # Remove axis titles
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    panel.border = element_blank(), # Remove panel border
    legend.position = "none"
  )


library(gridExtra)

grid.arrange(plot1, plot2, ncol=2) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Արտահանման 75 տոկոսը ոսկի և ադամանդ է",
    subtitle = "Հայաստանի արտահանման տեսակարար կշիռը ըստ ԱՏԳ ԱԱ երկնիշ ապրանքանիշերի",
    caption = paste0(caption_arm, "    |    Տվյալների աղբյուր` armstat.am")
  )

month(max_date)


max_date <- 
  arm_trade_commodity %>% 
  filter(date == max(date)) %>%  
  pull(date) %>% 
  unique()

date_legend <- 
  tibble(
    x = 1300, y = c(1.5, 2.5), 
    color = c("#2f4b7c", "#f95d6a"), 
    text = c("2024 առաջի եռամսյակ", "2023 առաջի եռամսյակ"),
  )

arm_trade_commodity |> 
  group_by(year) |> 
  summarise(
    export = sum(export), import = sum(import)
  ) |> 
  mutate(change = import/export)


arm_trade_commodity |>
  # group_by(year, date, month, hs2) |>
  # summarise(export = sum(export), .groups = "drop") |>
  # complete(nesting(year, date, month), hs2, fill = list(export = 0)) |>
  # arrange(hs2, date) |>
  # group_by(hs2) |>
  # mutate(
  #   export_yoy = roll_sumr(export, 12)
  # ) |>
  # ungroup() |>
  # filter(date %in% c(max_date, max_date - years(1))) |>
  # # filter(
  # #   year %in% {year(max_date) - 1:2},
  # #   month == 12
  # # ) |>
  # select(-c(date, month, export)) |> 
  
  filter(
    year %in% {year(max_date) - 0:1},
    month <= month(max_date)
  ) |> 
  
  group_by(year, hs2) |> 
  summarise(export = sum(export), import = sum(import)) |> 
  group_by(year) |> 
  mutate(
    export_pct = export / sum(export),
    import_pct = import / sum(import)
  ) |> 
  View()

arm_trade_commodity |> 
  filter(hs2 == 71, year == 2024) |> 
  group_by(commodity, hs4) |> 
  summarise(
    across(contains("port"), ~sum(.x))
  ) |> 
  view()



  group_by(hs2, year) |>
  summarise(export_yoy = sum(export), .groups = "drop") |>
  
  mutate(
    year = ifelse(year == max(year), "end", "start")
  ) |> 
  pivot_wider(names_from = year, values_from = export_yoy) |> 
  mutate(
    hs2 = as.character(hs2),
    hs2 = fct_lump_n(hs2, 17, w = end)
  ) |>
  group_by(hs2) |> 
  summarise(
    start = sum(start),
    end = sum(end),
    .groups = "drop"
  ) |> 
  left_join(names_dic, by = join_by(hs2)) |> 
  mutate(
    across(c(end, start), ~.x/1000),
    change = end / start,
    change_text = case_when(
      change > 2 ~ number(change, accuracy = 0.01, suffix = " անգամ"),
      change < 0.5 ~ number(-1/change, accuracy = 0.01, suffix = " անգամ"),  
      TRUE ~ percent(change - 1, accuracy = 0.1)
    ),
    color = ifelse(start > end, "#f95d6a", "#2f4b7c"),
    hs2 = ifelse(
      hs2 == "Other", 
      "Այլ ապրանքներ",
      paste0(hs2, ". ", str_trunc(name_short_arm, 30))
    ),
    hs2 = fct_reorder(hs2, sqrt(end * start)),
    hs2 = fct_relevel(hs2, "Այլ ապրանքներ", after = 0),
  ) |> 
  ggplot(aes(y = hs2)) +
  geom_segment(
    aes(x = start, xend = end,
        yend = hs2, group = hs2, color = I(color)),
    linewidth = 1.2,
    lineend = "round", linejoin = "round",
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x=start, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x=end, color=I("#2f4b7c")), size = 3) +
  geom_text(aes(x = sqrt(end * start), label = change_text), vjust = -0.5) +
  geom_point(data = date_legend, aes(x, y, color = I(color)), size = 3) +
  geom_text(data = date_legend, aes(x * 2, y, label = text)) +
  scale_x_log10(
    breaks = 10^(1:6), minor_breaks = as.vector(c(1:9) %*% t(10^(1:6))),
    labels = number_format(), 
    # limits = c(40, 10^4)
  ) +
  scale_y_discrete() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    # title = "Միջին ամսական անվանական աշխատավարձը ըստ մարզերի և տնտեսության հատվածների",
    # subtitle = "Ներկայացված են իրական աշխատավարձերը, 2022 թվականի 8.3 տոկոս գնաճով ճշգրտված\nհազար ՀՀ դրամ",
    # caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
    title = "Average monthly salary by regions and sectors of the economy",
    subtitle = "Real nominal salaries, thousand AMD, 2021 (red) is adjusted by 8.3% inflation in 2022 (blue)",
    caption = paste0(caption_arm, "    |    Տվյալների աղբյուր` armstat.am")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_line(
        colour = "gray", 
        linetype = "dotted"
      ),
    panel.grid.major.x =  element_line(
      colour = "gray30", 
      linetype = 2
    ),
  )

  
  
commodity_groups_dic <- read_csv("C:/Users/Administrator/Desktop/newsletter/2024/2024_05_11_reexports_gold/commodity_groups_dic.csv")



arm_trade_commodity |>
  filter(
    year %in% {year(max_date) - 0:1},
    month <= month(max_date)
  ) |>
  left_join(commodity_groups_dic) |> 
  group_by(commodity_code_names_arm, year) |>
  summarise(export_yoy = sum(export), .groups = "drop") |> 
  rename(hs2 = commodity_code_names_arm) |> 
  mutate(
    year = ifelse(year == max(year), "end", "start")
  ) |> 
  pivot_wider(names_from = year, values_from = export_yoy) |> 
  mutate(
    across(c(end, start), ~.x/1000),
    change = end / start,
    change_text = case_when(
      change > 2 ~ number(change, accuracy = 0.01, suffix = " անգամ"),
      change < 0.5 ~ number(-1/change, accuracy = 0.01, suffix = " անգամ"),  
      TRUE ~ percent(change - 1, accuracy = 0.1)
    ),
    color = ifelse(start > end, "#f95d6a", "#2f4b7c"),
    hs2 = fct_reorder(hs2, sqrt(end * start)),
  ) |> 
  ggplot(aes(y = hs2)) +
  geom_segment(
    aes(x = start, xend = end,
        yend = hs2, group = hs2, color = I(color)),
    linewidth = 1.2,
    lineend = "round", linejoin = "round",
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x=start, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x=end, color=I("#2f4b7c")), size = 3) +
  geom_text(aes(x = sqrt(end * start), label = change_text), vjust = -0.5) +
  geom_point(data = date_legend, aes(x, y, color = I(color)), size = 3) +
  geom_text(data = date_legend, aes(x * 1.2, y, label = text), hjust = 0) +
  scale_x_log10(
    breaks = 10^(1:6), minor_breaks = as.vector(c(1:9) %*% t(10^(1:6))),
    labels = number_format(), 
    # limits = c(40, 10^4)
  ) +
  scale_y_discrete() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    # title = "Միջին ամսական անվանական աշխատավարձը ըստ մարզերի և տնտեսության հատվածների",
    # subtitle = "Ներկայացված են իրական աշխատավարձերը, 2022 թվականի 8.3 տոկոս գնաճով ճշգրտված\nհազար ՀՀ դրամ",
    # caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
    title = "Average monthly salary by regions and sectors of the economy",
    subtitle = "Real nominal salaries, thousand AMD, 2021 (red) is adjusted by 8.3% inflation in 2022 (blue)",
    caption = paste0(caption_arm, "    |    Տվյալների աղբյուր` armstat.am")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_line(
      colour = "gray", 
      linetype = "dotted"
    ),
    panel.grid.major.x =  element_line(
      colour = "gray30", 
      linetype = 2
    ),
  )

