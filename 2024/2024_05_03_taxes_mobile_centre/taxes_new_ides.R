
taxes_data <- read_csv("hhpektt_2021_2024_I.csv")


taxes_data |> 
  filter(year %in% c(2023:2024), Q == 1) |> 
  mutate(
    name = str_to_title(name),
    total_taxes = total_taxes / 1e3
  ) |> 
  select(-Q, -date, -total_taxes_Q, -n_Q, -location) |> 
  pivot_wider(
    names_from = "year", values_from = c(total_taxes, n, name),
    # names_vary = "slowest"
  ) |> 
  select(-name_2023) |> 
  mutate(
    change = total_taxes_2024 / total_taxes_2023
  ) |> 
  arrange(desc(change)) |> 
  filter(!is.na(change)) |> 
  # slice_head(n = 20) |> 
  slice_tail(n = 20) |> 
  mutate(
    color = ifelse(total_taxes_2024 < total_taxes_2023, "#f95d6a", "#2f4b7c"),
    name_2024 = fct_reorder(name_2024, n_2024, .desc = TRUE)
  ) |> 
  ggplot(aes(y= name_2024)) +
  geom_segment(
    aes(x = total_taxes_2023, xend= total_taxes_2024,
        yend=name_2024, group = name_2024, color = I(color)),
    linewidth = 1.2,
    lineend = 'round', linejoin = 'round',
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x = total_taxes_2023, color=I("#f95d6a")), size = 3) +
  geom_point(aes(x = total_taxes_2024, color=I("#2f4b7c")), size = 3) +
  geom_text(aes(x = total_taxes_2023, label = n_2023, color=I("#f95d6a")), hjust = 1.3) +
  geom_text(aes(x = total_taxes_2024, label = n_2024, color=I("#2f4b7c")), hjust = -0.3) +
  geom_text(aes(
    x = sqrt(total_taxes_2024 * total_taxes_2023),
    label = number(change, accuracy = 0.01)
  ), vjust = -0.8) +
  scale_x_log10(label = number_format(), n.breaks = 6) +
  theme(
    panel.grid.major.y = element_blank()
  )
  
  # geom_point(data = date_legend, aes(x, y, color = I(color)), size = 3) +
  # geom_text(data = date_legend, aes(x + 25, y, label = text)) +
  annotate(
    geom = "text", x = c(95, 280, 595), y = 13, #dividers are changed manually
    label = c(
      "Ամենաաղքատ 10%-ի\nվերին շեմը (1-ին դեցիլ)",
      "Միջին ընտանիքի\nեկամուտ (median)",
      "Ամենահարուստ 10%-ի\nստորին շեմը (9-րդ դեցիլ)"
    )
  ) +
    

  scale_y_discrete(expand = c(0.05,0,0.2,0)) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Տնային տնտեսությունների եկամուտների փոփոխությունը ըստ մարզերի",
    subtitle = "2022-ին Հայաստանի եկամուտները պակասել են, բացի Կոտայքի, Գեղարքունիքի և Տավուշի մարզից,\nհազար ՀՀ դրամ",
    caption = paste0(caption_arm, "    |    տվյալների աղբյուր` armstat.am")
  )
  
  view()
