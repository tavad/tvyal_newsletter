theme_set(ggthemes::theme_few())


transfers_total_chartdata |> 
  ggplot(aes(year, K_USD / 1e3)) +
  geom_col(aes(fill = country), alpha = 1) +
  geom_text(
    aes(label = pct_text, fill = country),
    position = position_stack(vjust = .5),
    color = "white", family = "GHEA Grapalat"
  ) +
  geom_text(
    data = transfers_total_chartdata |> 
      group_by(year) |> 
      summarise(
        K_USD = sum(K_USD) / 1000,
        text_USD = number(K_USD, accuracy = 1),
        .groups = "drop"
      ),
    aes(year, 0, label = text_USD),
    vjust = 2, family = "GHEA Grapalat"
  ) +
  # facet_wrap(~direction) +
  scale_y_continuous(labels = number_format(accuracy = 1), n.breaks = 6) +
  # scale_fill_manual(
  #   values = new_p,
  #   labels = c("Ռուսատանի Դաշնություն", "ԱՄՆ", "Այլ պետություններ")
  # ) +
  scale_fill_manual(
    values = c("midnightblue", "#194470", "#197070"),
    labels = c("Russian Federation", "USA", "Other countries")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "External Influences and Vulnerabilities",
    #     subtitle = "Ֆիզիկական անձանց կողմից դեպի Հայաստան կատարված փոխանցումներ
    # Առաջին կիսամյակ, նորհոսք, միլիոն դոլար",
    subtitle = "Money Transfers made by physical persons to Armenia 2013-2024,\nInflow, Million Dollars",
    captions = "Data Source: https://www.cba.am/stat/stat_data_eng/5_Money_transfers_of_individuals_by_countries-eng.xlsx"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
  )

ggsave(
  "plots/plot_11_money_transfers.png", ggplot2::last_plot(),
  width = 8, height = 6
)


transfers_clean |>
  filter(
    country != "Total",
    direction == "Inflow",
    type == "non-commercial"
  ) |>
  mutate(
    country = case_when(
      grepl("Emirates", country) ~ "UAE",
      grepl("Kingdom", country) ~ "UK",
      TRUE ~ country
    ),
    country = fct_lump_n(
      country, n = 2, w = K_USD,
      other_level = "Others"
    ),
    country = fct_reorder(country, K_USD)
  ) |>
  group_by(date, country, direction, type) |>
  summarise(K_USD = sum(K_USD), .groups = "drop") |>
  arrange(country, direction, type, date) |>
  group_by(country, direction, type) |>
  mutate(K_USD_YOY = roll_sumr(K_USD, 12)) |>
  ungroup() |>
  na.omit() |>
  right_join(GDP, by = join_by(date)) |>
  mutate(
    pct = (K_USD_YOY / 1000) / gdp_usd
  ) |>
  ggplot(aes(date, pct)) +
  geom_area(aes(fill = country), alpha = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0, 0.5, 0.02),
    labels = percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = c("#197070", "#194470", "midnightblue"),
    labels = c("Other countries", "USA", "Russian Federation")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "External Influences and Vulnerabilities",
    subtitle = "Inflow of Remittances into Armenia to GDP",
    captions = "Data Source: https://www.cba.am/stat/stat_data_eng/5_Money_transfers_of_individuals_by_countries-eng.xlsx\nhttps://armstat.am/am/?nid=202"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(linetype = "dotted")
  )

ggsave(
  "plots/plot_12_money_transfers.png", ggplot2::last_plot(),
  width = 8, height = 6
)
