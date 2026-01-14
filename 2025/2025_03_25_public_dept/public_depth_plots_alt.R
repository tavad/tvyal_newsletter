theme_set(ggthemes::theme_few())

dept_plotter <- function(currency_type){

  plot_output <- 
    dept_clean |> 
    filter(
      indicator %in% c("արտաքին պարտք", "ներքին պարտք" )
    ) |>
    mutate(
      indicator = case_when(
        indicator == "արտաքին պարտք" ~ "external debt",
        indicator == "ներքին պարտք"  ~ "domestic debt",
        TRUE ~ indicator
      )
    ) |> 
    group_by(date, unit_FX) |> 
    mutate(
      pct = value / sum(value, na.rm = TRUE)
    ) |> 
    group_by(year) |> 
    mutate(
      pct_text = percent(pct, accuracy = 0.1),
      value_text = number(value/1000, accuracy = 0.1),
      text = ifelse(
        month == max(month) & year != 2025,
        paste0(value_text, "\n", pct_text),
        NA
      ),
    ) |>
    group_by(unit_FX, indicator) |> 
    mutate(
      text_correcton = ifelse(date == max(date), text, lead(text, 6)),
      text_correcton = ifelse(
        date == (max(date) + days(1) - months(6) - days(1)),
        NA, 
        text_correcton
      ),
      value = value / 1000,
      unit_FX = case_when(
        unit_FX == "մլրդ դրամ" ~ "trillion dram",
        unit_FX == "մլն ԱՄՆ դոլար" ~ "billion USD",
      ),
    ) |> 
    filter(
      unit_FX %in% currency_type
    ) |> 
    ungroup() |> 
    mutate(
      indicator = fct_rev(indicator),
      unit_FX = fct_rev(unit_FX),
    ) |> 
    ggplot(aes(date, value, fill = indicator, label = text_correcton)) +
    geom_area(alpha = 0.8) +
    geom_text(
      position = position_stack(vjust = .5),
      color = "white"
    ) +
    facet_wrap(~unit_FX, scales = "free_y") +
    # facet_grid(unit_FX~indicator, scales = "free_y") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = number_format(accuracy = 1)) +
    scale_fill_manual(values = c("midnightblue", "purple")) +
    labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = "Fiscal Stability and Public Debt",
      caption = "Data Source: https://minfin.am/hy/page/amsakan_vichakagrakan_teghekagrer/"
    )
  
  return(plot_output)
}


dept_plotter("trillion dram") +
  labs(
    subtitle = "Government Debt of the Republic of Armenia\nRatio of domestic and external debt in Armenian dram",
  )

ggsave(
  "plots/plot_08_public_dept.png", ggplot2::last_plot(),
  width = 8, height = 6
)


dept_plotter("billion USD") +
  labs(
    subtitle = "Government Debt of the Republic of Armenia\nRatio of domestic and external debt in foreign currency",
  )


ggsave(
  "plots/plot_09_public_dept.png", ggplot2::last_plot(),
  width = 8, height = 6
)



##############################################



dept_to_GDP |> 
  filter(year <= 2021) |> 
  bind_rows(dept_to_GDP_2) |> 
  filter(year >= 2002) |> 
  mutate(
    labs = percent(dept_to_GDP, accuracy = 0.1)
  ) |> 
  ggplot(aes(year, dept_to_GDP, label = labs)) +
  geom_hline(yintercept = 0.5, color = "purple", linetype = "dotted") +
  geom_hline(yintercept = 0.6, color = "purple", linetype = "dotted") +
  geom_col(color = "midnightblue", alpha = 1) +
  geom_text(vjust = -0.5) +
  geom_text(
    aes(2010, 0.52, label = "Debt / GDP 50% threshold"), 
    color = "#197070", size = 3, hjust = 0
  ) +
  geom_text(
    aes(2010, 0.62, label = "Debt / GDP 60% threshold"), 
    color = "#197070", size = 3, hjust = 0
  ) +
  scale_x_continuous(breaks = seq(2002, 2024, 1)) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1), labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Government Debt to GDP Ratio in Armenia",
    subtitle = "percent",
    caption = "Data Source: https://minfin.am/hy/page/amsakan_vichakagrakan_teghekagrer/\nhttps://armstat.am/am/?nid=202"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(
  "plots/plot_10_public_dept_to_GDP.png", ggplot2::last_plot(),
  width = 8, height = 6
)
