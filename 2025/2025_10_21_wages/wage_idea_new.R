wages_db_clean <- read_csv("~/R/Gcapatker/2024_08_13_unemployment/wages_db_clean.csv")


wages_db_clean |>  # TODO there is a problem with 2 last months investigate
  filter(
    !is.na(wages_cumulative) & !is.na(employees),
    nace_2_code %in% LETTERS,
  ) |> 
  mutate(
    top_wages = ifelse(nace_2_code %in% c("J", "K"), "fin & tech", "other")
  ) |> view()
  group_by(date, top_wages) |> 
  summarise(
    wages = sum(wages_cumulative * employees) / sum(employees),
    .groups = "drop"
  ) |> 
  left_join(inf_cumprod, join_by(date)) |> 
  mutate(
    wages =  wages * adj_price
  ) |> 
  ggplot(aes(date, wages / 1000, color = top_wages)) +
  geom_line(linewidth = 1.5, alpha = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 8) +
  scale_color_manual(
    values = new_palette_colors[c(2,6)], 
    labels = c("Տեղեկատվություն, կապ և ֆինանսական ծառայություններ", "Տնտեսության այլ ճյուղեր")
    
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Հայաստանում առկա է երկու տարբեր տնտեսություն՝\nՏՏ ու բանկային ոլորտը և մնացած ճյուղերը",
    subtitle = "Իրական աշխատավարձերի աճը Հայաստանում\nԱշխատավարձերը ճշգրտված են 2024թ․ գների մակարդակով",
    caption = caption_f(source = "ԱՎԾ")
  )

  
ggsave("~/Downloads/plots/wages_2economy_plot.png", ggplot2::last_plot(), width = 12, height = 8)
