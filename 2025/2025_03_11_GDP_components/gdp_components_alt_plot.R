


theme_set(ggthemes::theme_few())


library(dplyr)

# Load the data (assuming it's already in a tibble called 'data')
# If not, you would need to create it first

# Translation function for column values
translate_armenian_to_english <- function(data) {
  data %>%
    # Translate groupped_arm column
    mutate(
      groupped_arm = case_when(
        groupped_arm == "Պետական կառավարում" ~ "Public Administration",
        groupped_arm == "Հանքագործություն" ~ "Mining",
        groupped_arm == "Գյուղատնտեսություն" ~ "Agriculture",
        groupped_arm == "Էներգիա և ջրամատակարարում" ~ "Energy and Water Supply",
        groupped_arm == "H+I+M+N+P+Q+R+S+T" ~ "H+I+M+N+P+Q+R+S+T", # Keeping as is
        groupped_arm == "Արդյունաբերություն" ~ "Manufacturing",
        groupped_arm == "Տեղեկատվություն և կապ" ~ "Information and Communication",
        groupped_arm == "Շինարարություն" ~ "Construction",
        groupped_arm == "Ֆինանսներ" ~ "Finance",
        groupped_arm == "Անշարժ գույք" ~ "Real Estate",
        groupped_arm == "Առևտուր" ~ "Trade",
        groupped_arm == "Արտադրանքի հարկեր" ~ "Product Taxes",
        groupped_arm == "Ֆինանս.միջնորդության անուղղակի չափվող ծառայություններ" ~ "Financial Intermediation Indirectly Measured Services (FISIM)",
        TRUE ~ groupped_arm
      ),
      
      # Translate labels in annotation column
      annotation = case_when(
        annotation == "O — Պետական կառավարում" ~ "O — Public Administration",
        annotation == "B — Հանքագործություն" ~ "B — Mining",
        annotation == "A — Գյուղատնտեսություն" ~ "A — Agriculture",
        annotation == "D+E — Էներգիա և ջրամատակարարում" ~ "D+E — Energy and Water Supply",
        annotation == "Այլ ծառա-ներ — H+I+M+N+P+Q+R+S+T" ~ "Other Services — H+I+M+N+P+Q+R+S+T",
        annotation == "C — Արդյունաբերություն" ~ "C — Manufacturing",
        annotation == "J — Տեղեկատվություն և կապ" ~ "J — Information and Communication",
        annotation == "F — Շինարարություն" ~ "F — Construction",
        annotation == "K — Ֆինանսներ" ~ "K — Finance",
        annotation == "L — Անշարժ գույք" ~ "L — Real Estate",
        annotation == "G — Առևտուր" ~ "G — Trade",
        annotation == "TA — Արտադրանքի հարկեր" ~ "TA — Product Taxes",
        TRUE ~ as.character(annotation)
      ),
      
      # Translate groupped_codes descriptions
      groupped_codes = case_when(
        groupped_codes == "Այլ\nծառա-ներ" ~ "Other\nServices",
        groupped_codes == "ՀՆԱ\nաճ" ~ "GDP\nGrowth",
        TRUE ~ as.character(groupped_codes)
      ),
      
      # Translate fill_ column
      fill_ = case_when(
        fill_ == "Բացասական" ~ "Negative",
        fill_ == "Դրական" ~ "Positive",
        fill_ == "Ընդհանուր" ~ "Total",
        TRUE ~ fill_
      )
    )
}

# Apply the translati

# Display the translated data
# print(translated_data)


groupping_contributions_annual(
    select_year = 2024, 
    groupped_codes_ = c("H","I","M","N","P","Q","R","S","T")
    # groupped_codes_ = c("H","I","L","M","N","P","Q","S","T")
  ) |> 
  arrange(groupped_codes) |> 
  translate_armenian_to_english() |> 
  mutate(
    groupped_codes = fct_inorder(groupped_codes),
    annotation = fct_inorder(annotation)
  ) |> 
  mutate(
    labels = ifelse(groupped_codes == "GDP\nGrowth", labels, str_remove(labels, "\\%$"))
  ) |>
  ggplot() +
  geom_hline(yintercept = 0, color = "midnightblue") +
  geom_rect(
    aes(xmin = id - 0.45, xmax = id + 0.45,
        ymin = end, ymax = start, fill = fill_, linetype = annotation),
  ) +
  geom_text(aes(x = groupped_codes, y = (end+start)/2, label = labels), color = "white") +
  geom_segment(
    aes(x = id - 0.45, xend = ifelse(id == max(id), id + 0.45, id + 0.45 + 1),
        y = end, yend = end),
    linetype = 1, color = "midnightblue"
  ) +
  scale_y_continuous(labels = percent_format(), n.breaks = 6) +
  # scale_fill_manual(values = new_palette_colors[c(6,2,8)]) + 
  
  scale_fill_manual(values = c("purple",  "midnightblue", "#197070")) +
  
  labs(
    x = NULL,
    y = NULL,
    fill = "Impact:",  # Translation of "Ազդեցություն՝"
    linetype = NULL,
    title = "Key Economic Sectors and their Contributions", 
    subtitle = "Factors Contributing to GDP Growth in 2024\nPercentage Points",  # Translation of "Տոկոսային կետ"
    caption = "FISIM — Financial Intermediation Indirectly Measured Services\n\nData Source: https://www.armstat.am/am/?nid=202"
  )  +
  guides(
    linetype = guide_legend(override.aes = list(fill = "#FFFFFF00"), nrow = 3),
    color = guide_legend(order = 2),
    fill = guide_legend(order = 1),
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.background = element_rect(),
    legend.direction = "horizontal",
    legend.box = "vertical",
  )



ggsave(
  "plots/plot_06_GDP_contributions_2024.png", ggplot2::last_plot(),
  width = 8, height = 8
)



bind_rows(GDP_quarter_pct) |> 
  mutate(
    quarter = str_extract(date, "\\d$") |> as.numeric(),
    quarter_text = paste(ordinal(quarter), "Quarter"),
    quarter_text = ifelse(is.na(quarter), "ՏԱՐԵԿԱՆ", quarter_text),
    date = ifelse(grepl("annual", date), paste(date, "4"), date),
    date = yq(date) + months(3) - days(1),
    year = year(date),
    GDP_growth = GDP_growth * 100
  ) |> 
  filter(
    year >= 2021,
    source %in% c("revised", "annual")
  ) |> 
  complete(nesting(quarter, quarter_text), year) |>
  ggplot(aes(year, GDP_growth, fill = as.factor(year))) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.9, alpha = 1
  ) +
  geom_text(
    aes(
      y = GDP_growth + 0.4, label = percent(GDP_growth/100, accuracy = 0.1),
      color = ifelse(GDP_growth >= 0, "black", "white")
    ),
    position = position_dodge(width = 0.8)
  ) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  scale_fill_manual(values = c("#197070", "#194470", "midnightblue", "purple")) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  facet_wrap(~quarter_text, nrow = 1, strip.position = "bottom") + 
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "GDP Growth Indicator",
    subtitle = "compared to the same quarter of the previous year, %",
    caption = "Data Source: https://www.armstat.am/am/?nid=202"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "gray", linewidth = 0.1,
      linetype = 1
    ),
    axis.text = element_blank(),
    strip.background = element_rect(fill = "#2f4b7c11"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  "plots/plot_07_GDP_quarter.png", ggplot2::last_plot(),
  width = 8, height = 6
)




